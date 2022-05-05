#***************************
# some network functions
#***************************

get_acc_network <- function(sample, covid, start, end, soleAuthored=TRUE){
  # returns igraph network based on the sample (as from read_data())
  # covid - if TRUE returns covid activity, else others
  # start - first month included
  # end - second month included
  # soleAuthored - if TRUE, node attribute all papers sole authored, otherwise all papers overall (including collab papers) 

  # unpack sample
  nodelist <- sample$nodelist
  edgelist <- sample$edgelist
  nodeActivity <- sample$nodeActivity

  # restrict edgelist
  idx <- edgelist$month >= start & edgelist$month <= end
  edgelist <- edgelist[idx,]

  # establish edge weights
  if(covid){
    edgelist$weight <- edgelist$weight_corona
  }
  # restrict to `loaded` edges
  edgelist <- edgelist[edgelist$weight>0,]
  
  # establish node activity
  if(covid){
    if(soleAuthored){
      nodeActivity$activity <- nodeActivity$solePubsCorona
    }else{
      nodeActivity$activity <- nodeActivity$collabPubsCorona
    }
  }else{
    if(soleAuthored){
      nodeActivity$activity <- nodeActivity$solePubs
    }else{
      nodeActivity$activity <- nodeActivity$collabPubs
    }
  }
  
  # aggregate edgelist
  edgelist <- aggregate(edgelist$weight, by=list(sourceId=edgelist$sourceId, targetId=edgelist$targetId),
            function(x) sum(x))
  names(edgelist)[3] <- "weight"
  
  # aggregate nodeActivity
  nodeActivity <- aggregate(nodeActivity$activity, by=list(nodeId=nodeActivity$nodeId),
                        function(x) sum(x))
  names(nodeActivity)[2] <- "activity"
  
  # merge activity to nodelist
  nodelist <- merge(nodelist,nodeActivity,by="nodeId",all.x=TRUE)
  
  # create network
  edgeseq <- as.vector(t(as.matrix(edgelist[,c("sourceId","targetId")])))
  g <- make_empty_graph(n=0,directed=FALSE) %>%
    add_vertices(nrow(nodelist), attr=list(countryCode=nodelist$countryCode, activity=nodelist$activity)) %>%
    add_edges(edgeseq) %>%
    set_edge_attr("weight", value=edgelist$weight)
  
  return(g)
}


add_s_core <- function(g){
  # igraph g with vertex.attribute countryCode
  
  # copy graph to be pruned
  gPruned <- g
  
  # result container
  sCores <- list()
  
  # initialization
  n <- 0
  sCores[[n+1]] <- list(nodes=vertex_attr(gPruned,"countryCode",V(gPruned)), s=0)
  # prune graph
  while(length(V(gPruned))>0){
    # next shell  
    n <- n+1
    strengths <- strength(gPruned)
    s_n <- min(strengths)
    # delete all nodes with strength <= s_n
    while(any(strengths <= s_n)){
      idx <- which(strength(gPruned) <= s_n)
      gPruned <- delete_vertices(gPruned,v=V(gPruned)[idx])
      strengths <- strength(gPruned)
    }
    sCores[[n+1]] <- list(nodes=vertex_attr(gPruned,"countryCode",V(gPruned)), s=s_n)
  }
  # add s-core info to original graph
  V(g)$sCore <- sCores[[1]]$s
  for(i in 1:length(sCores)){
    V(g)$sCore <- ifelse(V(g)$countryCode %in% sCores[[i]]$nodes, sCores[[i]]$s, V(g)$sCore)
  }
  return(g)  
}


communityDetection_RESTService <- function(g, weight=NULL, algorithm, parameters){
  # igraph g, name of edge.attribute used for weight if NULL unweighted
  # char algorithm e.g. "oslom"
  # list parameters, e.g. list('--p_val'="0.4",'--cp'="0.5",'--singlet'=NULL) # NULL has to be checked
  
  # here is the service running
  url <- "http://cdservice.cytoscape.org/cd/communitydetection"
  
  # check server status
  status <- cyrestGET("v1/status", base.url=url)
  if(status$status!="ok"){
    stop(paste("Server status: ",status$status,collapse=""))
  }
  
  # prepare network data for cd
  edgelist <- get.edgelist(g)
  if(!is.null(weight)){
    edgelist <- cbind(edgelist, get.edge.attribute(g,weight,E(g)))
  }
  e1 <- apply(edgelist, 1, function(x) paste(x, collapse="\t"))
  e1 <- paste(paste(e1,collapse="\n"),"\n",sep="",collapse="")
  
  # prepare body
  body <- list(algorithm=algorithm, data=e1, customParameters=parameters) 
  # submit work  
  id <- cyrestPOST("v1", body=body, base.url=url)
  # wait until done
  while(cyrestGET(paste("v1/",id,"/status",sep=""),base.url=url)$status != "complete"){
    Sys.sleep(1)
  }
  cdRes <- cyrestGET(paste("v1/",id,sep=""),base.url=url)
  
  # unpack result
  cdRes <- unlist(strsplit(cdRes$result,";"))
  cdRes <- cdRes[-length(cdRes)]
  cdRes <- strsplit(cdRes,",")
  cdRes <- do.call(rbind, cdRes)
  # relations among communities - later saved as `hierarchy`
  elCom <- cdRes[cdRes[,3]=="c-c",c(1,2)]
  if(class(elCom)=="character"){
    elCom <- matrix(elCom,ncol=2)
  }
  # nodes that give their name to the cluster
  clusterHeads <- sort(unique(c(elCom[,1],elCom[,2])))
  # assign members to clusters  
  elMem <- cdRes[cdRes[,3]=="c-m",]
  clusters <- list()
  for(cl in clusterHeads){
    clusters[[cl]] <- sort(as.integer(unique(elMem[elMem[,1]==cl,2]))) # c(cl,
  }
  # package result
  communities <- list(hierarchy=elCom,members=clusters)
  
  # realize hierarchical structure
  communities$hierarchy <- communities$hierarchy[order(communities$hierarchy[,2],communities$hierarchy[,1]),]
  if(class(communities$hierarchy)=="character"){
    communities$hierarchy <- data.frame(matrix(communities$hierarchy,ncol=2),stringsAsFactors = FALSE)
  }
  for(i in 1:nrow(communities$hierarchy)){
    c1 <- communities$hierarchy[i,1]
    c2 <- communities$hierarchy[i,2]
    communities$members[[c1]] <- sort(unique(c(communities$members[[c1]],communities$members[[c2]])))
  }
  
  # add community size
  csize <- rep(NA,times=length(communities$members))
  for(i in 1:length(communities$members)){
    csize[i] <- length(communities$members[[i]])
  }
  communities$csize <- csize
  
  # replace community names
  cnamesOld <- names(communities$members)
  cnamesNew <- as.character(1:length(communities$members))
  names(communities$members) <- cnamesNew
  for(i in 1:length(cnamesOld)){
    communities$hierarchy[communities$hierarchy==cnamesOld[i]] <- cnamesNew[i] 
  }
  
  return(communities)
}



#### Archived 
# convert adjacency to vector and vice versa
adj2vec <- function(adj, diag=TRUE){
  adj <- as.matrix(adj)
  return(adj[upper.tri(adj,diag=diag)])
}
vec2adj <- function(flatadj,nrow,diag=TRUE){
  adj <- matrix(0,nrow=nrow,ncol=nrow)
  adj[upper.tri(adj,diag=diag)] <- flatadj
  return(adj)
}



