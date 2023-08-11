#*********************
# decompose city network as in
# Bao, Michailidis, 2018, Core community structure recovery and phase transtion detection...
# 13.07.2020, MM
#*********************


########## prelude ###########
# clear memory #
rm(list = ls()) ; gc();gc();gc();gc();gc(); ls(); search()

# load fcts
#source("/Users/moritz/Documents/Research/TechnologicalChange/CoronaSci/RProgs/src/lowrankAdjacency/sim_fcts.R")
source("/Users/moritz/Documents/Research/TechnologicalChange/CoronaSci/RProgs/src/lowrankAdjacency/opt_fcts.R")

# dependency
# library(MASS)
library("plot.matrix")
library("Matrix")

#******** read in data
# get all files inside directory
p <- "/Users/moritz/Documents/Research/TechnologicalChange/CoronaSci/RProgs/Resources/network_matrices_1007/"
fileNames <- list.files(path = p,full.names=FALSE,recursive=TRUE)

# get dates of files
netwDates <- sort(as.integer(unique(gsub(".*([[:digit:]]{6}).*", "\\1", fileNames))))

###### START data preparation #####
# start analysis in e.g. 201501
netwDates <- netwDates[netwDates>=201500]

# collect networks
netws <- list()
for(date in netwDates){

  # get nodelist alt: final_collab_aff_nodes
  idx <- grepl("final_collab_aff_nodes",fileNames,fixed=TRUE) & grepl(as.character(date),fileNames,fixed=TRUE)
  nodelist <- read.csv(paste(p,fileNames[idx],sep=""),header=TRUE,sep=",",stringsAsFactors = FALSE,
                       row.names="X")
  idx <- grepl("final_collab_aff_edges",fileNames,fixed=TRUE) & grepl(as.character(date),fileNames,fixed=TRUE)
  # get adjacency
  adj <- read.csv(paste(p,fileNames[idx],sep=""),header=TRUE,sep=",", stringsAsFactors = FALSE,
                       row.names="X")
  adj <- as.matrix(adj)
  netws[[as.character(date)]] <- list(nodelist=nodelist,adj=adj)
}


# create node superset over years
nodelists = list()
for(i in 1:length(netws)){
  nodelists[[i]] <- data.frame(city=netws[[i]]$nodelist[,1],date=as.integer(names(netws)[i]),
                               stringsAsFactors = FALSE)
}
supNodelist <- do.call(rbind.data.frame,nodelists)
supNodelist <- supNodelist[order(supNodelist$city,supNodelist$date,decreasing = FALSE),]

cityStats <- aggregate(supNodelist$date, by=list(city=supNodelist$city), function(x) c(entry=min(x),exit=max(x),periods=length(x)))
supNodelist <- data.frame(city=cityStats$city,cityStats$x,stringsAsFactors = FALSE)


# define focal nodes, e.g. (put aside the issue of entrants and inactive cities for a moment)
focalCityIdx <- supNodelist$entry < 201700 & supNodelist$exit > 202000 & supNodelist$periods>30
focalNodes <- supNodelist[focalCityIdx,]
nrow(focalNodes)

# build adjacency matrices
N <- length(unique(focalNodes$city)) # 185 focal nodes
supAdj <- Matrix(0, nrow=N, ncol=N)
rownames(supAdj) <- focalNodes$city
colnames(supAdj) <- focalNodes$city

supNetws <- list()
supNetws$Nodelist <- focalNodes
supNetws$adjs <- list()

for(i in 1:length(netws)){
  supAdjTmp <- supAdj
  el <- netws[[i]]$adj   
  colnames(el) <- rownames(el)

  idx1 <- colnames(el) %in% focalNodes$city
  for(j in 1:nrow(supAdjTmp)){
    idx2 <- rownames(el) == focalNodes$city[j]
    if(any(idx2)){
      supAdjTmp[j,colnames(el)[idx1]] <- el[idx2,colnames(el)[idx1]]
    }
  }
  supNetws$adjs[[names(netws)[i]]] <- supAdjTmp
}

# aggregate supNetw over years
supNetwsY <- list()
supNetwsY[["Nodelist"]] <- supNetws$Nodelist 
supNetwsY[["adjs"]] <- list()
for(year in as.character(2015:2020)){
  
  months <- which(grepl(year,names(supNetws$adjs)))
  adj <- Matrix(0,nrow=nrow(supNetwsY$Nodelist),ncol=nrow(supNetws$Nodelist))
  for(month in months){
    adj <- adj + supNetws$adjs[[month]]
  }
  supNetwsY[["adjs"]][[year]] <- adj
}

###### END data preparation #####

###### START analysis #######

#******** city entries
par(mfrow=c(1,1))
supNodelist$entryDate <- paste(substr(supNodelist$entry,1,4),"-",substr(supNodelist$entry,5,6),"-01",sep="")
supNodelist$entryDate <- as.Date(supNodelist$entryDate)

idx <- supNodelist$exit>202000
tmp <- table(supNodelist[idx,"entryDate"])
plot(as.Date(names(tmp)),as.integer(tmp),ylab="Entrants",xlab="Date")
# strong entry after covid

#****** plot networks
library(igraph)

# focal cities
someMonths <- c(seq(from=1,to=length(supNetws$adjs),by=10),length(supNetws$adjs))
par(mfrow=c(2,4))
for(month in someMonths){
  print(names(supNetws$adjs[month]))
  g <- graph_from_adjacency_matrix(adjmatrix=supNetws$adjs[[month]], 
                                   mode = "directed", weighted = TRUE,
                                   diag = FALSE)
  print(summary(g))
  plot.igraph(g,vertex.size=0.5,vertex.label=NA,edge.arrow.size=0.1,edge.size=1,
              edge.width=E(g)$weight,main=names(supNetws$adjs[month]))
  
}

# all cities - looks different
someMonths <- c(seq(from=1,to=length(supNetws$adjs),by=10),length(supNetws$adjs))
par(mfrow=c(2,4))
for(month in someMonths){
  print(names(netws))
  
  g <- graph_from_adjacency_matrix(adjmatrix=netws[[month]]$adj, 
                                   mode = "directed", weighted = TRUE,
                                   diag = FALSE)
  print(summary(g))
  plot.igraph(g,vertex.size=0.5,vertex.label=NA,edge.arrow.size=0.1,edge.size=1,
              edge.width=E(g)$weight,main=names(netws)[month])
  
}



#***** yearly decomposition
path <- "/Users/moritz/Documents/Research/TechnologicalChange/CoronaSci/RProgs/src/lowrankAdjacency/output/"
for(year in as.character(2015:2020)){
  #year <- "2015"
  
  # extract and normalize adjacency  
  A <- supNetwsY$adjs[[year]]
  A <- as.matrix(A)
  #diag(A) <- 0
  A[A>1] <- 1
  
  # set optim parameters
  N <- sum(rowSums(A)>0)  
  eps=0.0001
  alpha <- 5/(N+(8*N)^0.5)^0.5
  beta <- 0.15*N^2 / l1_norm(A)
  gamma <- 1/N^0.5
  
  # decompose
  decomp <- decomp_A(A=A,eps=eps,alpha=alpha,beta=beta,gamma=gamma,verbose=TRUE)
  
  # prepare output
  file <- paste(path,"decomp2_",year,".pdf",sep="")
  pdf(file=file,width=15,height=15)
  par(mfrow=c(2,2),oma=c(4,4,4,5))
  
  plot(as.matrix(as.matrix(A)),main=paste("A",year))
  plot(as.matrix(decomp$L),main=paste("L",year))
  plot(as.matrix(decomp$S),main=paste("S",year))
  plot(as.matrix(decomp$E),main=paste("E",year))
  dev.off() 
  
}




2000000/32


############# reste




# decompose supNetw from 2017 to 2020
start <- 201700
end <- 202000

# decompose multiple As and average Ls
eps <- 0.0001
alpha <- NULL #1 #NULL
gamma <- NULL  #NULL

avgL <- matrix(0,nrow=N,ncol=N)
avgA <- avgL
counter <- 0
months <- names(supNetws$adjs)
months <- months[as.integer(months)>start & as.integer(months)<end]
for(month in months){
  #month <- months[1]
  cat("******",month,"*****\n")
  A <- supNetws$adjs[[month]]
  # make symmetric
  #A[A>0] <- 1
  #A <- A%*%t(A)
  A <- as.matrix(A)
  A[upper.tri(A)] <- A[lower.tri(A)]
  diag(A) <- 0
  A <- log(A+1)
  A <- A/max(A)
  avgA <- avgA + A
  
  #idx <- rowSums(A) > 0 | colSums(A) > 0
  #A <- A[idx,idx]
  #avgL[idx,idx] <- avgL[idx,idx] + decomp_A(A=A,eps=eps,alpha=alpha,gamma=gamma,verbose=FALSE)$L
  avgL <- avgL + decomp_A(A=A,eps=eps,alpha=alpha,gamma=gamma,verbose=TRUE)$L
  counter <- counter+1
}
avgL <- avgL / counter
avgA <- avgA / counter

# plot `stable' component
g <- graph_from_adjacency_matrix(adjmatrix=avgL, 
                                 mode = "undirected", weighted = TRUE,
                                 diag = FALSE)

plot.igraph(g,vertex.size=0.5,vertex.label=NA,edge.arrow.size=0.1,edge.size=1,
            edge.width=log(E(g)$weight+1))

g <- graph_from_adjacency_matrix(adjmatrix=as.matrix(avgA), 
                                 mode = "undirected", weighted = TRUE,
                                 diag = FALSE)

plot.igraph(g,vertex.size=0.5,vertex.label=NA,edge.arrow.size=0.1,edge.size=1,
            edge.width=log(E(g)$weight+1))





sum(supNetwsY$adjs[[5]])


# decompose multiple As and average Ls
eps <- 0.0001
alpha <- NULL #1 #NULL
gamma <- NULL  #NULL

avgL <- matrix(0,nrow=N,ncol=N)
avgA <- avgL
counter <- 0
months <- names(supNetws$adjs)
months <- months[as.integer(months)>start & as.integer(months)<end]
for(year in as.character(2015:2019)){
  #month <- months[1]
  year <- "2015"
  cat("******",month,"*****\n")
  A <- supNetwsY$adjs[[year]]
  # make symmetric
  #A[A>0] <- 1
  #A <- A%*%t(A)
  A <- as.matrix(A)
  #A[upper.tri(A)] <- A[lower.tri(A)]
  diag(A) <- 0
  #A <- log(A+1)
  #A <- A/max(A)
  A[A>1] <- 1
  avgA <- avgA + A
  
  N <- sum(rowSums(A)>0)  
  alpha <- 5/(N+(8*N)^0.5)^0.5
  beta <- 0.15*N^2 / l1_norm(A)
  gamma <- 1/N^0.5

  #idx <- rowSums(A) > 0 | colSums(A) > 0
  #A <- A[idx,idx]
  #avgL[idx,idx] <- avgL[idx,idx] + decomp_A(A=A,eps=eps,alpha=alpha,gamma=gamma,verbose=FALSE)$L
  avgL <- avgL + decomp_A(A=A,eps=eps,alpha=alpha,beta=beta,gamma=gamma,verbose=TRUE)$L
  counter <- counter+1
}
avgL <- avgL / counter
avgA <- avgA / counter

# plot `stable' component


summary(as.numeric(A))
hist(A[A>0])

g <- graph_from_adjacency_matrix(adjmatrix=avgL, 
                                 mode = "directed", weighted = TRUE,
                                 diag = FALSE)

plot.igraph(g,vertex.size=0.5,vertex.label=NA,edge.arrow.size=0.1,edge.size=1,
            edge.width=E(g)$weight+0.5)

plot(as.matrix(avgL))

#####


