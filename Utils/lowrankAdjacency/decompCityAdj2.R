#*********************
# decompose city network as in
# Bao, Michailidis, 2018, Core community structure recovery and phase transtion detection...
# 13.07.2020, MM, updated 12.11.2020
#*********************



#installed.packages()
#install.packages("xtable")




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
library(countrycode)
library(igraph)

#******** read in data
# get all files inside directory
edgelistFile <- "/Users/moritz/Documents/Research/TechnologicalChange/CoronaSci/RProgs/Resources/countryPubmed/edge_list.csv"
edgelist <- read.csv(file=edgelistFile,header=TRUE,sep=",",stringsAsFactors = FALSE)

# clean up a bit
idx <- edgelist$source %in% c("country","Eswatini","Kosovo","Micronesia") |
  edgelist$target %in% c("country","Eswatini","Kosovo","Micronesia")
edgelist <- edgelist[!idx,]


###### START data preparation #####

countries <- sort(unique(c(edgelist$source,edgelist$target)))
countryIsocodes <- countrycode(countries, origin ='country.name', destination ='iso3c')
countries <- data.frame(countryId=1:length(countries), countryName=countries, countryCode=countryIsocodes, stringsAsFactors = FALSE)
countries$region <- countrycode(countries$countryCode, origin='iso3c', destination='continent')
head(countries)

edgelist <- merge(edgelist, countries[,c("countryName","countryId")], by.x=c("source"), by.y="countryName", all.x=TRUE)
names(edgelist)[names(edgelist)=="countryId"] <- "sourceId" 

edgelist <- merge(edgelist, countries[,c("countryName","countryId")], by.x=c("target"), by.y="countryName", all.x=TRUE)
names(edgelist)[names(edgelist)=="countryId"] <- "targetId" 

head(edgelist)
edgelist <- edgelist[order(edgelist$month, edgelist$source, edgelist$target),]


# start analysis in e.g. 201501
months <- sort(unique(edgelist$month))
# collect networks
netws <- list()
for(month in months){
  
  cEdgelist <- edgelist[edgelist$month==month,]
  cEdges <- as.vector(t(as.matrix(cEdgelist[,c("sourceId","targetId")])))
  
  g <- make_empty_graph() %>%
    add_vertices(nrow(countries), name = countries$countryName, code=countries$countryCode) %>%
    add_edges(cEdges, pubs=cEdgelist$weight, covidPubs=cEdgelist$weight_corona)
  
  netws[[as.character(month)]] <- g
}
length(netws)

# print network 
someMonths <- c(seq(from=1,to=length(netws),by=7),20)

month <- "202001"
g <- netws[[month]]
g <- simplify(g, remove.loops = TRUE)
deg <- degree(g, V(g))
g <- delete.vertices(g,V(g)[deg==0])

regions <- countrycode(V(g)$code, origin="iso3c", destination="continent")
cols <- rainbow(n=length(unique(regions)))
names(cols) = unique(regions)

vlabs <- ifelse(V(g)$code %in% c("USA","GBR","CHN","FRA"),V(g)$code, NA)
plot.igraph(g,vertex.size=log(deg/5+1),vertex.label=vlabs, 
            vertex.color=cols[regions],
            edge.arrow.size=0.1,edge.size=1,
            edge.width=log(E(g)$pubs+1),main=month)
legend("topleft", legend=names(cols), pch=1, col=cols)

######


#***** yearly decomposition
path <- "/Users/moritz/Documents/Research/TechnologicalChange/CoronaSci/RProgs/src/lowrankAdjacency/output2/"
for(month in names(netws)){

  # extract and normalize adjacency  
  g <- netws[[month]]
  A <- get.adjacency(g=g,type="both",attr="pubs")
  A <- as.matrix(A)
  diag(A) <- 0
  
  #adj[adj>1] <- 1
  
  # set optim parameters
  N <- sum(rowSums(A)>0)  
  eps=0.0001
  alpha <- 5/(N+(8*N)^0.5)^0.5
  beta <- 0.15*N^2 / l1_norm(A)
  gamma <- 1/N^0.5
  
  # decompose
  decomp <- decomp_A(A=log(A+1),eps=eps,alpha=alpha,beta=beta,gamma=gamma,verbose=TRUE)
  
  str(decomp)
  dim(decomp$L)  
  # prepare output
  file <- paste(path,"decomp2_",year,".pdf",sep="")
  pdf(file=file,width=15,height=15)
  par(mfrow=c(2,2),oma=c(4,4,4,5))
  
  idx <- rowSums(A) < 100 | colSums(A) < 100
  A <- A[!idx,!idx]
  
  plot(as.matrix(as.matrix(log(A[!idx,!idx]+1))),main=paste("A",month))
  plot(as.matrix(decomp$L[!idx,!idx]),main=paste("L",month))
  plot(as.matrix(decomp$S[!idx,!idx]),main=paste("S",month))
  plot(as.matrix(decomp$E[!idx,!idx]),main=paste("E",month))
  dev.off() 
  
}


### plot L
exp(2)
par(mfrow=c(1,1))
L <- decomp$L
L[L<2] <- 0
g <- graph.adjacency(L,mode="undirected", weighted=TRUE, diag=FALSE)
summary(g)
nrow(countries)
dim(decomp$L)

degs <- strength(g,V(g))
degs <- data.frame(country=countries$countryName,code=countries$countryCode,region=countries$region, 
                   degree=degs, stringsAsFactors = FALSE)

degs <- degs[order(degs$degree, decreasing=TRUE),]
head(degs)
#g <- delete.vertices(g,V(g)[deg==0])
#cbind(V(g)$code,regions)

isolates <- which(degs$degree==0)

cols <- rainbow(n=length(unique(countries$region)))
names(cols) = unique(sort(countries$region))

vlabs <- ifelse(countries$countryCode %in% c("USA","GBR","CHN","FRA"),countries$countryCode, NA)


l <- layout.circle(g)

l <- layout_with_graphopt(g, start = NULL, niter = 500,
                          charge = 0.001, mass = 30, spring.length = 0,
                          spring.constant = 1, max.sa.movement = 5)

l <- layout_with_fr(g, weights = E(g)$weight/1000, grid="auto")

l <- layout_with_kk(g, maxiter = 50 *vcount(g), epsilon = 0, kkconst = vcount(g)/10,
               weights = 8-E(g)$weight)


lec <- cluster_leading_eigen(g)

install.packages("RedeR")


if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("RedeR")
library(RedeR)
rdp <- RedPort()
calld(rdp)

data(ER.deg)
str(ER.deg)
dt <- ER.deg$dat
head(dt)
sg <- ER.deg$ceg
summary(sg)

head(countries)
g <- set_vertex_attr(g,name="name",index=V(g),value=countries$countryCode)
sg <- att.mapv(g,countries,refcol=1)

addGraph(rdp,sg)


hc <- hclust(dist(get.adjacency(sg,attr="weight")))
str(hc)


library("linkcomm")
idx <- edgelist$month == "202001" & edgelist$sourceId < edgelist$targetId
tmp <- edgelist[idx,c("source","target","weight")]

tmp <- edgelist[idx,c("source","target","weight")]
tmp$weight <- log(tmp$weight+1)
dim(tmp)


lc <- getLinkCommunities(network=tmp, hcmethod = "average",
                         check.duplicates=TRUE)
plot(lc, type = "dend")

getAllNestedComm(lc, plot=TRUE)

plot(lc, type = "graph", clusterids = c(62,63))


tmp <- degs[1:40,]
tmp <- tmp[order(tmp$region),]
plot(lc, type="members", nodes=tmp$country)

degs[1:20,"country"]


print(lc)


nesthc(rdp, hc, cutlevel=3, nmemb=5, cex=0.3, labels=V(sg)$name)
mergeOutEdges(rdp,nlev=2)

relax(rdp,50,400)
plot(hc)

plot.igraph(g, layout=l,
            vertex.size=log(deg/5+1),vertex.label=vlabs, 
            vertex.color=cols[regions],
            edge.arrow.size=0.1,edge.size=1,
            edge.width=(E(g)$weight/mean(E(g)$weight))^2,
            mark.groups = lec,
            #axes=TRUE, xlim=c(-0.5,0.2), ylim=c(-0.7,0),
            main=month)
legend("topleft", legend=names(cols), pch=1, col=cols)

hc<-hclust(dist(get.adjacency(g)))
plot(hc)

str(lec)
length(lec)
lec[[120]]

membership(lec)

plot(density(as.vector(L[L>0])))
table(as.vector(decomp$L)>2)


par(mfrow=c(2,2))
for(month in someMonths){
  print(names(supNetws$adjs[month]))
  g <- graph_from_adjacency_matrix(adjmatrix=supNetws$adjs[[month]], 
                                   mode = "directed", weighted = TRUE,
                                   diag = FALSE)
  print(summary(g))
  plot.igraph(g,vertex.size=0.5,vertex.label=NA,edge.arrow.size=0.1,edge.size=1,
              edge.width=E(g)$weight,main=names(supNetws$adjs[month]))
  
}





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


