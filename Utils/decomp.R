



###### prelude ############
# clear up
rm(list = ls())
sink();sink();sink()

detach_loaded_libraries <- function(){
  # detach loaded libraries
  # except for base pckgs 
  basePckgs <- c("package:stats","package:graphics","package:grDevices",
                 "package:utils","package:datasets","package:methods","package:base")
  pckgs <- search()
  pckgs <- pckgs[grepl("package:",pckgs) & !pckgs %in% basePckgs]
  for(pckg in pckgs){
    try(detach(pckg,character.only = TRUE))  
  }
}
detach_loaded_libraries()


library(igraph)
library(dplyr)
library(tidyr)

########### settings ###########
SETTINGS <- list(
  inputPath = "/Users/moritz/Documents/Research/CovidScience/RProgs/scientometricsRev/data/",
  outputPath = "/Users/moritz/Documents/Research/CovidScience/RProgs/scientometricsRev/src/output/",
  figFile = "Fig2.pdf",
  endOfPeriod = 202212,
  S = 200, # bootstraps
  verbose = TRUE,
  logFile="tmp.txt",
  log=FALSE
)

# logging
if(SETTINGS$log){
  sink(file=paste0(SETTINGS$outputPath,SETTINGS$logFile))
  cat("SETTINGS\n\n")
  print(SETTINGS)
  cat("\n*********************\n\n")
}else{
  sink()
}


######## load functions #########
source("/Users/moritz/Documents/Research/CovidScience/RProgs/scientometricsRev/src/aux_fcts.R")
source("/Users/moritz/Documents/Research/CovidScience/RProgs/scientometricsRev/src/netw_fcts.R")
source("/Users/moritz/Documents/Research/CovidScience/RProgs/scientometricsRev/src/lowrankAdjacency/opt_fcts.R")




sample <- get_country_panel(inputPath=SETTINGS$inputPath)


# set optim parameters
N <- sum(rowSums(A)>0)  
eps=0.0001
alpha <- 0.36 #5/(N+(8*N)^0.5)^0.5
beta <- 0.004 #0.15*N^2 / l1_norm(A)
gamma <- 1/N^0.5

# decompose
decomp <- list()
years <- rbind(c(201901,201912),c(202001,202012),c(202101,202112))
for(i in 1:nrow(years)){
  g <- get_acc_network(sample=sample, covid=NA, start=years[i,1], end=years[i,2]) 
  A <- as_adjacency_matrix(g,type="both",attr="weight") %>% as.matrix()
  decomp[[i]] <- decomp_A(A=log(A+1),eps=eps,alpha=alpha,beta=beta,gamma=gamma,verbose=TRUE)
}

View(decomp[[1]]$L)

library(Matrix)
dim(A)
rankMatrix(decomp[[1]]$L,tol=0.001)
rankMatrix(decomp[[2]]$L)
rankMatrix(decomp[[3]]$L)

####### spinglass #######
require(PBSmapping) # to clip polygons
require(ggthemes) # for theme_map, if desired

library("rnaturalearth")
library("rnaturalearthdata")
library("rgeos")
# community special
get_Q <- function(A, gamma, membership){
  # calculate Q = 1/2m \sum_{ij} ( A_{ij} - gamma (k_i k_j)/2m ) delta_{c_i,c_j},
  # for given adjacency A, and membership vector c
  # - or hamiltonian in Reichhardt/Bornholdt
  # H = - sum_{i neq j} (A_ij - gamma p_ij) delta(sigma_i, sigma_j)
  # INPUT: matrix A, adj
  #        numeric gamma tuning parameter (default = 1)
  #        vector sigma - community memberships
  
  # Sigma
  cluster <- rep(0, times=length(membership))
  Sigma <- cluster %*% t(cluster)
  for(i in 1:max(membership)){
    cluster_i <- cluster
    cluster_i[which(membership==i)] <- 1
    Sigma <- Sigma + cluster_i %*% t(cluster_i)
  }
  
  # A_star - excess observations
  k <- rowSums(A)
  A_star <- A - gamma * (k %*% t(k)) / sum(A)
  Q <- 1/sum(A) * sum(A_star * Sigma)
  
  return(Q)
}

get_VIs <- function(memberships){
  # see https://appliednetsci.springeropen.com/articles/10.1007/s41109-021-00371-w#appendices
  # calculate variational information
  # N countries partitioned in M partitions
  # INPUT: matrix memberships, NxM, set of partitions for countries
  
  N <- nrow(memberships)
  M <- ncol(memberships)
  
  get_VI <- function(X,Y){
    # two member matrices (memberMats)
    p <- 1/N * colSums(X)
    q <- 1/N * colSums(Y)
    
    R <- (t(X) %*% Y) / N
    VI <- - R * (log(R/p) + log(t(t(R)/q)))
    VI[is.nan(VI)] <- 0
    
    return(sum(VI))
  }
  
  # transform each membership into a model matrix - to get dummies
  memberMats <- list()
  for(i in 1:M){
    memberMats[[i]] <- model.matrix(~ members - 1, data = data.frame(members=as.factor(memberships[,i])))
  }
  
  # VI   
  VIs <- matrix(0, nrow=M, ncol=M)
  for(i in 1:(M-1)){
    for(j in (i+1):M){
      VIs[i,j] <- get_VI(X=memberMats[[i]],Y=memberMats[[j]])
    }
  }
  VIs[lower.tri(VIs,diag=FALSE)] <- t(VIs)[lower.tri(VIs,diag=FALSE)]
  
  return(VIs)
}

gL <- graph_from_adjacency_matrix(exp(decomp[[2]]$L)-1,weighted="weight")
gL <- set_vertex_attr(gL,name="countryCode",index=V(gL),value=V(g)$countryCode)
gL <- delete.vertices(gL,v=V(gL)[degree(gL,v=V(gL))==0])
summary(gL)

# search robust clustering
S <- 20
gammas <- seq(from=0.8,to=1.6,by=0.1)
spring_communities <- list()
for(gamma in gammas){
  memberships <- matrix(0, nrow=length(V(g)), ncol=S)
  modularities <- numeric(length=S)
  for(s in 1:S){
    cat("gamma:",gamma,"\t s:",s,"\n")
    sc <- spinglass.community(g, gamma=gamma, spins=20)
    memberships[,s] <- sc$membership
    modularities[s] <- sc$modularity
  }
  spring_communities[[as.character(gamma)]] <- list(memberships=memberships,
                                                    modularities=modularities)
}

meanVI <- numeric(length=length(spring_communities))
meanMod <- numeric(length=length(spring_communities))
for(i in 1:length(spring_communities)){
  VI <- get_VIs(memberships=spring_communities[[i]]$memberships)
  meanVI[i] <- mean(VI[upper.tri(VI)])
  meanMod[i] <- mean(spring_communities[[i]]$modularities)
}
names(meanVI) <- gammas
meanVI
# decide for a robust community clustering
robust_com <- 5
#spring_communities[[robust_com]]$modularities
#par(mfrow=c(1,1))
#plot(meanVI,meanMod,cex=0.1)
#text(meanVI,meanMod,1:length(spring_communities))
# get representing cluster
VI <- get_VIs(spring_communities[[robust_com]]$memberships)
com_rep <- which(rowMeans(VI)==min(rowMeans(VI)))[1]
membership <- spring_communities[[robust_com]]$memberships[,com_rep]
com <- data.frame(countryCode=V(g)$countryCode, membership=membership, 
                  stringsAsFactors=FALSE)



world <- ne_countries(scale = "medium", returnclass = "sf")
world <- merge(world,com,by.x="adm0_a3", by.y="countryCode")

#library(RColorBrewer)
if(length(unique(membership)) > 12){
  mycolors <- colorRampPalette(brewer.pal(12, "Set3"))(length(unique(membership)))
}else{
  mycolors <- brewer.pal(length(unique(membership)), "Set3")
}

world_coms <- ggplot(data = world) +
  geom_sf(aes(fill = factor(membership)), show.legend = TRUE) +
  scale_fill_manual(values = mycolors) + # scale_fill_brewer(palette = "Set1") + #scale_fill_discrete(scale_fill_brewer) +
  labs(title="Communities", fill="Community") + 
  theme(text=element_text(size=16,  family="Helvetica", color="black"),
        plot.title = element_text(size=20),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title.position="panel",
        panel.background = element_rect(fill = NA),
        plot.margin = unit(c(0,0,0,0), "cm"))

world_coms

####### spinglass END #######




rankMatrix(decomp[[1]]$S,tol=0.001)


library(Hmisc)
rcorr(as.vector(decomp[[1]]$L),as.vector(decomp[[2]]$L))
rcorr(as.vector(decomp[[1]]$L),as.vector(decomp[[3]]$L))

par(mfrow=c(1,2))
plot(as.vector(decomp[[1]]$L),as.vector(decomp[[2]]$L))
plot(as.vector(decomp[[1]]$L),as.vector(decomp[[3]]$L))

plot(density(decomp[[3]]$L - decomp[[1]]$L))

summary(g)
colnames(A) <- V(g)$countryCode

View(decomp$L)
View(A)
par(mfrow=c(3,3))
for(i in 1:3){
  plot(density(decomp[[i]]$L),main=i)
  plot(density(decomp[[i]]$S),main=i)
  plot(density(decomp[[i]]$E),main=i)
}

data.frame(countryCode=V(g)$countryCode,
           spikes1=rowSums(decomp[[1]]$E)/N,
           spikes2=rowSums(decomp[[2]]$E)/N,
           spikes3=rowSums(decomp[[3]]$E)/N) %>%
  as_tibble() %>% arrange(spikes1) %>% View()

