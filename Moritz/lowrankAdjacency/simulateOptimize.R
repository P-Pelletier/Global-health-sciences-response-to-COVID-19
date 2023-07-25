#*********************
# simulate a network and optimization of network decomposition as in
# Bao, Michailidis, 2018, Core community structure recovery and phase transtion detection...
# 13.07.2020, MM
#*********************


########## prelude ###########
# clear memory #
rm(list = ls()) ; gc();gc();gc();gc();gc(); ls(); search()

# load fcts
source("/Users/moritz/Documents/Research/CovidScience/RProgs/src/lowrankAdjacency/sim_fcts.R")
source("/Users/moritz/Documents/Research/CovidScience/RProgs/src/lowrankAdjacency/opt_fcts.R")

# dependency
# library(MASS)
library('plot.matrix')


#**********************
# SETTINGS
#**********************

# seed
set.seed(5)

# SIMULATION
# number of time periods
Times <- 7

# number of stable periods (mTimes knows which phase we are in at t, how many phases there are)
# breaking periods, last entry always Inf
breaks <-  c(5,Inf) # NULL #c(5)
mTimes <- rep(1,times=Times)
if(!is.null(breaks)){
  m <- 1
  for(t in 1:Times){
    mTimes[t] <- m
    if(t == breaks[m]){
      m <- m+1
    }
  }
}

# number of nodes
N <- 150
# number of communities
K <- 5
# minimum activity, (r_in, 1) for community members
r_in <- 0.4
# maximum activity, (r_out, 1) for community members
r_out <- 0.6
# sparse noise - probability that you get it
ps <- 0.1
# dense noise
r <- 0.1

# optimization parameter eps (stopping crit.)
eps <- 0.01

# simulate networks
comps <- get_A(N=N,mTimes=mTimes,K=K,r_in=r_in,r_out=r_out,ps=ps,r=r)

# decompose one matrix A
t <- 1
decomp <- decomp_A(A=comps[[t]]$A,eps=eps,verbose=TRUE,comps=comps[[t]])

# decompose multiple As and average Ls
avgL <- matrix(0,nrow=N,ncol=N)
counter <- 0
for(t in 1:breaks[1]){
  avgL <- avgL + decomp_A(A=comps[[t]]$A,eps=eps,verbose=FALSE,comps=comps[[t]])$L
  counter <- counter+1
}
avgL <- avgL / counter


#### investigate matrices
# correlate
cor(as.numeric(avgL),as.numeric(comps[[1]]$L$L))

# plot
par(mar=c(5.1, 4.1, 4.1, 4.1),mfrow=c(2,1)) # adapt margins

plot(comps[[1]]$L$L)
plot(avgL)




