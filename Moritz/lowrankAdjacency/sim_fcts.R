#*********************
# simulates the network as in
# Bao, Michailidis, 2018, Core community structure recovery and phase transtion detection...
# 10.07.2020, MM
#*********************


#******************
# help fcts
get_lowRankL <- function(N, K, r_in, r_out){
  
  # community membership vector - everybody belongs to one or no community
  comMem <- sample.int(n=K+1, size=N, replace =TRUE ) - 1
  
  # get member-community interaction
  U <- matrix(0, nrow = N, ncol=K)
  
  for(i in 1:N){
    for(j in 1:K){
      if(comMem[i]==j){
        U[i,j] <- runif(n=1,min=r_in,max=1)
      }else{
        U[i,j] <- runif(n=1,min=0,max=r_out)
      }
    }
  }
  
  # construct L
  L <- U%*%t(U)
  
  return(list(L=L, comMem=comMem))  
}

get_sparseNoise <- function(N,ps){
  S1 <- matrix(sample(c(0,1),size=N*N, replace=TRUE, prob=c(1-ps,ps)),nrow=N,ncol=N)
  S2 <- matrix(runif(n=N*N,min=-1,max=1),nrow=N,ncol=N)
  S <- S1*S2
  return(S)  
}

#******************

get_L <- function(M,N,K,r_in,r_out){
  # INPUT: M stable periods, N nodes, K communities, 
  #   r_in min. interaction among community members,
  #   r_out max. interaction among non-members
  # OUTPUT: list L, with one L for each stable phase
  
  # create common low rank components for M stable periods
  L <- list()
  for(m in 1:M){
    L[[m]] <- get_lowRankL(N, K, r_in, r_out)
  }
  return(L)
}

get_A <- function(N,mTimes,K,r_in,r_out,ps=0.1,r){
  # INPUT: N nodes, numeric vector mTimes with names (1..T) and entry=number of stable period m
  # K communities each period
  # r_in min. activity among community member, r_max max activity among non-members
  # ps sparse noise - probability that ij is subject to sparse noise ~ unif(-1,1)
  # r dense noise - ij ~ unif(-r,r)
  # create A over time
  # OUTPUT: A and components
  
  # unpack
  Times <- length(mTimes)
  
  # get stable adj. L for each stable phase m
  L <- get_L(M=length(unique(mTimes)),N=N,K=K,r_in=r_in,r_out=r_out)
  
  # result container
  comps <- list()
  for(t in 1:Times){
    
    m <- mTimes[t] # identify stable phase m
    S <- get_sparseNoise(N,ps) # create sparse noise - spikes
    E <- matrix(runif(n=N*N, min=-r, max=r), nrow=N, ncol=N) # create dense noise
    # construct A
    A <- L[[m]][[1]] + S + E 
    # respect 0 <= A_ij <= 1
    A[A<0] <- 0
    A[A>1] <- 1
    # collect
    comps[[t]] <- list(A=A,L=L[[m]],S=S,E=E)
  }
  return(comps)
}




