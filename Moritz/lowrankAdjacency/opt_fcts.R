#*********************
# optimization of network decomposition as in
# Bao, Michailidis, 2018, Core community structure recovery and phase transtion detection...
# 10.07.2020, MM
#*********************

# dependency
library(MASS)

# initialization
init <- function(N){
  # initialization zero matrix for E,S,L,Lambda
  zeroMat <- matrix(0, nrow=N, ncol=N)
  return(zeroMat)
}

# individual update fcts.
update_E <- function(A,S,L,Lambda,alpha,beta){
  # update E in step q
  # A observation, (S,L,Lambda) from last update step q 
  E <- beta / (2*alpha + beta) * (1/beta * Lambda + A - L - S)
  return(E)  
}

S_fct <- function(X,mue){
  # S function, used in update_S and update_L
  X2 <- (abs(X) - mue)
  X2[X2<0] <- 0 # max(X_ij,0)
  return(X2 * sign(X))
}

update_S <- function(A,E,L,Lambda,beta,gamma){
  # update S in step q to q+1
  # A obs., (L,Lambda) from q, E from q+1
  mT <- 1/beta * Lambda + A - L - E
  mue <- gamma / beta
  
  return(S_fct(X=mT,mue=mue))
}

update_L <- function(A,S,E,Lambda,beta,gamma){
  # update L in step q to q+1
  # A obs., Lambda from q, (E,S) from q+1
  Y <- 1/beta*Lambda + A - E - S
  mue <- 1/beta
  # Delta function
  svdY <- svd(Y) # sing.value.decomp.
  U <- svdY$u
  Sigma <- diag(svdY$d)
  Vt <- t(svdY$v)
  L <- U %*% S_fct(X=Sigma,mue=mue) %*% Vt
  return(L)
}
  
update_Lambda <- function(A,L,S,E,Lambda, beta){
  # A obs., (L,S,E) q+1, Lambda q
  Lambda <- Lambda + beta*(A - L - S - E)
  return(Lambda)
}

# norms
frobenius_norm <- function(X){
  return((sum(X^2))^0.5)
}

nuclear_norm <- function(X){ 
  # with * in the paper
  X <- X%*%t(X)
  sigma <- (eigen(X)$values)^2 # square roots of eigenvalues
  return(sum(sigma))
}

l1_norm <- function(X){
  return(sum(abs(X)))
}

# convergence 
update_threshold <- function(L_q,L_q1,S_q,S_q1){
  threshold <- ((frobenius_norm(L_q1 - L_q))^2 + (frobenius_norm(S_q1 - S_q))^2) / 
    ((frobenius_norm(L_q))^2 + (frobenius_norm(S_q))^2 + 1)
  return(threshold)
}

# decomp alg.
decomp_A <- function(A,eps,alpha=NULL,beta=NULL,gamma=NULL,verbose=TRUE,comps=NULL){
  # INPUT: A obs., eps for threshold, alpha/beta/gamma - optional alg. tuning parameters 
  #       verbose, if comps and verbose: comparison sim.vs.alg. offered 
  # OUTPUT: L-stable adj.,S-spark noise,E-dense noise,threshold
  
  # number of nodes
  N <- nrow(A)
  
  # tuning parameters
  if(is.null(alpha)){
    alpha <- 5/(N+(8*N)^0.5)^0.5
  }
  if(is.null(beta)){
    beta <- 0.15*N^2 / l1_norm(A)
  }
  if(is.null(gamma)){
    gamma <- 1/N^0.5
  }
  
  # initialize components
  E_q <- init(N=nrow(A))
  S_q <- init(N=nrow(A))
  L_q <- init(N=nrow(A))
  Lambda_q <- init(N=nrow(A))
  
  # set threshold/stop/counter and update
  thresh <- 1
  stop <- FALSE
  count <- 0
  while(!stop){
    count <- count+1
    # update
    E_q1 <- update_E(A=A,S=S_q,L=L_q,Lambda=Lambda_q,alpha=alpha,beta=beta)
    S_q1 <- update_S(A=A,E=E_q1,L=L_q,Lambda=Lambda_q,beta=beta,gamma=gamma)
    L_q1 <- update_L(A=A,S=S_q1,E=E_q1,Lambda=Lambda_q,beta=beta,gamma=gamma)
    Lambda_q1 <- update_Lambda(A=A,L=L_q1,S=S_q1,E=E_q1,Lambda=Lambda_q,beta=beta)
    
    # check convergence
    thresh <- update_threshold(L_q=L_q,L_q1=L_q1,S_q=S_q,S_q1=S_q1)
    if(thresh < eps | count > 10000000){
      cat("convergence threshold: ",thresh,"\n")
      stop <- TRUE
    }else{
      if(verbose){
        cat("**********",count,"******\n")
        #print("E_q1:")
        #print(E_q1)
        #print("S_q1:")
        #print(S_q1)
        #print("L_q1:")
        #print(L_q1)
        cat("thresh: ",thresh,"\n")
        
        cat("*** matrix norms ****\n")
        cat("L-nuclear norm: ",nuclear_norm(L_q1),"\n")
        cat("S-l1 norm: ",l1_norm(S_q1),"\n")
        cat("E-Frob.norm: ",frobenius_norm(E_q1),"\n")
        if(!is.null(comps)){
          cat("*** compare with simulated ****\n")
          cat("L cor: ",cor(as.numeric(comps$L$L),as.numeric(L_q1)),"\n")
          cat("E cor: ",cor(as.numeric(comps$E),as.numeric(E_q1)),"\n")
          cat("S cor: ",cor(as.numeric(comps$S),as.numeric(S_q1)),"\n")
        }
      }
    }
    
    # roll-over
    E_q <- E_q1
    S_q <- S_q1
    L_q <- L_q1
    Lambda_q <- Lambda_q1
  }
  return(list(L=L_q1,S=S_q1,E=E_q1,thresh=thresh))
}






