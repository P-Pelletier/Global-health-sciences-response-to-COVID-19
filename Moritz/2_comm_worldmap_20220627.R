#**************************************************************
# Creates the `network convergence' figure, Fig. 3, in the paper
# MM, last update 27.06.2022
#**************************************************************


#***********************
# plot networks
# - s-core network pre-pandemic non-covid
# - community network pre-pandemic covid
# - evolution pre-pandemic to post-pandemic network covid
#***********************


########## prelude ###########
# clear memory #
rm(list = ls()) ; gc();gc();gc();gc();gc(); ls(); search()

# load libraries # order sna/igraph plays a role for graph manipulations
library(sna)
library(igraph)

library(reshape2)
library(ggmap)
library(ggplot2)
library(scales)
library(cowplot)
library(gridExtra)
library(RColorBrewer)
library(tikzDevice)

library(dplyr)
##############
library(PBSmapping) # to clip polygons
require(ggthemes) # for theme_map, if desired

library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)

#detach("package:igraph")
#search()
#library(network)
#library(sna)
#library(blockmodeling)
#library(graphlayouts) 
#library(ggraph)
#library(threejs)
#library(RColorBrewer)
#library(RCy3)
#library(extrafont)
#library(vegan)
#library(perturbR)
#library(modMax)
############################


########### settings ###########
SETTINGS <- list(
  inputPath = "G:/Github/Global-health-sciences-response-to-COVID-19/data/",
  outputPath = "G:/Github/Global-health-sciences-response-to-COVID-19/Results",
  #figFile = "netwConvergence.pdf",
  #endOfPeriod = 202012,
  S = 200, # bootstraps
  verbose = TRUE,
  logFile="netw_convergence_log.txt"
)

# logging

sink(file=paste0(SETTINGS$outputPath,SETTINGS$logFile))
cat("SETTINGS\n\n")
print(SETTINGS)
cat("\n*********************\n\n")



########### load functions #########
source("G:/Github/Global-health-sciences-response-to-COVID-19/Moritz/aux_fcts.R")
source("G:/Github/Global-health-sciences-response-to-COVID-19/Moritz/netw_fcts.R")

# plot functions
get_community_plot <- function(g, gamma, title="Community"){
  
  # extract adj.
  adj <- as.matrix(as_adjacency_matrix(graph=g, type="both", attr="weight"))
  rownames(adj) <- V(g)$countryCode
  colnames(adj) <- V(g)$countryCode
  
  # get cluster
  membership <- V(g)$cluster
  #names(membership) <- V(g)$countryCode
  #sort(membership)
  
  #fc <- cluster_fast_greedy(g)
  
  # transform membership to correspond to average degree in 2019 non-CRR network
  g_noncrr <- get_acc_network(sample=sample, covid=FALSE, start=201901, end=201912, # t_end 
                                    soleAuthored=FALSE) 
  if(all(V(g)$countryCode==V(g_noncrr)$countryCode)){
    communities <- aggregate(strength(g_noncrr), by=list(community = membership), function(x) mean(x))
    communities <- communities[order(communities$x,decreasing=TRUE),]
    communities$newCommunity <- 1:nrow(communities)
    communities <- communities[order(communities$community,decreasing=FALSE),]
    community_conc <- communities$newCommunity
    names(community_conc) <- communities$community
  }

  # rename membership vector
  membership <- community_conc[as.character(membership)]
  #names(membership) <- V(g)$countryCode
  #sort(membership)
  
  # calculate `excess' weights
  k <- rowSums(adj)
  adj <- adj - gamma * (k %*% t(k)) / sum(adj)
  adj[adj<0] <- 0

  #for(i in 1:12){
  #  print(i)
  #  print(sum(rowSums(adj[membership==i,membership==i])))
  #}  

  
  # order by cluster and degree
  idx_x <- order(membership,-strength(g),decreasing=TRUE)
  idx_y <- order(-membership,strength(g),decreasing=TRUE)
  adj_fc <- adj[idx_x,idx_y]
  # reshape from wide to long format
  rownames(adj_fc) <- NULL
  colnames(adj_fc) <- NULL
  long_adj_fc <- melt(adj_fc)
  #long_adj_fc$value <- log(long_adj_fc$value+1)
  
  com_size <- table(membership)
  com_size <- com_size[com_size>1]
  cum_com_size <- cumsum(com_size)
  cum_com_size_y <- c(length(membership),length(membership) - cum_com_size,0)
  cum_com_size_x <- c(0,cum_com_size,length(membership))
  
  mylog <- trans_new("mylog",transform=function(x) log(x+1), inverse=function(x) exp(x)-1)
  
  axes_breaks <- c(1,50,100,150,200)
  fill_breaks <- c(1,10,50)
  if(max(long_adj_fc$value) > 100){
    fill_breaks <- c(1,10,100,500)
  }
  
  oma2 <- c(1,1,1,1)
  
  fc_plot <- ggplot(long_adj_fc) + 
    geom_raster(aes(x = Var2, y = Var1, fill=value)) + 
    scale_fill_gradient(low="grey100", high="red4", trans=mylog, 
                        breaks=fill_breaks) +
    labs(title=title,fill=quote(w-E(w))) + 
    scale_x_continuous(name="Country",breaks=axes_breaks,labels=as.character(axes_breaks),
                       expand = c(0,0)) +
    scale_y_continuous(name="Country",breaks=axes_breaks,labels=as.character(axes_breaks[length(axes_breaks):1]),
                       expand = c(0,0)) +
    theme(text=element_text(size=6,  family="Helvetica", color="black"),
          title = element_text(size=10),
          plot.title = element_text(size=12),
          axis.text.x = element_text(angle = 0, hjust = 0.5), 
          plot.title.position="panel",
          plot.margin = unit(oma2, "cm"),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(colour = "grey"))
  
  for(i in 1:length(cum_com_size)){
    df_h <- data.frame(x1 = cum_com_size_x[i], x2 = cum_com_size_x[i+2], 
                       y1 = cum_com_size_y[i+1], y2 = cum_com_size_y[i+1])
    df_v <- data.frame(x1 = cum_com_size_x[i+1], x2 = cum_com_size_x[i+1], 
                       y1 = cum_com_size_y[i], y2 = cum_com_size_y[i+2])
    
    if(i==length(cum_com_size)){
      df_h <- data.frame(x1 = cum_com_size_x[i], x2 = cum_com_size_x[i+1], 
                         y1 = cum_com_size_y[i+1], y2 = cum_com_size_y[i+1])
      df_v <- data.frame(x1 = cum_com_size_x[i+1], x2 = cum_com_size_x[i+1], 
                         y1 = cum_com_size_y[i], y2 = cum_com_size_y[i+1])
      
    }
    
    fc_plot <- fc_plot + 
      geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), data=df_h) +
      geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), data=df_v)
  }
  return(fc_plot)  
}

get_nested_plot <- function(g, title="Nested hierarchy"){
  
  # extract adjacency
  adj <- as.matrix(as_adjacency_matrix(graph=g, type="both", attr="weight"))
  
  # do eigenvector decomp.
  e <- eigen(adj)
  lambda_max <- Mod(e$values[1])
  x_max <- abs(e$vectors[,1])
  
  # order by ev
  idx_x <- order(x_max,decreasing=TRUE)
  idx_y <- order(x_max,decreasing=FALSE)
  adj_ev <- adj[idx_y,idx_x]
  
  # create longitudinal matrix  
  long_adj_ev <- melt(adj_ev)
  
  # define log-transform
  mylog <- trans_new("mylog",transform=function(x) log(x+1), inverse=function(x) exp(x)-1)
  
  
  axes_breaks <- c(1,50,100,150,200)
  fill_breaks <- c(1,10,20,50)
  if(max(long_adj_ev$value) > 100){
    fill_breaks <- c(1,10,100,500)
    if(max(long_adj_ev$value) > 1000){
      fill_breaks <- c(1,10,100,1000)
    }
  }
  
  oma2 <- c(1,1,1,1)
  
  # do plot  
  nested_plot <- ggplot(long_adj_ev) + 
    geom_raster(aes(x = Var2, y = Var1, fill=value)) + 
    scale_fill_gradient(low="grey100", high="red4", trans=mylog, 
                        breaks=fill_breaks) +
    labs(title=title,fill=quote(w)) + 
    scale_x_continuous(name="Country",breaks=axes_breaks,labels=as.character(axes_breaks),
                       expand = c(0,0)) +
    scale_y_continuous(name="Country",breaks=axes_breaks,labels=as.character(axes_breaks[length(axes_breaks):1]),
                       expand = c(0,0)) +
    theme(text=element_text(size=6,  family="Helvetica", color="black"),
          title = element_text(size=10),
          plot.title = element_text(size=12),
          axis.text.x = element_text(angle = 0, hjust = 0.5), 
          plot.title.position="panel",
          plot.margin = unit(oma2, "cm"),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(colour = "grey"))
  
  
  return(nested_plot)
}

get_cor_plot <- function(res_mat){
  
  # add month
  res_mat <- cbind(month=1:nrow(res_mat),res_mat)
  res_mat <- as.data.frame(res_mat)
  
  if(nrow(res_mat) > 24){
    xbreaks <- c(1,7,13,19,24,29)
    xlabels <- c("Jan 2019","July 2019", "Jan 2020","July 2020","Dec. 2020","May 2021")
  }else{
    xbreaks <- c(1,7,13,19,24)
    xlabels <- c("Jan 2019","July 2019", "Jan 2020","July 2020","Dec. 2020")
  }
  
  oma2 <- c(1,1,1,1)
  
  # plot it
  cor_lineplot <- ggplot(res_mat, aes(x=month, y=r, group=1)) +
    geom_line() +
    geom_point() +
    geom_vline(xintercept=12.5, linetype=2) +
    scale_y_continuous(limits=c(0,1)) +
    scale_x_discrete(limits=as.character(1:nrow(res_mat)),breaks=xbreaks,labels=xlabels) +
    #scale_x_continuous(breaks=xbreaks,labels=xlabels) + #,limits=c(0.5,nrow(res_mat)+0.5)) +
    labs(title=element_blank(), x=element_blank(), y="corr. coefficient", caption = element_blank()) + 
    theme(text=element_text(size=6,  family="Helvetica", color="black"),
          title = element_text(size=10),
          plot.title = element_text(size=12),
          axis.text.x = element_text(angle = 0, hjust = 0.5), 
          plot.title.position="panel",
          plot.margin = unit(oma2, "cm"),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(colour = "grey"))

  return(cor_lineplot)
}

get_lambda_q_plot <- function(res_mat){
  
  oma2 <- c(1,1,1,1)
  
  # add month
  res_mat <- cbind(month=1:nrow(res_mat),res_mat)
  res_mat <- as.data.frame(res_mat)
  
  if(nrow(res_mat) > 24){
    xbreaks <- c(1,7,13,19,24,29)
    xlabels <- c("Jan 2019","July 2019", "Jan 2020","July 2020","Dec. 2020","May 2021")
  }else{
    xbreaks <- c(1,7,13,19,24)
    xlabels <- c("Jan 2019","July 2019", "Jan 2020","July 2020","Dec. 2020")
  }
  
  
  qlambda_lineplot <- ggplot(res_mat) +
    geom_line(aes(x=month, y=z_q, colour="z_q")) +
    geom_point(aes(x=month, y=z_q, colour="z_q")) +
    geom_line(aes(x=month, y=z_lambda, colour="z_lambda")) +
    geom_point(aes(x=month, y=z_lambda, colour="z_lambda")) +
    geom_vline(xintercept=12.5, linetype=2) +
    scale_x_discrete(limits=as.character(1:nrow(res_mat)),breaks=xbreaks,labels=xlabels) +
    scale_colour_manual("", 
                        breaks = c("z_q", "z_lambda"),
                        values = c("z_q"="blue", "z_lambda"="red"),
                        labels = c(bquote(z[q]),bquote(z[lambda]))) +
    labs(title=element_blank(), x=element_blank(), y=bquote(z[q]~z[lambda]), caption = element_blank()) + 
    theme(text=element_text(size=6,  family="Helvetica", color="black"),
          title = element_text(size=10),
          plot.title = element_text(size=12),
          axis.text.x = element_text(angle = 0, hjust = 0.5), 
          plot.title.position="panel",
          plot.margin = unit(oma2, "cm"),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(colour = "grey"),
          legend.position=c(0.1,0.3),
          #legend.text = element_text(size=10),
          legend.key = element_rect(colour = NA, fill = NA),
          legend.background=element_rect(fill = "white"))
    
  
  return(qlambda_lineplot)
}


get_community_plot_DEPRICATED <- function(g){
  
  # extract adj.
  adj <- as.matrix(as_adjacency_matrix(graph=g, type="both", attr="weight"))
  rownames(adj) <- V(g)$countryCode
  colnames(adj) <- V(g)$countryCode
  
  # get cluster
  fc <- cluster_fast_greedy(g)
  
  # transform membership to correspond to average degree
  communities <- aggregate(strength(g), by=list(community = fc$membership), function(x) mean(x))
  communities <- communities[order(communities$x,decreasing=TRUE),]
  communities$newCommunity <- 1:nrow(communities)
  communities <- communities[order(communities$community,decreasing=FALSE),]
  
  # rename membership vector
  membership <- communities[fc$membership,"newCommunity"]
  
  # order by cluster and degree
  idx_x <- order(membership,-strength(g),decreasing=TRUE)
  idx_y <- order(-membership,strength(g),decreasing=TRUE)
  
  adj_fc <- adj[idx_x,idx_y]
  long_adj_fc <- melt(adj_fc)
  long_adj_fc$value <- log(long_adj_fc$value+1)
  
  com_size <- table(membership)
  com_size <- com_size[com_size>1]
  cum_com_size <- cumsum(com_size)
  cum_com_size_y <- c(length(membership),length(membership) - cum_com_size,0)
  cum_com_size_x <- c(0,cum_com_size,length(membership))
  
  fc_plot <- ggplot(long_adj_fc, aes(x = Var2, y = Var1)) + 
    geom_raster(aes(fill=value)) + 
    scale_fill_gradient(low="grey100", high="red4") +
    labs(x="country", y="country", title="community") +
    theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                       axis.text.y=element_text(size=9),
                       plot.title=element_text(size=11))
  
  for(i in 1:length(cum_com_size)){
    df_h <- data.frame(x1 = cum_com_size_x[i], x2 = cum_com_size_x[i+2], y1 = cum_com_size_y[i+1], y2 = cum_com_size_y[i+1])
    df_v <- data.frame(x1 = cum_com_size_x[i+1], x2 = cum_com_size_x[i+1], y1 = cum_com_size_y[i], y2 = cum_com_size_y[i+2])
    
    fc_plot <- fc_plot + 
      geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), data=df_h) +
      geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), data=df_v)
  }
  return(fc_plot)  
}

get_qzr_plot_DEPRICATED <- function(res_mat){
  # generates line plot  
  oma2 <- c(1,1,1,1)
  
  # add month
  res_mat <- cbind(month=1:nrow(res_mat),res_mat)
  res_mat <- as.data.frame(res_mat)
  
  res_mat$lambda_z <- ifelse(res_mat$lambda_z>0,log(abs(res_mat$lambda_z)+1),-log(abs(res_mat$lambda_z)+1))
  res_mat$q_z <- ifelse(res_mat$q_z>0,log(abs(res_mat$q_z)+1),-log(abs(res_mat$q_z)+1))
  
  cor_lineplot <- ggplot(res_mat) +
    geom_line(aes(x=month, y=q_z, colour="z_q")) +
    geom_point(aes(x=month, y=q_z, colour="z_q")) +
    geom_line(aes(x=month, y=lambda_z, colour="z_lambda")) +
    geom_point(aes(x=month, y=lambda_z, colour="z_lambda")) +
    geom_vline(xintercept=12.5, linetype=2) +
    scale_y_continuous(limits=c(-4,7)) +
    scale_x_discrete(limits=as.character(1:24),breaks=c(1,7,13,19,24), 
                     labels = c("Jan 2019","July 2019", "Jan 2020","July 2020","Dec. 2020" )) +
    scale_colour_manual("", 
                        breaks = c("z_q", "z_lambda"),
                        values = c("z_q"="blue", "z_lambda"="red"),
                        labels = c(bquote(z[q]),bquote(z[lambda]))) +
    labs(title=element_blank(), x=element_blank(), y=bquote(z[q]~z[lambda]), caption = element_blank()) + 
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 6, color="black"), # rel(0.8), color="black"),
          axis.text.y = element_text(size=6, color="black"),
          axis.title.y = element_text(size=12),
          plot.caption = element_text(hjust = 0.5),
          legend.text = element_text(colour="black",size=12),
          legend.position=c(0.15,0.85),
          plot.margin = unit(oma2, "cm"),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(colour = "grey"))
  
  return(cor_lineplot)
}

# stats functions
get_z_q <- function(sample, t, S, countryClusters, gamma){
  # Input: data.frame sample as obtained form read_data,
  #        integer end_period, analysis period -> [201901:t_end] 
  #        integer S, number of resamples from the Null
  # OUTPUT: num. vector with: q,z_q, lambda, z_lambda (for that t_end)
  
  # result container
  stats <- c(q=NA, z_q=NA)
  
  # resampling function
  get_null_distr <- function(sample, S, W, gamma, memberships){
    # re-sampling from non-CRR to calculate z values under the null
    # INPUT: as above + W=sum(adj1) - total weight of resampled matrix
    # OUTPUT: matrix with cols (q, lambda) and S rows
    
    # result container
    qs <- numeric(length=S)

    # data
    g2 <- get_acc_network(sample=sample, covid=FALSE, start=201901, end=201912, 
                          soleAuthored=FALSE) 
    adj2 <- as.matrix(as_adjacency_matrix(graph=g2, type="both", attr="weight"))
    
    for(s in 1:S){
      
      # get random draw
      adj2_upper <- adj2[upper.tri(adj2,diag=FALSE)] # to get probabilities
      draw <- sample.int(n=length(adj2_upper), size=as.integer(W/2), prob=adj2_upper, replace=TRUE)
      draw <- table(draw) # number of draws per position in upper adjacency
      adj2_draw <- matrix(0, nrow=nrow(adj2), ncol=ncol(adj2)) 
      adj2_draw[which(upper.tri(adj2_draw,diag=FALSE))[as.integer(names(draw))]] <- as.integer(draw) # enter draws in upper adj.
      adj2_draw[lower.tri(adj2_draw,diag=FALSE)] <- t(adj2_draw)[lower.tri(adj2_draw,diag=FALSE)] # mirror into lower adj.

      # q
      qs[s] <- get_Q(A=adj2_draw, gamma=gamma, membership=memberships)
    }
    return(qs)
  }
  
  #### calculate statistics on observed CRR network 
  g1 <- get_acc_network(sample=sample, covid=TRUE, start=201901, end=t, 
                        soleAuthored=FALSE) 
  v_idx <- V(g1)
  names(v_idx) <- V(g1)$countryCode
  g1 <- set.vertex.attribute(g1, "cluster", v_idx[countryClusters[,"countryCode"]],
                             countryClusters[,"cluster"])
  adj1 <- as.matrix(as_adjacency_matrix(graph=g1, type="both", attr="weight"))
  stats["q"] <- get_Q(A=adj1, gamma=gamma, membership=V(g1)$cluster)
  
  #### re-sampling from the null (same structure as non-CRR network) 
  null_dist <- get_null_distr(sample=sample, S=S, W=sum(adj1), gamma=gamma, memberships=V(g1)$cluster)
  
  # calculate z-values based on null distribution
  stats["z_q"] <- (stats["q"] - mean(null_dist)) / sd(null_dist)
  
  cat("t : ",t, ",\t",
      "q : ",stats["q"],",\t",
      "z_q : ",stats["z_q"],",\n")

  return(stats)
}

# stats functions
get_z_lambda <- function(sample, t, S){
  # Input: data.frame sample as obtained form read_data,
  #        integer end_period, analysis period -> [201901:t_end] 
  #        integer S, number of resamples from the Null
  # OUTPUT: num. vector with: q,z_q, lambda, z_lambda (for that t_end)
  
  # result container
  stats <- c(lambda=NA, z_lambda=NA)
  
  # resampling function
  get_null_distr <- function(sample, t, S, W){
    # re-sampling from non-CRR to calculate z values under the null
    # INPUT: as above + W=sum(adj1) - total weight of resampled matrix
    # OUTPUT: matrix with cols (q, lambda) and S rows
    
    # result container
    lambdas <- numeric(length=S)
    
    # data
    g2 <- get_acc_network(sample=sample, covid=FALSE, start=201901, end=201912, 
                          soleAuthored=FALSE) 
    adj2 <- as.matrix(as_adjacency_matrix(graph=g2, type="both", attr="weight"))
    
    
    for(s in 1:S){
      
      # get random draw
      adj2_upper <- adj2[upper.tri(adj2,diag=FALSE)] # to get probabilities
      draw <- sample.int(n=length(adj2_upper), size=as.integer(W/2), prob=adj2_upper, replace=TRUE)
      draw <- table(draw) # number of draws per position in upper adjacency
      adj2_draw <- matrix(0, nrow=nrow(adj2), ncol=ncol(adj2)) 
      adj2_draw[which(upper.tri(adj2_draw,diag=FALSE))[as.integer(names(draw))]] <- as.integer(draw) # enter draws in upper adj.
      adj2_draw[lower.tri(adj2_draw,diag=FALSE)] <- t(adj2_draw)[lower.tri(adj2_draw,diag=FALSE)] # mirror into lower adj.
      
      # normalize
      #adj2_draw <- log(adj2_draw+1)
      #adj2_draw <- adj2_draw/sum(adj2_draw)
      #g_ran_norm <- graph_from_adjacency_matrix(adj2_draw,mode="undirected",weighted=TRUE)
      
      # lambda 
      e <- eigen(adj2_draw)
      lambdas[s] <- Mod(e$values[1])
    }
    return(lambdas)
  }
  
  #### CRR network TRUE
  g1 <- get_acc_network(sample=sample, covid=TRUE, start=201901, end=t, 
                        soleAuthored=FALSE) 
  adj1 <- as.matrix(as_adjacency_matrix(graph=g1, type="both", attr="weight"))
  # normalized, observed CRR network
  
  #### calculate statistics on observed CRR network 
  # get lambda (largest eigenvalue)
  e <- eigen(adj1)
  stats["lambda"] <- Mod(e$values[1])
  
  #### re-sampling from the null (same structure as non-CRR network) 
  null_dist <- get_null_distr(sample=sample, t=t, S=S, W=sum(adj1))
  
  # calculate z-values based on null distribution
  stats["z_lambda"] <- (stats["lambda"] - mean(null_dist)) / sd(null_dist)

  cat("t : ",t, ",\t",
      "lambda : ",stats["lambda"],",\t",
      "z_lambda : ",stats["z_lambda"],",\n")
  
  return(stats)
}

# stats functions
get_qz_stat_DEPRICATED <- function(sample, t_end, S){
  # Input: data.frame sample as obtained form read_data,
  #        integer end_period, analysis period -> [201901:t_end] 
  #        integer S, number of resamples from the Null
  # OUTPUT: num. vector with: q,z_q, lambda, z_lambda (for that t_end)
  
  # result container
  stats <- c(q=NA, z_q=NA, lambda=NA, z_lambda=NA)
  
  # resampling function
  get_null_distr <- function(sample, t_end, S, W){
    # re-sampling from non-CRR to calculate z values under the null
    # INPUT: as above + W=sum(adj1) - total weight of resampled matrix
    # OUTPUT: matrix with cols (q, lambda) and S rows
    
    # result container
    res <- matrix(NA, nrow=S, ncol=2)
    colnames(res) <- c("q","lambda")
    
    # data
    g2 <- get_acc_network(sample=sample, covid=FALSE, start=201901, end=201912, # t_end 
                          soleAuthored=FALSE) 
    adj2 <- as.matrix(as_adjacency_matrix(graph=g2, type="both", attr="weight"))
    
    for(s in 1:S){
      
      # get random draw
      adj2_upper <- adj2[upper.tri(adj2,diag=FALSE)] # to get probabilities
      draw <- sample.int(n=length(adj2_upper), size=as.integer(W/2), prob=adj2_upper, replace=TRUE)
      draw <- table(draw) # number of draws per position in upper adjacency
      adj2_draw <- matrix(0, nrow=nrow(adj2), ncol=ncol(adj2)) 
      adj2_draw[which(upper.tri(adj2_draw,diag=FALSE))[as.integer(names(draw))]] <- as.integer(draw) # enter draws in upper adj.
      adj2_draw[lower.tri(adj2_draw,diag=FALSE)] <- t(adj2_draw)[lower.tri(adj2_draw,diag=FALSE)] # mirror into lower adj.
      
      # normalize
      #adj2_draw <- log(adj2_draw+1)
      #adj2_draw <- adj2_draw/sum(adj2_draw)
      g_ran_norm <- graph_from_adjacency_matrix(adj2_draw,mode="undirected",weighted=TRUE)
      
      # lambda 
      e <- eigen(adj2_draw)
      res[s,"lambda"] <- Mod(e$values[1])
      
      # q
      fc <- cluster_fast_greedy(g_ran_norm)
      res[s,"q"] <- max(fc$modularity)
    }
    return(res)
  }
  
  #### CRR network TRUE
  g1 <- get_acc_network(sample=sample, covid=TRUE, start=201901, end=t_end, 
                        soleAuthored=FALSE) 
  adj1 <- as.matrix(as_adjacency_matrix(graph=g1, type="both", attr="weight"))
  # normalized, observed CRR network
  adj1_norm <- adj1
  #adj1_norm <- log(adj1+1)
  #adj1_norm <- adj1_norm/sum(adj1_norm)
  g1_norm <- graph_from_adjacency_matrix(adj1_norm,mode="undirected",weighted=TRUE)
  
  #### calculate statistics on observed CRR network 
  # get lambda (largest eigenvalue)
  e <- eigen(adj1_norm)
  stats["lambda"] <- Mod(e$values[1])
  # get q
  fc <- cluster_fast_greedy(g1_norm)
  stats["q"] <- max(fc$modularity)
  
  #### re-sampling from the null (same structure as non-CRR network) 
  null_dist <- get_null_distr(sample=sample, t_end=t_end, S=S, W=sum(adj1))
  
  # calculate z-values based on null distribution
  stats["z_lambda"] <- (stats["lambda"] - mean(null_dist[,"lambda"])) / sd(null_dist[,"lambda"])
  stats["z_q"] <- (stats["q"] - mean(null_dist[,"q"])) / sd(null_dist[,"q"])
  
  cat("t_end : ",t_end, ",\t",
      "lambda : ",stats["lambda"],",\t",
      "z_lambda : ",stats["z_lambda"],",\t",
      "q : ",stats["q"],",\t",
      "z_q : ",stats["z_q"],",\n")
  
  return(stats)
}

get_cor_stat <- function(sample, t_end, S){
  
  # non-corona network
  g1 <- get_acc_network(sample=sample, covid=FALSE, start=201901, end=201912, 
                        soleAuthored=TRUE) 
  adj1 <- as.matrix(as_adjacency_matrix(graph=g1, type="both", attr="weight"))
  adj1 <- log(adj1+1)
  
  # corona network
  g2 <- get_acc_network(sample=sample, covid=TRUE, start=201901, end=t_end, 
                        soleAuthored=TRUE) 
  adj2 <- as.matrix(as_adjacency_matrix(graph=g2, type="both", attr="weight"))
  adj2 <- log(adj2+1)
  
  # join networks
  g <- array(dim=c(2,nrow(adj1),ncol(adj1)))
  g[1,,] <- adj1
  g[2,,] <- adj2
  
  # correlate with QAP
  res <- sna::qaptest(g, gcor, reps=S, g1=1, g2=2)
  
  # return
  return(c(r=res$testval,r_sig=res$pgreq))
}

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



########### load data #####
# read data
inputPath <-  "Data/"
sample <- read_pubmed(inputPath=SETTINGS$inputPath)


########### network communities ##############
# get communities from non-CRR 2019
g <- get_acc_network(sample=sample, covid=FALSE, start=201901, end=201912) 
deg_noncrr_2019 <- data.frame(countryCode=V(g)$countryCode, degree=degree(g,v=V(g)),
                              stringsAsFactors = FALSE)

# remove isolates
g <- delete.vertices(g,v=V(g)[degree(g,v=V(g))==0])

# search robust clustering
S <- 100
gammas <- seq(from=0.8,to=1.6,by=0.1)
spring_communities <- list()
for(gamma in gammas){
  memberships <- matrix(0, nrow=length(V(g)), ncol=S)
  for(s in 1:S){
    cat("gamma:",gamma,"\t s:",s,"\n")
    sc <- spinglass.community(g, gamma=gamma, spins=20)
    memberships[,s] <- sc$membership
  }
  spring_communities[[as.character(gamma)]] <- memberships
}

#save(spring_communities, file=paste0(SETTINGS$outputPath,"spring_communities.RData"))
load(paste0(SETTINGS$outputPath,"spring_communities.RData"))
meanVI <- numeric(length=length(spring_communities))
for(i in 1:length(spring_communities)){
  VI <- get_VIs(memberships=spring_communities[[i]])
  meanVI[i] <- mean(VI[upper.tri(VI)])
}
names(meanVI) <- gammas
# decide for a robust community clustering
robust_com <- 5

# get representing cluster
VI <- get_VIs(spring_communities[[robust_com]])
com_rep <- which(rowMeans(VI)==min(rowMeans(VI)))[1]
membership <- spring_communities[[robust_com]][,com_rep]
countryClusters <- data.frame(countryCode=V(g)$countryCode, cluster=membership, 
                              stringsAsFactors=FALSE)

# order country clusters by average degree
v_idx <- V(g)
names(v_idx) <- V(g)$countryCode
g <- set.vertex.attribute(g, "cluster", v_idx[countryClusters[,"countryCode"]], countryClusters[,"cluster"])

communities <- aggregate(strength(g), by=list(community = V(g)$cluster), function(x) mean(x))
communities <- communities[order(communities$x,decreasing=TRUE),]
communities$newCommunity <- 1:nrow(communities)
communities <- communities[order(communities$community,decreasing=FALSE),]
community_conc <- communities$newCommunity
names(community_conc) <- communities$community
countryClusters$cluster <- community_conc[as.character(countryClusters$cluster)]

table(countryClusters$cluster)

#### plot cluster ######

com_201912 <- get_community(g)

world <- ne_countries(scale = "medium", returnclass = "sf")
world <- merge(world,com_201912,by.x="adm0_a3", by.y="countryCode")

colorset <- brewer.pal(8, "Set1")

crr_world <- ggplot(data = world) +
  geom_sf(aes(fill = factor(membership)), show.legend = TRUE) +
  scale_discrete_manual(aesthetics="fill", values=colorset) +
  labs(title="Communities in non-CRR, 2019", fill="Community") + 
  theme(text=element_text(size=16,  family="Helvetica", color="black"),
        plot.title = element_text(size=20),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title.position="panel",
        panel.background = element_rect(fill = NA),
        plot.margin = unit(c(0,0,0,0), "cm"))
  

### same plot for CRR
# get communities from non-CRR 2019
g <- get_acc_network(sample=sample, covid=FALSE, start=201901, end=201912, # t_end 
                     soleAuthored=FALSE) 
deg_noncrr_2019 <- data.frame(countryCode=V(g)$countryCode, degree=degree(g,v=V(g)),
                              stringsAsFactors = FALSE)

# remove isolates
g <- delete.vertices(g,v=V(g)[degree(g,v=V(g))==0])
sc <- spinglass.community(g, gamma=gammas[robust_com], spins=20)

# plot cluster
countryClusters_crr <- data.frame(countryCode=V(g)$countryCode, cluster=sc$membership, 
                              stringsAsFactors=FALSE)
world <- ne_countries(scale = "medium", returnclass = "sf")
world <- merge(world,countryClusters_crr,by.x="adm0_a3", by.y="countryCode")

non_crr_world <- ggplot(data = world) +
  geom_sf(aes(fill = factor(cluster)), show.legend = FALSE) +
  scale_discrete_manual(aesthetics="fill", values=colorset[c(2,3,5,4,7,1,8,6)]) +
  labs(title="Communities in CRR, 2020") + 
  theme(text=element_text(size=20,  family="Helvetica", color="black"),
        plot.title = element_text(size=20),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title.position="panel",
        panel.background = element_rect(fill = NA),
        plot.margin = unit(oma2, "cm"))

# compare clusters
community_conc <- c(2,3,5,4,7,1,8,6)
countryClusters_crr$cluster_new <- NA
for(i in 1:length(community_conc)){
  idx <- countryClusters_crr$cluster==i
  countryClusters_crr$cluster_new[idx] <- community_conc[i]
}

countryClusters_joined <- merge(countryClusters,countryClusters_crr,by="countryCode")
countryClusters_joined <- countryClusters_joined[order(countryClusters_joined$cluster.x),]  
countryClusters_joined[countryClusters_joined$cluster.x!=countryClusters_joined$cluster_new,]
# perfect match... DJI does not belong to any community in either network.

# plot one clustering only - both are the same
outputFile <- paste0(SETTINGS$outputPath,"communities_worldmap.pdf")
pdf(file=outputFile, width=10, height=7, family="Helvetica")#, pointsize=2)
crr_world + labs(title=NULL)
dev.off()

# print cluster members
for(i in sort(unique(countryClusters$cluster))){
  cat("cluster",i,":\t")
  idx <- countryClusters$cluster==i
  cat(paste0(countrycode(countryClusters$countryCode[idx],origin="iso3c",
                         destination="country.name"),collapse=", "),"\n")
}

# add isolates to countryClusters
isolates <- deg_noncrr_2019[deg_noncrr_2019$degree==0,]
names(isolates)[2] <- "cluster" 
isolates[,"cluster"] <- (max(countryClusters[,"cluster"])+1):(max(countryClusters[,"cluster"])+nrow(isolates))
countryClusters <- rbind.data.frame(countryClusters,isolates)


########### collect stats ############# 
# define month sequence (i.t.o. ints)
ts <- c(201901:201912,202001:202012,202101:202105)


# result container
res_mat <- matrix(NA,nrow=length(ts),ncol=6)
colnames(res_mat) <- c("q","z_q","lambda","z_lambda","r", "r_sig")

for(i in 1:length(ts)){
  print(ts[i])
  
  res_mat[i,c("q","z_q")] <- get_z_q(sample=sample, t=ts[i], S=SETTINGS$S, 
          countryClusters=countryClusters, gamma=gammas[robust_com])
  res_mat[i,c("lambda","z_lambda")] <- get_z_lambda(sample=sample, t=ts[i], S=SETTINGS$S)
  res_mat[i,c("r","r_sig")] <- get_cor_stat(sample,t_end=ts[i],S=SETTINGS$S)
}

# save(res_mat,file=paste0(SETTINGS$outputPath,"res_mat.RData"))
load(paste0(SETTINGS$outputPath,"res_mat.RData"))
#res_mat

#### extra: collect z_q for each cluster only #####
# define month sequence (i.t.o. ints)
ts <- c(201901:201912,202001:202012,202101:202105)

nbClusters <- length(unique(countryClusters$cluster))
z_q_clevel <- matrix(NA, nrow=length(ts), ncol=nbClusters)
for(i in 1:length(ts)){
  print(i)
  for(cl in 1:7){
    
    cc_tmp <- countryClusters
    idx <- cc_tmp$cluster==cl
    cc_tmp[!idx,"cluster"] <- 100:(99+sum(!idx))
    
    # for each country cluster individually
    z_q_clevel[i,cl] <- get_z_q(sample=sample, t=ts[i], S=50, 
                                       countryClusters=cc_tmp, gamma=gammas[robust_com])[2]
    
  }
}



########### plotting ####
# plot colors
redBlueCols <- rev(RColorBrewer::brewer.pal(9,"RdBu"))
redCols <- redBlueCols[5:9]

##### get individual plots
plots <- list()
# line plots
plots[["lambda_q_plot"]] <- get_lambda_q_plot(res_mat[1:24,])
plots[["cor_plot"]] <- get_cor_plot(res_mat[1:24,])

# community, hierarchy plots for different months
g <- get_acc_network(sample=sample, covid=TRUE, start=201901, end=201912, soleAuthored=FALSE) 
v_idx <- V(g)
names(v_idx) <- V(g)$countryCode
g <- set.vertex.attribute(g, "cluster", v_idx[countryClusters[,"countryCode"]], countryClusters[,"cluster"])
plots[["com_201912"]] <- get_community_plot(g,gamma=1.2,title="Community, Dec. 2019")
plots[["nest_201912"]] <- get_nested_plot(g, "Nested hierarchy, Dec. 2019")

g <- get_acc_network(sample=sample, covid=TRUE, start=201901, end=202002, soleAuthored=FALSE) 
v_idx <- V(g)
names(v_idx) <- V(g)$countryCode
g <- set.vertex.attribute(g, "cluster", v_idx[countryClusters[,"countryCode"]], countryClusters[,"cluster"])
plots[["com_202002"]] <- get_community_plot(g, gamma=1.2, title="Community, Feb. 2020")
plots[["nest_202002"]] <- get_nested_plot(g, "Nested hierarchy, Feb. 2020")

g <- get_acc_network(sample=sample, covid=TRUE, start=201901, end=202012, soleAuthored=FALSE) 
v_idx <- V(g)
names(v_idx) <- V(g)$countryCode
g <- set.vertex.attribute(g, "cluster", v_idx[countryClusters[,"countryCode"]], countryClusters[,"cluster"])
plots[["com_202012"]] <- get_community_plot(g,gamma=1.2, title="Communities, Dec. 2020")
plots[["nest_202012"]] <- get_nested_plot(g, "Nested hierarchy, Dec. 2020")

# overwrite theme in all plots - to fit with pdf
oma2 <- c(1,1,1,1)
for(i in 1:length(plots)){
  plots[[i]] <- plots[[i]] +  
    theme(text=element_text(size=20,  family="Helvetica", color="black"),
          title = element_text(size=20),
          plot.title = element_text(size=20),
          axis.text.x = element_text(angle = 0, hjust = 0.5), 
          plot.title.position="panel",
          plot.margin = unit(oma2, "cm"),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(colour = "grey"))
}


# put plots together
pdf_width <- 15
pdf_height <- 15
wh <- pdf_width / pdf_height * 0.8

all_plots <- ggdraw() +
  draw_plot(plots[["nest_201912"]], x=0, y=0, width=1/3, height=1/3*wh) +
  draw_plot(plots[["nest_202002"]], x=1/3, y=0, width=1/3, height=1/3*wh) +
  draw_plot(plots[["nest_202012"]], x=2/3, y=0, width=1/3, height=1/3*wh) +
  draw_plot(plots[["com_201912"]], x=0, y=1/3*wh, width=1/3, height=1/3*wh) +
  draw_plot(plots[["com_202002"]], x=1/3, y=1/3*wh, width=1/3, height=1/3*wh) +
  draw_plot(plots[["com_202012"]], x=2/3, y=1/3*wh, width=1/3, height=1/3*wh) + 
  draw_plot(plots[["lambda_q_plot"]], x=0, y=2/3*wh, width=1, height=(1-2/3*wh)/2) + 
  draw_plot(plots[["cor_plot"]], x=0, y=1/3*wh+1/2, width=1, height=(1-2/3*wh)/2) +
  draw_plot_label(label="A",x = 0, y=1, family="Helvetica", size=20) +
  draw_plot_label(label="B",x = 0, y=1/3*wh+1/2, family="Helvetica", size=20) +
  draw_plot_label(label="C",x = 0, y=2/3*wh, family="Helvetica", size=20)

# write out plot
outputFile <- paste0(SETTINGS$outputPath,SETTINGS$figFile)
pdf(file=outputFile, width=pdf_width, height=pdf_height, family="Helvetica")
all_plots
dev.off()



########### plotting extended analysis ########
# plot colors

##### get individual plots
plots <- list()
# line plots
plots[["lambda_q_plot"]] <- get_lambda_q_plot(res_mat[1:29,])
plots[["cor_plot"]] <- get_cor_plot(res_mat[1:29,])

oma2 <- c(1,1,1,1)
for(i in 1:length(plots)){
  plots[[i]] <- plots[[i]] +  
    theme(text=element_text(size=20,  family="Helvetica", color="black"),
          title = element_text(size=20),
          plot.title = element_text(size=20),
          axis.text.x = element_text(angle = 0, hjust = 0.5), 
          plot.title.position="panel",
          plot.margin = unit(oma2, "cm"),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(colour = "grey"),
          legend.background=element_rect(fill = NA))
}

# put plots together
pdf_width <- 12
pdf_height <- 6

all_plots <- ggdraw() +
  draw_plot(plots[["lambda_q_plot"]], x=0, y=0, width=1, height=1/2) + 
  draw_plot(plots[["cor_plot"]], x=0, y=1/2, width=1, height=1/2) +
  draw_plot_label(label="A",x = 0, y=1, family="Helvetica", size=20) +
  draw_plot_label(label="B",x = 0, y=1/2, family="Helvetica", size=20) 
  
# write out plot
outputFile <- paste0(SETTINGS$outputPath,"Fig3_extended.pdf")
pdf(file=outputFile, width=pdf_width, height=pdf_height, family="Helvetica")
all_plots
dev.off()






