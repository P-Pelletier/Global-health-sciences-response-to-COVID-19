#***********************
# plot networks
# - s-core network pre-pandemic non-covid
# - community network pre-pandemic covid
# - evolution pre-pandemic to post-pandemic network covid
# MM, 22.12.2020, updated (minor) 21.04.2021
#***********************




########## prelude ###########
# clear memory #
rm(list = ls()) ; gc();gc();gc();gc();gc(); ls(); search()

# load libraries #
library(igraph)
library(network)
library(sna)
library(blockmodeling)

library(ggplot2)
library(cowplot)
library(gridExtra)
library(RColorBrewer)
library(tikzDevice)


setwd('C:/Users/Beta/Documents/GitHub/Global-health-sciences-response-to-COVID-19')
# load functions #
source("Network/aux_fcts.R")
source("Network/netw_fcts.R")

# define paths
outputPath <- "Results"

# read data
sample <- read_data(path='Data')

###### network correlation with QAP over time #######

# result container
months <- sort(unique(sample$nodeActivity$month))
months <- c(201901:201912,202001:202012)

gcor_res <- matrix(NA, nrow=length(months), ncol=2)
rownames(gcor_res) <- as.character(months)
colnames(gcor_res) <- c("r","sig_level")

for(month in months){
  # non-corona
  g1 <- get_acc_network(sample=sample, covid=FALSE, start=month, end=month, 
                            soleAuthored=TRUE) 
  adj1 <- as.matrix(as_adjacency_matrix(graph=g1, type="both", attr="weight"))
  adj1 <- log(adj1+1)
  # corona 
  g2 <- get_acc_network(sample=sample, covid=TRUE, start=month, end=month, 
                        soleAuthored=TRUE) 
  adj2 <- as.matrix(as_adjacency_matrix(graph=g2, type="both", attr="weight"))
  adj2 <- log(adj2+1)
  # join
  g <- array(dim=c(2,nrow(adj1),ncol(adj1)))
  g[1,,] <- adj1
  g[2,,] <- adj2
  # correlate with QAP
  res <- sna::qaptest(g, gcor, reps=10, g1=1,g2=2)
  gcor_res[as.character(month),] <- c(res$testval,res$pgreq)
}

###### nested split graphs and blockmodel pre-covid/covid #######
# result container
adjLst <- list()

#### non-corona, pre-covid network - our reference network
g1 <- get_acc_network(sample=sample, covid=FALSE, start=201901, end=201912, 
                      soleAuthored=TRUE) 
adj1 <- as.matrix(as_adjacency_matrix(graph=g1, type="both", attr="weight"))
adj1 <- log(adj1+1)
rownames(adj1) <- V(g1)$countryCode

# get maximal eigenvalue and assoc. eigenvector
e <- eigen(adj1)
lambda_max <- e$values[1]
x_max <- abs(e$vectors[,1])

# order adjacency by eigenvector centrality
idx <- order(x_max,decreasing=TRUE)
ev_ranks <- data.frame(countryCode=V(g1)$countryCode, x_max)

# save ordered adjacency
adjLst$noncor_precov <- adj1[idx,idx]

#### corona, pre-covid network
g1 <- get_acc_network(sample=sample, covid=TRUE, start=201901, end=201912, 
                      soleAuthored=TRUE) 
adj1 <- as.matrix(as_adjacency_matrix(graph=g1, type="both", attr="weight"))
adj1 <- log(adj1+1)
rownames(adj1) <- V(g1)$countryCode
adjLst$cor_precov <- adj1[idx,idx]

#### corona, covid - same ordering
g <- get_acc_network(sample=sample, covid=TRUE, start=202001, end=202012, 
                      soleAuthored=TRUE) 
adj <- as.matrix(as_adjacency_matrix(graph=g, type="both", attr="weight"))
adj <- log(adj+1)
rownames(adj) <- V(g1)$countryCode
adjLst$cor_cov <- adj[idx,idx]

### block model, corona, pre-covid #####
g1 <- get_acc_network(sample=sample, covid=TRUE, start=201901, end=201912, 
                      soleAuthored=TRUE) 
adj1 <- as.matrix(as_adjacency_matrix(graph=g1, type="both", attr="weight"))
adj1 <- log(adj1+1)
rownames(adj1) <- V(g1)$countryCode
colnames(adj1) <- V(g1)$countryCode
summary(g1)

# Optimizing random chosen partitions with optRandomParC
idx <- rowSums(adj1)>0
adj1 <- adj1[idx,idx]


# that one - oslom gives the same 
res <- optRandomParC(M = adj1, k = 6, rep = 20,
                     approaches = "hom", homFun="ad",
                     blocks = c("com","nul"), seed=4) # reg

# similar
#res <- optRandomParC(M = adj1, k = 6, rep = 20,
#                     approaches = "bin", #regFun="mean",
#                     blocks = c("com","nul"), seed=4) # reg


# reorder clusters manually
clu <- res$best[[1]]$clu
clu2 <- -clu
clu2[clu==3] <- 1
clu2[clu==1] <- 2
clu2[clu==5] <- 3
clu2[clu==6] <- 4
clu2[clu==2] <- 5
clu2[clu==4] <- 6
res$best[[1]]$clu <- clu2
#plot(res)


# save clusters
clusters <- data.frame(countryCode=rownames(adj1), cluster=clu2, stringsAsFactors = FALSE)
clusters <- merge(clusters, ev_ranks, by="countryCode",all.x=TRUE)
clusters <- clusters[order(cbind(clusters$cluster,-clusters$x_max),decreasing=FALSE),]
adjLst$cor_precov_bm <- clusters


####### plotting ######

# create colors
redBlueCols <- rev(RColorBrewer::brewer.pal(9,"RdBu"))
redCols <- redBlueCols[5:9]

oma <- c(0,0,0,0)
oma2 <- c(0.3,0,0,0)

####### corona, pre-covid with inlet (A, B) #######
# pre-covid corona - full picture
adj <- adjLst$cor_precov
colnames(adj) <- rownames(adj)
source <- rownames(adj)
target <- colnames(adj)
cor_precov <- expand.grid(source=source, target=target, stringsAsFactors = FALSE)
cor_precov$weight <- as.vector(adj)
countries <- rownames(adj)
cor_precov$source <- factor(cor_precov$source, levels=countries)
cor_precov$target <- factor(cor_precov$target, levels=countries[length(countries):1])

# limit for selection
clu <- adjLst$cor_precov_bm
clu <- clu[clu$cluster<5,]
lastCountry <- clu$countryCode[clu$x_max==min(clu$x_max)]
which(source==lastCountry)

x_lim <- which(source==lastCountry)
y_lim <- nrow(adj)-which(source==lastCountry)
y_lim2 <- nrow(adj)

coronaPrePlot <- ggplot(cor_precov, aes(source, target)) + 
  geom_tile(aes(fill=weight), show.legend=FALSE) + 
  geom_segment(aes(x = 0, y = y_lim, xend = x_lim, yend = y_lim), colour = "grey", linetype=2) +
  geom_segment(aes(x = x_lim, y = y_lim, xend = x_lim, yend = y_lim2), colour = "grey", linetype=2) +
  scale_fill_gradientn(colors=redCols, limits=c(0,3.1),
                       breaks=c(0,log(1+1),log(2+1),log(5+1),log(10+1),log(20+1)), 
                       labels=c(0,1,2,5,10,20)) + 
  labs(title=element_blank(), y=element_blank(), x=element_blank(), caption = element_blank(), 
       fill="joint papers\n (log-scale)") +
  theme(axis.text.x=element_blank(), axis.ticks.x = element_blank(),  # element_text(angle = 45, hjust = 1, size = rel(0.8)), 
        axis.text.y=element_blank(), axis.ticks.y = element_blank(),
        legend.key.size = unit(0.1*2/5, "npc"), legend.key.width = unit(0.05*1/3, "npc"),
        legend.text = element_text(family="Helvetica", size=6, color="black"),
        legend.title = element_text(family="Helvetica", size=6, color="black"),
        # border
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        plot.margin = unit(oma2, "cm")) 


# corona, pre-covid - block model inlet
cluster <- adjLst$cor_precov_bm
focCountries <- cluster[cluster$cluster<5,"countryCode"]
adj <- adjLst$cor_precov
colnames(adj) <- rownames(adj)
adj <- adj[focCountries,focCountries]

source <- rownames(adj)
target <- colnames(adj)
cor_precov_bm <- expand.grid(source=source, target=target, stringsAsFactors = FALSE)
cor_precov_bm$weight <- as.vector(adj)
countries <- rownames(adj)
cor_precov_bm$source <- factor(cor_precov_bm$source, levels=countries)
cor_precov_bm$target <- factor(cor_precov_bm$target, levels=countries[length(countries):1])

cluster <- cluster[cluster$countryCode %in% focCountries,]
xClu1 <- sum(cluster$cluster <= 1)+0.5
xClu2 <- sum(cluster$cluster <= 2)+0.5
xClu3 <- sum(cluster$cluster <= 3)+0.5
xClu4 <- sum(cluster$cluster <= 4)+0.5
yClu1 <- sum(cluster$cluster > 1)+0.5
yClu2 <- sum(cluster$cluster > 2)+0.5
yClu3 <- sum(cluster$cluster > 3)+0.5
yClu4 <- sum(cluster$cluster > 4)+0.5

coronaPrePlot_Focus <- ggplot(cor_precov_bm, aes(source, target)) + 
  geom_tile(aes(fill=weight), show.legend=TRUE) + 
  scale_fill_gradientn(colors=redCols, limits=c(0,3.1),
                       breaks=c(0,log(1+1),log(2+1),log(5+1),log(10+1),log(20+1)), 
                       labels=c(0,1,2,5,10,20)) + 
  labs(title=element_blank(), y=element_blank(), x=element_blank(), caption = element_blank(), 
       fill="joint papers\n (log-scale)\n") +
  geom_vline(xintercept=xClu1, linetype=1) + 
  geom_vline(xintercept=xClu2, linetype=1) + 
  geom_vline(xintercept=xClu3, linetype=1) + 
  #geom_vline(xintercept=xClu4, linetype=1) + 
  geom_hline(yintercept=yClu1, linetype=1) + 
  geom_hline(yintercept=yClu2, linetype=1) + 
  geom_hline(yintercept=yClu3, linetype=1) + 
  #geom_hline(yintercept=yClu4, linetype=1) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), 
    axis.text.y=element_text(family="Helvetica", size=6, color="black"),
    plot.caption = element_text(hjust = 0.5,family="Helvetica", size=6, color="black"),
    plot.margin = unit(c(0,0,0,0), "pt"),
    panel.border = element_rect(colour = "black", fill=NA, size=1),
    panel.spacing = unit(0, "pt"),
    # legend
    legend.key.size = unit(0.1*2/5, "npc"), legend.key.width = unit(0.05*1/3, "npc"),
    legend.title = element_text(family="Helvetica", size=6, color="black"),
    legend.text = element_text(family="Helvetica", size=6, color="black"),
    legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
    legend.box.spacing = unit(0,"pt"))


# put together pre-covid corona
coronaPre <- ggdraw() +
  draw_plot(coronaPrePlot, x=0, y=0, width=0.7, height=1) +
  draw_plot(coronaPrePlot_Focus, x=0.15, y=0.0, width=0.85, height=0.85) +
  draw_plot_label(label=c("A", "B"),x=c(0, 0.15),y=c(1, 0.9), family="Helvetica", size = 10) 


####### non-corona, pre-covid (C) #####
adj <- adjLst$noncor_precov
adj <- adj[nrow(adj):1,]
source <- row.names(adj)
target <- row.names(adj)
adj_other_pre <- expand.grid(source=source, target=target)
adj_other_pre$weight <- as.vector(t(adj))


noncoronaPre <- ggplot(adj_other_pre, aes(source, target)) + 
  geom_tile(aes(fill=weight), show.legend=TRUE) + 
  scale_fill_gradientn(colors=redCols, limits=c(0,10),
                       breaks=c(0,log(10+1),log(100+1),log(1000+1),log(10000+1)), 
                       labels=c(0,10,100,1000,10000)) + 
  labs(title=element_blank(), y=element_blank(), x=element_blank(),
       caption = element_blank(),
       fill="joint papers\n (log-scale)\n") +
  theme(axis.text.x =element_blank(), axis.ticks.x = element_blank(),  # element_text(angle = 45, hjust = 1, size = rel(0.8)), 
        axis.text.y=element_blank(), axis.ticks.y = element_blank(),
        plot.caption = element_text(hjust = 0.5),
        # legend
        legend.key.size = unit(0.1*2/5, "npc"), legend.key.width = unit(0.05*1/3, "npc"),
        legend.text = element_text(family="Helvetica", size=6, color="black"),
        legend.title = element_text(family="Helvetica", size=6, color="black"),
        # border
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        plot.margin = unit(oma2, "cm"),
        legend.box.spacing = unit(0,"pt")) 


####### corona, covid (D) ####### 
adj <- adjLst$cor_cov
adj <- adj[nrow(adj):1,]
source <- row.names(adj)
target <- row.names(adj)
adj_corona_cov <- expand.grid(source=source, target=target)
adj_corona_cov$weight <- as.vector(t(adj))


coronaCov <- ggplot(adj_corona_cov, aes(source, target)) + 
  geom_tile(aes(fill=weight), show.legend=TRUE) + 
  scale_fill_gradientn(colors=redCols, limits=c(0,6),
        breaks=c(0,log(10+1),log(100+1),log(400)), 
        labels=c(0,10,100,400)) + 
  labs(title=element_blank(), y=element_blank(), x=element_blank(),
       caption = element_blank(),
       fill="joint papers\n (log-scale)\n") +
  theme(axis.text.x =element_blank(), axis.ticks.x = element_blank(),  # element_text(angle = 45, hjust = 1, size = rel(0.8)), 
        axis.text.y=element_blank(), axis.ticks.y = element_blank(),
        plot.caption = element_text(hjust = 0.5),
        # legend
        legend.key.size = unit(0.1*2/5, "npc"), legend.key.width = unit(0.05*1/3, "npc"),
        legend.text = element_text(family="Helvetica", size=6, color="black"),
        legend.title = element_text(family="Helvetica", size=6, color="black"),
        # border
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        plot.margin = unit(oma2, "cm"),
        legend.box.spacing = unit(0,"pt")) 


#######  network correlation (E) ####### 
months <- rownames(gcor_res)
sxd <- scale_x_discrete(breaks=c(months[1],months[7],months[13],months[16]), 
                        labels = c("Jan 2019","Juli 2019", "Jan 2020","April 2020" ))

gcor_res <- data.frame(month=rownames(gcor_res),gcor_res)



cor_lineplot <- ggplot(gcor_res, aes(x=month, y=r, group=1)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept=12.5, linetype=2) +
  scale_y_continuous(limits=c(0,1)) +
  scale_x_discrete(breaks=c(months[1],months[7],months[13],months[16]), 
                 labels = c("Jan 2019","July 2019", "Jan 2020","April 2020" )) +
  labs(title=element_blank(), x=element_blank(), y="corr. coefficient", caption = element_blank()) + 
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 6, color="black"), # rel(0.8), color="black"),
        axis.text.y = element_text(size=6, color="black"),
        axis.title.y = element_text(size=6),
        plot.caption = element_text(hjust = 0.5),
        plot.margin = unit(oma2, "cm"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "grey"))





########  assemble picture  ####### 


allthree <- ggdraw() +
  draw_plot(coronaPre, x=0, y=1/3, width=1/3, height=2/3) +
  draw_plot(noncoronaPre, x=1/3, y=1/3, width=1/3, height=2/3) +
  draw_plot(coronaCov, x=2/3, y=1/3, width=1/3, height=2/3) +
  draw_plot(cor_lineplot, x=0, y=0, width=1, height=1/3) +
  draw_plot_label(label=c("","C", "D", "E"),x=c(0, 1/3, 2/3, 0),y=c(1, 1, 1, 1/3), 
                size = 10)

#filename <- "/Users/moritz/Documents/Research/TechnologicalChange/CoronaSci/COVID_Network_PNAS/networks.pdf"
filename <- "Results_2020/Fig2.pdf"
pdf(file=filename, width=5.7, height=3.5, family="Helvetica", pointsize=6)
allthree
dev.off()


