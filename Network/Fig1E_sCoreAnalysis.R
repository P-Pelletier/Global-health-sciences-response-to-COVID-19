#**********************
# Plots s-core decomposition
# MM, 22.12.2020, updated 06.03.2021
#**********************


########## prelude ###########
# clear memory #
rm(list = ls()) ; gc();gc();gc();gc();gc(); ls(); search()

# load libraries #
library(igraph)
library(RColorBrewer)
library(lattice)
library(tikzDevice)

setwd('C:/Users/Beta/Documents/GitHub/Global-health-sciences-response-to-COVID-19')
# load functions #
source("Network/aux_fcts.R")
source("Network/netw_fcts.R")

# read data
sample <- read_data(path = 'Data')

table(sample$nodeActivity$month)



########## calculate s-cores ###########
months <- sort(unique(sample$nodeActivity$month))
months <- c(201901:201912,202001:202012)
sCore <- matrix(NA, nrow=nrow(sample$nodelist), ncol=length(months))
rownames(sCore) <- sample$nodelist$countryCode
colnames(sCore) <- as.character(months)
sCoreCovid <- sCore

for(covid in c(TRUE,FALSE)){
  for(i in 1:length(months)){
    g <- get_acc_network(sample=sample, covid=covid, start=months[i], end=months[i], 
                         soleAuthored=TRUE) # note: soleAuthored or not is irrelevant here
    # add score
    g <- add_s_core(g)
    # normalize sCore from 0 to 1 and add to sCore
    if(covid){
      sCoreCovid[,i] <- V(g)$sCore / max(V(g)$sCore)
    }else{
      sCore[,i] <- V(g)$sCore / max(V(g)$sCore)
    }
  }
}

# order countries by (s-core, strength)
g <- get_acc_network(sample=sample, covid=covid, start=201901, end=202004, 
                     soleAuthored=TRUE) # soleAuthored irrelevant here
orderidx <- order(rowSums(sCore), strength(g,V(g)), decreasing=TRUE)
sCore <- sCore[orderidx,]
sCoreCovid <- sCoreCovid[orderidx,]


######## plot s-cores ########
library(ggplot2)
library(cowplot)
library(gridExtra)


# top M countries
M <- 50

# transform data
thisData <- sCore[M:1,]
countries <- row.names(thisData)
months <- colnames(thisData)
sCore_df <- expand.grid(month=months, country=countries)
sCore_df$sCore <- as.vector(t(thisData))

thisData <- sCoreCovid[M:1,]
countries <- row.names(thisData)
months <- colnames(thisData)
sCoreCovid_df <- expand.grid(month=months, country=countries)
sCoreCovid_df$sCore <- as.vector(t(thisData))

thisData <- sCoreCovid[M:1,] - sCore[M:1,]
countries <- row.names(thisData)
months <- colnames(thisData)
sCoreDiff_df <- expand.grid(month=months, country=countries)
sCoreDiff_df$sCore <- as.vector(t(thisData))


# create colors
redBlueCols <- rev(RColorBrewer::brewer.pal(9,"RdBu"))
redCols <- redBlueCols[5:9]

# common x labels
sxd <- scale_x_discrete(breaks=c(months[1],months[7],months[13],months[16]), 
                 labels = c("Jan 2019","Juli 2019", "Jan 2020","April 2020" )) 

# margins
oma1 <- c(0.5,0,0.5,0)
oma2 <- c(0.5,0,0.5,0)

# one y label
ylabs <- countries
seq1 <- length(countries) - seq(0,M,by=3)
ylabs[seq1] <- paste0(countries[seq1],"  ------------------")
seq2 <- length(countries) - seq(1,M,by=3)
ylabs[seq2] <- paste0(countries[seq2],"  ----------")
syd <- scale_y_discrete(breaks=countries,
                        labels = ylabs)

# non-corona s-core
otherPlot <- ggplot(sCore_df, aes(month, country)) + 
  geom_tile(aes(fill=sCore), show.legend=FALSE) + 
  scale_fill_gradientn(colors=redCols) +
  geom_vline(xintercept=12.5, linetype=2) + 
  labs(title=element_blank(), y=element_blank(), x=element_blank(),
       caption = "s-core in \n non-corona research") +
  sxd + syd +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.8)),
        axis.text.y = element_text(size=rel(0.6)),
        plot.caption = element_text(hjust = 0.5),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        plot.margin = unit(oma1, "cm"))
        
# corona s-core
coronaPlot <- ggplot(sCoreCovid_df, aes(month, country)) + 
  geom_tile(aes(fill=sCore), show.legend=FALSE) + 
  scale_fill_gradientn(colors=redCols) +
  geom_vline(xintercept=12.5, linetype=2) + 
  labs(title=element_blank(), y=element_blank(), x=element_blank(),
       caption = "s-core in \n corona research") +
  sxd + #syd +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.8)), 
         axis.text.y=element_blank(), axis.ticks.y = element_blank(),
        plot.caption = element_text(hjust = 0.5),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        plot.margin = unit(oma2, "cm"))

# difference in s-cores
diffPlot <- ggplot(sCoreDiff_df, aes(month, country)) + 
  geom_tile(aes(fill=sCore)) + 
  scale_fill_gradientn(colors=redBlueCols) + 
  geom_vline(xintercept=12.5, linetype=2) + 
  labs(title=element_blank(), y=element_blank(), x=element_blank(),
       caption = "s-core difference \n (corona - non-corona s-core)",
       fill="s-core\n") +
  #labs(title="difference", y="", x="", fill="s-core\n") +
  sxd + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.8)), 
        axis.text.y=element_blank(), axis.ticks.y = element_blank(), 
        plot.caption = element_text(hjust = 0.5),
        # legend
        legend.key.size = unit(0.875, "cm"), legend.key.width = unit(0.3, "cm"),
        legend.title = element_text(size = rel(0.8)),
        legend.text = element_text(size = rel(0.8)),
        #legend.title = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        plot.margin = unit(oma2, "cm")) 



# plot into tikz
filename <- "Results/sCore_202007.tex"
tikz(file=filename, width=5, height=3.5, engine="pdftex")

#grid.arrange(otherPlot, coronaPlot, diffPlot, legend, widths = c(3, 2, 2.5), nrow = 1)
grid.arrange(otherPlot, coronaPlot, diffPlot, widths = c(3, 2.2, 3), nrow = 1)

dev.off()










