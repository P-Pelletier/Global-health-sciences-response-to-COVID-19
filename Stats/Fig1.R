# clear memory #
rm(list = ls()) ; gc();gc();gc();gc();gc(); ls(); search()

# load libraries #
library(igraph)
library(RColorBrewer)
library(lattice)
library(tikzDevice)
library(ggplot2)
library(cowplot)
library(magick)
library(scales) 
library(zoo)
library(countrycode)
library(reshape2)
library(reldist)

setwd("D:/Github/Global-health-sciences-response-to-COVID-19")

dataa = read.csv(file="Data/Data_2020/fig1a.csv",
                 header=TRUE,sep=",",stringsAsFactors = FALSE)
datab = read.csv(file="Data/Data_2020/fig1b.csv",
                 header=TRUE,sep=",",stringsAsFactors = FALSE)
datac = read.csv(file="Data/Data_2020/fig1c.csv",
                 header=TRUE,sep=",",stringsAsFactors = FALSE)
datapareto_share = read.csv(file="Data/Data_2020/pareto_share.csv",
                 header=TRUE,sep=",",stringsAsFactors = FALSE)
datapareto = read.csv(file="Data/Data_2020/pareto.csv",
                      header=TRUE,sep=",",stringsAsFactors = FALSE)

# plot 1 ####

months = unique(dataa$month)

# xlabel
sxd <- scale_x_discrete(breaks=c(months[1],months[7],months[13],months[19],months[24]),##,months[29]), 
                        labels = c("Jan 2019","July 2019", "Jan 2020","July 2020","Dec 2020"))##, "May 2021" )) 

# margin size
oma2 <- c(0.3,0,0,0)

#figa

figa = ggplot(dataa, aes(month)) + 
  geom_line(size=1.25,aes(y = Coronavirus, colour = "Coronavirus",group = 1)) + 
  geom_line(size=1.25,aes(y = non_Coronavirus, colour = "non_Coronavirus",group = 1))+
  # covid19 treshold
  geom_vline(xintercept = 12,linetype="dashed") +
  scale_color_manual(values=c('#ff7f0e','#1f77b4'))+
  labs(title=element_blank(), y="Number of papers", x=element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        axis.text.y = element_text(size = 6),
        axis.title.x = element_text(size = 6),
        axis.title.y = element_text(size = 6),
        panel.border = element_rect(colour = "black", fill=NA, size=0.75),
        plot.margin = unit(oma2, "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.key=element_blank(),
        legend.title=element_blank(),
        legend.position="top",
        legend.text=element_text(size = 6)) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
              labels = trans_format("log10", math_format(10^.x))) +
  sxd

figa
#save for future powerpoint
ggsave("Results/Results_2020/fig1a.png", plot = figa)

# plot 2 ####

# Todo log scale starts at 1 need to start at 0
colnames(datab) <- c("pre Covid-19","Covid-19","country")
datab$country = countrycode(datab$country, origin ='country.name', destination ='iso3c')
datab <- melt(datab, id = "country")
datab


figb = ggplot(datab) +
  geom_bar(aes(x = country, y = value, fill = variable), position = "dodge", stat = "identity")+
  scale_fill_manual(values=c("#8B008B", "#696969")) +
  labs(title=element_blank(), y="Coronavirus papers", x=element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        axis.text.y = element_text(size = 6),
        axis.title.x = element_text(size = 6),
        axis.title.y = element_text(size = 6),
        panel.border = element_rect(colour = "black", fill=NA, size=0.75),
        plot.margin = unit(oma2, "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.key=element_blank(),
        legend.title=element_blank(),
        legend.position="top",
        legend.text=element_text(size = 6)) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))

figb
ggsave("Results/Results_2020/fig1b.png", plot = figb)

# plot 3 ####

#Todo geom ribbon not appearing in legend
months = unique(datac$month)

sxd <- scale_x_discrete(breaks=c(months[1],months[7],months[13],months[19],months[24]),##,months[29]), 
                        labels = c("Jan 2019","July 2019", "Jan 2020","July 2020","Dec 2020"))##, "May 2021" )) 

figc = ggplot(datac) + 
  geom_ribbon(aes(x=month,ymin=LCI, ymax=UCI,fill="Tau", group = 1), 
              alpha=0.2,       #transparency
              linetype=1,      #solid, dashed or other line types
              color=NA, #border line color
              size=1,          #border line size
              fill="#808080")+
  geom_line(size=1.25,aes(x=month,y = Tau, color = "black",group = 1)) + 
  geom_vline(xintercept = 12,linetype="dashed") +
  scale_fill_manual(values = c("red", "grey", "seagreen3"))+
  scale_colour_manual("", 
                      breaks = c("black"),
                      labels = c("Tau"),
                      values = c("black")) +
  labs(title=element_blank(), y="Rank correlation", x=element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        axis.text.y = element_text(size = 6),
        axis.title.x = element_text(size = 6),
        axis.title.y = element_text(size = 6),
        panel.border = element_rect(colour = "black", fill=NA, size=0.75),
        plot.margin = unit(oma2, "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.key=element_blank(),
        legend.title=element_blank(),
        legend.position="top",
        legend.text=element_text(size = 6))+
    sxd

figc
ggsave("Results/Results_2020/fig1c.png", plot = figc)

# plot 5 ####

# load functions #
source("Network/aux_fcts.R")
source("Network/netw_fcts.R")

# read data
sample <- read_data(path = 'Data/Data_2020')

table(sample$nodeActivity$month)



########## calculate s-cores ###########
months <- sort(unique(sample$nodeActivity$month))
months <- c(201901:201912,202001:202012)#,202101:202105)
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
g <- get_acc_network(sample=sample, covid=covid, start=201901, end=202012, 
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
sxd <- scale_x_discrete(breaks=c(months[1],months[7],months[13],months[19],months[24]),##,months[29]), 
                        labels = c("Jan 2019","July 2019", "Jan 2020","July 2020","Dec 2020"))##, "May 2021" )) 

# margins
oma1 <- c(0.5,0,0.5,0)
oma2 <- c(0.5,0,0.5,0)

# one y label
ylabs <- countries
seq1 <- length(countries) - seq(0,M,by=3)
ylabs[seq1] <- paste0(countries[seq1],"  --------")
seq2 <- length(countries) - seq(1,M,by=3)
ylabs[seq2] <- paste0(countries[seq2],"  ----")
syd <- scale_y_discrete(breaks=countries,
                        labels = ylabs)

# non-corona s-core
otherPlot <- ggplot(sCore_df, aes(month, country)) + 
  geom_tile(aes(fill=sCore), show.legend=FALSE) + 
  scale_fill_gradientn(colors=redCols) +
  geom_vline(xintercept=12.5, linetype=2) + 
  labs(title=element_blank(), y=element_blank(), x=element_blank(),
       caption = "s-core in \n non-coronavirus") +
  sxd + syd +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        axis.text.y = element_text(size=rel(0.5)),
        plot.caption = element_text(hjust = 0.5,size = 6),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        plot.margin = unit(oma1, "cm"))

# corona s-core
coronaPlot <- ggplot(sCoreCovid_df, aes(month, country)) + 
  geom_tile(aes(fill=sCore), show.legend=FALSE) + 
  scale_fill_gradientn(colors=redCols) +
  geom_vline(xintercept=12.5, linetype=2) + 
  labs(title=element_blank(), y=element_blank(), x=element_blank(),
       caption = "s-core in \n coronavirus") +
  sxd + #syd +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6), 
        axis.text.y=element_blank(), axis.ticks.y = element_blank(),
        plot.caption = element_text(hjust = 0.5,size = 6),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        plot.margin = unit(oma2, "cm"))

# difference in s-cores
diffPlot <- ggplot(sCoreDiff_df, aes(month, country)) + 
  geom_tile(aes(fill=sCore)) + 
  scale_fill_gradientn(colors=redBlueCols) + 
  geom_vline(xintercept=12.5, linetype=2) + 
  labs(title=element_blank(), y=element_blank(), x=element_blank(),
       caption = "s-core difference \n(corona - non-corona s-core)",
       fill="s-core\n") +
  #labs(title="difference", y="", x="", fill="s-core\n") +
  sxd + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6), 
        axis.text.y=element_blank(), axis.ticks.y = element_blank(), 
        plot.caption = element_text(hjust = 0.5,size = 6),
        # legend
        legend.key.size = unit(0.875, "cm"), legend.key.width = unit(0.3, "cm"),
        legend.title = element_text(size  = 6),
        legend.text = element_text(size  = 6),
        #legend.title = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        plot.margin = unit(oma2, "cm")) 



#grid.arrange(otherPlot, coronaPlot, diffPlot, legend, widths = c(3, 2, 2.5), nrow = 1)
fige = grid.arrange(otherPlot, coronaPlot, diffPlot, widths = c(3, 2.2, 3), nrow = 1)
ggsave("Results/Results_2020/fig1e.png", plot = fige)

# plot 6

datapareto_share$share_corona_pre <- cumsum(sort(datapareto_share$share_corona_pre,decreasing = TRUE))
datapareto_share$share_corona_post <- cumsum(sort(datapareto_share$share_corona_post,decreasing = TRUE))
datapareto_share$share_others_pre <- cumsum(sort(datapareto_share$share_others_pre,decreasing = TRUE))
datapareto_share$share_others_post <- cumsum(sort(datapareto_share$share_others_post,decreasing = TRUE))




figf = ggplot(datapareto_share,aes(x=1:nrow(datapareto_share))) + 
  geom_line(size=1,aes(y = share_corona_post, colour = "Coronavirus",group = 1)) + 
  geom_line(size=1,aes(y = share_others_post, colour = "non_Coronavirus",group = 1))+
  geom_line(size=1,aes(y = (1:nrow(datapareto_share))/nrow(datapareto_share), colour = "Bisector",group = 1)) + 
  scale_color_manual(values=c('#000000','#ff7f0e','#1f77b4'))+
  labs(title=element_blank(), y="Number of papers", x=element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        axis.text.y = element_text(size = 6),
        axis.title.x = element_text(size = 6),
        axis.title.y = element_text(size = 6),
        panel.border = element_rect(colour = "black", fill=NA, size=0.75),
        plot.margin = unit(oma2, "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.key=element_blank(),
        legend.title=element_blank(),
        legend.position="top",
        legend.text=element_text(size = 6))

figf
#save for future powerpoint
ggsave("Results/Results_2020/fig1f_post.png", plot = figf)

# gini figf

gini(datapareto$corona_pre)
gini(datapareto$corona_post)
gini(datapareto$others_pre)
gini(datapareto$others_post)


# avengers assemble
  
img = "Results/Results_2020/Fig1d.png"

fig1 = ggdraw() +
  draw_plot(figa, x=0, y=1.75/3, width=0.85/3, height=1.4/3)+
  draw_plot(figb, x=1/3, y=1.75/3, width=0.85/3, height=1.4/3)+
  draw_plot(figc, x=2/3, y=1.75/3, width=0.85/3, height=1.4/3)+
  draw_image(img, x=0, y=0, width=1.20/3, height=1.7/3)+
  draw_plot(fige, x=1.25/3, y=0, width=1.75/3, height=1.7/3)+
  draw_plot_label(label=c("A","B", "C", "D","E"),x=c(0, 1/3, 2/3, 0, 0.45),y=c(0.99, 0.99, 0.99, 0.55,0.55), 
                  size = 10)


filename <- "Results/Results_2020/Fig1.pdf"
pdf(file=filename, width=8, height=5, family="Helvetica", pointsize=6)
fig1
dev.off()

# Avengers not so assemble 1

fig1 = ggdraw() +
  draw_plot(figa, x=0.05, y=0.05, width=0.9, height=0.9)
fig1

filename <- "Results/Results_2020/Fig1.pdf"
pdf(file=filename, width=4, height=2.5, family="Helvetica", pointsize=6)
fig1
dev.off()

# Avengers not so assemble 2

fig2 = ggdraw() +
  draw_plot(figc, x=0.01, y=0.1, width=0.45, height=1)+
  draw_plot(fige, x=1.5/3, y=0, width=0.5, height=1)+
  draw_plot_label(label=c("A","B"),x=c(0.01, 0.5),y=c(0.98, 0.98), 
                  size = 10)
fig2

filename <- "Results/Results_2020/Fig1bis.pdf"
pdf(file=filename, width=8, height=3, family="Helvetica", pointsize=6)
fig2
dev.off()
