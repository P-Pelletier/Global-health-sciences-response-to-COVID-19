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

setwd("G:/Github/Global-health-sciences-response-to-COVID-19")

dataa = read.csv(file="Data/Data_2022/fig1a.csv",
                 header=TRUE,sep=",",stringsAsFactors = FALSE)
datab = read.csv(file="Data/Data_2022/fig1b.csv",
                 header=TRUE,sep=",",stringsAsFactors = FALSE)
datac = read.csv(file="Data/Data_2022/fig1c.csv",
                 header=TRUE,sep=",",stringsAsFactors = FALSE)
datapareto_share = read.csv(file="Data/Data_2022/pareto_share.csv",
                 header=TRUE,sep=",",stringsAsFactors = FALSE)
datapareto = read.csv(file="Data/Data_2022/pareto.csv",
                      header=TRUE,sep=",",stringsAsFactors = FALSE)

# plot 1 ####

months = unique(dataa$month)

# xlabel
dataa$month = as.character(c(201901:201912,202001:202012,202101:202112,202201:202212))
datac$month = as.character(c(201901:201912,202001:202012,202101:202112,202201:202212))
month = as.character(c(201901:201912,202001:202012,202101:202112,202201:202212))

sxd <- scale_x_discrete(breaks=c(month[1],month[7],month[13],month[19],month[25],month[31],month[37],month[43]),
                        labels = c("Jan 2019","July 2019", "Jan 2020","July 2020","Jan 2021", "July 2021","Jan 2022","July 2022"))
# margin size
oma2 <- c(0.3,0,0,0)


dataa$months_number = c(1:length(months))
datac$months_number = c(1:length(months))

#figa



figa = ggplot(dataa, aes(x = month)) + 
  geom_line(size=1.25,aes(y = Coronavirus, colour = "Coronavirus",group = 1)) + 
  geom_line(size=1.25,aes(y = non_Coronavirus, colour = "non_Coronavirus",group = 1))+
  # covid19 treshold
  geom_vline(xintercept = 12,linetype="dashed") +
  scale_color_manual(values=c('#ff7f0e','#1f77b4'))+
  labs(title=element_blank(), y="Number of papers", x=element_blank()) +
  sxd +
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

figa
#save for future powerpoint
ggsave("Results/Results_2022/fig1a.png", plot = figa)

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
ggsave("Results/Results_2022/fig1b.png", plot = figb)

# plot 3 ####

figc = ggplot(datac) + 
  geom_ribbon(aes(x=month,ymin=LCI, ymax=UCI,fill="Tau", group = 1), 
              alpha=0.2,       #transparency
              linetype=1,      #solid, dashed or other line types
              color=NA, #border line color
              size=1,          #border line size
              fill="#808080")+
  geom_line(size=1.25,aes(x=months_number,y = Tau, color = "black",group = 1)) + 
  geom_vline(xintercept = 12,linetype="dashed") +
  scale_fill_manual(values = c("red", "grey", "seagreen3"))+
  scale_colour_manual("", 
                      breaks = c("black"),
                      labels = c("Tau"),
                      values = c("black")) +
  labs(title=element_blank(), y="Rank correlation", x=element_blank()) +
  sxd +
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

figc
ggsave("Results/Results_2022/fig1c.png", plot = figc)

# plot 5 ####

# load functions #
source("Utils/aux_fcts_deprecated.R")
source("Utils/netw_fcts_deprecated.R")

# read data
sample <- read_data(path = 'Data/Data_2022')

table(sample$nodeActivity$month)



########## calculate s-cores ###########
months <- sort(unique(sample$nodeActivity$month))
months <- c(201901:201912,202001:202012,202101:202112,202201:202212)
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
sxd <- scale_x_discrete(breaks=c(month[1],month[7],month[13],month[19],month[25],month[31],month[37],month[43]), 
                        labels = c("Jan 2019","July 2019", "Jan 2020","July 2020","Jan 2021", "July 2021","Jan 2022","July 2022"))

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
ggsave("Results/Results_2022/fig1e.png", plot = fige)

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
ggsave("Results/Results_2022/fig1f_post.png", plot = figf)

# gini figf

gini(datapareto$corona_pre)
gini(datapareto$corona_post)
gini(datapareto$others_pre)
gini(datapareto$others_post)

# Avengers not so assemble 1

fig1 = ggdraw() +
  draw_plot(figa, x=0.05, y=0.05, width=0.9, height=0.9)
fig1

filename <- "Results/plots/Fig1.pdf"
pdf(file=filename, width=4, height=2.5, family="Helvetica", pointsize=6)
fig1
dev.off()

# Avengers not so assemble 2

fig2 = ggdraw() +
  draw_plot(figc, x=0.01, y=0.1, width=0.35, height=1)+
  draw_plot(fige, x=1.20/3, y=0, width=0.60, height=1)+
  draw_plot_label(label=c("A","B"),x=c(0.01, 0.4),y=c(0.98, 0.98), 
                  size = 10)
fig2

filename <- "Results/plots/Fig4.pdf"
pdf(file=filename, width=8, height=3, family="Helvetica", pointsize=6)
fig2
dev.off()

