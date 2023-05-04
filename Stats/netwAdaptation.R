#********************************************************************************
# Analyze adaptation of global collaboration network to covid pandemic.
# 1. Correlation of Covid network with pre-Covid network and pre-Global network
# 2. Investigate error
# MM, 01.12.2020
#********************************************************************************




########## prelude ###########
# clear memory #
rm(list = ls()) ; gc();gc();gc();gc();gc(); ls(); search()

# load libraries #
library(igraph)
library(plot.matrix)
library(np)
setwd('G:/GitHub/Global-health-sciences-response-to-COVID-19')

# load functions #
source("Network/aux_fcts.R")
source("Network/netw_fcts.R")
#source("Network/lowrankAdjacency/opt_fcts.R")

# read data
sample <- read_data(path="Data/Data_2022")

############# get networks ##########
# get four networks
gCovid19 <- get_acc_network(sample=sample, covid=TRUE, start=201501, end=201912, soleAuthored=TRUE)
gCovid20 <- get_acc_network(sample=sample, covid=TRUE, start=202001, end=202212, soleAuthored=TRUE)
g19 <- get_acc_network(sample=sample, covid=FALSE, start=201501, end=201912, soleAuthored=TRUE)
g20 <- get_acc_network(sample=sample, covid=FALSE, start=202001, end=202212, soleAuthored=TRUE)

summary(gCovid19)
summary(gCovid20)
summary(g19)
summary(g20)

# get adjacency matrices of networks
adjCovid19 <- get.adjacency(gCovid19,type="upper",attr="weight")
#diag(adjCovid19) <- get.vertex.attribute(gCovid19,name="activity",V(gCovid19))

adj19 <- get.adjacency(g19,type="upper",attr="weight")
#diag(adj19) <- get.vertex.attribute(g19,name="activity",V(g19))

adjCovid20 <- get.adjacency(gCovid20,type="upper",attr="weight")
#diag(adjCovid20) <- get.vertex.attribute(gCovid20,name="activity",V(gCovid20))

######## complete edgelist with variables #######
# create data.frame
N <- length(V(gCovid19))

el <- expand.grid(1:N,1:N)
names(el) <- c("sourceId","targetId") 
el <- el[el$sourceId < el$targetId,]

elCovid19 <- as.data.frame(get.edgelist(gCovid19))
names(elCovid19) <- c("sourceId","targetId")
elCovid19$Covid19 <- as.vector(E(gCovid19)$weight)

el19 <- as.data.frame(get.edgelist(g19))
names(el19) <- c("sourceId","targetId")
el19$w19 <- as.vector(E(g19)$weight)

elCovid20 <- as.data.frame(get.edgelist(gCovid20))
names(elCovid20) <- c("sourceId","targetId")
elCovid20$Covid20 <- as.vector(E(gCovid20)$weight)

el <- merge(el,elCovid19,by=c("sourceId","targetId"),all.x=TRUE)
el <- merge(el,el19,by=c("sourceId","targetId"),all.x=TRUE)
el <- merge(el,elCovid20,by=c("sourceId","targetId"),all.x=TRUE)
el[is.na(el$Covid19),"Covid19"] <- 0
el[is.na(el$w19),"w19"] <- 0
el[is.na(el$Covid20),"Covid20"] <- 0

el$logCovid19 <- log(el$Covid19+1)
el$log19 <- log(el$w19+1)
el$logCovid20 <- log(el$Covid20+1)

el$hasCovid20 <- ifelse(el$Covid20>0,1,0)
el$hasCovid19 <- ifelse(el$Covid19>0,1,0)
el$has19 <- ifelse(el$w19>0,1,0)


# get node activities
actCovid19 <- data.frame(nodeId=1:length(V(gCovid19)), actCovid19=V(gCovid19)$activity)
act19 <- data.frame(nodeId=1:length(V(g19)), actCovid19=V(g19)$activity)
actCovid20 <- data.frame(nodeId=1:length(V(gCovid20)), actCovid19=V(gCovid20)$activity)

names(actCovid19) <- c("sourceId","sActCovid19")
el <- merge(el, actCovid19, by="sourceId", all.x=TRUE)
names(actCovid19) <- c("targetId","tActCovid19")
el <- merge(el, actCovid19, by="targetId", all.x=TRUE)

names(act19) <- c("sourceId","sAct19")
el <- merge(el, act19, by="sourceId", all.x=TRUE)
names(act19) <- c("targetId","tAct19")
el <- merge(el, act19, by="targetId", all.x=TRUE)

names(actCovid20) <- c("sourceId","sActCovid20")
el <- merge(el, actCovid20, by="sourceId", all.x=TRUE)
names(actCovid20) <- c("targetId","tActCovid20")
el <- merge(el, actCovid20, by="targetId", all.x=TRUE)

el$logSumActCovid19 <- log(el$sActCovid19 + el$tActCovid19 + 1)
el$logDiffActCovid19 <- log(abs(el$sActCovid19 - el$tActCovid19) + 1)

el$logSumAct19 <- log(el$sAct19 + el$tAct19 + 1)
el$logDiffAct19 <- log(abs(el$sAct19 - el$tAct19) + 1)

el$logSumActCovid20 <- log(el$sActCovid20 + el$tActCovid20 + 1)
el$logDiffActCovid20 <- log(abs(el$sActCovid20 - el$tActCovid20) + 1)

summary(el)

########### non-parametric ############
# have a look
pairs(el[,c("logCovid20","log19","logCovid19")])

model.bw <- npregbw(logCovid20 ~ log19 + logCovid19, 
                    data = el)#[sample.int(n=nrow(el),size=2000),])
model.np <- npreg(bws = model.bw,data=el, gradients = TRUE)

summary(model.np)
par(mfrow=c(1,1))
plot(model.np, phi=30)

plot(el$logCovid19,el$logCovid20)
lines(model.np$eval$logCovid19, model.np$mean, col = 2)

# Predict Y
# create sequences
log19_seq <- seq(0,max(el$log19),0.05)
logCovid19_seq <- seq(0,max(el$logCovid19),length.out = length(log19_seq))

# Create prediction matrix
# rows are log19, columns are logCovid19
logCovid20_pred = sapply(log19_seq,
                         FUN = function(s){
                           predict(model.np,
                                   newdata= data.frame(log19=rep(s,length(logCovid19_seq)),
                                                       logCovid19=logCovid19_seq))
                           }
                         )

# plot it 
list_id = sample$nodelist[,1:2]
colnames(list_id) = c('targetId','country_t')
el = merge(el, list_id, by = 'targetId')
colnames(list_id) = c('sourceId','country_s')
el = merge(el, list_id, by = 'sourceId')
library(dplyr)
library(plotly)
el = as_tibble(el) %>% rowwise() %>% mutate(collaboration = paste0(c(country_t,country_s),collapse = " and "))

logCovid20_pred2 = logCovid20_pred
n = dim(logCovid20_pred)[1]
# x^2+y^2=r^2
y = function(x){round(sqrt(150^2-x^2))}
y = sapply(1:100,FUN = y)

for(i in 1:100){
  logCovid20_pred[(n-y[i]):n,i] <- NA
}


x_max = max(el$log19)
y_max = max(el$logCovid19)
z_max = max(el$logCovid20)


tick_vals = log(c(1,2,5,10,20,50,100,200,500,1000,2000,5000,10000))
tick_text = as.character(c(1,2,5,TeX('10^1'),'','',TeX('10^2'),'','',TeX('10^3'),'','',TeX('10^4')))





plot_ly(x = ~log19_seq,
        y = ~logCovid19_seq,
        z = ~logCovid20_21_pred) %>%
  add_surface(opacity = 0.85,colorscale = list(c(0,"#ff7f0e" ),c(1, "#1f77b4"))) %>%
  layout(scene = list(xaxis = list(title = 'Non-coronavirus collab. 2019',
                                   ticktext = tick_text[1:birk::which.closest(tick_vals,x_max)], 
                                   tickvals = tick_vals[1:birk::which.closest(tick_vals,x_max)],
                                   ticketmode = 'array',
                                   titlefont = list(color=c('black'),family = 'Helvetica', size = 12)),
                      
                      yaxis = list(title = '  Coronavirus collab. 2019',
                                   ticktext = tick_text[1:birk::which.closest(tick_vals,y_max)], 
                                   tickvals = tick_vals[1:birk::which.closest(tick_vals,y_max)],
                                   ticketmode = 'array',
                                   titlefont = list(color=c('black'),family = 'Helvetica', size = 12)),
                      
                      zaxis = list(title = 'Coronavirus collab. 2020-22   ',
                                   ticktext = tick_text[1:birk::which.closest(tick_vals,z_max)], 
                                   tickvals = tick_vals[1:birk::which.closest(tick_vals,z_max)],
                                   ticketmode = 'array',
                                   titlefont = list(color=c('black'),family = 'Helvetica', size = 12)),
                      aspectmode='cube')) %>% 
  add_trace(data = el, x = el$log19, y = el$logCovid19, z = el$logCovid20, text = ~ el$collaboration, mode = "markers", type = "scatter3d", 
            marker = list(size = 5, color = "#4d4a4a"),hovertemplate = paste(
              "<b>%{text}</b><br><br>"
            )) #, symbol = 104))





tick_vals = log(c(seq(1,10,1),seq(20,100,10),seq(200,1000,100),seq(2000,10000,1000)))
tick_text = as.character(c('1',rep('',8),'10',rep('',8),'100',rep('',8),'1k',rep('',8),'10k'))

logCovid20_21_pred = logCovid20_pred

plot_ly(x = ~log19_seq,
        y = ~logCovid19_seq,
        z = ~logCovid20_21_pred) %>%
  add_surface(opacity = 0.85,colorscale = list(c(0,"#ff7f0e" ),c(1, "#1f77b4"))) %>%
  layout(scene = list(xaxis = list(title = 'Non-coronavirus collab. 2019',
                                   ticktext = tick_text[1:birk::which.closest(tick_vals,x_max)], 
                                   tickvals = tick_vals[1:birk::which.closest(tick_vals,x_max)],
                                   ticketmode = 'array',
                                   titlefont = list(color=c('black'),family = 'Helvetica', size = 15)),
                      
                      yaxis = list(title = '  Coronavirus collab. 2019',
                                   ticktext = tick_text[1:birk::which.closest(tick_vals,y_max)], 
                                   tickvals = tick_vals[1:birk::which.closest(tick_vals,y_max)],
                                   ticketmode = 'array',
                                   titlefont = list(color=c('black'),family = 'Helvetica', size = 15)),
                      
                      zaxis = list(title = 'Coronavirus collab. 2020-22    ',
                                   ticktext = tick_text[1:birk::which.closest(tick_vals,z_max)], 
                                   tickvals = tick_vals[1:birk::which.closest(tick_vals,z_max)],
                                   ticketmode = 'array',
                                   titlefont = list(color=c('black'),family = 'Helvetica', size = 15)),
                      aspectmode='cube')) %>% 
  add_trace(data = el, x = el$log19, y = el$logCovid19, z = el$logCovid20, text = ~ el$collaboration, mode = "markers", type = "scatter3d", 
            marker = list(size = 5, color = "#4d4a4a"),hovertemplate = paste(
              "<b>%{text}</b><br><br>"
            )) #, symbol = 104))


save.image(file = 'np_reg_2022.RData')






