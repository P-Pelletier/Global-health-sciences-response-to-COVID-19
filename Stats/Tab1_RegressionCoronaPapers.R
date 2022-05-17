#**************************************************************
# Creates the regresssion table, Table 1
# MM, 20.01.2021
#**************************************************************

########## prelude ###########
# clear memory #
rm(list = ls()) ; gc();gc();gc();gc();gc(); ls(); search()

setwd('G:/GitHub/Global-health-sciences-response-to-COVID-19')
# load functions
source("Network/aux_fcts.R")

# load libraries
library(np)
library(xtable)
library(sandwich) # for heteroskedasticity-robust standard errors

########## data handling ###########
# read data
sample <- read_data(path="Data/Data_2021")


# prepare data
nodeActivity <- sample$nodeActivity

head(nodeActivity)
nodeActivity$pubs <- rowSums(nodeActivity[,c("solePubs","collabPubs","solePubsCorona","collabPubsCorona")])
nodeActivity$coronaPubs <- rowSums(nodeActivity[,c("solePubsCorona","collabPubsCorona")])
nodeActivity$noncoronaPubs <- rowSums(nodeActivity[,c("solePubs","collabPubs")])

tmp <- aggregate(nodeActivity[,c("coronaPubs","noncoronaPubs")], by=list(month=nodeActivity$month), function(x) sum(x))

# output per month - corona and non-corona papers
xtable(t(tmp[1:12,]))
xtable(t(tmp[13:24,]))
xtable(t(tmp[25:36,]))
# July seems appropriate - do not want to speculate on holiday in August 

# make no difference between sole pubs and joint pubs
nodeActivity$coronaPubs <- nodeActivity$collabPubsCorona + nodeActivity$solePubsCorona
nodeActivity$nonCoronaPubs <- nodeActivity$collabPubs + nodeActivity$solePubs

# aggregate over pre-Covid-19
idx <- nodeActivity$month < 202001
preCovidPubs <- aggregate(nodeActivity[idx,c("coronaPubs", "nonCoronaPubs")],
          by=list(countryCode=nodeActivity$countryCode[idx]),
          function(x) sum(x))
names(preCovidPubs) <- c("countryCode","coronaPubsPreCovid","nonCoronaPubsPreCovid")

# aggregate over Covid-19
idx <- nodeActivity$month >= 202001
covidPubs <- aggregate(nodeActivity[idx,c("coronaPubs")],
                      by=list(countryCode=nodeActivity$countryCode[idx]),
                      function(x) sum(x))
names(covidPubs) <- c("countryCode","coronaPubsCovid")

# merge pre-covid with (total) covid papers
pubs <- merge(preCovidPubs,covidPubs,by="countryCode")

head(pubs)

# join (monthly) Covid-19 pubs
for(month in c(c(202001:202012),c(202101:202112))){
  idx <- nodeActivity$month==month
  tmp <- nodeActivity[idx,c("countryCode","coronaPubs")]
  names(tmp) <- c("countryCode",paste0("coronaPubs_",month))
  pubs <- merge(pubs,tmp,by="countryCode")
}


# log and transform all variables
vars <- names(pubs)[-1]
for(var in vars){
  tmp <- log(pubs[,var]+1)
  pubs[,var] <- (tmp-mean(tmp))/sd(tmp)
}

########## analysis and output ###########
# inspect data
pairs(pubs[,-1])

# np regression
model.bw <- npregbw(coronaPubsCovid ~ nonCoronaPubsPreCovid + coronaPubsPreCovid, 
                    data = pubs, regtype="ll")
model.np <- npreg(bws = model.bw,data=el, gradients = TRUE)
summary(model.np)
par(mfrow=c(1,1))
plot(model.np, phi=30)


head(pubs)
# do linear regressions
res <- list()
for(i in 1:12){
  if(i<10){
    outcomeVar <- paste0("coronaPubs_20200",i)
  }else{
    outcomeVar <- paste0("coronaPubs_2020",i)
  }
  res[[i]] <- lm(formula(paste0(outcomeVar," ~ coronaPubsPreCovid + nonCoronaPubsPreCovid -1")), data=pubs)
}

# collect results
resmat <- matrix(NA, nrow=5, ncol=12)
for(i in 1:length(res)){
  resmat[c(1,3),i] <- res[[i]]$coefficients
  resmat[c(2,4),i] <- diag(sandwich::vcovHC(res[[i]]))^0.5
  resmat[5,i] <- summary(res[[i]])$r.squared
}
resmat <- round(resmat,digits=3)

# output
xtable(resmat,digits=3)

