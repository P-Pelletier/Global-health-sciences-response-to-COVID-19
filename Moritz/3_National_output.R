#**************************************************************
# Deals with `national scientific output'
# Creates 
# 1. Descriptive statistics table
# 2. Regresssion table
# 
# MM, last update 28.04.2022
#**************************************************************

###### prelude ############
# clear up
rm(list = ls())
sink();sink();sink()

detach_loaded_libraries <- function(){
  # detach loaded libraries
  # except for base pckgs 
  basePckgs <- c("package:stats","package:graphics","package:grDevices",
                 "package:utils","package:datasets","package:methods","package:base")
  pckgs <- search()
  pckgs <- pckgs[grepl("package:",pckgs) & !pckgs %in% basePckgs]
  for(pckg in pckgs){
    try(detach(pckg,character.only = TRUE))  
  }
}
detach_loaded_libraries()

# load libraries
library(xtable)
library(Hmisc)

library(dplyr)
library(tidyr)
library(countrycode)
library(readxl)
library(mctest)

#library(sandwich) # for heteroskedasticity-robust standard errors

# data input
inputPath = "G:/Github/Global-health-sciences-response-to-COVID-19/data/"
# output path
outputPath = "G:/Github/Global-health-sciences-response-to-COVID-19/Results"
# log file
sink(file=paste0(outputPath,"national_output_log.txt"))
sink()
# load functions
source("G:/Github/Global-health-sciences-response-to-COVID-19/Moritz/aux_fcts.R")

get_mctest_summaries <- function(res){
  coefficients <- names(res[[4]]$coefficients)[-1]
  mctests <- matrix(NA,nrow=length(res),ncol=length(coefficients)+1)
  colnames(mctests) <- c(coefficients,"condition_number")
  
  for(i in 1:length(res)){
    # overall diagnostic
    tmp <- omcdiag(res[[i]])
    condition_number <- tmp$odiags[6,"results"]
    # coefficient level diagnostic
    tmp <- imcdiag(res[[i]])
    leamers <- tmp$idiags[,"Leamer"]
    # collect result
    mctests[i,names(leamers)] <- leamers
    mctests[i,"condition_number"] <- condition_number
  }
  
  
  avgmctests <- mctests %>% as_tibble() %>% 
    summarize(across(everything(),
                     ~ mean(.x,na.rm=TRUE)))
  
  sdmctests <- mctests %>% as_tibble() %>% 
    summarize(across(everything(),
                     ~ sd(.x,na.rm=TRUE)))
  
  mctests <- rbind(avgmctests,sdmctests) %>% as.matrix() %>% t()
  return(mctests)
}


# settings
last_month <- 202212
months <- c(202001:202012,202101:202112,202201:202212)

##### variables used in national regression
vars <- c("coronaPubsCum","coronaPubs","nonCoronaPubsPreCovid","coronaPubsPreCovid",
          "coronaPubsCum_pc","nonCoronaPubsPreCovid_pc","coronaPubsPreCovid_pc",
          "cum_deaths","cum_deaths_pc",
          "lockdown_days_past12m","border_closure_days_past12m",
          "pop","gdp_pc","hdi","t")

# all in logs (except for hdi and t)
log_vars <- vars[!(vars %in% c("hdi","t"))]

####### prepare sample ######## 
cat("\n\n ******** SAMPLE ******* \n\n")

panel <- get_country_panel(inputPath=inputPath)

# seychellen - outlier in coronapubs/pop due to one paper -> larger countries with at least 1 Mio people
#View(panel %>% filter(countryCode=="SYC"))

# cut into two periods - pre/precovid and covid
precovid <- panel %>%
  filter(month < 202001) %>%
  mutate(coronaPubsPreCovid = collabPubsCorona + solePubsCorona,
         nonCoronaPubsPreCovid = collabPubsNonCorona + solePubsNonCorona) %>%
  group_by(countryCode) %>%
  summarise(coronaPubsPreCovid = sum(coronaPubsPreCovid),
            nonCoronaPubsPreCovid = sum(nonCoronaPubsPreCovid),.groups="drop")

# join initial pre-covid condition and log all variables, except for hdi and t
covidPanel <- panel %>%
  filter(month >= 202001) %>%
  group_by(countryCode) %>%
  left_join(precovid,by="countryCode") %>%
  mutate(coronaPubsCum = cumsum(coronaPubs)) %>%
  mutate(coronaPubsCum_pc=coronaPubsCum/pop, 
         coronaPubs_pc = coronaPubs/pop,
         nonCoronaPubsPreCovid_pc = nonCoronaPubsPreCovid/pop,
         coronaPubsPreCovid_pc = coronaPubsPreCovid/pop,
         cum_deaths_pc = cum_deaths) %>%
  mutate_at(log_vars, .funs = ~ log(.x+1))


sink()
####### descr.stats.table ######## 
cat("\n\n ******** DESCRIPTIVES ******* \n\n")
descr_stats(covidPanel,vars)

####### do regression ######
cat("\n\n ******** REGRESSION ******* \n\n")
library(stargazer)

#covidPanel <- covidPanel %>% filter(pop > 1)

# main regression by month
res <- list()
for(i in 1:length(months)){
  print("*************")
  print(months[i])
  idx <- covidPanel$month==months[i]
  
  if(months[i]>202002){
    res[[i]] <- lm(coronaPubsCum ~ nonCoronaPubsPreCovid + coronaPubsPreCovid +
                     cum_deaths  +
                     border_closure_days_past12m + lockdown_days_past12m + 
                     gdp_pc + hdi # as.factor(region)
                   ,data=covidPanel[idx,])
    
  }else{ # no/too few closures/lockdown
    res[[i]] <- lm(coronaPubsCum ~ nonCoronaPubsPreCovid + coronaPubsPreCovid +
                     cum_deaths  +
                     #border_closure_days_past12m + lockdown_days_past12m + 
                     gdp_pc #+ hdi # as.factor(region)
                   ,data=covidPanel[idx,])
  }
  print(summary(res[[i]]))
}

stargazer(res[1:12]) # star.cutoffs = c(0.01,0.001,0.0005))
stargazer(res[13:length(res)]) # star.cutoffs = c(0.01,0.001,0.0005))

summary(res[[1]])
summary(res[[10]])


# collect multicollinearity tests 
stargazer(get_mctest_summaries(res))
# mc indicators do not depend on cumulated/current output
# all variables considerably correlated with population, but population 
# itself is insignificant and the aspects of
# population that matter are incorporated in the other variables 


# do regression with bootstrap in differences
S <- 1000L
diffvars <- c("nonCoronaPubsPreCovid","coronaPubsPreCovid")
bootstrap_diffs <- matrix(NA,nrow=S,ncol=length(months))
N <- nrow(month_covidPanel)

res <- list()
for(i in 1:length(months)){
  print("*************")
  print(months[i])
  idx <- covidPanel$month==months[i]
  month_covidPanel <- covidPanel[idx,]
  
  if(months[i]>202002){
    model_formula <- as.formula("coronaPubs ~ nonCoronaPubsPreCovid + 
                                coronaPubsPreCovid + cum_deaths  + 
                                border_closure_days_past12m + lockdown_days_past12m + 
                                gdp_pc")
  }else{
      model_formula <- as.formula("coronaPubs ~ nonCoronaPubsPreCovid + 
                                coronaPubsPreCovid + cum_deaths  + 
                                gdp_pc")
    }
    
    for(s in 1:S){
      s_panel <- month_covidPanel[sample.int(N,N,replace=TRUE),]
      s_res <- lm(model_formula,data=s_panel)
      bootstrap_diffs[s,i] <- s_res$coefficients[diffvars[1]] - s_res$coefficients[diffvars[2]] 
    }
}

# 95% conf-interval on differences
bootstrap_diffs <- apply(bootstrap_diffs, 2, function(x) sort(x))
lconf_idx <- ceiling(0.025*S)
uconf_idx <- floor(0.975*S)
conf_int <- bootstrap_diffs[c(lconf_idx,uconf_idx), ]

plot(1:24,conf_int[1,],ylim=c(0,2))
lines(1:24,conf_int[2,])
lines(c(1,25),c(0,0),lty=2)
# dominating factor is pre-covid health science

##### alternative regression by month - variables normalized by population
res <- list()
for(i in 1:length(months)){
  print("*************")
  print(months[i])
  idx <- covidPanel$month==months[i] & covidPanel$pop > 1
  
  if(months[i]>202002){
    res[[i]] <- lm(coronaPubsCum_pc ~ nonCoronaPubsPreCovid_pc + coronaPubsPreCovid_pc +
                     cum_deaths_pc  +
                     border_closure_days_past12m + lockdown_days_past12m + 
                     gdp_pc # as.factor(region)
                   ,data=covidPanel[idx,])
    
  }else{ # no/too few closures/lockdown
    res[[i]] <- lm(coronaPubsCum_pc ~ nonCoronaPubsPreCovid_pc + coronaPubsPreCovid_pc +
                     cum_deaths_pc  +
                     #border_closure_days_past12m + lockdown_days_past12m + 
                     gdp_pc  # as.factor(region)
                   ,data=covidPanel[idx,])
  }
  print(summary(res[[i]]))
}

stargazer(res[1:12]) # star.cutoffs = c(0.01,0.001,0.0005))

stargazer(res[13:length(res)]) # star.cutoffs = c(0.01,0.001,0.0005))


stargazer(get_mctest_summaries(res))
# per capita normalized variables do not improve mc measure.



#### some multi-collinearity stuff
library("mctest")

i <- 1
idx <- covidPanel$month==months[i]

names(covidPanel)
dim(covidPanel)
undebug(descr_stats)
descr_stats(covidPanel %>% ungroup() %>% filter(month==months[i]),vars)

nt0 <- covidPanel %>% ungroup() %>% filter(month==months[i]) %>% pull(nonCoronaPubsPreCovid_pc)
ct0 <- covidPanel %>% ungroup() %>% filter(month==months[i]) %>% pull(coronaPubsPreCovid_pc)
cc <- covidPanel %>% ungroup() %>% filter(month==months[i]) %>% pull(countryCode)

plot(nt0,ct0)
text(nt0,ct0,cc)

i <- 4
idx <- covidPanel$month==months[i]
res <- lm(coronaPubsCum ~  coronaPubsPreCovid + nonCoronaPubsPreCovid + #
                 cum_deaths  +
                 border_closure_days_past12m + lockdown_days_past12m + 
                 gdp_pc + hdi + pop #+ hdi pop +  # as.factor(region)
               ,data=covidPanel[idx,])
summary(res)

omcdiag(res)
# condition number at 67 (should be below 30)
imcdiag(res)

# 
i <- 4
idx <- covidPanel$month==months[i]
res <- lm(coronaPubsCum_pc ~  coronaPubsPreCovid_pc + nonCoronaPubsPreCovid_pc + #
            cum_deaths_pc  +
            border_closure_days_past12m + lockdown_days_past12m + 
            gdp_pc # + hdi #+ hdi pop +  # as.factor(region)
          ,data=covidPanel[idx,])
summary(res)

omcdiag(res)
# condition number at 67 (should be below 30)
imcdiag(res)
# looks pretty ok





###


i <- 4
idx <- covidPanel$month==months[i]
res <- lm(coronaPubsCum_pc ~ coronaPubsPreCovid_pc + nonCoronaPubsPreCovid_pc +
            cum_deaths_pc  +
            border_closure_days_past12m + lockdown_days_past12m #+ 
            #gdp_pc #+ hdi # as.factor(region)
          ,data=covidPanel[idx,])
summary(res)



omcdiag(res)
imcdiag(res)




### make picture....
library(dotwhisker)

res_lst <- list()
for(i in 1:length(months)){
  
  model <- i
  month <- months[i]
  tmp <- summary(res[[i]])
  terms <- rownames(tmp$coefficients)
  estimates <- tmp$coefficients[,1] 
  std.errors <- tmp$coefficients[,2] 
  res_lst[[length(res_lst)+1]] <- data.frame(model=model,
                                             month=month,
                                             term=terms,
                                             estimate=estimates,
                                             std.error=std.errors)
}

res <- do.call(rbind.data.frame, res_lst) %>% tibble() %>%
  mutate(lb=estimate-1.96*std.error, 
         ub=estimate+1.96*std.error)



pd <- position_dodge(width=0.5)
oma2 <- c(1,1,1,1)

p <- res %>% filter(term %in% c("coronaPubsPreCovid","nonCoronaPubsPreCovid")) %>% #filter(!term %in% c("(Intercept)","hdi")) %>%
ggplot(aes(x=model, y=estimate, color=term, group=term)) +
  scale_colour_manual("", 
                      labels = c("pre-pandemic Corona. research","pre-pandemic non-Corona. research"),
                      breaks = c("coronaPubsPreCovid", "nonCoronaPubsPreCovid"),
                      values = c("orange", "darkblue")) +
  geom_line(position=pd) + 
  geom_point(position=pd) + 
  scale_x_continuous(name="",
                     breaks=c(1,7,13,19,24),
                     labels = c("Jan 2020","Jul 2020",
                                "Jan 2021","Jul 2021",
                                "Dec 2021")) +
  scale_y_continuous(name="coefficient estimate") +
  geom_errorbar(aes(ymin=lb,ymax=ub),color="grey", width=0.1,position=pd) +
  geom_hline(yintercept=0, lty=2) +
  theme(text=element_text(size=12,  family="Helvetica", color="black"),
        axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.margin = unit(oma2, "cm"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.background = element_rect(fill = "white"),
        #panel.grid.major = element_line(colour = "grey"),
        legend.position = "top",
        legend.key = element_rect(fill = NA, colour = NA, size = 0.25))
  

outfile <- paste0(outputPath,"National_output_regression_dw.pdf")
#ggsave(outfile, width = 6, height = 3)
pdf(file=outfile, width=6, height=4, family="Helvetica")#, pointsize=2)
p
dev.off()



# bye
sink()

