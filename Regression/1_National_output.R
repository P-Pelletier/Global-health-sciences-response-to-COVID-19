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

Sys.setenv(LANG = "en") # For english language
Sys.setlocale("LC_TIME", "English")

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
library(stargazer)
library(dplyr)
library(tidyr)
library(countrycode)
library(readxl)
library(mctest)
library(lubridate)
library(sandwich) # for heteroskedasticity-robust standard errors

# data input
inputPath = "G:/Github/Global-health-sciences-response-to-COVID-19/data/"
# output path
outputPath = "G:/Github/Global-health-sciences-response-to-COVID-19/Results/table"
# log file

# load functions
source("G:/Github/Global-health-sciences-response-to-COVID-19/Utils/aux_fcts.R")

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
last_month <- 202112
months <- c(202001:202012,202101:202112,202201:202212)

##### variables used in national regression
vars <- c("coronaPubsCum","nonCoronaPubsPreCovid","coronaPubsPreCovid",
          "cum_deaths","border","locked",
          "gdp_pc","dep_internationalPreCovid_normalized","hdi")

####### prepare sample ######## 
panel <- get_country_panel(inputPath=inputPath)

precovid <- panel %>%
  filter(month < 202001) %>%
  mutate(coronaPubsPreCovid = collabPubsCorona + solePubsCorona,
         nonCoronaPubsPreCovid = collabPubsNonCorona + solePubsNonCorona,
         dep_international = dependance_international + 
                             dependance_international_agency +
                             dependance_international_and_agency,
         dep_international_corona = dependance_international_corona + 
                                    dependance_international_agency_corona +
                                    dependance_international_and_agency_corona) %>%
  group_by(countryCode) %>%
  summarise(coronaPubsPreCovid = sum(coronaPubsPreCovid),
            nonCoronaPubsPreCovid = sum(nonCoronaPubsPreCovid),
            dep_internationalPreCovid = sum(dep_international_corona+ dep_international),
            dep_internationalPreCovid_normalized = sum(dep_international_corona+ dep_international)/sum(N_paper_with_grantCorona+N_paper_with_grant))
            #dep_international_coronaPreCovid = sum(dep_international_corona)/sum(N_paper_with_grantCorona))


covidPanel <- panel %>%
  filter(month >= 202001) %>%
  group_by(countryCode) %>%
  mutate(coronaPubsCum = cumsum(coronaPubs),
         border = lag(cumsum(border_closure_days)),
         locked = lag(cumsum(lockdown_days)),
         cum_deaths = lag(cum_deaths/pop)) %>%  
  left_join(precovid,by="countryCode") %>%
  mutate_at(vars[1:(length(vars)-2)], .funs = ~ log(.x+1))

# Remove countries

#covidPanel <- subset(covidPanel, !(countryCode %in% c("CHN", "USA")))

covidPanel %<>%
  group_by(month) %>%
  mutate(
         coronaPubs_fw = coronaPubs %>% percent_rank(),
         coronaPubsCum_fw = coronaPubsCum %>% percent_rank(),
         nonCoronaPubsPreCovid_fw = nonCoronaPubsPreCovid %>% percent_rank(),
         coronaPubsPreCovid_fw = coronaPubsPreCovid %>% percent_rank(),
         cum_deaths_fw = cum_deaths %>% percent_rank(),
         dep_internationalPreCovid_fw =  dep_internationalPreCovid %>% percent_rank(),  
         dep_internationalPreCovid_normalized_fw =  dep_internationalPreCovid_normalized %>% percent_rank(),
         locked_fw =  locked %>% percent_rank(),
         border_fw =  border %>% percent_rank(),
         pop_fw = pop %>% percent_rank(),
         gdp_pc_fw = gdp_pc %>% percent_rank()
         ) %>%
  ungroup()



unistats <- covidPanel %>%
  ungroup() %>%
  #select(all_of(vars)) %>%
  summarise(across(.cols=all_of(vars),
                   .fns=list(min = ~ min(.x,na.rm=TRUE),
                             q1 = ~ quantile(.x, 0.25, na.rm = TRUE),
                             median = ~ median(.x, na.rm = TRUE),
                             q3 = ~ quantile(.x, 0.75, na.rm = TRUE),                             
                             max = ~ max(.x,na.rm=TRUE),
                             mean = ~ mean(.x,na.rm=TRUE), 
                             sd = ~ sd(.x,na.rm=TRUE),
                             obs = ~ n(),
                             NAs = ~ sum(is.na(.x))),
                   .names="{.col}X{.fn}")
  ) %>%
  gather(stat, val) %>%
  separate(stat, into = c("var", "stat"), sep = "X") %>%
  spread(stat, val) %>%
  select(var, mean, sd, min, q1, median, q3, max, obs, NAs) %>%
  as.data.frame()


rownamesUnistats <- unistats[,"var"]
unistats <- as.matrix(unistats[,-1])
rownames(unistats) <- rownamesUnistats
print(unistats)

stats_df <- data.frame(unistats)
rownames(stats_df) <- c("$border_{i,t'}$", "$c_{i,t'}$", "$c_{i,t0}$",
                        "$deaths_{i,t'}$","$Int Dep_{i,t0}$",
                        "$gdp_{i,t0}$", "$hdi_{i,t0}$","$locked_{i,t'}$","$n_{i,t0}$")
# Use stargazer to create the table
stargazer(stats_df, 
          header = TRUE,        # Hide column names of the matrix in the output
          rownames = TRUE,      # Hide row names of the data frame in the output
          align = TRUE, 
          summary = FALSE, # Align columns of the table
          type = "text" ,
          out = paste0(outputPath,"/national_output_stats.tex") 
)



cors <- covidPanel %>% ungroup() %>% select(all_of(vars)) %>%
  as.matrix() %>% rcorr()

descr <- round(cbind(unistats[rownames(cors$r),c("mean","sd")],cors$r),
               digits=3)

cat("univariate stats and correlation after logging.\n")
xtable(descr)
print(descr)


# main regression by month
models <- list()
cov <- list()

for(i in 1:length(months[0:36])){
  print("*************")
  print(months[i])
  idx <- covidPanel$month==months[i]
  
  if(months[i]>=202004){
    models[[i]] <- lm(coronaPubsCum ~ nonCoronaPubsPreCovid + coronaPubsPreCovid +
                        dep_internationalPreCovid_normalized  +
                        cum_deaths  + border + locked + 
                        gdp_pc + hdi
                      ,data=covidPanel[idx,])
    cov[[i]] = sqrt(diag(sandwich::vcovCL(models[[i]], cluster = covidPanel[idx,]$region))) 
  }else{ # no/too few closures/lockdown
    models[[i]] <- lm(coronaPubsCum ~ nonCoronaPubsPreCovid + coronaPubsPreCovid +
                        dep_internationalPreCovid_normalized  +
                        gdp_pc + hdi
                      ,data=covidPanel[idx,])
    cov[[i]] = sqrt(diag(sandwich::vcovCL(models[[i]], cluster = covidPanel[idx,]$region))) 
  }
  #  print(summary(models[[i]]))
}

# Define the new variable names
var_labels <- c("log(1 + nonCoronaPubsPreCovid)" = "$n_{t0}$","log(1 + coronaPubsPreCovid))"="$c_{t0}$",
                "log(1 + dep_internationalPreCovid)" = "$Int Dep_{t0}$",
                "log(1 + cum_deaths)" = "$deaths_{t'}$", "log(1 + border_closure_days_past12m)"="$border_{t'}$",
                "log(1 + lockdown_days_past12m)"="$locked_{t'}$","log(1 + gdp_pc)"="$gdp_{t0}$"#"log(1 + pop)" = "$pop_{t0}$",
                #"hdi"="$hdi_{t0}$"
                  )

# Omit the desired statistics
omit_stats <- c("max.rsq", "ll", "wald", "lr","logrank","res.dev","f","ser","n")

months_clean <- parse_date_time(as.character(months), orders = "ym")   # Convert to date
months_clean <- format(months_clean, format = "%b %Y")  

stargazer::stargazer(
  models[0:12], 
  se = cov, 
  align = TRUE, type = "text",  omit = c('Constant'), font.size = "small",
  dep.var.labels.include = FALSE, dep.var.caption = "",column.labels = months_clean[0:12],
  title = "", covariate.labels = var_labels, omit.stat = omit_stats,
  out = paste0(outputPath,"/national_output_early.tex")
)

stargazer::stargazer(
  models[13:24], 
  se = cov, 
  align = TRUE, type = "text",  omit = c('Constant'), font.size = "small",
  dep.var.labels.include = FALSE, dep.var.caption = "",column.labels = months_clean[13:24],
  title = "", omit.stat = omit_stats,
  out = paste0(outputPath,"/national_output_mid.tex")
)

stargazer::stargazer(
  models[25:36], 
  se = cov, 
  align = TRUE, type = "text",  omit = c('Constant'), font.size = "small",
  dep.var.labels.include = FALSE, dep.var.caption = "",column.labels = months_clean[25:36],
  title = "", omit.stat = omit_stats,
  out = paste0(outputPath,"/national_output_end.tex")
)



"""
# Field weighted
models <- list()
cov <- list()

for(i in 1:length(months[0:36])){
  print("*************")
  print(months[i])
  idx <- covidPanel$month==months[i]
  
  if(months[i]>=202003){
    models[[i]] <- lm(coronaPubsCum_fw ~ nonCoronaPubsPreCovid_fw + coronaPubsPreCovid_fw +
                        dep_internationalPreCovid_fw +
                        cum_deaths_fw  + border_fw + locked_fw + 
                        gdp_pc_fw + hdi
                      ,data=covidPanel[idx,])
    cov[[i]] = sqrt(diag(sandwich::vcovCL(models[[i]], cluster = covidPanel[idx,]$region))) 
  }else{ # no/too few closures/lockdown
    models[[i]] <- lm(coronaPubsCum_fw ~ nonCoronaPubsPreCovid_fw + coronaPubsPreCovid_fw +
                        dep_internationalPreCovid_fw +
                        gdp_pc_fw + hdi
                      ,data=covidPanel[idx,])
    cov[[i]] = sqrt(diag(sandwich::vcovCL(models[[i]], cluster = covidPanel[idx,]$region))) 
  }
  print(summary(res[[i]]))
}

# Define the new variable names
var_labels <- c("log(1 + nonCoronaPubsPreCovid)" = "$n_{t0}$","log(1 + coronaPubsPreCovid))"="$c_{t0}$",
                "log(1 + dep_internationalPreCovid)" = "$Int Dep_{t0}$",
                "log(1 + cum_deaths)" = "$deaths_{t'}$", "log(1 + border_closure_days_past12m)"="$border_{t'}$",
                "log(1 + lockdown_days_past12m)"="$locked_{t'}$","log(1 + gdp_pc)"="$gdp_{t0}$",#"log(1 + pop)" = "$pop_{t0}$",
                "hdi"="$hdi_{t0}$"
)

# Omit the desired statistics
omit_stats <- c("max.rsq", "ll", "wald", "lr","logrank","res.dev","adj.rsq","f","ser","n")

months_clean <- parse_date_time(as.character(months), orders = "ym")   # Convert to date
months_clean <- format(months_clean, format = "%b %Y")  

stargazer::stargazer(
  models[0:12], 
  se = cov, 
  align = TRUE, type = "text",  omit = c('Constant'), font.size = "small",
  dep.var.labels.include = FALSE, dep.var.caption = "",column.labels = months_clean[25:36],
  title = "", covariate.labels = var_labels, omit.stat = omit_stats,
  out = paste0(outputPath,"/national_output_early.tex")
)


# collect multicollinearity tests 
stargazer(get_mctest_summaries(models))
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
                                dep_internationPreCovid +
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
"""
