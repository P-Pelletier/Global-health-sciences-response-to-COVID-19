#**************************************************************
# Creates the `IRC link formation' regression table, Table 3, in the paper
# MM, last update 27.04.2022
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

# libraries
library(dplyr)
library(tidyr)
library(sna)
library(igraph)
library(pscl) # zero-inflated models
library(parallel)

########### SETTINGS ###########
SETTINGS <- list(
  # data input
  inputPath = "G:/Github/Global-health-sciences-response-to-COVID-19/data/",
  # output path
  outputPath = "G:/Github/Global-health-sciences-response-to-COVID-19/Results",
  logfile = NULL, #"link_formation_regression_log.txt",
  verbose = TRUE,
  
  # for data
  # constant variables (except for initial paper output)
  vars_const = c("iso_code","country","region","coronaPubsPreCovid", "nonCoronaPubsPreCovid",
                 "pop","gdp_pc","hdi","latitude","longitude"), 
  # time varying vars (except for network vars)
  vars_t = c("total_cases","total_deaths", "total_cases_per_million", "total_deaths_per_million",
             "Quarantine", "Ban", "Closure", "Recommend_home", "Require_home", "Lock_home"),
  # country to dyad transformations of variables
  vars_mult = c("coronaPubsPreCovid","nonCoronaPubsPreCovid","border_closure","lockdown","pop"), # log(x)+log(y), ".mult"
  vars_sum_absdiff = c("nonCoronaPubsPreCovid", "total_deaths", "hdi", "gdp_pc"), # log(sum(x,y)), log(abs(x-y)) , ".sum",".absdiff"
  vars_log = c("coronaPubsPreCovid.xy","nonCoronaPubsPreCovid.xy","geodist"), # log(a+1) #keep name
  
  # for regression
  t= c(201910:201912,202001:202012,202101:202112,202201:202212), # 202007:202012, #c(202002:202012,202101:202104) # 202101:202104, #202007:202012, # c(202002:202012,202101:202104),  # c(202002:202012), #202101:202104, # c(202002:202012,202104),
  vars=list(
    depvar="coronaPubs.xy",
    countvars=c("coronaPubsPreCovid.xy","nonCoronaPubsPreCovid.xy",
                "coronaPubsPreCovid.mult", # nonCoronaPubsPreCovid.mult
                "total_deaths.sum", "total_deaths.absdiff", "border_closure.mult", "lockdown.mult",
                "pop.mult","gdp_pc.sum","gdp_pc.absdiff", "sameRegion"), # ,"hdi.sum","hdi.absdiff","geodist", "sameRegion"
    zerovars=c("geodist","nonCoronaPubsPreCovid.mult") # "nonCoronaPubsPreCovid.sum", "nonCoronaPubsPreCovid.absdiff"
  ),
  B=30, # number of permutations in mrqap
  onesided = FALSE,
  cores=8,
  mrqap_outfile="link_formation_xqap_extendedtmp.RData"
)

# logging
if(!is.null(SETTINGS$logfile)){
  sink(file=paste0(SETTINGS$outputPath,SETTINGS$logfile))
  cat("SETTINGS\n\n")
  print(SETTINGS)
  cat("\n*********************\n\n")
}else{
  sink()
}


####### load functions #######
source("G:/Github/Global-health-sciences-response-to-COVID-19/Moritz/aux_fcts.R")
source("G:/Github/Global-health-sciences-response-to-COVID-19/Moritz/netw_fcts.R")

RNGkind("L'Ecuyer-CMRG")
set.seed(1)



add_qap <- function(B,res_set,dyads_t){
  # adds qap p-values to res_set 
  get_p_val <- function(z,z_dist,onesided){
    # returns p-value of z being from z_dist (one or two sided possible)
    z_dist <- z_dist[!is.na(z_dist)]
    
    if(onesided){
      # look on the left and right separately 
      if(z<0){
        p_val <- sum(z > z_dist) / length(z_dist) 
      }else{
        p_val <- sum(z < z_dist) / length(z_dist) 
      }
    }else{ # twosided - more extreme in absolute value? - With non-symmetric distribution under the null, this is actually not correct.
      p_val <- sum(abs(z) > abs(z_dist)) / length(z_dist) 
    }
    return(p_val)
  }
  
  get_z_val <- function(res_set,var,count_var){
    # get z-stat
    if(count_var){
      x_hat <- res_set$coefficients$count
    }else{
      x_hat <- res_set$coefficients$zero
    }
    x_hat <- x_hat[var]
    
    if(count_var){
      varname2 <- paste0("count_",var)
    }else{
      varname2 <- paste0("zero_",var)
    }
    x_sd <- vcov(res_set)[varname2,varname2]^0.5
    
    # z-value
    return(x_hat / x_sd)
  }
  
  do_one_permutation <- function(i,adj,dyads_t,res_set,var,count_var){
    # do permutation on the (permuted) adjacency
    adj <- sna::rmperm(adj)
    # replace permuted var
    dyads_t[,var] <- as.vector(adj[lower.tri(adj,diag=FALSE)])
    # estimate model
    mb <- zeroinfl(formula = res_set$formula, data = dyads_t, 
                   dist = "negbin", link="logit", 
                   EM = FALSE ,reltol=0.000001, 
                   start=list(count=res_set$coefficients$count,
                              zero=res_set$coefficients$zero,
                              theta=res_set$theta)) # , start=start
    # get z-value for that simulation
    z <- get_z_val(res_set=mb,var=var,count_var=count_var)
    return(z)
  }
  
  # dep var
  dep_var <- SETTINGS$vars$depvar
  # zero-vars
  zero_vars <- attributes(res_set$terms$zero)$term.labels
  # count-vars
  count_vars <- attributes(res_set$terms$count)$term.labels
  
  # all rhs vars
  vars <- c(zero_vars,count_vars)
  names(vars) <- c(rep("zero",times=length(zero_vars)),
                   rep("count",times=length(count_vars)))
  
  # formula
  form_m1 <- paste0(depvar, " ~ ", paste(countvars,collapse=" + "), " | ",
                    paste(zerovars,collapse=" + "))
  
  # result container for p-values
  z_values <- matrix(NA, nrow=B, ncol=length(vars))
  colnames(z_values) <- paste0(names(vars),".",vars)
  p_values <- rep(NA, times=length(vars))
  names(p_values) <- paste0(names(vars),".",vars)
  for(i in 1:length(vars)){
    
    # indicate zero/count var
    count_var <- ifelse(names(vars)[i]=="count",TRUE,FALSE)
    
    # get estimated z_value
    z_hat <- get_z_val(res_set=res_set,var=vars[i],count_var=count_var)
    
    # graph to get adjacency weighted by variable
    g <- igraph::graph.edgelist(cbind(dyads_t$nodeId.x,dyads_t$nodeId.y))
    g <- igraph::set.edge.attribute(g,vars[i],index=E(g),value=dyads_t[,vars[i]])
    adj <- as.matrix(igraph::get.adjacency(g,type="both",attr=vars[i]))
    
    z_values[,i] <- mclapply(1:SETTINGS$B, 
                      do_one_permutation,
                      adj=adj,dyads_t=dyads_t,res_set=res_set,var=vars[i],count_var=count_var,
                      mc.cores = SETTINGS$cores) %>% unlist()

    # calculate p-value
    p_values[i] <- 1-get_p_val(z=z_hat,z_dist=z_values[,i],onesided=FALSE)
  }
  
  # package and return
  res_set$qap <- list(z_val=z_values,p_val=p_values)
  return(res_set)  
}

add_qap_depricated <- function(B,res_set,dyads_t){
  # adds qap p-values to res_set 
  get_p_val <- function(z,z_dist,onesided){
    # returns p-value of z being from z_dist (one or two sided possible)
    z_dist <- z_dist[!is.na(z_dist)]
    
    if(onesided){
      # look on the left and right separately 
      if(z<0){
        p_val <- sum(z > z_dist) / length(z_dist) 
      }else{
        p_val <- sum(z < z_dist) / length(z_dist) 
      }
    }else{ # twosided - more extreme in absolute value? - With non-symmetric distribution under the null, this is actually not correct.
      p_val <- sum(abs(z) > abs(z_dist)) / length(z_dist) 
    }
    return(p_val)
  }
  
  get_z_val <- function(res_set,var,count_var){
    # get z-stat
    if(count_var){
      x_hat <- res_set$coefficients$count
    }else{
      x_hat <- res_set$coefficients$zero
    }
    x_hat <- x_hat[var]
    
    if(count_var){
      varname2 <- paste0("count_",var)
    }else{
      varname2 <- paste0("zero_",var)
    }
    x_sd <- vcov(res_set)[varname2,varname2]^0.5
    
    # z-value
    return(x_hat / x_sd)
  }
  
  # dep var
  dep_var <- SETTINGS$vars$depvar
  # zero-vars
  zero_vars <- attributes(res_set$terms$zero)$term.labels
  # count-vars
  count_vars <- attributes(res_set$terms$count)$term.labels
  
  # all rhs vars
  vars <- c(zero_vars,count_vars)
  names(vars) <- c(rep("zero",times=length(zero_vars)),
                   rep("count",times=length(count_vars)))
  
  # formula
  form_m1 <- paste0(depvar, " ~ ", paste(countvars,collapse=" + "), " | ",
                    paste(zerovars,collapse=" + "))
  
  # result container for p-values
  z_values <- matrix(NA, nrow=B, ncol=length(vars))
  colnames(z_values) <- paste0(names(vars),"_",vars)
  p_values <- rep(NA, times=length(vars))
  names(p_values) <- paste0(names(vars),"_",vars)
  for(i in 1:length(vars)){
    
    # indicate zero/count var
    count_var <- ifelse(names(vars)[i]=="count",TRUE,FALSE)
    
    # get estimated z_value
    z_hat <- get_z_val(res_set=res_set,var=vars[i],count_var=count_var)
    
    # graph to get adjacency weighted by variable
    g <- igraph::graph.edgelist(cbind(dyads_t$nodeId.x,dyads_t$nodeId.y))
    g <- igraph::set.edge.attribute(g,vars[i],index=E(g),value=dyads_t[,vars[i]])
    adj <- as.matrix(igraph::get.adjacency(g,type="both",attr=vars[i]))
    
    # permute on copy
    dyads_t <- dyads_t
    for(b in 1:B){
      # do permutation on the (permuted) adjacency
      adj <- sna::rmperm(adj)
      # replace permuted var
      dyads_t[,vars[i]] <- as.vector(adj[lower.tri(adj,diag=FALSE)])
      # estimate model
      mb <- zeroinfl(formula = as.formula(form_m1), data = dyads_t, dist = "negbin", link="logit", 
                     EM = FALSE ,reltol=0.000001, 
                     start=list(count=res_set$coefficients$count,
                                zero=res_set$coefficients$zero,
                                theta=res_set$theta)) # , start=start
      # get z-value for that simulation
      z_values[b,i] <- get_z_val(res_set=mb,var=vars[i],count_var=count_var)
    }
    # calculate p-value
    p_values[i] <- get_p_val(z=z_hat,z_dist=z_values[b,],onesided=FALSE)
  }
  
  # package and return
  res_set$qap <- list(z_val=z_values,p_val=p_values)
  return(res_set)  
}

####### prepare sample ######
# load data
panel <- get_country_panel(inputPath=SETTINGS$inputPath)

# extract initial conditions
preCovid <- panel %>%
  filter(month <= 201909) %>%
  group_by(countryCode) %>%
  summarize(coronaPubsPreCovid = mean(coronaPubs),
            nonCoronaPubsPreCovid = mean(nonCoronaPubs))

# get country-level panel
panel <- panel %>% 
  left_join(preCovid,by="countryCode")

# add node ids
node_ids <- tibble(countryCode=sort(unique(panel$countryCode)),
                   nodeId=1:length(unique(panel$countryCode)))

panel1 <- panel %>% 
  select(-alters) %>%
  left_join(node_ids,by="countryCode")

# extract links pre covid
linksPreCovid <- panel %>%
  unnest(alters) %>%
  filter(month <= 201909) %>% # 201912
  rename(countryCode.x=countryCode,
         countryCode.y=targetCountryCode) %>%
  group_by(countryCode.x,countryCode.y) %>%
  summarise(coronaPubsPreCovid.xy = mean(weight_corona),
            nonCoronaPubsPreCovid.xy = mean(weight_noncorona), .groups="drop")

# extract any links
links <- panel %>% 
  unnest(alters) %>%
  rename(countryCode.x=countryCode,
         countryCode.y=targetCountryCode,
         coronaPubs.xy = weight_corona,
         nonCoronaPubs.xy=weight_noncorona) %>%
  select(countryCode.x,countryCode.y,month,coronaPubs.xy,nonCoronaPubs.xy)

# create tibble with all potential dyad-months, add edges pre-covid and covid
dyads <- full_join(x=panel1,y=panel1,by="month") %>%
  filter(month >= 201910) %>%
  left_join(links, by=c("countryCode.x","countryCode.y","month")) %>%
  left_join(linksPreCovid,  by=c("countryCode.x","countryCode.y")) %>%
  mutate(coronaPubs.xy=ifelse(is.na(coronaPubs.xy),0,coronaPubs.xy),
         nonCoronaPubs.xy=ifelse(is.na(nonCoronaPubs.xy),0,nonCoronaPubs.xy),
         coronaPubsPreCovid.xy=ifelse(is.na(coronaPubsPreCovid.xy),0,coronaPubsPreCovid.xy),
         nonCoronaPubsPreCovid.xy=ifelse(is.na(nonCoronaPubsPreCovid.xy),0,nonCoronaPubsPreCovid.xy)) %>%
  group_by(countryCode.x,countryCode.y) %>%
  mutate(coronaPubsCum.xy = cumsum(coronaPubs.xy)) %>%
  ungroup()

# add variables
x <- dyads %>% select(longitude.x,latitude.x) %>% rename(lon=longitude.x,lat=latitude.x)
y <- dyads %>% select(longitude.y,latitude.y) %>% rename(lon=longitude.y,lat=latitude.y)

mult_xy <- function(x,y){ log((x*y)+1)}
sum_xy <- function(x,y){log(x+y+1)}
absdiff_xy <- function(x,y){ log(abs(x-y)+1)}

dyads <- dyads %>% 
  mutate(geodist = geodist::geodist(x=x,y=y,paired=TRUE, measure="geodesic") / 1000,
         geodist = log(geodist+1),
         sameRegion = ifelse(region.x==region.y,1,0),
         coronaPubsPreCovid.xy = log(coronaPubsPreCovid.xy+1),
         nonCoronaPubsPreCovid.xy = log(nonCoronaPubsPreCovid.xy+1),
         nonCoronaPubsPreCovid.sum = sum_xy(nonCoronaPubsPreCovid.x,nonCoronaPubsPreCovid.y),
         nonCoronaPubsPreCovid.absdiff = absdiff_xy(nonCoronaPubsPreCovid.x,nonCoronaPubsPreCovid.y),
         coronaPubsPreCovid.mult = mult_xy(coronaPubsPreCovid.x,coronaPubsPreCovid.y),
         nonCoronaPubsPreCovid.mult = mult_xy(nonCoronaPubsPreCovid.x,nonCoronaPubsPreCovid.y),
         border_closure.mult = mult_xy(border_closure_days_past12m.x,border_closure_days_past12m.y),
         lockdown.mult = mult_xy(lockdown_days_past12m.x,lockdown_days_past12m.y),
         pop.mult = mult_xy(pop.x,pop.y),
         total_deaths.sum = sum_xy(cum_deaths.x,cum_deaths.y),
         total_deaths.absdiff = absdiff_xy(cum_deaths.x,cum_deaths.y),
         gdp_pc.sum = sum_xy(gdp_pc.x,gdp_pc.y),
         gdp_pc.absdiff = absdiff_xy(gdp_pc.x,gdp_pc.y),
         hdi.sum = sum_xy(hdi.x,hdi.y),
         hdi.absdiff = absdiff_xy(hdi.x,hdi.y))



###### regression ######

#last_month <- 202112
#months <- c(202003:202012,202101:last_month)
SETTINGS$periods <- list(c(201909,201912),c(202001,202006),c(202007,202012),
                         c(202101,202106),c(202107,202112))
res_lst <- list()
for(i in 1:length(SETTINGS$periods)){
  
  # get estimation variables
  depvar <- SETTINGS$vars$depvar 
  countvars <- SETTINGS$vars$countvars
  zerovars <- SETTINGS$vars$zerovars
  
  if(SETTINGS$verbose){
    print("**********")
    print(SETTINGS$periods[i])
  }
  startYear <- SETTINGS$periods[[i]][1]
  endYear <- SETTINGS$periods[[i]][2]
  
  
  # extract estimation data
  dyads_t <- dyads %>% 
    filter(month >= startYear & month <= endYear,
             nodeId.x < nodeId.y) %>% 
    group_by(nodeId.x,nodeId.y) %>%
    summarise(across(.cols=all_of(c(depvar,countvars,zerovars)),.fns=sum),.groups="drop") %>%
    as.data.frame()
  
  # remove CHN
  # country_id <- dyads_t %>% filter(countryCode.x=="CHN") %>% slice(1) %>% pull(nodeId.x)
  # dyads_t <- dyads_t %>% filter(countryCode.x!="CHN" & countryCode.y!="CHN") %>%
  #   mutate(nodeId.x=ifelse(nodeId.x<country_id,nodeId.x,nodeId.x-1),
  #          nodeId.y=ifelse(nodeId.y<country_id,nodeId.y,nodeId.y-1))
  # 
  # country_id <- dyads_t %>% filter(countryCode.x=="USA") %>% slice(1) %>% pull(nodeId.x)
  # dyads_t <- dyads_t %>% filter(countryCode.x!="USA" & countryCode.y!="USA") %>%
  #   mutate(nodeId.x=ifelse(nodeId.x<country_id,nodeId.x,nodeId.x-1),
  #          nodeId.y=ifelse(nodeId.y<country_id,nodeId.y,nodeId.y-1))
  # 
  
  # check whether all variables are varying over observations - remove otherwise
  sd_countvars <- apply(as.matrix(dyads_t[,countvars]), 2, function(x) sd(x))
  if(any(sd_countvars==0)){
    countvars <- countvars[!countvars %in% names(sd_countvars)[sd_countvars==0]]
  }
  
  sd_zerovars <- apply(as.matrix(dyads_t[,zerovars]), 2, function(x) sd(x))
  if(any(sd_zerovars==0)){
    zerovars <- zerovars[!zerovars %in% names(sd_zerovars)[sd_zerovars==0]]
  }
  
  # create formula
  form_m1 <- as.formula(paste0(depvar, " ~ ", paste(countvars,collapse=" + "), " | ",
                    paste(zerovars,collapse=" + ")))
  
  m1 <- zeroinfl(formula = form_m1, data = dyads_t, 
                 dist = "negbin", link="logit", EM = FALSE)
  m1$formula <- form_m1 # just more handy
  
  if(SETTINGS$verbose){
    print(summary(m1))
  }

  #system.time(add_qap(B=SETTINGS$B, res_set=m1, dyads_t=dyads_t))
  m1 <- add_qap(B=SETTINGS$B, res_set=m1, dyads_t=dyads_t)
  
  if(SETTINGS$verbose){
    print("pvals")
    print(m1$qap$p_val)
    print("NAs")
    print(apply(m1$qap$z_val,2,function(x)sum(is.nan(x)|is.na(x))))
  }
  
  # save this result
  res_lst[[i]] <- m1
}


plot(dyads_t$nonCoronaPubsPreCovid.xy,log(dyads_t$nonCoronaPubs.xy))

####### print result #####


extract_result <- function(res){
  # get coefficients
  coeffs <- unlist(res$coefficients)
  coeff_names <- names(coeffs)
  # get z values
  z_vals <- coeffs / diag(vcov(res))^0.5
  
  # get p values
  p_vals <- coeffs
  p_vals[] <- NA
  p_vals[names(res$qap$p_val)] <- res$qap$p_val
  
  
  zp_line <- paste0("(",round(z_vals,3),", ",round(p_vals,3),")")
  names(zp_line) <- paste0(names(coeffs),"_zp")
  
  stars <- case_when(p_vals < 0.001 ~ "***",
                     p_vals < 0.01 ~ "**",
                     p_vals < 0.05 ~ "*",
                     TRUE ~ "")
  
  coeffs <- paste0(round(coeffs,digits=3),stars)
  names(coeffs) <- coeff_names
  # theta
  sum_est <- summary(res)
  theta <- sum_est$coefficients[[1]]["Log(theta)",c(1,2,4)]
  
  theta_star <- ifelse(theta[3] < 0.05,"*","")
  theta_star <- ifelse(theta[3] < 0.01,"**",theta_star)
  theta_star <- ifelse(theta[3] < 0.001,"***",theta_star)
  
  theta <- c(logTheta=paste0(round(theta[1],digits=3),theta_star),
             logTheta_zp=paste0("(",round(theta[1]/theta[2],3),", ",round(theta[3],3),")"))
  
  modelStats <- c(loglik=round(sum_est$loglik), obs=sum_est$n)
  
  return(c(coeffs,zp_line,theta,modelStats))
}

res_lst2 <- lapply(res_lst,extract_result)
row_names <- unique(unlist((lapply(res_lst2,names))))

res_mat <- matrix(NA,nrow=length(row_names),ncol=length(res_lst2))
rownames(res_mat) <- row_names
for(i in 1:length(res_lst2)){
  res_mat[names(res_lst2[[i]]),i] <- res_lst2[[i]]
}
res_mat <- res_mat[order(row_names),]

View(res_mat)

colnames(res_mat) <- SETTINGS$t
ncol(res_mat)
cat("************* output table **************\n\n\n")
library(xtable)
print(xtable(res_mat))
print(xtable(res_mat[,1:12]))
print(xtable(res_mat[,13:20]))
print(xtable(res_mat[,21:27]))


######### plot result ######
library(ggplot2)

# collect results
res_lst2 <- list()
for(i in 1:length(res_lst)){
  # extract estimation from observation 
  count_coeffs <- res_lst[[i]]$coefficients$count
  names(count_coeffs) <- paste0("count_",names(count_coeffs))
  zero_coeffs <- res_lst[[i]]$coefficients$zero
  names(zero_coeffs) <- paste0("zero_",names(zero_coeffs))
  coeffs <- c(count_coeffs,zero_coeffs)
  sds <- diag(res_lst[[i]]$vcov)^0.5
  
  res_lst2[[i]] <- data.frame(model=i,
                              term=names(coeffs),
                              estimate=coeffs,
                              std.error=sds)
}
res <- do.call(rbind.data.frame, res_lst2) %>% tibble() %>%
  mutate(lb=estimate-1.96*std.error, 
         ub=estimate+1.96*std.error)

pd <- position_dodge(width=0.5)
oma2 <- c(1,1,1,1)

p <- res %>% 
  filter(term %in% c("count_coronaPubsPreCovid.xy","count_nonCoronaPubsPreCovid.xy",
                     "count_coronaPubsPreCovid.mult","count_nonCoronaPubsPreCovid.mult")) %>% 
  filter(model>1) %>%
  ggplot(aes(x=model, y=estimate, color=term, linetype=term, group=term)) +
  scale_colour_manual("", 
                      labels = c("c_ij,t0","n_ij,t0",
                                 "c_i,t0", "n_i,t0"),
                      breaks = c("count_coronaPubsPreCovid.xy","count_nonCoronaPubsPreCovid.xy",
                                 "count_coronaPubsPreCovid.mult","count_nonCoronaPubsPreCovid.mult"),
                      values = c("red", "darkblue", "orange", "lightblue")) +
  scale_linetype_manual(name= "", 
                        breaks = c("count_coronaPubsPreCovid.xy","count_nonCoronaPubsPreCovid.xy",
                                   "count_coronaPubsPreCovid.mult","count_nonCoronaPubsPreCovid.mult"),
                        labels = c("c_ij,t0","n_ij,t0","c_i,t0", "n_i,t0"),
                        values = c("solid","solid","dashed","dashed")) +
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

outfile <- paste0(SETTINGS$outputPath,"IRC_regression_dw.pdf")
#ggsave(outfile, width = 6, height = 3)
pdf(file=outfile, width=6, height=4, family="Helvetica")#, pointsize=2)
p
dev.off()



#### b
unique(res$term)

breaks <- c(#"count_gdp_pc.sum","count_gdp_pc.absdiff",
            "count_hdi.sum", "count_hdi.absdiff"
            #"count_total_deaths.sum", "count_total_deaths.absdiff"
            )
labels <- c(#"sum(gdp)","diff(gdp)",
            "sum(hdi)","diff(hdi)"
            #"sum(deaths)","diff(deaths)"
            )

p <- res %>% 
  filter(term %in% breaks) %>% 
  filter(model>1) %>%
  ggplot(aes(x=model, y=estimate, color=term, linetype=term, group=term)) +
  scale_colour_manual("", 
                      labels = labels,
                      breaks = breaks,
                      values = c(#"dodgerblue", "dodgerblue3",
                                 "tomato1", "tomato3"
                                 #"springgreen2", "springgreen4"
                                 )) +
  scale_linetype_manual(name= "", 
                        breaks = breaks,
                        labels = labels,
                        values = c(#"solid","dashed",
                                   #"solid","dashed",
                                   "solid","dashed")) +
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

p

outfile <- paste0(SETTINGS$outputPath,"IRC_regression_dw_hdi.pdf")
#ggsave(outfile, width = 6, height = 3)
pdf(file=outfile, width=5, height=4, family="Helvetica")#, pointsize=2)
p
dev.off()





res %>% 
  filter(term %in% c("count_coronaPubsPreCovid.xy","count_nonCoronaPubsPreCovid.xy",
                     "count_coronaPubsPreCovid.mult","count_nonCoronaPubsPreCovid.mult")) %>% 
  filter(model>1) %>%
  ggplot(aes(x=model, y=estimate, color=term, group=term)) +
  geom_line(position=pd) +
  ?scale_linetype_discrete(#name= "", 
    #breaks = c("count_coronaPubsPreCovid.xy","count_nonCoronaPubsPreCovid.xy",
    #           "count_coronaPubsPreCovid.mult","count_nonCoronaPubsPreCovid.mult"),
    #labels = c("c_ij,t0","n_ij,t0","c_i,t0", "n_i,t0"),
    values = c("solid","solid","dashed","dashed"))




plot(dyads_t$nonCoronaPubsPreCovid.sum,log(dyads_t$coronaPubsCum.xy+1))

library(purrr)
SETTINGS$vars_mult <- c("solePubs","collabPubs")
tmp <- map_dfr(.x=SETTINGS$vars_mult, 
    .f= ~ el %>% 
      select(starts_with(.x)) %>%
      mutate("{.x}.xy" := log(.[[1]]+1) + log(.[[2]]+1)) %>%
      select(!!!paste0(.x,".xy"))
)

View(tmp)


xvars <- paste0(SETTINGS$vars_mult)
SETTINGS$vars_mult
coronaPubsPreCovid.xy = log(coronaPubsPreCovid.x+1)+log(coronaPubsPreCovid.y+1)

View(el)

sink()


