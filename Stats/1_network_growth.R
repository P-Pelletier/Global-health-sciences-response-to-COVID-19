#********************
# network growth
# MM. 08.06.2022
#********************



########## prelude ###########
# clear memory #
rm(list = ls()) ; gc();gc();gc();gc();gc(); ls(); search()

# load libraries #
library(igraph)
library(kableExtra)
library(knitr)

setwd("G:/Github/Global-health-sciences-response-to-COVID-19")

# load functions #
source("Moritz/aux_fcts.R")
source("Moritz/netw_fcts.R")

# define paths
inputPath <- "data/"

# read data
sample <- read_pubmed(inputPath=inputPath)

get_netw_growth <- function(CRR){
 
  ts <- c(201901:201912,202001:202012,202101:202112,202201:202212)
  res_mat <- matrix(NA,nrow=length(ts),ncol=2)
  rownames(res_mat) <- c(paste0(19,"/",1:12),paste0(20,"/",1:12),paste0(21,"/",1:12),paste0(22,"/",1:12))
  colnames(res_mat) <- c("nodes","weights")
  
  for(i in 1:length(ts)){
    t_end <- ts[i]
    g1 <- get_acc_network(sample=sample, covid=CRR, start=201901, end=t_end) 
    s <- strength(g1,V(g1))
    adj <- get.adjacency(g1,type="upper",attr="weight")
    
    res_mat[i,1] <- sum(s>0)
    res_mat[i,2] <- sum(adj)
    
  }
  netw_g <- tibble(month_name=rownames(res_mat),month=ts,
                   active_countries=res_mat[,"nodes"],collab_links=res_mat[,"weights"]) %>%
    mutate(country_g = (active_countries/lag(active_countries) -1)*100,
           collab_g = (collab_links/lag(collab_links) -1)*100)
  return(netw_g)
}


netw_g <- netw_g_CRR %>%
  rename_with(.cols=active_countries:collab_g,function(x){paste0("CRR_",x)}) %>%
  left_join(netw_g_nonCRR %>%
              select(-month_name) %>%
              rename_with(.cols=active_countries:collab_g,function(x){paste0("nonCRR_",x)}),
            by="month")


View(netw_g)
# largest expansion - February 24, March 20, April 35, May 14, June 6 countries added


netw_g %>% select(-month) %>%
kbl(longtable = T, booktabs = T, caption = "Longtable", format="latex") %>%
  add_header_above(c(" ", "CRR" = 4, "non CRR" = 4)) %>%
  kable_styling(latex_options = c("repeat_header"))

library(ggplot2)

netw_g %>% mutate(t=1:n()) %>%
  ggplot(aes(x=t)) +
  geom_line(aes(y=CRR_active_countries, group=1)) +
  geom_line(aes(y=nonCRR_active_countries, group=1))

netw_g %>% mutate(t=1:n()) %>%
  ggplot(aes(x=t)) +
  geom_line(aes(y=CRR_collab_links, group=1)) +
  geom_line(aes(y=nonCRR_collab_links, group=1)) + 
  scale_y_log10()

netw_g %>% mutate(t=1:n()) %>%
  ggplot(aes(x=t)) + 
  geom_line(aes(y=CRR_country_g, group=1)) +
  geom_line(aes(y=nonCRR_country_g, group=1),col="red") +
  geom_line(aes(y=CRR_collab_g, group=1),linetype=2) +
  geom_line(aes(y=nonCRR_collab_g, group=1),col="red",linetype=2)
  
  



