library(dplyr)
library(data.table)
library(plotly)
library(tidyr)
library(processx)
library(stringr)
library(jsonlite)
library(mongolite)

summary_stat <- function(dataset,date_var,txt){
  setDT(dataset)
  dataset1 = dataset[eval(parse(text =paste0('stringr::str_detect(',date_var,',"2019")'))),]
  dataset2 = dataset[eval(parse(text =paste0('stringr::str_detect(',date_var,',"2020")'))),]
  txt_file <- stringr::str_replace_all(paste(c("
ççbegin{table}[t!]ççfootnotesize
ççcentering  
ççcaption{Descriptive statistics of the variables (",stringr::str_replace_all(txt,'_',' '),")}
ççbegin{threeparttable}
ççlabel{tab:Descriptives} 
ççbegin{tabular}{@{ççextracolsep{-7pt}} lcccccc} 
ççhline
& ççmulticolumn{2}{c}{ççtextit{Coronavirus related}} 
& ççmulticolumn{2}{c}{ççtextit{Others Documents}}
& ççmulticolumn{2}{c}{ççtextit{All Documents}} çççç 
ççcline{2-7} 
çççç[-1.8ex] & ççmulticolumn{1}{c}{2019}
&ççmulticolumn{1}{c}{2020}
&ççmulticolumn{1}{c}{2019}
& ççmulticolumn{1}{c}{2020}
&ççmulticolumn{1}{c}{2019}
&ççmulticolumn{1}{c}{2020} çççç 
  ççhline
  çççç[5pt]
  Nb of authors & ",
                                      round(median(dataset1[is_coronavirus_lower == 1,team_size]),2),"/",
                                      round(mean(dataset1[is_coronavirus_lower == 1,team_size]),2)," (",
                                      round(sd(dataset1[is_coronavirus_lower == 1,team_size]),2),") & ",
                                      round(median(dataset2[is_coronavirus_lower == 1,team_size]),2),"/",
                                      round(mean(dataset2[is_coronavirus_lower == 1,team_size]),2)," (",
                                      round(sd(dataset2[is_coronavirus_lower == 1,team_size]),2),") & ",
                                      
                                      round(median(dataset1[is_coronavirus_lower == 0,team_size]),2),"/",
                                      round(mean(dataset1[is_coronavirus_lower == 0,team_size]),2)," (",
                                      round(sd(dataset1[is_coronavirus_lower == 0,team_size]),2),") & ",
                                      round(median(dataset2[is_coronavirus_lower == 0,team_size]),2),"/",
                                      round(mean(dataset2[is_coronavirus_lower == 0,team_size]),2)," (",
                                      round(sd(dataset2[is_coronavirus_lower == 0,team_size]),2),") & ",
                                      
                                      round(median(dataset1[,team_size]),2),"/",
                                      round(mean(dataset1[,team_size]),2)," (",
                                      round(sd(dataset1[,team_size]),2),") & ",
                                      round(median(dataset2[,team_size]),2),"/",
                                      round(mean(dataset2[,team_size]),2)," (",
                                      round(sd(dataset2[,team_size]),2),") çççç 
  çççç[5pt]
  Solo authors  & ",
                                      round(mean(dataset1[is_coronavirus_lower == 1,solo_aut])*100,2)," & ",
                                      round(mean(dataset2[is_coronavirus_lower == 1,solo_aut])*100,2)," & ",
                                      round(mean(dataset1[is_coronavirus_lower == 0,solo_aut])*100,2)," & ",
                                      round(mean(dataset2[is_coronavirus_lower == 0,solo_aut])*100,2)," & ",
                                      round(mean(dataset1[,solo_aut])*100,2)," & ",
                                      round(mean(dataset2[,solo_aut])*100,2),"çççç
    çççç[5pt]
  International collab.  & ",
                                      round(mean(dataset1[is_coronavirus_lower == 1,inter_collab])*100,2)," & ",
                                      round(mean(dataset2[is_coronavirus_lower == 1,inter_collab])*100,2)," & ",
                                      round(mean(dataset1[is_coronavirus_lower == 0,inter_collab])*100,2)," & ",
                                      round(mean(dataset2[is_coronavirus_lower == 0,inter_collab])*100,2)," & ",
                                      round(mean(dataset1[,inter_collab])*100,2)," & ",
                                      round(mean(dataset2[,inter_collab])*100,2),
  #çççç[5pt]
  #Intranational collab. & ",
  #                                    round(mean(dataset1[is_coronavirus_lower == 1,intra_collab])*100,2)," & ",
  #                                    round(mean(dataset2[is_coronavirus_lower == 1,intra_collab])*100,2)," & ",
  #                                    round(mean(dataset1[is_coronavirus_lower == 0,intra_collab])*100,2)," & ",
  #                                    round(mean(dataset2[is_coronavirus_lower == 0,intra_collab])*100,2)," & ",
  #                                    round(mean(dataset1[,intra_collab])*100,2)," & ",
  #                                    round(mean(dataset2[,intra_collab])*100,2),"çççç
  #çççç[5pt]
  #Intra/Inter collab. & ",
  #                                    round(mean(dataset1[is_coronavirus_lower == 1,intra_inter])*100,2)," & ",
  #                                    round(mean(dataset2[is_coronavirus_lower == 1,intra_inter])*100,2)," & ",
  #                                    round(mean(dataset1[is_coronavirus_lower == 0,intra_inter])*100,2)," & ",
  #                                    round(mean(dataset2[is_coronavirus_lower == 0,intra_inter])*100,2)," & ",
  #                                    round(mean(dataset1[,intra_inter])*100,2)," & ",
  #                                    round(mean(dataset2[,intra_inter])*100,2),"çççç
  #"çççç
  #çççç[5pt]
  # Nb of references & ",
  #                                    round(median(dataset1[is_coronavirus_lower == 1,reference.count]),2),"/",
  #                                    round(mean(dataset1[is_coronavirus_lower == 1,reference.count]),2)," (",
  #                                    round(sd(dataset1[is_coronavirus_lower == 1,reference.count]),2),") & ",
  #                                    round(median(dataset2[is_coronavirus_lower == 1,reference.count]),2),"/",
  #                                    round(mean(dataset2[is_coronavirus_lower == 1,reference.count]),2)," (",
  #                                    round(sd(dataset2[is_coronavirus_lower == 1,reference.count]),2),") & ",
  #                                    
  #                                    round(median(dataset1[is_coronavirus_lower == 0,reference.count]),2),"/",
  #                                    round(mean(dataset1[is_coronavirus_lower == 0,reference.count]),2)," (",
  #                                    round(sd(dataset1[is_coronavirus_lower == 0,reference.count]),2),") & ",
  #                                    round(median(dataset2[is_coronavirus_lower == 0,reference.count]),2),"/",
  #                                    round(mean(dataset2[is_coronavirus_lower == 0,reference.count]),2)," (",
  #                                    round(sd(dataset2[is_coronavirus_lower == 0,reference.count]),2),") & ",
  #                                    
  #                                    round(median(dataset1[,reference.count]),2),"/",
  #                                    round(mean(dataset1[,reference.count]),2)," (",
  #                                    round(sd(dataset1[,reference.count]),2),") & ",
  #                                    round(median(dataset2[,reference.count]),2),"/",
  #                                    round(mean(dataset2[,reference.count]),2)," (",
  #                                    round(sd(dataset2[,reference.count]),2),") çççç 
  #çççç[5pt]
  #Nb of citations & ",
  #                                    round(median(dataset1[is_coronavirus_lower == 1,is.referenced.by.count]),2),"/",
  #                                    round(mean(dataset1[is_coronavirus_lower == 1,is.referenced.by.count]),2)," (",
  #                                    round(sd(dataset1[is_coronavirus_lower == 1,is.referenced.by.count]),2),") & ",
  #                                    round(median(dataset2[is_coronavirus_lower == 1,is.referenced.by.count]),2),"/",
  #                                    round(mean(dataset2[is_coronavirus_lower == 1,is.referenced.by.count]),2)," (",
  #                                    round(sd(dataset2[is_coronavirus_lower == 1,is.referenced.by.count]),2),") & ",
  #                                    
  #                                    round(median(dataset1[is_coronavirus_lower == 0,is.referenced.by.count]),2),"/",
  #                                    round(mean(dataset1[is_coronavirus_lower == 0,is.referenced.by.count]),2)," (",
  #                                    round(sd(dataset1[is_coronavirus_lower == 0,is.referenced.by.count]),2),") & ",
  #                                    round(median(dataset2[is_coronavirus_lower == 0,is.referenced.by.count]),2),"/",
  #                                    round(mean(dataset2[is_coronavirus_lower == 0,is.referenced.by.count]),2)," (",
  #                                    round(sd(dataset2[is_coronavirus_lower == 0,is.referenced.by.count]),2),") & ",
  #                                    
  #                                    round(median(dataset1[,is.referenced.by.count]),2),"/",
  #                                    round(mean(dataset1[,is.referenced.by.count]),2)," (",
  #                                    round(sd(dataset1[,is.referenced.by.count]),2),") & ",
  #                                    round(median(dataset2[,is.referenced.by.count]),2),"/",
  #                                    round(mean(dataset2[,is.referenced.by.count]),2)," (",
  #                                    round(sd(dataset2[,is.referenced.by.count]),2),#") çççç 
#çççç[5pt]
#  Nb of locations & ",
#                                      round(median(dataset1[is_coronavirus_lower == 1,nb_loc]),2),"/",
#                                      round(mean(dataset1[is_coronavirus_lower == 1,nb_loc]),2)," (",
#                                      round(sd(dataset1[is_coronavirus_lower == 1,nb_loc]),2),") & ",
#                                      round(median(dataset2[is_coronavirus_lower == 1,nb_loc]),2),"/",
#                                      round(mean(dataset2[is_coronavirus_lower == 1,nb_loc]),2)," (",
#                                      round(sd(dataset2[is_coronavirus_lower == 1,nb_loc]),2),") & ",
#                                      
#                                      round(median(dataset1[is_coronavirus_lower == 0,nb_loc]),2),"/",
#                                      round(mean(dataset1[is_coronavirus_lower == 0,nb_loc]),2)," (",
#                                      round(sd(dataset1[is_coronavirus_lower == 0,nb_loc]),2),") & ",
#                                      round(median(dataset2[is_coronavirus_lower == 0,nb_loc]),2),"/",
#                                      round(mean(dataset2[is_coronavirus_lower == 0,nb_loc]),2)," (",
#                                      round(sd(dataset2[is_coronavirus_lower == 0,nb_loc]),2),") & ",
#                                      
#                                      round(median(dataset1[,nb_loc]),2),"/",
#                                      round(mean(dataset1[,nb_loc]),2)," (",
#                                      round(sd(dataset1[,nb_loc]),2),") & ",
#                                      round(median(dataset2[,nb_loc]),2),"/",
#                                      round(mean(dataset2[,nb_loc]),2)," (",
#                                      round(sd(dataset2[,nb_loc]),2),
                                      ") çççç 

çççç[5pt]
  Nb of country & ",
                                      round(median(dataset1[is_coronavirus_lower == 1,nb_country]),2),"/",
                                      round(mean(dataset1[is_coronavirus_lower == 1,nb_country]),2)," (",
                                      round(sd(dataset1[is_coronavirus_lower == 1,nb_country]),2),") & ",
                                      round(median(dataset2[is_coronavirus_lower == 1,nb_country]),2),"/",
                                      round(mean(dataset2[is_coronavirus_lower == 1,nb_country]),2)," (",
                                      round(sd(dataset2[is_coronavirus_lower == 1,nb_country]),2),") & ",
                                      
                                      round(median(dataset1[is_coronavirus_lower == 0,nb_country]),2),"/",
                                      round(mean(dataset1[is_coronavirus_lower == 0,nb_country]),2)," (",
                                      round(sd(dataset1[is_coronavirus_lower == 0,nb_country]),2),") & ",
                                      round(median(dataset2[is_coronavirus_lower == 0,nb_country]),2),"/",
                                      round(mean(dataset2[is_coronavirus_lower == 0,nb_country]),2)," (",
                                      round(sd(dataset2[is_coronavirus_lower == 0,nb_country]),2),") & ",
                                      
                                      round(median(dataset1[,nb_country]),2),"/",
                                      round(mean(dataset1[,nb_country]),2)," (",
                                      round(sd(dataset1[,nb_country]),2),") & ",
                                      round(median(dataset2[,nb_country]),2),"/",
                                      round(mean(dataset2[,nb_country]),2)," (",
                                      round(sd(dataset2[,nb_country]),2),") çççç 
çççç[5pt]
 Days received-accept & ",
                                      round(median(na.omit(dataset1[is_coronavirus_lower == 1,diff_time])),2),"/",
                                      round(mean(na.omit(dataset1[is_coronavirus_lower == 1,diff_time])),2)," (",
                                      round(sd(na.omit(dataset1[is_coronavirus_lower == 1,diff_time])),2),") & ",
                                      round(median(na.omit(dataset2[is_coronavirus_lower == 1,diff_time])),2),"/",
                                      round(mean(na.omit(dataset2[is_coronavirus_lower == 1,diff_time])),2)," (",
                                      round(sd(na.omit(dataset2[is_coronavirus_lower == 1,diff_time])),2),") & ",
                                      
                                      round(median(na.omit(dataset1[is_coronavirus_lower == 0,diff_time])),2),"/",
                                      round(mean(na.omit(dataset1[is_coronavirus_lower == 0,diff_time])),2)," (",
                                      round(sd(na.omit(dataset1[is_coronavirus_lower == 0,diff_time])),2),") & ",
                                      round(median(na.omit(dataset2[is_coronavirus_lower == 0,diff_time])),2),"/",
                                      round(mean(na.omit(dataset2[is_coronavirus_lower == 0,diff_time])),2)," (",
                                      round(sd(na.omit(dataset2[is_coronavirus_lower == 0,diff_time])),2),") & ",
                                      
                                      round(median(na.omit(dataset1[,diff_time])),2),"/",
                                      round(mean(na.omit(dataset1[,diff_time])),2)," (",
                                      round(sd(na.omit(dataset1[,diff_time])),2),") & ",
                                      round(median(na.omit(dataset2[,diff_time])),2),"/",
                                      round(mean(na.omit(dataset2[,diff_time])),2)," (",
                                      round(sd(na.omit(dataset2[,diff_time])),2),") çççç 
  çççç[5pt]
 Share aff. captured & ",
                                      round(median(dataset1[is_coronavirus_lower == 1,share_aff_captured]),2),"/",
                                      round(mean(dataset1[is_coronavirus_lower == 1,share_aff_captured]),2)," (",
                                      round(sd(dataset1[is_coronavirus_lower == 1,share_aff_captured]),2),") & ",
                                      round(median(dataset2[is_coronavirus_lower == 1,share_aff_captured]),2),"/",
                                      round(mean(dataset2[is_coronavirus_lower == 1,share_aff_captured]),2)," (",
                                      round(sd(dataset2[is_coronavirus_lower == 1,share_aff_captured]),2),") & ",
                                      
                                      round(median(dataset1[is_coronavirus_lower == 0,share_aff_captured]),2),"/",
                                      round(mean(dataset1[is_coronavirus_lower == 0,share_aff_captured]),2)," (",
                                      round(sd(dataset1[is_coronavirus_lower == 0,share_aff_captured]),2),") & ",
                                      round(median(dataset2[is_coronavirus_lower == 0,share_aff_captured]),2),"/",
                                      round(mean(dataset2[is_coronavirus_lower == 0,share_aff_captured]),2)," (",
                                      round(sd(dataset2[is_coronavirus_lower == 0,share_aff_captured]),2),") & ",
                                      
                                      round(median(dataset1[,share_aff_captured]),2),"/",
                                      round(mean(dataset1[,share_aff_captured]),2)," (",
                                      round(sd(dataset1[,share_aff_captured]),2),") & ",
                                      round(median(dataset2[,share_aff_captured]),2),"/",
                                      round(mean(dataset2[,share_aff_captured]),2)," (",
                                      round(sd(dataset2[,share_aff_captured]),2),") çççç 

  ççhline
   çç# Document & ",
                                      dim(dataset1[is_coronavirus_lower == 1,])[1],"(",
                                      round((dim(dataset1[is_coronavirus_lower == 1,])[1]/dim(dataset1)[1])*100,2),"çç%) & ",
                                    dim(dataset2[is_coronavirus_lower == 1,])[1],"(",
                                    round((dim(dataset2[is_coronavirus_lower == 1,])[1]/dim(dataset2)[1])*100,2),"çç%) & ",
                              
                              dim(dataset1[is_coronavirus_lower == 0,])[1],"(",
                              round((dim(dataset1[is_coronavirus_lower == 0,])[1]/dim(dataset1)[1])*100,2),"çç%) & ",
                              dim(dataset2[is_coronavirus_lower == 0,])[1],"(",
                              round((dim(dataset2[is_coronavirus_lower == 0,])[1]/dim(dataset2)[1])*100,2),"çç%) & ",

                              dim(dataset1)[1]," & ",
                              dim(dataset2)[1],"çççç
  ççhline
  ççend{tabular}
  ççbegin{tablenotes}
  ççfootnotesize
  ççitem {ççit Notes:} Binary indicators in [çç%], for continuous measures [median/mean (s.d.)].
  ççend{tablenotes}
 ççend{threeparttable}
ççend{table}

"),collapse = ""),'çç','\\\\')
  write.table(txt_file,file= paste(c('Descriptive_stat_data_table',txt,'.txt'),collapse = ''))
  
}

make_box_plot <-  function(df,var,is_covid){
  covid = ifelse(is_covid ==T,1,0)
  main_title = ifelse(is_covid ==T,'(Coronavirus related)','(Others)')
  df = df[is_coronavirus_lower==covid,]
  date = c(paste0(rep('2019',9),'-0',as.character(seq(1,9))),
           paste0(rep('2019',3),'-',as.character(seq(10,12))),
           paste0(rep('2020',9),'-0',as.character(seq(1,9))),
           paste0(rep('2020',3),'-',as.character(seq(10,12))))
  fig <- plot_ly(type = 'box')
  for(i in date){
    fig <- fig %>% add_boxplot(y = as.numeric(as.matrix(df[date_received==i,..var])),
                              #boxpoints = 'all',
                              color = '#aaabff',
                              marker= list( color = '#aaabff'),
                              line = list(color = 'rgb(9,56,105)'),
                             name = i) 
  
  }
 
  title_ = list(
    text = sprintf(paste('Team size over time ',main_title)),
    xref = "paper",
    yref = "paper",
    yanchor = "bottom",
    xanchor = "center",
    align = "center",
    x = 0.5,
    y = 1,
    showarrow = FALSE)
  fig <- fig %>% layout(
                        xaxis = list(title = 'Time'),
                        yaxis = list(title = '# of Authors',
                                     range = c(0,20)),
                        annotations = title_)
  
  return(fig)
}

setwd('C:/Users/Beta/Documents/GitHub/Global-health-sciences-response-to-COVID-19/Stats')
library(lubridate)

df <- fread("D:/otherdata/pubmed_covid_240621.csv")
setDT(df)

#df[,intra_collab:= ifelse(team_size>nb_country) ]
#df[,intra_inter:=inter_collab*intra_collab]
df[,solo_aut:=ifelse(team_size==1,1,0)]
df[,date_received:=ifelse(unix_received=='null'|unix_received=='','',as.character(format(lubridate::as_datetime(as.numeric(as.character(unix_received))),'%Y-%m')))]
df[,date_received_d:=ifelse(unix_received=='null'|unix_received=='','',as.character(format(lubridate::as_datetime(as.numeric(as.character(unix_received))),'%Y-%m-%d')))]
df[,date_accepted:=ifelse(unix_accepted=='null'|unix_accepted=='','',as.character(format(lubridate::as_datetime(as.numeric(as.character(unix_accepted))),'%Y-%m')))]
df[,date_medline:=ifelse(unix_medline=='null'|unix_medline =='','',as.character(format(lubridate::as_datetime(as.numeric(as.character(unix_medline))),'%Y-%m')))]
df[,diff_time := ifelse(date_accepted=='null'|date_received=='null','',(as.numeric(as.character(unix_accepted))-as.numeric(as.character(unix_received)))/3600/24)]
df[,diff_time_unix := ifelse(date_received=='null','',(as.numeric(as.character(unix))-as.numeric(as.character(unix_received)))/3600/24)]

df[,diff_time_medline := ifelse(date_received=='null','',(as.numeric(unix_medline)-as.numeric(as.character(unix_received)))/3600/24)]
df[,diff_time_published := ifelse(date_received=='null','',(as.numeric(unix)-as.numeric(as.character(unix_received)))/3600/24)]
#df[,diff_time_unix_published := ifelse(year_month=='null'|date_received=='null','',(as.numeric(as.POSIXct(ymd_hms(as.character(date))))-as.numeric(as.character(unix)))/3600/24)]
df[,granted := ifelse(grants=='null'|grants=='null',0,1)]
df[,year_month:= date_received]
# published before received or accepted
df[,is_pub_beforerec := ifelse((as.numeric(as.character(unix_accepted))<as.numeric(as.character(unix_received))),
                               1,
                               0)]



df_c = as_tibble(df) %>% separate_rows(country_list,sep= ';')
df_2019 = df[which(stringr::str_detect(df_c$date_received,'2019')),]
df_2020 = df[which(stringr::str_detect(df_c$date_received,'2020')),]

table_2019_all = table(df_2019$country_list)
table_2019_covid = table(df_2019$country_list[which(df_2019$is_coronavirus_lower==1)])
table_2020_all = table(df_2020$country_list)
table_2020_covid = table(df_2020$country_list[which(df_2020$is_coronavirus_lower==1)])

df_2019_c = data.frame(country = names(table_2019_all), value_all = as.numeric(table_2019_all))
df_2019_c_c = data.frame(country = names(table_2019_covid), value_covid = as.numeric(table_2019_covid))
df_2020_c = data.frame(country = names(table_2020_all), value_all = as.numeric(table_2020_all))
df_2020_c_c = data.frame(country = names(table_2020_covid), value_covid = as.numeric(table_2020_covid))

table_2019_f = left_join(df_2019_c,df_2019_c_c)
table_2020_f = left_join(df_2020_c,df_2020_c_c)

table_2019_f[which(is.na(table_2019_f[,3])),3] = 0
table_2020_f[which(is.na(table_2020_f[,3])),3] = 0

table_2019_f[,2] = table_2019_f[,3]/table_2019_f[,2] 
table_2020_f[,2] = table_2020_f[,3]/table_2020_f[,2] 

table_2019_f = table_2019_f[,1:2]
table_2020_f = table_2020_f[,1:2]
colnames(table_2019_f)[2] = 'share_2019'
colnames(table_2020_f)[2] = 'share_2020'
final_table = left_join(table_2020_f,table_2019_f)
# network

#dtm_j = as.matrix(as_tibble(df[date_received == '2020-02'&is_coronavirus_lower==0]) %>% 
#                  separate_rows(country_list,sep=';') %>% 
#                  group_by(pmid) %>% count(country_list)%>%tidytext::cast_dtm(pmid,country_list,n))
#dtm_j[dtm_j>1]=1
#dtm_j =  t(dtm_j) %*% dtm_j
#dtm_j_c =as.matrix(as_tibble(df[date_received == '2020-02'&is_coronavirus_lower==1]) %>% 
#                     separate_rows(country_list,sep=';') %>% 
#                     group_by(pmid) %>% count(country_list)%>%tidytext::cast_dtm(pmid,country_list,n))
#dtm_j_c[dtm_j_c>1]=1
#dtm_j_c =  t(dtm_j_c) %*% dtm_j_c
#
#aa = as_tibble(df[date_received == '2020-02'&is_coronavirus_lower==0]) %>% 
#  separate_rows(meshwords,sep='\n') %>% 
#  group_by(inter_collab,intra_collab) %>% count(meshwords) %>% arrange(desc(n))
#bb = as_tibble(df[date_received == '2020-02'&is_coronavirus_lower==1]) %>% 
#  separate_rows(meshwords,sep='\n') %>% 
#  group_by(inter_collab,intra_collab) %>% count(meshwords) %>% arrange(desc(n))
#
#df_t = as_tibble(df) %>%
#  separate_rows(country_list,sep=';') %>% filter(country_list == 'China') %>% 
#  group_by(date_received,country_list) %>% summarize(mean = sum(is.referenced.by.count)) 
#df_t_c = as_tibble(df[is_coronavirus_lower == 1,]) %>%
#  separate_rows(country_list,sep=';') %>% filter(country_list == 'China') %>% 
#  group_by(date_received,country_list) %>% summarize(mean = sum(is.referenced.by.count)) 
#


############################
########### TO JULY ########
############################
#df = df[date_received<"2020-07"]


full_sample = summary_stat(df,'year_month','_publication')
full_sample = summary_stat(df,'date_accepted','_accepted_date_and_medline')
full_sample = summary_stat(df,'date_received','_received_date')


issn_covlist = unique(df[is_coronavirus_lower==1&(stringr::str_detect(date_received,'2019')|stringr::str_detect(date_received,'2020')),ISSN])


# plot density team size

ts_2019_int = ggplot(df[stringr::str_detect(date_received,'2019')],
                 aes(x=log(team_size+1),color=as.factor(intra_collab))) +
                   geom_density(alpha = 0.2,size=0.9)  +
                   theme_bw() +
                   xlab("Team Size (log)") + ylab("Density") + labs(title = 'International collaboration (2019)',color = "Corona-virus \nRelated")+ theme(
                     legend.position = c(.95, .95),
                     legend.justification = c("right", "top"),
                     legend.box.just = "right",
                     legend.margin = margin(6, 6, 6, 6)
                   )

#### Plot density for laf between accetance and publishing
pub2019 = ggplot(as_tibble(df[is_pub_beforerec==0&stringr::str_detect(date_received,'2019'),]), aes(diff_time_published, color = as.factor(is_coronavirus_lower))) +
  geom_density(alpha = 0.2,size=0.9)  +
  scale_color_manual(values=c('#3a3a76','#b96800'))+ theme_bw()+
  xlab("Numer of days") + ylab("Density") + labs(title = 'Days between received  \nand publishing on pubmed (2019)',color = "Corona-virus \nRelated")+ theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )#+xlim(c(0,6))

pub2020 = ggplot(as_tibble(df[is_pub_beforerec==0&stringr::str_detect(date_received,'2020'),]), aes(diff_time_published, color = as.factor(is_coronavirus_lower))) +
  geom_density(alpha = 0.2,size=0.9)  +
  scale_color_manual(values=c('#3a3a76','#b96800'))+ theme_bw()+
  xlab("Numer of days") + ylab("Density") + labs(title = 'Days between received  \nand publishing on pubmed (2020)',color = "Corona-virus \nRelated")+ theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )#+xlim(c(0,6))


acc2019 = ggplot(as_tibble(df[is_pub_beforerec==0&stringr::str_detect(date_received,'2019'),]), aes(diff_time, color = as.factor(is_coronavirus_lower))) + 
  geom_density(alpha = 0.2,size=0.9) +
  scale_color_manual(values=c('#3a3a76','#b96800'))+ theme_bw() + labs(title = 'Days between received  \nand acceptance (2019)',color = "Corona-virus \nRelated")+
  xlab("Numer of days") + ylab("Density")+ theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )#+xlim(c(0,6))
acc2020 = ggplot(as_tibble(df[is_pub_beforerec==0&stringr::str_detect(date_received,'2020'),]), aes(diff_time, color = as.factor(is_coronavirus_lower))) + 
  geom_density(alpha = 0.2,size=0.9) +
  scale_color_manual(values=c('#3a3a76','#b96800'))+ theme_bw() + labs(title = 'Days between received  \nand acceptance (2020)',color = "Corona-virus \nRelated")+
  xlab("Numer of days") + ylab("Density")+ theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )#+xlim(c(0,6))

## test for diff between densities
par(mfrow=c(2,2))
library(cowplot)
puc_acc = plot_grid(pub2019,pub2020,acc2019,acc2020)
ggsave2('days_pub_acc.png',puc_acc,dpi= 300 ,width = 34, height = 17,units = 'cm')
library(stats)
ks_2019 = ks.test(df[is_pub_beforerec==0&is_coronavirus_lower==0&stringr::str_detect(date_received,'2019'),diff_time],
        df[is_pub_beforerec==0&is_coronavirus_lower==1&stringr::str_detect(date_received,'2019'),diff_time])
ks_2020 = ks.test(df[is_pub_beforerec==0&is_coronavirus_lower==0&stringr::str_detect(date_received,'2020'),diff_time],
        df[is_pub_beforerec==0&is_coronavirus_lower==1&stringr::str_detect(date_received,'2020'),diff_time])
library(coin)

#mt_2019 = median_test(diff_time ~ as.factor(is_coronavirus_lower),
#            data = df[is_pub_beforerec==0&stringr::str_detect(date_received,'2019'),],
#            distribution = approximate(B = 10000))
#mt_2020 = median_test(diff_time ~ as.factor(is_coronavirus_lower),
#            data = df[is_pub_beforerec==0&stringr::str_detect(date_received,'2020'),],
#            distribution = approximate(B = 10000))

txt = stringr::str_replace_all(paste(c('
��centering
��begin{tabular}{lcc}
��hline
year & 2019 & 2020 ����
Kolmogorov-Smirnov Test & ',ks_2019$p.value,' & ',ks_2020$p.value,'����
��hline
��end{tabular}'),collapse = ''),'��','\\\\')


write.table(txt,'ks_test.txt')
## to throwaway 
unix2019 = ggplot(as_tibble(df[is_pub_beforerec==0&stringr::str_detect(date_received,'2019'),]), aes(diff_time_unix, color = as.factor(is_coronavirus_lower))) + 
  geom_density(alpha = 0.2,size=0.9) +
  scale_color_manual(values=c('#3a3a76','#b96800'))+ theme_bw() + labs(title = 'Days between received and Pubmed Date (2019)',color = "Corona-virus \nRelated")+
  xlab("Numer of days") + ylab("Density")#+xlim(c(0,6))
unix2020 = ggplot(as_tibble(df[is_pub_beforerec==0&stringr::str_detect(date_received,'2020'),]), aes(diff_time_unix, color = as.factor(is_coronavirus_lower))) + 
  geom_density(alpha = 0.2,size=0.9) +
  scale_color_manual(values=c('#3a3a76','#b96800'))+ theme_bw() + labs(title = 'Days between received and Pubmed Date (2020)',color = "Corona-virus \nRelated")+
  xlab("Numer of days") + ylab("Density")#+xlim(c(0,6))
library(gridExtra)
grid.arrange(unix2019,unix2020,nrow = 2)
#ggplot(as_tibble(df), aes(, color = as.factor(is_coronavirus_lower))) + geom_density(alpha = 0.2)

### Teamsize
fig_n_c = make_box_plot(df,'team_size',F)
fig_c = make_box_plot(df,'team_size',T)
fig = subplot(fig_c,fig_n_c, nrows = 2,margin = 0.05)
Sys.setenv("plotly_username" = "PPeltouz")
Sys.setenv("plotly_api_key" = "3h5XlbSN5WdxTAiUDGbS")
plotly_IMAGE(fig, format = "png",
             out_file = "team_size_month_boxplot.png")
htmlwidgets::saveWidget(fig, "team_size_month_boxplot_date_received.html")

#make_share_graph <- function(var,nb=T,share_=T,title){
#  df_cov = df[is_coronavirus_lower==1,]
#  df_nn = df[is_coronavirus_lower==0,]
#  if(nb){
#    cont_table_nn = table(df_nn[eval(parse(text=paste0(var,'==1'))),date_received])[1:16]
#    cont_table_cov = table(df_cov[eval(parse(text=paste0(var,'==1'))),date_received])[1:16]
#  } else {
#    cont_table_nn = table(df_nn[eval(parse(text=paste0(var,"!='null'"))),date_received])[1:16]
#    cont_table_cov = table(df_cov[eval(parse(text=paste0(var,"!='null'"))),date_received])[1:16]
#  }
#  table_all_nn = table(df_nn[,date_received])[1:16]
#  table_all_cov = table(df_cov[,date_received])[1:16]
#  if(share_){
#    share_nn = cont_table_nn/table_all_nn[names(cont_table_nn)]*100
#    share_cov =cont_table_cov/table_all_cov[names(cont_table_cov)]*100
#  } else {
#    share_nn = cont_table_nn
#    share_cov = cont_table_cov
#  }
#  
#  share = as.data.frame(t(rbind(names(share_nn),as.numeric(share_nn))))
#  colnames(share) = c('Date','Share_nn')
#  share = left_join(share,data.frame(Date = names(share_cov),Share_cov = as.numeric(share_cov)))
#  share$Share_cov[which(is.na(share$Share_cov))] = 0
#  share$Date = as.character(share$Date)
#  share$Share_nn = as.numeric(as.character(share$Share_nn))
#  share$Share_cov = as.numeric(as.character(share$Share_cov))
#  ggplot(data = share,aes(x = Date,group=1))+
#    geom_line(aes(y=Share_nn),color = '#3a3a76', size = 0.9)+
#    geom_line(aes(y=Share_cov),color = '#b96800', size = 0.9)+ theme_bw() +
#    labs(title = title) + ylab("Share (%)")+ xlab("")+ scale_x_discrete(breaks=share$Date[c(1,4,8,12,16)])
#  
#}
#
#make_count_graph = function(var,title,lab){
#  month_nb_country = as_tibble(df) %>% separate_rows(var,sep = ';') %>% 
#    group_by(date_received,is_coronavirus_lower) %>%
#    summarize(nb_count_cov = length(unique(eval(parse(text=var))))) 
#  
#  month_nb_country_cov = month_nb_country %>% filter(is_coronavirus_lower==1) %>% select(-is_coronavirus_lower)
#  colnames(month_nb_country_cov)[2] = 'nb_count_cov'
#  month_nb_country_nn = month_nb_country %>% filter(is_coronavirus_lower==0) %>% select(-is_coronavirus_lower)
#  colnames(month_nb_country_nn)[2] = 'nb_count_nn'
#  month_nb_country = left_join(month_nb_country_nn[1:16,],month_nb_country_cov[1:16,])
#  ggplot(data = as.data.frame(month_nb_country),aes(x = date_received,group=1))+
#    geom_line(aes(y=nb_count_nn),color = '#3a3a76', size = 0.9)+
#    geom_line(aes(y=nb_count_cov),color = '#b96800', size = 0.9)+ theme_bw() +
#    labs(title = title) + ylab(lab) +xlab("")+ scale_x_discrete(breaks=month_nb_country$date_received[c(1,4,8,12,16,20)])
#}
#
#
#iter_graph = make_share_graph('inter_collab',share=T,title= 'Share of document with International Collab.')
#itra_graph = make_share_graph('intra_collab',share=T,title= 'Share of document with Intranational Collab.')
#solo_graph = make_share_graph('solo_aut',share=T,title= 'Share of document Solo Author')
#granted_graph =make_share_graph('granted',share=T,title= 'Share of document granted')
#country_graph = make_count_graph('country_list',title='Unique Number of countries involved',lab= "Number of Countries")
#cities_graph = make_count_graph('country_city_list',title='Unique Number of cities involved',lab= "Number of Cities")
#info = grid.arrange(iter_graph,itra_graph,
#             solo_graph,granted_graph,
#             country_graph,cities_graph,
#             nrow = 3)
#ggsave('graphs_desc.png',info,dpi=300,width = 27, height = 20.25,units = 'cm')
#
#plot_cat <- function(cat_name,i,all=F){
#  ## share of CS in health 
#  if(all == F){
#    cat_DL <- df[ which(df[,cat_name]>0),]
#    # total cat paper
#    tot_cat <- table(cat_DL$date_received)
#    cat_CS_DL <- df[ which(df[,cat_name]>0&df[,'privat']==1),]
#    
#  } else {
#    cat_DL <- df
#    # total cat paper
#    tot_cat <- table(cat_DL$date_received)
#    cat_CS_DL <- df[ which(df[,'is_coronavirus_lower']==1),]
#    
#  }
#  
#  total_cat_CS <- table(cat_CS_DL$date_received)
#  #tot_cat[years[-which(years %in% names(tot_cat))]] <- 0
#  #total_cat_CS[years[-which(years %in% names(total_cat_CS))]] <- 0
#  #tot_cat <- tot_cat[years]
#  #total_cat_CS <- total_cat_CS[years]
#  share_CS_in_cat <- total_cat_CS/tot_cat
#  
#  share = as.numeric(share_CS_in_cat)
#  share[is.na(share)] <- 0
#  ll.smooth = loess(share~as.numeric(years), span=0.75)
#  ll.pred = predict(ll.smooth, se = TRUE)
#  ll.df = data.frame(x=ll.smooth$x, fit=ll.pred$fit,
#                     lb = ll.pred$fit - (1.96 * ll.pred$se),
#                     ub = ll.pred$fit + (1.96 * ll.pred$se))
#  ll.df = ll.df[order(ll.df$as.numeric.years.),]
#  
#  p.llci = plot_ly()
#  p.llci = add_trace(p.llci,x=as.numeric(years), y=share, type="scatter", mode = "markers", color= '#e3b577', name="Data")
#  p.llci = add_lines(p.llci, x=as.numeric(years), y=ll.pred$fit, name="Mean", line=list(color='rgba(58,58,118,1)', width=2))
#  p.llci = add_ribbons(p.llci, x=ll.df$as.numeric.years, ymin=ll.df$lb, ymax=ll.df$ub, name="95% CI",
#                       line=list(opacity=0.4, width=0, color='rgba(58,58,118,0.8)'),
#                       fillcolor = 'rgba(153,178,255,0.5)' )
#  p.llci = layout(p.llci,  xaxis = list(title = names[i]),showlegend = FALSE)
#  p.llci}
#
#names <- c("Panel A : Technology","Panel B : Physical Sciences","Panel C : Life Sciences Biomedicine","Panel D :Health Sciences","Panel E : Social Sciences","Panel F : Arts Humanities","Panel G : All Documents")
#subplot(list(plot_cat(cat_name = 'Technology',1,all=T),

#write.csv(df[pmid %in% as.matrix(pmid_list[,2]),pmid],'pmid_list.csv')

