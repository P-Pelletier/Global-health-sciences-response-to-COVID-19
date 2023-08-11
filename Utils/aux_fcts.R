#***************************************
# auxiliary functions for Covid paper
# MM, 01.12.2020
#***************************************

# load packages
library(countrycode)
library(readxl)
library(magrittr)

####### read in data fcts #######

read_pubmed <- function(inputPath, verbose=TRUE){
  # INPUT: char inputPath
  # Reads in node_list.csv and node_pub_activity.csv (from fixed paths)
  # Returns country-month panel as a nested tibble: country characteristics, nested alters characteristics
  
  # read data
  edgelistFile <- paste0(inputPath,"edge_list.csv")
  edgelist <- read.csv(file=edgelistFile,header=TRUE,sep=",",stringsAsFactors = FALSE) %>%
    tibble()
  
  nodeActivityFile <- paste0(inputPath,"country_pub_info.csv")
  nodeActivity <- read.csv(file=nodeActivityFile,header=TRUE,sep=",",stringsAsFactors = FALSE) %>%
    tibble()
  
  # edge - "month"         "source"        "target"        "weight"        "weight_corona"
  # node - "month"            "solePubs"         "collabPubs"       "solePubsCorona"   "collabPubsCorona" "country"
  
  nodeActivity %<>% 
    mutate(dependance_international=(I_funding_solo+I_funding_collab),
           dependance_international_agency=(IAG_funding_solo+IAG_funding_collab),
           dependance_international_and_agency=(I_IAG_funding_solo+I_IAG_funding_collab),
           dependance_international_corona=(I_funding_soloCorona+I_funding_collabCorona),
           dependance_international_agency_corona=(IAG_funding_soloCorona+IAG_funding_collabCorona),
           dependance_international_and_agency_corona=(I_IAG_funding_soloCorona+I_IAG_funding_collabCorona))
  
  # create edgelist
  el <- edgelist %>% 
    mutate(countryCode=countrycode(source, origin ='country.name', destination ='iso3c'),
           targetCountryCode=countrycode(target, origin ='country.name', destination ='iso3c')) %>%
    rename(weight_noncorona=weight) %>%
    select(countryCode, month, targetCountryCode, target, weight_noncorona, weight_corona,dep_i,dep_j,paper_funded) %>%
    arrange(countryCode, month, targetCountryCode)
  
  # combine panel
  panel <- nodeActivity %>% 
    mutate(countryCode = countrycode(country, origin ='country.name', destination ='iso3c'),
           region = countrycode(country, origin='country.name', destination='continent')) %>%
    rename(solePubsNonCorona=solePubs, collabPubsNonCorona=collabPubs) %>%
    relocate(countryCode, month, country, region, solePubsNonCorona, collabPubsNonCorona, solePubsCorona, collabPubsCorona) %>%
    arrange(countryCode, month) %>%
    left_join(el,by=c("countryCode","month")) 
  
  panel %<>% 
    nest_by(countryCode, month, country, region, solePubsNonCorona, collabPubsNonCorona, 
            solePubsCorona, collabPubsCorona,
            dependance_international,
            dependance_international_agency,
            dependance_international_and_agency,
            dependance_international_corona,
            dependance_international_agency_corona,
            dependance_international_and_agency_corona,
            N_paper_with_grant,
            N_paper_with_grantCorona,
            .key="alters")

  if(verbose){
    cat("In read_pubmed()\n")
    cat("Countries : ", length(unique(panel$countryCode)),"\n")
    cat("Sole pubs non corona : ", sum(panel$solePubsNonCorona),"\t")
    cat("Collab pubs non corona : ", sum(panel$collabPubsNonCorona),"\n")
    cat("Sole pubs corona : ", sum(panel$solePubsCorona),"\t")
    cat("Collab pubs corona : ", sum(panel$collabPubsCorona),"\n")
  }
  
  return(panel)
}

read_owid_covid <- function(inputPath, verbose=TRUE){
  
  file <- paste(inputPath,"owid-covid-data.csv",sep="")
  owid <- read.csv(file=file, stringsAsFactors = FALSE) %>%
    tibble()
  
  owid <- owid %>%
    select(iso_code,date,new_cases,new_deaths,new_cases_per_million,new_deaths_per_million) %>%
    mutate(month=as.integer(gsub("-","",substr(date,1,7)))) %>%
    group_by(iso_code,month) %>%
    summarise(new_cases=sum(new_cases,na.rm=TRUE),
              new_deaths=sum(new_deaths,na.rm=TRUE),
              new_cases_per_million=sum(new_cases_per_million,na.rm=TRUE),
              new_deaths_per_million=sum(new_deaths_per_million,na.rm=TRUE),.groups="drop") %>%
    rename(countryCode=iso_code)
  
  
  if(verbose){
    cat("In read_owid_covid() \n")
    cat("Countries : ",length(unique(owid$countryCode)),"\n")
  }
  
  return(owid)

}

read_oxford_policytracker <- function(inputPath, verbose=TRUE){
  
  file <- paste(inputPath,"OxCGRT_latest.csv",sep="")
  ocgrt <- read.csv(file=file, stringsAsFactors = FALSE) %>%
    tibble()
  
  
  # C7_Restrictions on internal movement
  # 0 - no measures
  # 1 - recommend not to travel between regions/cities
  # 2 - internal movement restrictions in place
  # Blank - no data
  # C7_Flag
  # 0 - targeted
  # 1- general
  # Blank - no data
  # C8_International travel controls
  # 0 - no restrictions
  # 1 - screening arrivals
  # 2 - quarantine arrivals from some or all regions
  # 3 - ban arrivals from some regions
  # 4 - ban on all regions or total border closure
  # Blank - no data
  
  ocgrt <- ocgrt %>% 
    select(CountryCode, Date, C7M_Restrictions.on.internal.movement, C7M_Flag, C8EV_International.travel.controls) %>%
    mutate(month=as.integer(substr(Date,1,6)),
           lockdown=ifelse(C7M_Restrictions.on.internal.movement==2 & !is.na(C7M_Restrictions.on.internal.movement),1,0),
           border_closure=ifelse(C8EV_International.travel.controls==4 & !is.na(C8EV_International.travel.controls),1,0)) %>%
    group_by(CountryCode, month) %>%
    summarize(lockdown_days=sum(lockdown, na.rm=TRUE),
              border_closure_days=sum(border_closure, na.rm=TRUE), .groups="drop") %>%
    ungroup() %>% 
    arrange(CountryCode, month) %>%
    rename(countryCode=CountryCode)
  
  if(verbose){
    cat("In read_oxford_policytracker() \n")
    cat("Countries : ", length(unique(ocgrt$countryCode)), "\n")
  }
  
  return(ocgrt)
}

read_pwt <- function(inputPath, verbose=TRUE){
  # rgdpe (Expenditure-side real GDP at chained PPPs (in mil. 2017US$)), 
  #   population (Population (in millions)) -> gdp per capita in chained PPP 2017US$
  
  file <- paste(inputPath,"pwt100.xlsx",sep="")
  pwt <- read_excel(file,sheet = "Data") %>%
    filter(year==2019) %>%
    select(countrycode,rgdpe,pop) %>%
    mutate(gdp_pc=rgdpe / pop) %>%
    rename(countryCode=countrycode)
  
  return(pwt)
}

read_hdi <- function(inputPath, verbose=TRUE){
  
  file <- paste(inputPath,"Human Development Index (HDI).csv",sep="")
  hdi <- read.csv(file = file, stringsAsFactors = FALSE, skip = 5) %>%
    mutate(countryCode = countrycode(Country,origin="country.name",destination="iso3c"),
           X2019=as.numeric(X2019)) %>%
    rename(hdi=X2019) %>%
    select(countryCode, hdi) %>%
    filter(!is.na(countryCode))
  
  return(hdi)
}

read_geolocation <- function(inputPath, verbose=TRUE){
  
  ##### geo location
  file <- paste(inputPath,"geoCountry.csv",sep="")
  geo <- read.csv(file = file, sep=",", stringsAsFactors=FALSE, na.strings ="") %>%
    mutate(countryCode=countrycode(country,origin="iso2c",destination = "iso3c")) %>%
    filter(!is.na(countryCode)) %>%
    select(countryCode, longitude, latitude)
  
  return(geo)
}

get_country_panel <- function(inputPath, verbose=TRUE){

  pubs <- read_pubmed(inputPath)
  owid <- read_owid_covid(inputPath)
  ocgrt <- read_oxford_policytracker(inputPath)
  pwt <- read_pwt(inputPath)
  hdi <- read_hdi(inputPath)
  geo <- read_geolocation(inputPath)
  
  panel <- pubs %>%
    left_join(owid %>% 
                mutate(owid_obs=1),
              by=c("countryCode","month")) %>%
    mutate_at(.vars=names(owid)[!names(owid) %in% names(pubs)],
              .funs= ~ ifelse(is.na(.x),0,.x)) %>%
    left_join(ocgrt %>% 
                mutate(ocgrt_obs=1),
              by=c("countryCode","month")) %>%
    mutate_at(.vars=names(ocgrt)[!names(ocgrt) %in% names(pubs)],
              .funs= ~ ifelse(is.na(.x),0,.x)) %>%
    left_join(pwt %>% 
                mutate(pwt_obs=1),
              by=c("countryCode")) %>%
    left_join(hdi %>% 
                mutate(hdi_obs=1),
              by=c("countryCode")) %>%
    left_join(geo %>% 
                mutate(geo_obs=1),
              by=c("countryCode")) %>%
    mutate(owid_obs=ifelse(is.na(owid_obs),0,owid_obs),
           ocgrt_obs=ifelse(is.na(ocgrt_obs),0,ocgrt_obs),
           pwt_obs=ifelse(is.na(pwt_obs),0,pwt_obs),
           hdi_obs=ifelse(is.na(hdi_obs),0,hdi_obs),
           geo_obs=ifelse(is.na(geo_obs),0,geo_obs)) 

  # check which countries are not in the different datasets
  countryMissings <- panel %>% 
    group_by(countryCode) %>%
    summarize(owid_obs=max(owid_obs),
              ocgrt_obs=max(ocgrt_obs),
              pwt_obs=max(pwt_obs),
              hdi_obs=max(hdi_obs),
              geo_obs=max(geo_obs)) %>%
    mutate(missing_datasets = 5 - rowSums(across(owid_obs:geo_obs)))
  
  panel <- panel %>%
    filter(countryCode %in%
             (countryMissings %>% filter(missing_datasets==0) %>% pull(countryCode)))

  if(verbose){
    cat("Missing datasets on country level:\n")    
    print(table(countryMissings$missing_datasets))
    cat("Remaining countries with complete data:",length(unique(panel$countryCode)),"\n")
  }
  
  # add some variables
  panel <- panel %>%
    group_by(countryCode) %>%
    mutate(lockdown_days_past12m = lag(zoo::rollsumr(lockdown_days, 12, fill=0, align="right"),default=0),
           border_closure_days_past12m = lag(zoo::rollsumr(border_closure_days, 12, fill=0, align="right"),default=0),
           border_cum = lag(cumsum(border_closure_days)),
           lockdown_cum = lag(cumsum(lockdown_days)),
           cum_cases = cumsum(new_cases),
           cum_deaths = lag(cumsum(new_deaths)),
           cum_cases_per_million = cumsum(new_cases_per_million),
           cum_deaths_per_million = cumsum(new_deaths_per_million),
           coronaPubs = collabPubsCorona + solePubsCorona,
           nonCoronaPubs = collabPubsNonCorona + solePubsNonCorona,
           t = 1:n())

  return(panel)
}



###### convenience fcts ########

descr_stats <- function(data,vars){
  # tibble data
  # char vector vars

  # univariates
  unistats <- data %>%
    ungroup() %>%
    summarise(across(.cols=all_of(vars),
                     .fns=list(min = ~ min(.x,na.rm=TRUE),
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
    select(var, mean, sd, min, max, obs, NAs) %>%
    as.data.frame()
  
  # some re-ordering
  rownamesUnistats <- unistats[,"var"]
  unistats <- as.matrix(unistats[,-1])
  rownames(unistats) <- rownamesUnistats
  unistats <- unistats[vars,]
  
  # correlations
  cors <- data %>% ungroup() %>% select(all_of(vars)) %>%
    as.matrix() %>% rcorr()
  
  # glue for descriptive table
  corsr <- round(cors$r,digits=2)
  corsr[upper.tri(cors$r,diag=TRUE)] <- ""
  descr <- cbind(round(unistats[rownames(cors$r),c("mean","sd")],digits=2)
                 ,corsr)
  
  return(descriptives=list(univariateStats=unistats, correlations=cors,
                           descr_table=descr))
}




##########old stuff ########

read_data_depricated <- function(inputPath,verbose=TRUE){
  # INPUT: char inputPath
  # Reads in node_list.csv and node_pub_activity.csv (from fixed paths)
  # Returns a list with data.frames: i) nodelist, ii) edgelist, iii) nodeActivity
  
  # read in
  edgelistFile <- paste0(inputPath,"edge_list.csv")
  edgelist <- read.csv(file=edgelistFile,header=TRUE,sep=",",stringsAsFactors = FALSE)
  
  nodeActivityFile <- paste0(inputPath,"country_pub_info.csv")
  nodeActivity <- read.csv(file=nodeActivityFile,header=TRUE,sep=",",stringsAsFactors = FALSE)
  
  # establish common time span
  months <- sort(unique(c(unique(edgelist$month), unique(nodeActivity$month))))
  months <- months[months %in% edgelist$month & months %in% nodeActivity$month]
  
  edgelist <- edgelist[edgelist$month %in% months,]
  nodeActivity <- nodeActivity[nodeActivity$month %in% months,]
  
  # in the nodelist there are all countries with activity
  countries <- sort(nodeActivity[!duplicated(nodeActivity$country),c("country")])
  rmCountries <- c("country","Eswatini","Kosovo","Micronesia")
  countries <- countries[!countries %in% rmCountries]
  
  # enforce same countries in edgelist and nodeActivity
  idx <- edgelist$source %in% countries & edgelist$target %in% countries
  edgelist <- edgelist[idx,]
  idx <- nodeActivity$country %in% countries  
  nodeActivity <- nodeActivity[idx,]  
  
  # create nodelist - introduce nodeIds
  nodelist <- data.frame(nodeId=1:length(countries),
                         country=countries,
                         countryCode = countrycode(countries, origin ='country.name', destination ='iso3c'),
                         region = countrycode(countries, origin='country.name', destination='continent'),
                         stringsAsFactors = FALSE)
  
  # add nodeIds to edgelist
  edgelist <- merge(edgelist, nodelist[,c("country","nodeId")], by.x=c("source"), by.y="country", all.x=TRUE)
  names(edgelist)[names(edgelist)=="nodeId"] <- "sourceId" 
  edgelist <- merge(edgelist, nodelist[,c("country","nodeId")], by.x=c("target"), by.y="country", all.x=TRUE)
  names(edgelist)[names(edgelist)=="nodeId"] <- "targetId" 
  
  # add nodeIds and basic country info to nodeActivity
  nodeActivity <- merge(nodeActivity, nodelist, by="country")
  
  # remove duplicated edges and loops
  edgelist <- edgelist[edgelist$sourceId < edgelist$targetId, ]
  
  return(list(nodelist=nodelist,edgelist=edgelist,nodeActivity=nodeActivity))
}



get_country_panel_depricated <- function(inputPath, last_month=202012){
  
  ####### prepare dataset(s) ####
  ####### papers
  sample <- read_data(inputPath=inputPath)
  nodeActivity <- sample$nodeActivity
  nodeActivity <- nodeActivity[order(nodeActivity$countryCode, nodeActivity$month),]
  nodeActivity$coronaPubs <- nodeActivity$collabPubsCorona + nodeActivity$solePubsCorona
  nodeActivity$nonCoronaPubs <- nodeActivity$collabPubs + nodeActivity$solePubs
  
  # pre-pandemic publications
  idx <- nodeActivity$month < 202001
  preCovid <- aggregate(nodeActivity[idx,c("coronaPubs", "nonCoronaPubs")],
                        by=list(countryCode=nodeActivity$countryCode[idx]),
                        function(x) sum(x))
  names(preCovid) <- c("countryCode","coronaPubsPreCovid","nonCoronaPubsPreCovid")
  
  # pandemic papers
  nodeActivity2020 <- nodeActivity[nodeActivity$month >= 202001 & nodeActivity$month <= last_month,
                                   c("countryCode","month","country","region","coronaPubs","nonCoronaPubs")]
  
  # merge preCovid and Covid19 papers
  papers <- merge(nodeActivity2020,preCovid,by="countryCode",all.x=TRUE)
  papers <- papers[order(papers$countryCode,papers$month),]
  
  # prepare for merging outside of papers data
  names(papers)[1] <- "iso_code"
  
  ###### covid cases
  file <- paste(inputPath,"coronavirus-data-explorer.csv",sep="")
  cases <- read.csv(file=file, stringsAsFactors = FALSE)
  
  # no NAs check
  # all(!is.na(cases$iso_code) & !cases$iso_code=="" & !cases$date=="" & !is.na(cases$date))
  
  # reduce to monthly observations
  cases <- cases[order(cases$iso_code,cases$date,decreasing=TRUE),]
  cases$month <- as.integer(gsub("-","",substr(cases$date,1,7)))
  cases <- cases[!duplicated(cases[,c("iso_code","month")]),]
  cases <- cases[order(cases$iso_code,cases$month,decreasing=FALSE),]
  
  # keep essential information
  vars <- c("iso_code","month","total_cases","total_deaths","total_cases_per_million","total_deaths_per_million")
  cases <- cases[,vars]
  
  # decide on what's the meaning of a missing value
  # - some smaller countries seem to have been added later, remove all that show up after 10/2020 
  idx <- cases$month < 202010
  cases <- cases[cases$iso_code %in% cases$iso_code[idx],]
  
  # 195 countries remain - NAs and missing seem to imply zero cases here. 
  # create complete structure - and fill with zeros
  tmp <- expand.grid(iso_code=unique(cases$iso_code),month=unique(cases$month))
  cases <- merge(tmp,cases,by=c("iso_code","month"),all.x=TRUE)
  cases[is.na(cases)] <- 0
  
  cases <- cases[order(cases$iso_code,cases$month),]
  
  ##### covid travel restrictions
  file <- paste(inputPath,"international-travel-covid.csv",sep="")
  travels <- read.csv(file=file, stringsAsFactors = FALSE)
  
  # change Date
  travels$Date <- as.Date(travels$Day)
  travels <- travels[order(travels$Code, travels$Date),]
  
  # cumulate measures by country -> number of days with (at least) quarantine, ban, closure
  travels$Quarantine <- ifelse(travels$international_travel_controls >= 2,1,0)
  travels$Ban <- ifelse(travels$international_travel_controls >= 3,1,0)
  travels$Closure <- ifelse(travels$international_travel_controls >= 4,1,0)
  
  for(code in unique(travels$Code)){
    idx <- travels$Code==code
    travels$Quarantine[idx] <- cumsum(travels$Quarantine[idx])
    travels$Ban[idx] <- cumsum(travels$Ban[idx])
    travels$Closure[idx] <- cumsum(travels$Closure[idx])
  }
  
  # reduce to monthly observations
  travels <- travels[order(travels$Code, travels$Date,decreasing=TRUE),]
  travels$month <- as.integer(gsub("-","",substr(travels$Day,1,7)))
  travels <- travels[!duplicated(travels[,c("Code","month")]),]
  
  # order
  travels <- travels[order(travels$Code, travels$month),c("Code","month","Quarantine","Ban","Closure")]
  
  # June often not available, Liechtenstein (LIE) has some missing information, drop it.
  idx <- travels$month < 202105 & travels$Code != "LIE"
  #table(travels$month)
  #table(travels$Code[idx])
  # rest is there and looks good. 
  
  # restrict dataset
  travels <- travels[idx,]
  
  ##### covid stay-at-home policy
  file <- paste(inputPath,"stay-at-home-covid.csv",sep="")
  stayhome <- read.csv(file = file, stringsAsFactors = FALSE)
  
  # change Date
  stayhome$Date <- as.Date(stayhome$Day)
  stayhome <- stayhome[order(stayhome$Code, stayhome$Date),]
  
  # accumulate measures by country -> number of days with (at least) recommend, require, lock homes
  stayhome$Recommend_home <- ifelse(stayhome$stay_home_requirements >= 1,1,0)
  stayhome$Require_home <- ifelse(stayhome$stay_home_requirements >= 2,1,0)
  stayhome$Lock_home <- ifelse(stayhome$stay_home_requirements >= 3,1,0)
  
  for(code in unique(stayhome$Code)){
    idx <- stayhome$Code==code
    stayhome$Recommend_home[idx] <- cumsum(stayhome$Recommend_home[idx])
    stayhome$Require_home[idx] <- cumsum(stayhome$Require_home[idx])
    stayhome$Lock_home[idx] <- cumsum(stayhome$Lock_home[idx])
  }
  
  
  # reduce to monthly observations
  stayhome <- stayhome[order(stayhome$Code, stayhome$Date,decreasing=TRUE),]
  stayhome$month <- as.integer(gsub("-","",substr(stayhome$Day,1,7)))
  stayhome <- stayhome[!duplicated(stayhome[,c("Code","month")]),]
  
  # order
  stayhome <- stayhome[order(stayhome$Code, stayhome$month),c("Code","month","Recommend_home","Require_home","Lock_home")]
  
  # see above
  idx <- stayhome$month < 202105 & stayhome$Code != "LIE"
  #table(stayhome$month[idx])
  #table(stayhome$Code[idx])
  
  # restrict dataset
  stayhome <- stayhome[idx,]
  
  
  ##### economic wealth in 2019, rgdpe (Expenditure-side real GDP at chained PPPs (in mil. 2017US$)), population (Population (in millions)) -> gdp per capita
  file <- paste(inputPath,"pwt100.xlsx",sep="")
  pwt <- read_excel(file,sheet = "Data")
  
  pwt <- as.data.frame(pwt)
  pwt <- pwt[pwt$year==2019,c("countrycode","rgdpe","pop")]
  pwt$gdp_pc <- pwt$rgdpe / pwt$pop
  
  
  #### hdi in 2019
  file <- paste(inputPath,"Human Development Index (HDI).csv",sep="")
  hdi <- read.csv(file = file, stringsAsFactors = FALSE, skip = 5)
  
  # select info
  hdi <- hdi[,c("Country","X2019")]
  
  # introduce iso_code
  hdi$iso_code <- countrycode(hdi$Country,origin="country.name",destination="iso3c")
  hdi <- hdi[!is.na(hdi$iso_code),]
  hdi <- hdi[,c("iso_code","X2019")]
  names(hdi) <- c("iso_code","hdi")
  
  # change to numeric indic.
  hdi$hdi <- as.numeric(hdi$hdi)
  
  ##### geo location
  file <- paste(inputPath,"geoCountry.csv",sep="")
  geo <- read.csv(file = file, sep=",", stringsAsFactors=FALSE, na.strings ="")
  geo$iso_code <- countrycode(geo$country,origin="iso2c",destination = "iso3c")

  ####### create sample - combine datasets #####
  
  # papers are our main dataset
  sample <- papers
  cases$case_obs <- 1
  sample <- merge(sample,cases,by.x=c("iso_code","month"), by.y=c("iso_code","month"), all.x=TRUE)
  sample$case_obs[is.na(sample$case_obs)] <- 0
  travels$travel_obs <- 1
  sample <- merge(sample,travels,by.x=c("iso_code","month"), by.y=c("Code","month"), all.x=TRUE)
  sample$travel_obs[is.na(sample$travel_obs)] <- 0
  stayhome$stayhome_obs <- 1
  sample <- merge(sample,stayhome,by.x=c("iso_code","month"), by.y=c("Code","month"), all.x=TRUE)
  sample$stayhome_obs[is.na(sample$stayhome_obs)] <- 0
  pwt$pwt_obs <- 1
  sample <- merge(sample,pwt,by.x=c("iso_code"), by.y=c("countrycode"), all.x=TRUE)
  sample$pwt_obs[is.na(sample$pwt_obs)] <- 0
  hdi$hdi_obs <- 1
  sample <- merge(sample,hdi,by.x=c("iso_code"), by.y=c("iso_code"), all.x=TRUE)
  sample$hdi_obs[is.na(sample$hdi_obs)] <- 0
  sample <- merge(sample,geo[,c("iso_code","latitude","longitude")],by="iso_code",all.x=TRUE)
  sample$geo_obs <- ifelse(sample$iso_code %in% geo$iso_code, 1, 0)
  
  sample[sample$geo_obs==0,]
  idx <- rowSums(sample[,c("case_obs","travel_obs","stayhome_obs","pwt_obs","hdi_obs")])==5 & sample$geo_obs==0
  sample[idx,]
  
  sample <- sample[order(sample$iso_code, sample$month), ]
  
  # check completeness of dataset
  sample$all_obs <- ifelse(rowSums(sample[,c("case_obs","travel_obs","stayhome_obs","pwt_obs","hdi_obs","geo_obs")])==6,1,0)
  cat("Any NAs where obs complete?",any(is.na(sample[sample$all_obs==1,])),"\n")
  obs_country <- aggregate(sample$all_obs, by=list(iso_code=sample$iso_code), function(x) sum(x))
  names(obs_country)[2] <- "complete_obs"
  cat("complete countries: ", sum(obs_country$complete_obs==max(obs_country$complete_obs)),"\t")
  cat("complete country-months:", sum(sample$all_obs),"\t")
  
  # complete against non-complete countries
  cat("missing countries are small, below 5 Mio. inhabitants, and Taiwan because of missing HDI data.\n")
  cat("summary of population over countries with NAs: \n",summary(sample$pop[sample$all_obs==0 & !sample$iso_code=="TWN"]),"\n\n")
  
  # restrict to countries with complete information
  sample <- sample[sample$all_obs==1,]
  # order 
  sample <- sample[order(sample$iso_code,sample$month,decreasing=FALSE),]
  # return
  return(sample)
}


get_country_panel_Archived <- function(inputPath){
  
  ####### papers ######
  sample <- read_data(inputPath=inputPath)
  nodeActivity <- sample$nodeActivity
  nodeActivity <- nodeActivity[order(nodeActivity$countryCode, nodeActivity$month),]
  
  nodeActivity$coronaPubs <- nodeActivity$collabPubsCorona + nodeActivity$solePubsCorona
  nodeActivity$nonCoronaPubs <- nodeActivity$collabPubs + nodeActivity$solePubs
  
  # pre-Covid19 publications
  idx <- nodeActivity$month < 202001
  preCovid <- aggregate(nodeActivity[idx,c("coronaPubs", "nonCoronaPubs")],
                        by=list(countryCode=nodeActivity$countryCode[idx]),
                        function(x) sum(x))
  names(preCovid) <- c("countryCode","coronaPubsPreCovid","nonCoronaPubsPreCovid")
  
  # Covid19 papers
  nodeActivity2020 <- nodeActivity[nodeActivity$month >= 202001 & nodeActivity$month <= 202012,
                                   c("countryCode","month","country","region","coronaPubs","nonCoronaPubs")]
  
  # merge preCovid and Covid19 papers
  papers <- merge(nodeActivity2020,preCovid,by="countryCode",all.x=TRUE)
  papers <- papers[order(papers$countryCode,papers$month),]
  
  # prepare for merging outside of papers data
  names(papers)[1] <- "iso_code"
  
  ###### covid cases ######
  owid <- read.csv("~/Documents/Research/TechnologicalChange/CoronaSci/RProgs/Resources/countryPubmed/owid-covid-data.csv",stringsAsFactors = FALSE)
  
  # remove NAs
  idx <- !is.na(owid$iso_code) & !owid$iso_code=="" & !owid$date=="" & !is.na(owid$date)
  owid <- owid[idx,]
  
  # reduce to monthly observations
  owid <- owid[order(owid$iso_code,owid$date,decreasing=TRUE),]
  owid$month <- as.integer(gsub("-","",substr(owid$date,1,7)))
  owid <- owid[!duplicated(owid[,c("iso_code","month")]),]
  owid <- owid[order(owid$iso_code,owid$month,decreasing=FALSE),]
  
  # keep essential information
  vars <- c("iso_code","month","location","continent","total_cases","total_deaths","total_cases_per_million","total_deaths_per_million")
  owid <- owid[,vars]
  
  # merge papers and owid data -> sample
  countriesPapers <- unique(papers$iso_code)
  countriesOwid <- unique(owid$iso_code)
  
  missing <- countriesPapers[!countriesPapers %in% countriesOwid]
  missing <- unique(papers[papers$countryCode %in% missing,c("iso_code","country")])
  
  missed <- countriesOwid[!countriesOwid %in% countriesPapers]
  missed <- unique(owid[owid$iso_code %in% missed,c("iso_code","location")])
  # comparing missing and missed -> non overlapping sets
  # 14 countries in paper dataset are not in owid - will be dropped from the analysis
  
  # merge into sample
  # keep essential information
  owid_vars <- c("iso_code","month","total_cases","total_deaths","total_cases_per_million","total_deaths_per_million")
  sample <- merge(papers,owid[,owid_vars],by=c("iso_code","month"),all.x=TRUE)
  sample$owidObs <- ifelse(sample$iso_code %in% countriesOwid,1,0)
  sample <- sample[order(sample$iso_code,sample$month),]
  
  ###### gdp data ######
  gdp <- read.csv("~/Documents/Research/TechnologicalChange/CoronaSci/RProgs/Resources/countryPubmed/UNdata_Export_20210310_121906142.txt", 
                  sep=";", stringsAsFactors=FALSE)
  # introduce iso_code
  gdp$iso_code <- countrycode(gdp$Country.or.Area.Code,origin="un",destination="iso3c")
  gdp <- gdp[!is.na(gdp$iso_code),]
  
  # select info
  gdp <- gdp[,c("iso_code","Value")]
  names(gdp)[2] <- c("gdp_per_capita_2019") 
  
  # merge to sample
  sample <- merge(sample,gdp,by=c("iso_code"),all.x=TRUE)
  sample$gdpObs <- ifelse(sample$iso_code %in% gdp$iso_code,1,0)
  sample <- sample[order(sample$iso_code,sample$month),]
  
  ###### human development index data #######
  hdi <- read.csv("~/Documents/Research/TechnologicalChange/CoronaSci/RProgs/Resources/countryPubmed/HDI_MM.csv", 
                  stringsAsFactors=FALSE)
  
  # select info
  hdi <- hdi[,c("Country","X2019")]
  
  # introduce iso_code
  hdi$iso_code <- countrycode(hdi$Country,origin="country.name",destination="iso3c")
  hdi <- hdi[!is.na(hdi$iso_code),]
  hdi <- hdi[,c("iso_code","X2019")]
  names(hdi) <- c("iso_code","hdi_2019")
  
  # change to numeric indic.
  hdi$hdi_2019 <- as.numeric(hdi$hdi_2019)
  
  # merge to sample
  sample <- merge(sample,hdi,by=c("iso_code"),all.x=TRUE)
  sample$hdiObs <- ifelse(sample$iso_code %in% hdi$iso_code,1,0)
  sample <- sample[order(sample$iso_code,sample$month),]
  
  ##### geo location #####
  geo <- read.csv("~/Documents/Research/TechnologicalChange/CoronaSci/RProgs/Resources/countryPubmed/geoCountry.csv", 
                  sep=",", stringsAsFactors=FALSE)
  geo$iso_code <- countrycode(geo$country,origin="iso2c",destination = "iso3c")
  
  sample <- merge(sample,geo[,c("iso_code","latitude","longitude")],by="iso_code",all.x=TRUE)
  sample$geoObs <- ifelse(sample$iso_code %in% geo$iso_code, 1, 0)
  
  
  ###### handle missing observations ###########
  cat("Missing observations - country level \n")
  country_sample <- sample[!duplicated(sample$iso_code),]
  country_sample$data_sources <- rowSums(country_sample[,c("owidObs","gdpObs","hdiObs","geoObs")])
  print(table(country_sample$data_sources))
  cat("Countries dropped due to missing information : ", sum(country_sample$data_sources<4) ,"\n")
  cat("Countries kept due to complete information : ", sum(country_sample$data_sources==4) ,"\n")
  cat("Countries dropped:\t")  
  cat(country_sample[country_sample$data_sources<4,"iso_code"],"\n")
  
  # remove countries with missing data source
  sample <- sample[rowSums(sample[,c("owidObs","gdpObs","hdiObs","geoObs")])==4,]
  
  ####### 0 where missing for rest of sample NAs in sample
  # replace NA cases by zero - mostly early 2020
  vars <- c("total_cases","total_deaths", "total_cases_per_million", "total_deaths_per_million")
  for(var in vars){
    sample[is.na(sample[,var]),var] <- 0
  }
  
  # return
  sample <- sample[order(sample$iso_code,sample$month),]
  
  return(sample)
}


# simple univariate stats
get_univariate_stats <- function(sample,vars){
  sum_stats <- matrix(NA,nrow=length(vars),ncol=4)
  rownames(sum_stats) <- vars
  colnames(sum_stats) <- c("mean","sd","min","max")
  for(var in vars){
    x <- sample[,var]
    sum_stats[var,] <- c(mean(x),sd(x),min(x),max(x))
  }
  return(sum_stats)  
}

###### for link formation regression ###########
replace_nodeId <- function(nodelist,netw_sample){
  
  # replace nodeId - slightly complicated
  nl <- netw_sample$nodelist
  nl_new <- nodelist[,c("nodeId","iso_code")]
  names(nl_new) <- c("new_nodeId","countryCode")
  nl <- merge(nl,nl_new,by.x="countryCode")
  
  # replace nodeId in el
  conc <- nl[,c("nodeId","new_nodeId")]
  names(conc) <- c("nodeId","new_nodeId")
  
  # finish off nl
  nl$nodeId <- nl$new_nodeId
  nl <- nl[,1:4]
  
  # use conc to exchange node ids in el
  el <- netw_sample$edgelist
  names(conc) <- c("sourceId","new_sourceId")
  el <- merge(el,conc,by="sourceId")
  names(conc) <- c("targetId","new_targetId")
  el <- merge(el,conc,by="targetId")
  
  el$sourceId <- el$new_sourceId
  el$targetId <- el$new_targetId
  el <- el[,1:8]
  el <- el[order(el$sourceId,el$targetId),]
  head(el)
  
  # alter nodeActivity
  act <- netw_sample$nodeActivity
  names(conc) <- c("nodeId","new_nodeId")
  act <- merge(act,conc,by="nodeId")
  act$nodeId <- act$new_nodeId
  act <- act[,1:9]
  
  netw_sample <- list(edgelist=el,nodelist=nl,nodeActivity=act)
  return(netw_sample)
}


# some help functions - only used in this script
sum_xy <- function(x,varname){
  
  varname.x <- paste0(varname,".x")
  varname.y <- paste0(varname,".y")
  newVarname <- paste0(varname,".sum")
  x$`newVarname` <- x[,varname.x]+x[,varname.y]
  names(x)[ncol(x)] <- newVarname
  
  return(x)
}

absdiff_xy <- function(x,varname){
  
  varname.x <- paste0(varname,".x")
  varname.y <- paste0(varname,".y")
  newVarname <- paste0(varname,".absdiff")
  x$`newVarname` <- abs(x[,varname.x]-x[,varname.y])
  names(x)[ncol(x)] <- newVarname
  return(x)
}

mult_xy <- function(x,varname){
  # log(x*y) = log(x) + log(y)
  varname.x <- paste0(varname,".x")
  varname.y <- paste0(varname,".y")
  newVarname <- paste0(varname,".mult")
  x$`newVarname` <- log(x[,varname.x]+1) + log(x[,varname.y]+1)
  names(x)[ncol(x)] <- newVarname
  return(x)
}

maxweight_xy <- function(x,varname1,varname2){
  
  varname1.x <- paste0(varname1,".x")
  varname2.x <- paste0(varname2,".x")
  varname1.y <- paste0(varname1,".y")
  varname2.y <- paste0(varname2,".y")
  
  opt1 <- x[,varname1.x]*x[,varname2.y]
  opt2 <- x[,varname2.x]*x[,varname1.y]
  newVarname <- paste0(varname1,"_",varname2,".maxweight")
  x$`newVarname` <- ifelse(opt1>opt2,opt1,opt2)
  names(x)[ncol(x)] <- newVarname
  return(x)
}

get_dyad_const <- function(country_panel,netw_sample, vars, verbose=TRUE){
  # get first dyad data structure
  # returns also netw_sample because nodeIds must be inline with dyads
  
  # extract unique country-obs from panel
  country_const <- country_panel[!duplicated(country_panel$iso_code),vars]
  
  # create a nodelist with constant variables
  nodelist <- data.frame(nodeId=1:nrow(country_const),iso_code=country_const$iso_code,stringsAsFactors = FALSE)
  nodelist <- merge(country_const,nodelist,by="iso_code")
  
  # expand to dyadic data
  dyads <- expand.grid(nodelist[,"nodeId"],nodelist[,"nodeId"])
  names(dyads) <- c("nodeId.x","nodeId.y") 
  dyads <- dyads[dyads$nodeId.x < dyads$nodeId.y,]
  dyads <- dyads[order(dyads$nodeId.x,dyads$nodeId.y),]
  
  # add constant vars to i (x) and j (y)
  nodelist_names <- names(nodelist)
  names(nodelist) <- paste0(nodelist_names,".x")
  dyads <- merge(dyads,nodelist,by="nodeId.x")
  names(nodelist) <- paste0(nodelist_names,".y")
  dyads <- merge(dyads,nodelist,by="nodeId.y")
  names(nodelist) <- nodelist_names
  
  # add network information to dyadic data
  # nodeId has changed.... replace
  netw_sample <- replace_nodeId(nodelist=nodelist,netw_sample)
  dyads <- dyads[order(dyads$nodeId.x,dyads$nodeId.y),]
  
  # nonCorona collab pre-Covid (n_ij,t0)
  g <- get_acc_network(sample=netw_sample, covid=FALSE, start=201901, end=201912, soleAuthored=TRUE)
  adj <- get.adjacency(g,type="both",attr="weight")
  adj <- as.matrix(adj)
  dyads$nonCoronaPubsPreCovid.xy <- as.vector(adj[lower.tri(adj,diag=FALSE)])
  # corona collab pre-Covid (c_ij,t0)
  g <- get_acc_network(sample=netw_sample, covid=TRUE, start=201901, end=201912, soleAuthored=TRUE)
  adj <- get.adjacency(g,type="both",attr="weight")
  adj <- as.matrix(adj)
  dyads$coronaPubsPreCovid.xy <- as.vector(adj[lower.tri(adj,diag=FALSE)])
  
  # add geographic distance  (between country centers, in 1000 km)
  dyads$geodist <- geodist::geodist(
    cbind(longitude=dyads[,"longitude.x"],latitude=dyads[,"latitude.x"]), 
    cbind(longitude=dyads[,"longitude.y"],latitude=dyads[,"latitude.y"]),  
    paired=TRUE, measure="geodesic") / 1000 # in km now
  
  dyads$sameRegion <- ifelse(dyads$region.x==dyads$region.y,1,0)
  
  if(verbose){
    N <- length(unique(c(dyads$nodeId.x,dyads$nodeId.y)))
    cat("Number of countries :",length(unique(c(dyads$nodeId.x,dyads$nodeId.y))),"\n")
    cat("Number of dyads :", nrow(dyads), " should equal ", N*(N-1)/2,"\n")
  }
  
  # return result
  return <- list(dyads=dyads,netw_sample=netw_sample)
}

get_dyads_t <- function(dyads,country_panel,netw_sample,t,vars_t){
  
  # iso_code must be included for matching
  vars_t <- c("iso_code",vars_t)
  
  # extract data
  dyads_t <- dyads 
  x_t <- country_panel[country_panel$month==t,vars_t]
  
  names(x_t) <- paste0(vars_t,".x")
  dyads_t <- merge(dyads_t,x_t,by=c("iso_code.x"))
  names(x_t) <- paste0(vars_t,".y")
  dyads_t <- merge(dyads_t,x_t,by=c("iso_code.y"))
  dyads_t <- dyads_t[order(dyads_t$iso_code.x,dyads_t$iso_code.y),]
  
  # add outcome - dyad accum. coronavirus related papers info at time t
  g <- get_acc_network(sample=netw_sample, covid=TRUE, start=202001, end=t, soleAuthored=TRUE)
  #g <- get_acc_network(sample=netw_sample, covid=TRUE, start=t, end=t, soleAuthored=TRUE)
  adj <- get.adjacency(g,type="both",attr="weight")
  adj <- as.matrix(adj)
  dyads_t$coronaPubs.xy <- as.vector(adj[lower.tri(adj,diag=FALSE)])
  
  return(dyads_t)
}

transform_dyad_vars <- function(dyads_t,vars_sum_absdiff, vars_mult, vars_log){
  # applies two standard transformations for dyadic data  
  # transform variables - into sum (x+x), abs-diff |x-y|
  for(var in vars_sum_absdiff){
    dyads_t <- sum_xy(x=dyads_t,varname=var)
    dyads_t <- absdiff_xy(x=dyads_t,varname=var)
  }
  
  # transform variables into log(x*y)
  for(var in vars_mult){
    dyads_t <- mult_xy(x=dyads_t,varname=var)
  }
  
  # log sum, absdiff, and x
  logVars <- c(vars_log,paste(vars_sum_absdiff,".sum",sep=""),
               paste(vars_sum_absdiff,".absdiff",sep=""))
  # remove hdi from logVars
  logVars <- logVars[!grepl("hdi",logVars)]
  dyads_t[,logVars] <- log(dyads_t[,logVars]+1)
  
  return(dyads_t)
}

do_bootstrapDepricated <- function(form, dyad_sample, nb_countries, B){
  
  # create look-up matrix
  lookup <- matrix(0,nrow=nb_countries,ncol=nb_countries)
  counter <- 1
  for(i in 1:(nrow(lookup)-1)){
    for(j in (i+1):ncol(lookup)){
      lookup[i,j] <- counter
      counter <- counter+1
    }
  }
  # help function - get one draw
  draw_b <- function(nb_countries,lookup){
    
    country_draw <- data.frame(table(sample.int(nb_countries,nb_countries,replace=TRUE)))
    names(country_draw) <- c("id","draws")
    country_draw$id <- as.integer(as.character(country_draw$id))
    
    idx <- list()
    counter <- 1
    for(i in 1:(nrow(country_draw)-1)){
      for(j in 2:(nrow(country_draw))){
        idx[[counter]] <- rep(lookup[country_draw[i,1],country_draw[j,1]],times=country_draw[i,2]*country_draw[j,2])
        counter <- counter+1
      }
    }
    return(unlist(idx))
  }
  
  # get coefficient estimates
  res_main <- zeroinfl(formula = form, 
                       data = dyad_sample, 
                       dist = "negbin", link="logit", EM = FALSE)
  coeff <- unlist(res_main$coefficients)
  
  # get bootstrap sd of coeffs.
  coeff_B <- matrix(NA, nrow=B, ncol=length(coeff))
  for(b in 1:B){
    cat(b,"\n")
    idx <- draw_b(nb_countries,lookup)
    tryCatch(
      res_b <- zeroinfl(formula = form, 
                        data = dyad_sample[idx,], 
                        dist = "negbin", link="logit", EM = FALSE)
    )
    coeff_B[b,] <- unlist(res_b$coefficients)
  }
  sd_B <- apply(coeff_B,2,function(x) sd(x))
  
  # return results
  return(cbind(coeff,sd_B))
}

get_mrqap_x_z <- function(depvar,countvars,zerovars,B, x_var, form_part="count", dyads_t, verbose=TRUE){
  # input: SETTINGS as above
  #        x_var variable name of the 'x' to be permuted
  #        form_part - part of the formula 
  #        dyads_t - dataset see above
  # 
  
  if(form_part=="count"){
    # replace x_var by "x"
    x_idx <- which(grepl(x_var,countvars))
    countvars[x_idx] <- "x"
  }else{
    # replace x_var by "x"
    x_idx <- which(grepl(x_var,zerovars))
    zerovars[x_idx] <- "x"
  }
  # create formula
  form_mb <- as.formula(paste0(depvar, " ~ ", paste(countvars,collapse=" + "), " | ",
                               paste(zerovars,collapse=" + ")))
  
  # graph to get adjacency weighted by variable
  g <- graph.edgelist(cbind(dyads_t$nodeId.x,dyads_t$nodeId.y))
  g <- set.edge.attribute(g,"x",index=E(g),value=dyads_t[,x_var])
  adj <- as.matrix(get.adjacency(g,type="both",attr="x"))
  
  # result container for z-stats
  z <- numeric(length=B)
  for(b in 1:B){
    # do permutation on the (permuted) adjacency
    adj <- sna::rmperm(adj)
    # replace "x"
    dyads_t$x <- as.vector(adj[lower.tri(adj,diag=FALSE)])
    # estimate model
    mb <- zeroinfl(formula = form_mb, data = dyads_t, dist = "negbin", link="logit", EM = FALSE)
    # get z-stat
    if(form_part=="count"){
      x_hat <- mb$coefficients$count 
      x_hat <- x_hat[names(x_hat)=="x"]
    }else{
      x_hat <- mb$coefficients$zero
      x_hat <- x_hat[names(x_hat)=="x"]
    }
    
    sd_hat <- diag(vcov(mb))^0.5
    x_sd <- sd_hat[which(grepl("_x",names(sd_hat)))]
    z[b] <- x_hat / x_sd
  }
  
  if(verbose){
    cat("Permutation summary for '",x_var,"'\n")
    print(summary(z))
    cat("\n")
  }
  return(z)
}  

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

get_mrqap_x_z <- function(depvar,countvars,zerovars,B, x_var, form_part="count", dyads_t, start, verbose=TRUE){
  # input: SETTINGS as above
  #        x_var variable name of the 'x' to be permuted
  #        form_part - part of the formula 
  #        dyads_t - dataset see above
  # 
  
  if(form_part=="count"){
    # replace x_var by "x"
    x_idx <- which(grepl(x_var,countvars))
    countvars[x_idx] <- "x"
  }else{
    # replace x_var by "x"
    x_idx <- which(grepl(x_var,zerovars))
    zerovars[x_idx] <- "x"
  }
  # create formula
  form_mb <- as.formula(paste0(depvar, " ~ ", paste(countvars,collapse=" + "), " | ",
                               paste(zerovars,collapse=" + ")))
  
  # graph to get adjacency weighted by variable
  g <- graph.edgelist(cbind(dyads_t$nodeId.x,dyads_t$nodeId.y))
  g <- set.edge.attribute(g,"x",index=E(g),value=dyads_t[,x_var])
  adj <- as.matrix(get.adjacency(g,type="both",attr="x"))
  
  # result container for z-stats
  z <- numeric(length=B)
  for(b in 1:B){
    # do permutation on the (permuted) adjacency
    adj <- sna::rmperm(adj)
    # replace "x"
    dyads_t$x <- as.vector(adj[lower.tri(adj,diag=FALSE)])
    # estimate model
    #start_rand <- start
    #start_rand$count <- start_rand$count + runif(n=length(start_rand$count),min=-0.1,max=0.1)
    #start_rand$zero <- start_rand$zero + runif(n=length(start_rand$zero),min=-0.1,max=0.1)
    
    mb <- zeroinfl(formula = form_mb, data = dyads_t, dist = "negbin", link="logit", 
                   EM = FALSE, start=start ,reltol=0.00001)
    # get z-stat
    if(form_part=="count"){
      x_hat <- mb$coefficients$count 
      x_hat <- x_hat[names(x_hat)=="x"]
    }else{
      x_hat <- mb$coefficients$zero
      x_hat <- x_hat[names(x_hat)=="x"]
    }
    
    sd_hat <- diag(vcov(mb))^0.5
    x_sd <- sd_hat[which(grepl("_x",names(sd_hat)))]
    z[b] <- x_hat / x_sd
  }
  
  if(verbose){
    cat("Permutation summary for '",x_var,"'\n")
    print(summary(z))
    cat("\n")
  }
  return(z)
}  

