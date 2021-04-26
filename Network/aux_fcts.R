#***************************************
# auxiliary functions for Covid paper
# MM, 01.12.2020
#***************************************

# load packages
library(countrycode)


read_data <- function(verbose=TRUE,path){
  # Reads in node_list.csv and node_pub_activity.csv (from fixed paths)
  # Returns a list with i) nodelist, ii) edgelist, iii) nodeActivityFile
  
  
  # read in
  edgelistFile <- paste0(path,"/edge_list.csv")
  edgelist <- read.csv(file=edgelistFile,header=TRUE,sep=",",stringsAsFactors = FALSE)
  
  nodeActivityFile <- paste0(path,"/country_pub_info.csv")
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
  
  # enforce countries in edgelist and nodeActivity
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
  
  # dim(edgelist)
  # dim(nodelist)
  # dim(nodeActivity)

  return(list(nodelist=nodelist,edgelist=edgelist,nodeActivity=nodeActivity))
}





