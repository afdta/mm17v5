#pull in and format growth values
library(metromonitor)
library(tidyr)
library(dplyr)
library(jsonlite)

rm(list=ls())

#read in growth data
setwd("/home/alec/Projects/Brookings/metro-monitor/data/2017/")
values <- read.csv("raw/All Inclusion Indicators - Indexed Values (IS 2017.01.11).csv", stringsAsFactors=FALSE, colClasses=c(CBSA="character"))

#rename indicator
values$measure <- ifelse(values$Indicator=="Employment-to-Population Ratio", "epop", ifelse(values$Indicator=="Median Earnings", "medearn", "relpov"))
table(values$Indicator, values$measure)

#limit to the top 100 metros
values100 <- limit100(values, geoID="CBSA", vintage="2013")[c("CBSA", "Metro_Name", "Year", "measure", "Education", "Race", "value")]
values100 <- filter(values100, Education=="Total", Race=="Total")
names(values100) <- c("cbsa", "geo", "year", "measure", "edu", "race", "val")

#test name matches
tst100 <- limit100(values, geoID="CBSA", vintage="2013")
sum(gsub("\\s\\s| Metropolitan Statistical Area", "", gsub(";",",", tst100$CBSA_Name))==tst100$Metro_Name)
rm(tst100)

#target format: {indicator: {meta:{}, metros: {metro: [{val: , year: }, {...}], metro: [...], ...}}, indicator:{meta:{...}, metros:{...}, ...}
l <- lapply(split(values100, values100$measure), function(e){
  s <- split(e, e$cbsa)
  ss <- lapply(s, function(ee){
    f <- list()
    f$geo <- unbox(ee[1,"geo"])
    f$measure <- unbox(ee[1,"measure"])
    f$values <- ee[order(ee$year), c("year","val")]
    return(f)
  })
  return(ss) 
})

json <- toJSON(list(measures=list(list(key=unbox("epop"), value=unbox("Employment-to-population ratio")), 
                                  list(key=unbox("medearn"), value=unbox("Median earnings")), 
                                  list(key=unbox("relpov"), value=unbox("Relative income poverty"))),
                    years=2000:2015, 
                    category=unbox("Inclusion"),
                    values=l), digits=3, na="null")

writeLines(json, "json/inclusion_values.json")
