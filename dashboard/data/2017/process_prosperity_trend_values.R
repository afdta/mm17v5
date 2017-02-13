#pull in and format growth values
library(metromonitor)
library(tidyr)
library(dplyr)
library(jsonlite)

rm(list=ls())

#read in growth data
setwd("/home/alec/Projects/Brookings/metro-monitor/data/2017/")
valuesActual <- read.csv("raw/Prosperity Values.csv", stringsAsFactors=FALSE, colClasses=c(CBSA="character"))
values <- read.csv("raw/Prosperity Ind.csv", stringsAsFactors=FALSE, colClasses=c(CBSA="character"))

#rename indicator
values$measure <- ifelse(values$indicator=="Average Annual Wage, indexed to 2000", "avgwage", ifelse(values$indicator=="Output per Job, indexed to 2000", "gdpjob", "gdpcap"))
table(values$indicator, values$measure)

#limit to the top 100 metros
values100 <- limit100(values, geoID="CBSA", vintage="2013")[c("CBSA", "Metro_Name", "year", "measure", "value")]
names(values100) <- c("cbsa", "geo", "year", "measure", "val")

#test name matches
tst100 <- limit100(values, geoID="CBSA", vintage="2013")
sum(gsub("\\s\\s| Metropolitan Statistical Area", "", gsub(";",",", tst100$CBSA_name))==tst100$Metro_Name)
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

json <- toJSON(list(measures=list(list(key=unbox("gdpjob"), value=unbox("GDP per job")), 
                                  list(key=unbox("gdpcap"), value=unbox("GDP per capita")), 
                                  list(key=unbox("avgwage"), value=unbox("Average annual wage"))),
                    years=2000:2015, 
                    category=unbox("Prosperity"),
                    values=l), digits=3, na="null")

writeLines(json, "json/prosperity_values.json")
