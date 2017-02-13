#pull in and format growth values
library(metromonitor)
library(tidyr)
library(dplyr)
library(jsonlite)

rm(list=ls())

#read in growth data
setwd("/home/alec/Projects/Brookings/metro-monitor/data/2017/")
values <- read.csv("raw/Growth Values.csv", stringsAsFactors=FALSE, colClasses=c(cbsa="character"))

#rename indicator
values$measure <- ifelse(values$indicator=="Number of Jobs at Firms 0-5 Years Old, Indexed to Earliest Year Available", "emp5u", ifelse(values$indicator=="Employment, indexed to 2000", "emp", "output"))
table(values$indicator, values$measure)

#limit to the top 100 metros
values100 <- limit100(values, geoID="cbsa", vintage="2013")[c("cbsa", "Metro_Name", "year", "measure", "value")]
names(values100) <- c("cbsa", "geo", "year", "measure", "val")

#test name matches
tst100 <- limit100(values, geoID="cbsa", vintage="2013")
sum(gsub("\\s\\s| Metropolitan Statistical Area", "", gsub(";",",", tst100$cbsa_name))==tst100$Metro_Name)
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

json <- toJSON(list(measures=list(list(key=unbox("output"), value=unbox("Output")), 
                                  list(key=unbox("emp"), value=unbox("Employment")),
                                  list(key=unbox("emp5u"), value=unbox("Employment in new firms"))), 
                    years=2000:2015, 
                    category=unbox("Growth"),
                    values=l), digits=3, na="null")

writeLines(json, "json/growth_values.json")
