library(metromonitor)
library(tidyr)
library(dplyr)
library(jsonlite)

rm(list=ls())

#read in growth data
setwd("/home/alec/Projects/Brookings/metro-monitor/data/2017/")
changes <- read.csv("raw/Prosperity Change.csv", stringsAsFactors=FALSE, colClasses=c(CBSA="character"))

#rename period
changes$period <- ifelse(changes$year=="2005-2015", "ten", ifelse(changes$year=="2010-2015", "five", "one"))
table(changes$period, changes$year)

#rename indicator
changes$measure <- ifelse(changes$indicator=="Percent Change in Average Annual Wage", "avgwage", ifelse(changes$indicator=="Percent Change in Output per Job", "gdpjob", "gdpcap"))
table(changes$indicator, changes$measure)

#limit to the top 100 metros
changes100 <- limit100(changes, geoID="CBSA", vintage="2013")
table(changes100$indicator, changes100$measure)
#test name matches
sum(gsub("\\s\\s| Metropolitan Statistical Area", "", gsub(";",",", changes100$CBSA_name))==changes100$Metro_Name)

#compute ranks and z-scores
rz <- function(chunk){
  us <- chunk[chunk$CBSA=="99999", ]
  metro <- chunk[chunk$CBSA!="99999", ]
  
  metro$z <- as.numeric(scale(metro$value))
  metro$rnk <- rank(-metro$value, ties.method="min", na.last="keep")
  
  if(nrow(us) > 0){
    us$z <- NA
    us$rnk <- NA
    rtrn <- rbind(metro,us)
  } else{
    rtrn <- metro
  }
  return(rtrn)
}

ch1 <- changes100 %>% group_by(period, measure) %>% do(rz(.))
ch2 <- as.data.frame(ch1[c("CBSA","Metro_Name","period","measure","value","rnk","z")])
names(ch2) <- c("cbsa", "geo", "time", "measure", "val", "rank", "z")

ranker <- function(chunk){
  chunk$rnk <- rank(-chunk$meanz, na.last="keep")
  chunk$rnk2 <- rank(-chunk$sumz, na.last="keep")
  return(chunk)
}

ch3 <- ch2 %>% group_by(time, cbsa, geo) %>% summarise(meanz=mean(z), sumz=sum(z)) %>% group_by(time) %>% do(ranker(.))
sum(ch3$rnk==ch3$rnk2 | (is.na(ch3$rnk & is.na(ch3$rnk2))))

s1 <- split(ch2, ch2$time)
s2 <- lapply(s1, function(e){
  return(split(e, e$measure))
})
s3 <- lapply(split(ch3, ch3$time), function(e){
  d <- as.data.frame(e)
  return(d[c("cbsa","geo","meanz","rnk")])
})

json <- toJSON(list(measures=list(list(key=unbox("gdpjob"), value=unbox("GDP per job")), 
                                  list(key=unbox("gdpcap"), value=unbox("GDP per capita")), 
                                  list(key=unbox("avgwage"), value=unbox("Average annual wage"))),
                    years=2000:2015, 
                    category=unbox("Prosperity"),
                    values=s2,
                    ranks=s3), digits=6, na="null")

#map data
s1a <- split(ch2, ch2$time)
s2a <- lapply(s1a, function(d){
  d$measure_rank <- paste0(d$measure,"Rank")
  a <- spread(d[c("cbsa","geo","measure","val")], measure, val);
  b <- spread(d[c("cbsa","geo","measure_rank","rank")], measure_rank, rank);
  
  merged <- merge(a, b, by=c("cbsa","geo"))
  return(merged);
})
             
json2 <- toJSON(s2a, digits=6, na="null")

writeLines(json, "json/prosperity_change.json")

writeLines(json2, "json/prosperity_map_data.json")


##
library(ggplot2)
gg <- ggplot(ch2)
gg + geom_point(aes(x=1, y=z), alpha=0.15) + facet_grid(time~measure)




