library(dplyr)
library(tidyr)
library(jsonlite)

#look at shift share data
setwd("/home/alec/Projects/Brookings/metro-monitor/data/2017/raw/")

ss <- read.csv("Monitor Shiftshare Cumulative.csv")

names(ss)[2] <- "cbsa"

keepvars <- c("year", "cbsa", "cbsa2013_name", "naics", "industryname", "indicator", "value")

sum2 <- function(v){return(sum(v, na.rm=TRUE))}

#emp
#2005-15
ten <- ss[ss$indicator=="Employment", c(keepvars, "lsshare2005", "imshare2005", "nsshare2005")]
ten2 <- ten %>% spread(indicator, value)
ten3 <- merge(ten2, ten2[ten2$year==2005, c("cbsa", "cbsa2013_name", "naics", "Employment")], 
                    by=c("cbsa", "cbsa2013_name", "naics"), suffixes=c("", "_05"))
ten3$ns10 <- ten3$nsshare2005*ten3$Employment_05 
ten3$im10 <- ten3$imshare2005*ten3$Employment_05 
ten3$cs10 <- ten3$lsshare2005*ten3$Employment_05 

  #checks
  row.names(ten) <- NULL
  row.names(ten2) <- NULL
  row.names(ten3) <- NULL
  
  all.equal(ten[c(1:5,8:10)], ten2[1:8])
  all.equal(ten$value, ten2$Employment)
  all.equal(ten2, ten3[c(4,1:3,5:9)])
  
ten4 <- ten3[ten3$year==2015 & ten3$naics!="PNFTOT" & ten3$naics!="TOTAL", ]
ten4t <- ten3[ten3$year==2015 & ten3$naics=="TOTAL", ]

ten4$gr10 <- ifelse(ten4$Employment_05 == 0, NA, (ten4$Employment / ten4$Employment_05) - 1)
ten4$gr10ou <- ten4$cs10 / ten4$Employment_05

ten_tots <- ten4 %>% group_by(cbsa, cbsa2013_name) %>% summarise(ns=sum2(ns10), im=sum2(im10), cs=sum2(cs10), e1=sum2(Employment), e0=sum2(Employment_05))

#2010-15
five <- ss[ss$indicator=="Employment", c(keepvars, "lsshare2010", "imshare2010", "nsshare2010")]
five2 <- five %>% spread(indicator, value)
five3 <- merge(five2, five2[five2$year==2010, c("cbsa", "cbsa2013_name", "naics", "Employment")], 
              by=c("cbsa", "cbsa2013_name", "naics"), suffixes=c("", "_10"))
five3$ns5 <- five3$nsshare2010*five3$Employment_10 
five3$im5 <- five3$imshare2010*five3$Employment_10 
five3$cs5 <- five3$lsshare2010*five3$Employment_10 

#checks
row.names(five) <- NULL
row.names(five2) <- NULL
row.names(five3) <- NULL

all.equal(five[c(1:5,8:10)], five2[1:8])
all.equal(five$value, five2$Employment)
all.equal(five2, five3[c(4,1:3,5:9)])

five4 <- five3[five3$year==2015 & five3$naics!="PNFTOT" & five3$naics!="TOTAL", ]
five4t <- five3[five3$year==2015 & five3$naics=="TOTAL", ]

five4$gr5 <- ifelse(five4$Employment_10 == 0, NA, (five4$Employment / five4$Employment_10) - 1)
five4$gr5ou <- five4$cs5 / five4$Employment_10

five_tots <- five4 %>% group_by(cbsa, cbsa2013_name) %>% summarise(ns=sum2(ns5), im=sum2(im5), cs=sum2(cs5), e1=sum2(Employment), e0=sum2(Employment_10))


#2014-15
one <- ss[ss$indicator=="Employment", c(keepvars, "lsshare2014", "imshare2014", "nsshare2014")]
one2 <- one %>% spread(indicator, value)
one3 <- merge(one2, one2[one2$year==2014, c("cbsa", "cbsa2013_name", "naics", "Employment")], 
              by=c("cbsa", "cbsa2013_name", "naics"), suffixes=c("", "_14"))
one3$ns1 <- one3$nsshare2014*one3$Employment_14 
one3$im1 <- one3$imshare2014*one3$Employment_14 
one3$cs1 <- one3$lsshare2014*one3$Employment_14 

#checks
row.names(one) <- NULL
row.names(one2) <- NULL
row.names(one3) <- NULL

all.equal(one[c(1:5,8:10)], one2[1:8])
all.equal(one$value, one2$Employment)
all.equal(one2, one3[c(4,1:3,5:9)])

one4 <- one3[one3$year==2015 & one3$naics!="PNFTOT" & one3$naics!="TOTAL", ]
one4t <- one3[one3$year==2015 & one3$naics=="TOTAL", ]

one4$gr1 <- ifelse(one4$Employment_14 == 0, NA, (one4$Employment / one4$Employment_14) - 1)
one4$gr1ou <- one4$cs1 / one4$Employment_14

one_tots <- one4 %>% group_by(cbsa, cbsa2013_name) %>% summarise(ns=sum2(ns1), im=sum2(im1), cs=sum2(cs1), e1=sum2(Employment), e0=sum2(Employment_14))


sslist <- list()
sslist$ten <- ten_tots[c("cbsa","ns","im","cs","e0","e1")]
sslist$five <- five_tots[c("cbsa","ns","im","cs","e0","e1")]
sslist$one <- one_tots[c("cbsa","ns","im","cs","e0","e1")]
detail <- merge(merge(ten4[c("cbsa","naics","gr10","gr10ou")], 
                             five4[c("cbsa","naics","gr5","gr5ou")], by=c("cbsa","naics")),
                             one4[c("cbsa","naics","gr1","gr1ou")], by=c("cbsa","naics"))
sslist$detail <- lapply(split(detail, detail$cbsa), function(e){return(e[1:7])})

json <- toJSON(sslist, digits=5)

writeLines(json,con = "/home/alec/Projects/Brookings/metro-monitor/data/2017/json/shiftshare.json")





f2t <- function(fours){
  f <- sub("^\\s", "", as.character(fours))
  two <- substring(f,1,2)
  
  super <- data.frame(code=)
  
  return(f)
}