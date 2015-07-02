#######################################################################################################
# Investment data for Pocketbook 2015 from Brian Carisma on May 28 2015
# 
#
#######################################################################################################
library(gdata)
library(tidyr)
library(stringr)

d1 <- read.xls("./database/Data/Raw/InvestmentDataforStatPocketbook_28May2015.xlsx", sheet=1)
d1 <- d1[1:41,1:3]
names(d1) <- c("Year","oda_share_agriculture","share_of_agriculture_forestry_fishing")
d1$Year <- as.character(d1$Year)
d1$Year[d1$Year == "2013*"] <- "2013"
d1$Year <- factor(d1$Year)
d1$Year <- as.numeric(levels(d1$Year))[d1$Year]
d1$FAOST_CODE <- 5000
invest1 <- d1
save(invest1, file="./database/Data/Processed/invest1.RData")
###
d2 <- read.xls("./database/Data/Raw/InvestmentDataforStatPocketbook_28May2015.xlsx", sheet=2, skip=1)
d2 <- d2[1:41,1:3]
names(d2) <- c("Year","Bilateral","Multilateral")
d2$Bilateral <- as.character(d2$Bilateral)
d2$Bilateral <- str_replace_all(d2$Bilateral, ",", "")
d2$Bilateral <- as.factor(d2$Bilateral)
d2$Bilateral <- as.numeric(levels(d2$Bilateral))[d2$Bilateral]
d2$Year <- as.character(d2$Year)
d2$Year[d2$Year == "2013*"] <- "2013"
d2$Year <- factor(d2$Year)
d2$Year <- as.numeric(levels(d2$Year))[d2$Year]
d2$FAOST_CODE <- 5000
invest2 <- d2
save(invest2, file="./database/Data/Processed/invest2.RData")
###
d3 <- read.xls("./database/Data/Raw/InvestmentDataforStatPocketbook_28May2015.xlsx", sheet=3, skip=2)
d3 <- d3[1:24,c(-1,-3)]

d3 <- gather(d3, # data
             "Year", # name of the key variable
             "remittances", # name of valut var
             2:3) # variables NOT tidy
d3$Year <- as.character(d3$Year)
d3$Year[d3$Year == "X2001.AFF.remittances"] <- 2001
d3$Year[d3$Year == "X2013.AFF.remittance"] <- 2013

names(d3) <- c("FAOST_CODE","Year","remittances")
d3$Year <- factor(d3$Year)
d3$Year <- as.numeric(levels(d3$Year))[d3$Year]
invest3 <- d3
save(invest3, file="./database/Data/Processed/invest3.RData")
