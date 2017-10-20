## new data source
## can't get it to work with csv
library(readxl)
url <- paste0("http://fenixservices.fao.org/faostat/static/bulkdownloads/",region_to_report,"_Charts_data_final.xlsx")
destfile <- paste0(region_to_report,"_Charts_data_final.xlsx")
curl::curl_download(url, destfile)
temp <- read_excel(destfile, col_types = c("text", "text", "numeric", "text", 
                                           "text", "text", "numeric", "text", 
                                           "text", "text", "text", "text"))
## ---- part2_setup ----
source(paste0(root.dir,'/input/code/plot/plot_color.R'))

syb_part <- 2

## Part 2
colPart2 <- plot_colors(part = syb_part, 12)
col.main1 <- colPart2[["Main"]][1]
## color for the grid
col.main2 <- colPart2[["Main"]][2]

source(paste0(root.dir,"/input/code/plot/theme.R"))

# map functions
source(paste0(root.dir,'/input/code/plot/map_categories.R'))



#   _   _             _                                         _       _                              _
#  | | | | _ __    __| |  ___  _ __  _ __    ___   _   _  _ __ (_) ___ | |__   _ __ ___    ___  _ __  | |_
#  | | | || '_ \  / _` | / _ \| '__|| '_ \  / _ \ | | | || '__|| |/ __|| '_ \ | '_ ` _ \  / _ \| '_ \ | __|
#  | |_| || | | || (_| ||  __/| |   | | | || (_) || |_| || |   | |\__ \| | | || | | | | ||  __/| | | || |_
#   \___/ |_| |_| \__,_| \___||_|   |_| |_| \___/  \__,_||_|   |_||___/|_| |_||_| |_| |_| \___||_| |_| \__|



## ---- P2undernuTEXT ----
spread_title <- "Undernourishment"
if (region_to_report == "RAF") short_text <- "Undernourishment is a state, lasting for at least one year, of inability to acquire enough food, defined as a level of food intake insufficient to meet dietary energy requirements. About 225  million people – just over one in every five people – in Africa still lack sufficient food for conducting an active and healthy life. Yet progress has been made, even in the presence of significant population growth. About 51 million more people suffer from undernourishment than 25 years ago and 21 million more than a decade ago."
if (region_to_report == "RAP") short_text <- "Undernourishment is a state, lasting for at least one year, of inability to acquire enough food, defined as a level of food intake insufficient to meet dietary energy requirements. About 793 million people – just over one in every nine people – in the world still lack sufficient food for conducting an active and healthy life. Yet progress has been made, even in the presence of significant population growth. Two hundred and sixteen million million fewer people suffer from undernourishment than 25 years ago and 167 million fewer than a decade ago."
if (region_to_report == "REU") short_text <- "Undernourishment is a state, lasting for at least one year, of inability to acquire enough food, defined as a level of food intake insufficient to meet dietary energy requirements. About 793 million people – just over one in every nine people – in the world still lack sufficient food for conducting an active and healthy life. Yet progress has been made, even in the presence of significant population growth. Two hundred and sixteen million million fewer people suffer from undernourishment than 25 years ago and 167 million fewer than a decade ago."
if (region_to_report == "RNE") short_text <- "Undernourishment is a state, lasting for at least one year, of inability to acquire enough food, defined as a level of food intake insufficient to meet dietary energy requirements. About 793 million people – just over one in every nine people – in the world still lack sufficient food for conducting an active and healthy life. Yet progress has been made, even in the presence of significant population growth. The region as a whole, however, shows an increase of 0.5\\% of the number of hungry over last 25 years. Iraq and Yemen have been showing an increasing numbers till 2012-2014, while Sudan had the highest number in 2009-2011."
if (region_to_report == "GLO") short_text <- "Undernourishment is a state, lasting for at least one year, of inability to acquire enough food, defined as a level of food intake insufficient to meet dietary energy requirements. About 793 million people – just over one in every nine people – in the world still lack sufficient food for conducting an active and healthy life. Yet progress has been made, even in the presence of significant population growth. Two hundred and sixteen million million fewer people suffer from undernourishment than 25 years ago and 167 million fewer than a decade ago."

## ---- P2undernuData ----

if (!file.exists(paste0(data.dir,"/fsi_data.RDS"))){
  dat <- read.csv(paste0(data.dir,"/DisseminationDatasetRYB.csv"), stringsAsFactors=FALSE)
  dat$FAOST_CODE <- as.integer(dat$FAOST_CODE)
  dat_witout_country <- dat[dat$FAOST_CODE >= 5000,]
  # Lets replace the COUNTRY level figures with the latest version from Filippo
  dat_country <- read.csv(paste0(data.dir,"/DisseminationDataset090216.csv"), stringsAsFactors=FALSE)
  dat_country$FAOST_CODE  <- as.integer(dat_country$FAOST_CODE)
  dat_country <- dat_country[dat_country$FAOST_CODE < 5000,]
  dat <- bind_rows(dat_witout_country,dat_country)
  # dat <- bind_rows(dat_witout_country,dat)
  # dat_sofi <- read.csv(paste0(data.dir,"/DisseminationDataset090216_SOFIregions.csv"), stringsAsFactors=FALSE)
  # dat_sofi$FS.OA.NOU.P3D1 <- as.character(dat_sofi$FS.OA.NOU.P3D1)
  # dat <- bind_rows(dat_country,dat_sofi)

  # Cereal dependency ratio has odd numbers for year 2011. (China (351) is 100)
  # Recoding them to NA
  dat$FBS.IDR.CRLS.PCT3D[dat$Year == 2011]  <- NA
  
  # RAF
  dat$FAOST_CODE[dat$FAOST_CODE == "SOFIRafReg"] <- "12000" # Regional Office for Africa
  dat$FAOST_CODE[dat$FAOST_CODE == "5101"] <- 12002 # Eastern Africa
  dat$FAOST_CODE[dat$FAOST_CODE == "5102"] <- 12001 # Middle Africa (sofi) - central africa (RAF)
  dat$FAOST_CODE[dat$FAOST_CODE == "5104"] <- 12004 # Southern Africa
  dat$FAOST_CODE[dat$FAOST_CODE == "5105"] <- 12005 # Western Africa
  dat$FAOST_CODE[dat$FAOST_CODE == "421exclSudan"] <- 12003 # North Africa missing for SOFI
  
  # RAP
  dat$FAOST_CODE[dat$FAOST_CODE == "SOFIRapReg"] <- 13000  #  Regional Office for Asia and the Pacific
  dat$FAOST_CODE[dat$FAOST_CODE == "5834"] <- 13001  #	East Asia
  # dat$FAOST_CODE[dat$FAOST_CODE == 5100] <- 13002  #	Pacific Islands
  dat$FAOST_CODE[dat$FAOST_CODE == "5501"] <- 13003  #	Southeast Asia
  # dat$FAOST_CODE[dat$FAOST_CODE == 5100] <- 13004  #	South and Southwest Asia
  dat$FAOST_CODE[dat$FAOST_CODE == "5857"] <- 13005  #	Central Asia - 'Caucasus and central Asia' in SOFI
  # dat$FAOST_CODE[dat$FAOST_CODE == "5501"] <- 13006  #	Australia New Zealand ??
  dat$FAOST_CODE[dat$FAOST_CODE == "5500"] <- 13006  #	Australia New Zealand ??
  # dat$FAOST_CODE[dat$FAOST_CODE == "5502"] <- 13008  #	Melanesia ??
  # dat$FAOST_CODE[dat$FAOST_CODE == "5503"] <- 13009  #	Micronesia ??
  # dat$FAOST_CODE[dat$FAOST_CODE == "5504"] <- 13010  #	Polynesia ??
  dat$FAOST_CODE[dat$FAOST_CODE == "5303"] <- 13012  #	Southern Asia
  # dat$FAOST_CODE[dat$FAOST_CODE == "5856"] <- 13014  #	Western Asia ??
  ## RAP country level aggregates
  new_rows <- dat[dat$FAOST_CODE == "68",]
  new_rows$FAOST_CODE[new_rows$FAOST_CODE == "68"] <- 13007  #	France
  dat <- rbind(dat,new_rows)
  
  new_rows <- dat[dat$FAOST_CODE == "185",]
  new_rows$FAOST_CODE[new_rows$FAOST_CODE == "185"] <- 13011  #	Russian Federation
  dat <- rbind(dat,new_rows)
  
  new_rows <- dat[dat$FAOST_CODE == "231",]
  new_rows$FAOST_CODE[new_rows$FAOST_CODE == "231"] <- 13013  #	United States
  dat <- rbind(dat,new_rows)
  
  # REU  - ALL MISSING FROM SOFI
  dat$FAOST_CODE[dat$FAOST_CODE == "SOFIReuReg"] <- 14000 # Regional Office for Europe and Central Asia
  dat$FAOST_CODE[dat$FAOST_CODE == "REUCaucAndTurkey"] <- 14001 # REU Caucasus and Turkey ??
  dat$FAOST_CODE[dat$FAOST_CODE == "REUCentralAsia"] <- 14002 # REU Central Asia
  dat$FAOST_CODE[dat$FAOST_CODE == "REUCentralEasternEurope"] <- 14003 # REU Central Eastern Europe
  dat$FAOST_CODE[dat$FAOST_CODE == "REUCISeurope"] <- 14004 # REU CIS Europe
  dat$FAOST_CODE[dat$FAOST_CODE == "REUOtherAndEFTA"] <- 14006 # REU Other and EFTA
  dat$FAOST_CODE[dat$FAOST_CODE == "REUSouthEasternEurope"] <- 14007 # REU South Eastern Europe
  ## REU country level aggregates
  new_rows <- dat[dat$FAOST_CODE == "105",]
  new_rows$FAOST_CODE[new_rows$FAOST_CODE == "105"] <- 14005 # Israel
  dat <- rbind(dat,new_rows)
  
  # RNE - ALL MISSING FROM SOFI
  dat$FAOST_CODE[dat$FAOST_CODE == "SOFIRneReg"] <- 15000 # Regional Office for the Near East
  dat$FAOST_CODE[dat$FAOST_CODE == "RNEgccsy"] <- 15001 # Gulf Cooperation Council States and Yemen
  dat$FAOST_CODE[dat$FAOST_CODE == "RNEmaghreb"] <- 15002 # North Africa
  dat$FAOST_CODE[dat$FAOST_CODE == "RNEmashreq"] <- 15003 # Other Near East countries
  
  dat$FAOST_CODE <- as.factor(dat$FAOST_CODE)
  dat$FAOST_CODE <- as.numeric(levels(dat$FAOST_CODE))[dat$FAOST_CODE]
  
  dat <- dat[!is.na(dat$FAOST_CODE),]
  
  dat <- dat[!duplicated(dat[c("Year","FAOST_CODE")]),]
  
  # Add Area var from syb.df
  tmp <- syb.df[!duplicated(dat[c("FAOST_CODE")]),]
  dat <- merge(dat,tmp[c("FAOST_CODE","Area")],by="FAOST_CODE",all.x=TRUE)
  dat <- merge(dat,FAOcountryProfile[c("FAOST_CODE","SHORT_NAME")],by="FAOST_CODE", all.x=TRUE)
  
  dat$FAO_TABLE_NAME <- str_replace_all(dat$FAO_TABLE_NAME, "SOFI Regional Office for ", "")
  # dat$FAO_TABLE_NAME[dat$FAO_TABLE_NAME %in% "Near East and North Africa"] <- "Near East & N. Africa"
  # dat$FAO_TABLE_NAME[dat$FAO_TABLE_NAME %in% "Europe and Central Asia"] <- "Europe & C. Asia"
  # dat$FAO_TABLE_NAME[dat$FAO_TABLE_NAME %in% "Asia and the Pacific"] <- "Asia & the Pacific"
  
  # GLO
  # dat$FAOST_CODE[dat$FAOST_CODE == "LACregion"] <- 5205 # Regional Office for the Near East
  # dat$FAOST_CODE[dat$FAOST_CODE == "RAFregion"] <- 5100 # Gulf Cooperation Council States and Yemen
  # dat$FAOST_CODE[dat$FAOST_CODE == "RAPregion"] <- 5300 # North Africa
  # dat$FAOST_CODE[dat$FAOST_CODE == "REUregion"] <- 5400 # Other Near East countries
  # dat$FAOST_CODE[dat$FAOST_CODE == "RNEregion"] <- 5500 # Other Near East countries
  
  
  # As filippo stated in email on 22/10/15 that
  ## The aggregates in yellow have been created but not disseminated because
  ## they include developed countries. This means that you can use them but
  ## just for those statistics in which developed countries are shown
  ## (you can refer to the Food Security Indicators file for this). For example,
  ## you cannot show the Prevalence of Undernourishment for these aggregates.
  # -> so I am replacing values for those variables & those aggregates with NA
  # and they will appear empty in countryprofile tables
  
  aggregates_to_censore <- c( #13006,  #	Australia New Zealand ?? This is now Oceania!!
    13008,  #	Melanesia ??
    13009,  #	Micronesia ??
    13010,  #	Polynesia ??
    13011,  #	Russian Federation
    13007,  #	France
    13013,  #	United States
    14007,  # REU South Eastern Europe
    14006,  # REU Other and EFTA
    14001,  # REU Caucasus and Turkey ??
    14004,  # REU CIS Europe
    14003,  # REU Central Eastern Europe
    14005,  # Israel
    5400    # Europe
  )
  
  variables_to_censore <- c("FS.OA.POU.PCT3D1", # Prevalence of undernourishment (percent) (3 year averages)
                            "FS.OA.SFEP.PCT",    #	Share of food expenditure of the poor (percent)
                            "FS.OA.DOFD.KCD3D",	 # Depth of food decifit (kcal/capita/day) (3 year averages)
                            "FS.OA.POFI.PCT3D1", # Prevalence of food inadequacy (percent) (3 year avearages)
                            "SH.STA.WAST.ZS",    # Percentage of children under 5 years of age affected by wasting (percent)
                            "SH.STA.STNT.ZS",    #	Percentage of children under 5 years of age who are stunted (percent)
                            "SH.STA.MALN.ZS",	   #	Percentage of children under 5 years of age who are underweight (percent)
                            "SH.STA.AMALN.ZS",	 # 	Percentage of adults who are underweight (percent)
                            "FS.OU.VAD.PCT",     #	Prevalence of Vitamin A deficiency (%)
                            "FS.OU.IODINE.PCT",	#	Prevalence of Iodine deficiency (%)
                            "FS.OA.NOU.P3D1",	#	Number of people undernourished (millions) (3 year averages)
                            "FBS.PCS.PDES.KCD3D" # Dietary energy supply (kcal/cap/day) (3 year averages)
  )
  # Replace existing value with NA
  for (i in variables_to_censore){
    dat[[i]] <- ifelse(dat$FAOST_CODE %in% aggregates_to_censore, NA, dat[[i]])
  }
  
  # M49LatinAmericaAndCaribbean
  dat$Area[dat$FAOST_CODE == 5205] <- "M49macroReg"
  # dat$FS.OA.NOU.P3D1[dat$FS.OA.NOU.P3D1 == "<0.1"] <- 0.01
  # dat$FS.OA.NOU.P3D1[dat$FS.OA.NOU.P3D1 == "ns"] <- 0
  dat$FS.OA.NOU.P3D1 <- as.factor(dat$FS.OA.NOU.P3D1)
  dat$FS.OA.NOU.P3D1 <- as.numeric(levels(dat$FS.OA.NOU.P3D1))[dat$FS.OA.NOU.P3D1]
  dat$FS.OA.POU.PCT3D1[dat$FS.OA.POU.PCT3D1 == "<5.0"] <- 0.1
  dat$FS.OA.POU.PCT3D1 <- as.factor(dat$FS.OA.POU.PCT3D1)
  dat$FS.OA.POU.PCT3D1 <- as.numeric(levels(dat$FS.OA.POU.PCT3D1))[dat$FS.OA.POU.PCT3D1]
  
  dat <- dat[!duplicated(dat[c("FAOST_CODE","Year")]),]
  
  saveRDS(dat, file=paste0(data.dir,"/fsi_data.RDS"))
  # saveRDS(dat, file=paste0(data.dir,"/fsi_data_old.RDS")) # this is the old data 20170228
} else df <- readRDS(paste0(data.dir,"/fsi_data.RDS"))




## ---- P2undernuTOPRIGHT ----

# This should be thought twice how to produce it for regional books!
dat1 <- subset(temp, subset=Part %in% "P2undernu")
dat1 <- subset(dat1, subset=Position %in% "TOPRIGHT")
dat1 <- subset(dat1, select = c(AreaName,Year,Indicator,Value))
dat1$Value <- as.character(dat1$Value)

minYr <- min(dat1$Year)
maxYr <- max(dat1$Year)

dw <- dat1 %>%
  select(Year,AreaName,Value) %>%
  spread(key = Year,value = Value)

dw$FAO_TABLE_NAME[dw$FAO_TABLE_NAME == "Latin America and the Caribbean"] <- "Latin America and \n the Caribbean"
names(dw) <- c("",minYr,maxYr)
tbl_data <- dw
#dw <- dw[c(7,3,4,1,2,5,6),]
# Chiaras comments
if (table_type == "latex") cap <- paste("\\large{Prevalence of undernourishment (percent)",dag_char,"}")
if (table_type == "html")  cap <- "<b>Table: Prevalence of undernourishment (percent)</b>"
caption_text <- cap
if (rulang) caption_text <- ""

print.xtable(xtable(dw, caption = cap, digits = c(0,0,0,0),
                    align= "l{\raggedright\arraybackslash}p{1.7cm}rr"),
             type = table_type, table.placement = NULL,
             comment=FALSE,
             booktabs = TRUE, include.rownames = FALSE, size = "footnotesize", caption.placement = "top",
             html.table.attributes = 'class="table table-striped table-hover"')


## ---- P2undernuLEFT ----
# data
dat1 <- subset(temp, subset=Part %in% "P2undernu")
dat1 <- subset(dat1, subset=Position %in% "LEFT")
dat1 <- subset(dat1, select = c(AreaName,Year,Indicator,Value))
dat1 <- dat1 %>% 
  dplyr::mutate(Yr = substr(dat1$Year,1,4))
dat1$Yr <- as.integer((dat1$Yr))

minYr <- min(dat1$Year)
maxYr <- max(dat1$Year)

dat1 <- arrange(dat1, -Yr, -Value)

# Plot only as many countries as there are for particular region, max 20
nro_latest_cases <- nrow(dat1[dat1$Year == max(dat1$Year),])
if (nro_latest_cases < 20) {ncases <- nro_latest_cases} else ncases <- 20

# slice the data for both years
top15 <- dat1 %>% slice(1:ncases) %>% dplyr::mutate(color = maxYr)
top91 <- dat1 %>% filter(AreaName %in% top15$AreaName, Year == minYr) %>% dplyr::mutate(color = minYr)
dat_plot <- rbind(top15,top91)


# levels based on newest year
dat_plot$AreaName <- factor(dat_plot$AreaName, levels=arrange(top15,Value)$AreaName)
###############

# To make the latest point on top
dat_plot <- arrange(dat_plot, color)

p <- ggplot(data=dat_plot, aes(x=AreaName, y= Value, fill=color))
p <- p + geom_segment(data=dat_plot %>% select(Year,AreaName,Value) %>%
                        spread(key = Year, value = Value) %>% 
                        mutate(color=NA), 
                      aes(y = `1999-2001`, xend = AreaName,
                          yend = `2014-2016`), color="grey80")
p <- p + geom_point(aes(fill=color),size = 4, alpha = 0.75, pch=21, color="white") + theme(panel.grid.major.y = element_blank())
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="\nmillion people")
if (rulang) p <- p + labs(x="",y="\nмлн человек")
p <- p + guides(color = guide_legend(nrow = 2))
p


caption_text <- paste("World top ",ncases," countries with the highest number of undernourished in ",maxYr,")", sep = "")
if (rulang) caption_text <- ""

## ---- P2undernuRIGHT ----
# data
dat1 <- subset(temp, subset=Part %in% "P2undernu")
dat1 <- subset(dat1, subset=Position %in% "RIGHT")
dat1 <- subset(dat1, select = c(AreaName,Year,Indicator,Value))
dat1 <- dat1 %>% 
  dplyr::mutate(Yr = substr(dat1$Year,1,4))
dat1$Yr <- as.integer((dat1$Yr))

minYr <- min(dat1$Year)
maxYr <- max(dat1$Year)

dat1 <- arrange(dat1, -Yr, -Value)

# Plot only as many countries as there are for particular region, max 20
nro_latest_cases <- nrow(dat1[dat1$Year == max(dat1$Year),])
if (nro_latest_cases < 20) {ncases <- nro_latest_cases} else ncases <- 20

# slice the data for both years
top15 <- dat1 %>% slice(1:ncases) %>% dplyr::mutate(color = maxYr)
top91 <- dat1 %>% filter(AreaName %in% top15$AreaName, Year == minYr) %>% dplyr::mutate(color = minYr)
dat_plot <- rbind(top15,top91)


# levels based on newest year
dat_plot$AreaName <- factor(dat_plot$AreaName, levels=arrange(top15,Value)$AreaName)
###############

# To make the latest point on top
dat_plot <- arrange(dat_plot, color)

p <- ggplot(data=dat_plot, aes(x=AreaName, y= Value, fill=color))
p <- p + geom_segment(data=dat_plot %>% select(Year,AreaName,Value) %>%
                        spread(key = Year, value = Value) %>% 
                        mutate(color=NA), 
                      aes(y = `1999-2001`, xend = AreaName,
                          yend = `2014-2016`), color="grey80")
p <- p + geom_point(aes(fill=color),size = 4, alpha = 0.75, pch=21, color="white") + theme(panel.grid.major.y = element_blank())
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="\nmillion people")
if (rulang) p <- p + labs(x="",y="\nмлн человек")
p <- p + guides(color = guide_legend(nrow = 2))
p


# Caption
caption_text <- paste("Top",ncases,"countries with the highest number of undernourished in",unique(top15$color))
if (rulang) caption_text <- ""

## ---- P2undernuBOTTOM ----
dat1 <- subset(temp, subset=Part %in% "P2undernu")
dat1 <- subset(dat1, subset=Position %in% "BOTTOM")
dat1 <- subset(dat1, select = c(AreaName,Year,Indicator,Value))
dat1 <- dat1 %>% 
  dplyr::mutate(Yr = substr(dat1$Year,1,4))
dat1$Yr <- as.integer((dat1$Yr))

p <- ggplot(dat1, aes(x=Yr,y=Value,color=AreaName))
p <- p + geom_line(size=1.1, alpha=.7)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 5)[["Sub"]])
p <- p + labs(x="",y="million\n")
if (rulang) p <- p + labs(x="",y="\nмлн")
p <- p + scale_x_continuous(breaks = c(2000, 2005, 2010, 2015),
                            labels = c("1999-2001", "2004-06", "2009-11", "2014-16"))
p

maxYr <- max(dat1$Year)
# Caption
caption_text <- paste("Number of undernourished (million), top 5 countries in",maxYr,"")
if (rulang) caption_text <- ""


## ---- P2undernuMAP ----
dat1 <- subset(temp, subset=Part %in% "P2undernu")
dat1 <- subset(dat1, subset=Position %in% "MAP")
dat1 <- subset(dat1, select = c(AreaCode,Value,Year))
dat1$AreaCode <- as.integer(dat1$AreaCode)


map.plot <- left_join(map.df,dat1)

# Add region key and subset
map.plot <- map.plot[which(map.plot[[region_to_report]]),]

cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","FS.OA.POU.PCT3D1")]
cat_data$value_cat <- categories(x=cat_data$FS.OA.POU.PCT3D1, n=5, manual = TRUE, manual_breaks = c(0, 5, 15, 25, 35, 100), method="sd") # manualBreaks = c(0, 5, 15, 25, 35, 100),

map.plot <- left_join(map.plot,cat_data[c("FAOST_CODE","value_cat")])

# define map unit
map_unit <- "percent"
if (rulang) map_unit <- ""


p <- create_map_here()
p

yr = dat1$Year[1]
# Caption
caption_text <- paste("Prevalence of undernourishment (percent, ",yr,")", sep = "")
if (rulang) caption_text <- ""




#
#   ___  _               _ _
#  / _ \| |__   ___  ___(_) |_ _   _
# | | | | '_ \ / _ \/ __| | __| | | |
# | |_| | |_) |  __/\__ \ | |_| |_| |
#  \___/|_.__/ \___||___/_|\__|\__, |
#                              |___/


## ---- P2obesityTEXT ----
spread_title <- "Obesity/overweight"
if (region_to_report == "RAF") short_text <- "Overweight and obesity are defined as abnormal or excessive fat accumulation that may impair health. These phenomena are measured using BMI, with overweight greater than 25 and obesity higher than 30. A high body mass index is recognized as increasing the likelihood of incurring various non-communicable diseases and health problems, including cardiovascular disease, diabetes, various cancers and osteoarthritis. The prevalence of overweight and obesity has risen in all regions and is also increasing in nearly all countries."
if (region_to_report == "RAP") short_text <- "Overweight and obesity are defined as abnormal or excessive fat accumulation that may impair health. These phenomena are measured using BMI, with overweight greater than 25 and obesity higher than 30. A high body mass index is recognized as increasing the likelihood of incurring various non-communicable diseases and health problems, including cardiovascular disease, diabetes, various cancers and osteoarthritis. The prevalence of overweight and obesity has risen in all regions and is also increasing in nearly all countries."
if (region_to_report == "REU") short_text <- "Overweight and obesity are defined as abnormal or excessive fat accumulation that may impair health. These phenomena are measured using BMI, with overweight greater than 25 and obesity higher than 30. A high body mass index is recognized as increasing the likelihood of incurring various non-communicable diseases and health problems, including cardiovascular disease, diabetes, various cancers and osteoarthritis. The prevalence of overweight and obesity has risen in the region and is also increasing in nearly all countries."
if (region_to_report == "RNE") short_text <- "Overweight and obesity are defined as abnormal or excessive fat accumulation that may impair health. These phenomena are measured using BMI, with overweight greater than 25 and obesity higher than 30. A high body mass index is recognized as increasing the likelihood of incurring various non-communicable diseases and health problems, including cardiovascular disease, diabetes, various cancers and osteoarthritis. The prevalence of overweight and obesity has risen in all regions and is also increasing in nearly all countries."
if (region_to_report == "GLO") short_text <- "Overweight and obesity are defined as abnormal or excessive fat accumulation that may impair health. These phenomena are measured using BMI, with overweight greater than 25 and obesity higher than 30. A high body mass index is recognized as increasing the likelihood of incurring various non-communicable diseases and health problems, including cardiovascular disease, diabetes, various cancers and osteoarthritis. The prevalence of overweight and obesity has risen in all regions and is also increasing in nearly all countries."
if (rulang) spread_title <- "Ожирение/избыточный вес"
if (region_to_report == "REU" & rulang) short_text <- "Избыточный вес и ожирение определяются как чрезмерные и излишние жировые отложения, которые могут нанести ущерб здоровью. Ожирение и избыточный вес определяются с помощью Индекса массы тела (ИМТ). ИМТ больше 25 кг/м2 указывает на избыточный вес, ИМТ выше 30 кг/м2 указывает на ожирение. Считается, что высокий Индекс массы тела увеличивает вероятность возникновения различных неинфекционных заболеваний и проблем со здоровьем, в том числе сердечно-сосудистых заболеваний, сахарного диабета, различных видов рака и остеоартрита. Распространенность избыточного веса и ожирения возросла в регионе и растет почти в всех странах."

## ---- P2obesityTOPRIGHT
dat1 <- subset(temp, subset=Part %in% "P2obesity")
dat1 <- subset(dat1, subset=Position %in% "TOPRIGHT")
dat1 <- subset(dat1, select = c(AreaName,Year,Indicator,Value))

dat_plot <- dat1
# reorder
# dat_plot$SHORT_NAME <- factor(dat_plot$SHORT_NAME, levels=(dat_plot %>% filter(fill == "2014-16") %>% arrange(-value))$SHORT_NAME)
dat_plot$AreaName <- factor(dat_plot$AreaName, levels=c("Developed countries","Developing countries","World"))

if (rulang){
  dat_plot$Year <- paste(dat_plot$Year,"rr.")
  levels(dat_plot$AreaName)[levels(dat_plot$AreaName) == "Developed countries"] <- "Развитые \nстраны"
  levels(dat_plot$AreaName)[levels(dat_plot$AreaName) == "Developing countries"] <- "Развивающиеся \nстраны"
  levels(dat_plot$AreaName)[levels(dat_plot$AreaName) == "World"] <- "Мир"
}

p <- ggplot(dat_plot, aes(x=AreaName, y=Value, fill=Year))
p <- p + geom_bar(stat="identity", position="dodge")
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + labs(x="",y="percent\n")
if (rulang) p <- p + labs(x="",y="проценты\n")
p <- p + theme(axis.text.x = element_text(angle=45))
p

minYr <- min(dat1$Year)
maxYr <- max(dat1$Year)

# Caption
caption_text <- paste0("Prevalence of food over-acquisition (",minYr," and ",maxYr,")",dag_char, sep = "")
if (rulang) caption_text <- paste0("Распространенность избыточного приобретения пищи (",minYr," и ",maxYr," гг.)",dag_char, sep = "")



## ---- P2obesityLEFT ----
dat1 <- subset(temp, subset=Part %in% "P2obesity")
dat1 <- subset(dat1, subset=Position %in% "LEFT")
dat1 <- subset(dat1, select = c(AreaName,Year,Indicator,Value))
yr = dat1$Year[1]

dat_plot <- dat1 %>% dplyr::mutate(color = yr)

dat_plot$AreaName <- fct_reorder(dat_plot$AreaName, dat_plot$Value) 

p <- ggplot(dat_plot, aes(x=AreaName,y=Value))
p <- p + geom_segment(aes(y = min(dat_plot$Value), xend = AreaName, 
                          yend = Value, color=color), alpha=.5)
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75) + theme(panel.grid.major.y = element_blank())
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 1)[["Sub"]])
p <- p + theme(legend.position = "none") # hide legend as only one year plotted
p <- p + coord_flip()
p <- p + labs(x="",y="\npercent")
if (rulang) p <- p + labs(x="",y="\nпроценты")
p <- p + guides(color = guide_legend(nrow = 2))
p



# Caption
caption_text <- paste("Prevalence of overweight among children under 5, top ",nrow(dat_plot)," countries with the highest values, male (percent ",yr,"*)", sep = "")
if (rulang) caption_text <- paste("Процентная доля мальчиков в возрасте до пяти лет, имеющих избыточный вес, ",nrow(dat_plot)," стран с самыми высокими значениями (в процентах, ",yr," гг.*)", sep = "")

## ---- P2obesityRIGHT ----
dat1 <- subset(temp, subset=Part %in% "P2obesity")
dat1 <- subset(dat1, subset=Position %in% "RIGHT")
dat1 <- subset(dat1, select = c(AreaName,Year,Indicator,Value))
yr = dat1$Year[1]

dat_plot <- dat1 %>% dplyr::mutate(color = yr)

dat_plot$AreaName <- fct_reorder(dat_plot$AreaName, dat_plot$Value) 

p <- ggplot(dat_plot, aes(x=AreaName,y=Value))
p <- p + geom_segment(aes(y = min(dat_plot$Value), xend = AreaName, 
                          yend = Value, color=color), alpha=.5)
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75) + theme(panel.grid.major.y = element_blank())
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 1)[["Sub"]])
p <- p + theme(legend.position = "none") # hide legend as only one year plotted
p <- p + coord_flip()
p <- p + labs(x="",y="\npercent")
if (rulang) p <- p + labs(x="",y="\nпроценты")
p <- p + guides(color = guide_legend(nrow = 2))
p


# Caption
caption_text <- paste("Prevalence of overweight among children under 5, top ",nrow(dat_plot)," countries with the highest values, female (percent ",yr,"*)", sep = "")
if (rulang) caption_text <- paste("Процентная доля девочек в возрасте до 5 лет, имеющих избыточный вес, ",ncases," стран с самыми высокими значениями (в процентах, ",yr," гг.*)", sep = "")

#
## ---- P2obesityBOTTOM ----
dat1 <- subset(temp, subset=Part %in% "P2obesity")
dat1 <- subset(dat1, subset=Position %in% "BOTTOM")
dat1 <- subset(dat1, select = c(AreaName,Year,Indicator,Value))
dat1 <- dat1 %>% 
  dplyr::mutate(Yr = substr(dat1$Year,1,4))
dat1$Yr <- as.integer((dat1$Yr))

dat_plot <- dat1

if (rulang){
  dat_plot$AreaName[dat_plot$AreaName =="World"] <- "Весь мир"
  dat_plot$AreaName[dat_plot$AreaName =="Africa"] <- "Африка"
  dat_plot$AreaName[dat_plot$AreaName =="Asia"] <- "Азия"
  dat_plot$AreaName[dat_plot$AreaName =="Latin America and the Caribbean"] <- "Латинская Америка и \nКарибский бассейн"
  dat_plot$AreaName[dat_plot$AreaName =="Oceania"] <- "Океания"
}

p <- ggplot(data = dat_plot, aes(x = Yr, y = Value,group=AreaName,color=AreaName))
p <- p + geom_line(size=1.1, alpha=.7)
p <- p + scale_color_manual(values = plot_colors(part = 1, length(unique(dat_plot$AreaName)))[["Sub"]])
p <- p + labs(y="percent\n", x="")
if (rulang) p <- p + labs(x="",y="проценты\n")
p <- p + guides(color = guide_legend(nrow = 2))
p <- p + scale_x_continuous(breaks = c(1991, 2001, 2006, 2010,2015),
                            labels = c("1990-92", "2000-02", "2005-07", "2009-11","2014-16"))
p

minYr <- min(dat1$Year)
maxYr <- max(dat1$Year)
# Caption
caption_text <- paste("Prevalence of food over-acquisition (",minYr," to ",maxYr,")",dag_char, sep = "")
if (rulang) caption_text <- paste("Распространенность избыточного приобретения пищи (",minYr," to ",maxYr," гг.)",dag_char, sep = "")

## ---- P2obesityMAP ----
dat1 <- subset(temp, subset=Part %in% "P2obesity")
dat1 <- subset(dat1, subset=Position %in% "MAP")
dat1 <- subset(dat1, select = c(AreaCode,Value,Year))
dat1$AreaCode <- as.integer(dat1$AreaCode)

map.plot <- left_join(map.df,dat1) # so that each country in the region will be filled (value/NA)

# Add region key and subset

cat_data$value_cat <- categories(x=cat_data$Value, n=5, method="jenks")

map.plot <- left_join(map.plot,cat_data[c("FAOST_CODE","value_cat")])

# define map unit
map_unit <- "percent"
if (rulang) map_unit <- "проценты"

p <- create_map_here()
p

yr = dat1$Year[1]
# Caption
caption_text <- paste("Prevalence of overweight and obesity, adults (percent, ",yr,")", sep = "")
if (rulang) caption_text <- paste("Распространенность избыточного веса и ожирения среди взрослых (в процентах, ",yr," г.)", sep = "")


#   _____                       _                             _   _           _       _   _   _   _
#  |  ___|   ___     ___     __| |     __ _  __   __   __ _  (_) | |   __ _  | |__   (_) | | (_) | |_   _   _
#  |  _|   | (_) | | (_) | | (_| |   | (_| |  \ V /  | (_| | | | | | | (_| | | |_) | | | | | | | | |_  | |_| |
#  | |_     / _ \   / _ \   / _` |    / _` | \ \ / /  / _` | | | | |  / _` | | '_ \  | | | | | | | __| | | | |
#  |_|      \___/   \___/   \__,_|    \__,_|   \_/    \__,_| |_| |_|  \__,_| |_.__/  |_| |_| |_|  \__|  \__, |
#                                                                                                       |___/

## ---- P2availabTEXT ----
spread_title <- "Food availability"
if (region_to_report == "RAF") short_text <- "Availability is an important dimension of food security. Supplying enough food to the reference population is a necessary, but insufficient, condition for ensuring adequate access for individuals. Over recent decades, trends in food production per capita have been generally positive across most regions. However, growth rates in Africa have been lower for the last 20 years, despite notable exceptions. In most countries and regions, high food availability is associated with relatively low prevalence of undernourishment. However, outcome indicators show that high food availability does not always guarantee high food security."
if (region_to_report == "RAP") short_text <- "Availability is an important dimension of food security. Supplying enough food to the reference population is a necessary, but insufficient, condition for ensuring adequate access for individuals. Over recent decades, trends in food production per capita have been generally positive across most regions. In most countries and regions, high food availability is associated with relatively low prevalence of undernourishment. However, outcome indicators show that high food availability does not always guarantee high food security."
if (region_to_report == "REU") short_text <- "Availability is an important dimension of food security. Supplying enough food to the reference population is a necessary, but insufficient, condition for ensuring adequate access for individuals. Over the last decade, trends in food production per capita have been positive in the region. In most countries of the region, high food availability is associated with relatively low prevalence of undernourishment. However, outcome indicators show that high food availability does not always guarantee high food security."
if (region_to_report == "RNE") short_text <- "Availability is an important dimension of food security. Supplying enough food to the reference population is a necessary, but insufficient, condition for ensuring adequate access for individuals. Over recent decades, trends in food production per capita have been generally positive across most regions. Average dietary energy supply for Near East and North Africa has been always maintaining a good position at world level. The region, however, shows a middle position of the average supply of protein of animal origin, with few exception like Kuwait, Egypt and United Arab Emirates. However, outcome indicators show that high food availability does not always guarantee high food security."
if (region_to_report == "GLO") short_text <- "Availability is an important dimension of food security. Supplying enough food to the reference population is a necessary, but insufficient, condition for ensuring adequate access for individuals. Over recent decades, trends in food production per capita have been generally positive across most regions. In most countries and regions, high food availability is associated with relatively low prevalence of undernourishment. However, outcome indicators show that high food availability does not always guarantee high food security."
if (rulang) spread_title <- "Наличие продовольствия"
if (region_to_report == "REU" & rulang) short_text <- "Наличие продовольствия является важным фактором продовольственной безопасности. Наличие достаточного количества пищи у населения является необходимым, но недостаточным условием для обеспечения адекватного доступа. За последнее десятилетие тенденции в области производства продуктов питания на душу населения в регионе были позитивными. В большинстве стран региона высокий уровень наличия продовольствия связан с относительно низкой распространенностью недоедания. Однако индикаторы последствий показывают, что высокий уровень наличия продовольствияя не всегда является гарантией высокой продовольственной безопасности."

## ---- P2availabData ----
dat1 <- subset(temp, subset=Part %in% "P2availab")
dat1 <- subset(dat1, subset=Position %in% "TOPRIGHT")
dat1 <- subset(dat1, select = c(AreaName,Year,Indicator,Value))
dat1 <- dat1 %>% 
  dplyr::mutate(Yr = substr(dat1$Year,1,4))
dat1$Yr <- as.integer((dat1$Yr)) + 1

## ---- P2availabTOPRIGHT ----
dat_plot <- dat1

p <- ggplot(data = dat_plot, aes(x = Yr, y = Value,group=AreaName,color=AreaName))
p <- p + geom_line(size=1.1, alpha=.7)
p <- p + scale_color_manual(values = plot_colors(part = 1, length(unique(dat_plot$AreaName)))[["Sub"]])
p <- p + labs(y="percent\n", x="")
if (rulang) p <- p + labs(x="",y="проценты\n")
p <- p + guides(color = guide_legend(nrow = 5))
p <- p + scale_x_continuous(breaks = c(2000, 2006, 2015),
                            labels = c("1999-2001",  "2005-07", "2014-16"))
# p <- p + theme(axis.text.x = element_text(angle = 45))
p


minYr <- min(dat1$Year)
maxYr <- max(dat1$Year)
# Caption
caption_text <- paste("Average dietary energy supply adequacy, 3 year average (",minYr," to ",maxYr,")",dag_char, sep = "")
if (rulang) caption_text <- paste("Адекватность средней энергетической ценности пищевого рациона, средние значения за 3 года (с ",minYr," по ",maxYr," гг.)",dag_char, sep = "")


## ---- P2availabLEFT ----
dat1 <- subset(temp, subset=Part %in% "P2availab")
dat1 <- subset(dat1, subset=Position %in% "LEFT")
dat1 <- subset(dat1, select = c(AreaName,Year,Indicator,Value))
dat1 <- dat1 %>% 
  dplyr::mutate(Yr = substr(dat1$Year,1,4))
dat1$Yr <- as.integer((dat1$Yr))

minYr <- min(dat1$Year)
maxYr <- max(dat1$Year)

dat1 <- arrange(dat1, -Yr, -Value)

# Plot only as many countries as there are for particular region, max 20
nro_latest_cases <- nrow(dat1[dat1$Year == max(dat1$Year),])
if (nro_latest_cases < 20) {ncases <- nro_latest_cases} else ncases <- 20

# slice the data for both years
top15 <- dat1 %>% slice(1:ncases) %>% dplyr::mutate(color = maxYr)
top91 <- dat1 %>% filter(AreaName %in% top15$AreaName, Year == minYr) %>% dplyr::mutate(color = minYr)
dat_plot <- rbind(top15,top91)


# levels based on newest year
dat_plot$AreaName <- factor(dat_plot$AreaName, levels=arrange(top15,Value)$AreaName)
###############

# To make the latest point on top
dat_plot <- arrange(dat_plot, color)

p <- ggplot(data=dat_plot, aes(x=AreaName, y= Value, fill=color))
p <- p + geom_segment(data=dat_plot %>% select(Year,AreaName,Value) %>%
                        spread(key = Year, value = Value) %>% 
                        mutate(color=NA), 
                      aes(y = `1999-2001`, xend = AreaName,
                          yend = `2011-2013`), color="grey80")
p <- p + geom_point(aes(fill=color),size = 4, alpha = 0.75, pch=21, color="white") + theme(panel.grid.major.y = element_blank())
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="\npercent")
if (rulang) p <- p + labs(x="",y="\nпроценты")
p <- p + guides(color = guide_legend(nrow = 2))
p


# Caption
caption_text <- paste("Energy supply derived from cereals, roots and tubers, top ",ncases," countries in ",maxYr, sep = "")
if (rulang) caption_text <- paste("Доля злаков, корнеплодов и клубнеплодов в энергетической ценности пищевого рациона, ",ncases," стран с самыми высокими значениями в ",maxYr," гг.", sep = "")

## ---- P2availabRIGHT ----
dat1 <- subset(temp, subset=Part %in% "P2availab")
dat1 <- subset(dat1, subset=Position %in% "RIGHT")
dat1 <- subset(dat1, select = c(AreaName,Year,Indicator,Value))
dat1 <- dat1 %>% 
  dplyr::mutate(Yr = substr(dat1$Year,1,4))
dat1$Yr <- as.integer((dat1$Yr))

dat1 <- arrange(dat1, -Yr, -Value)

# Plot only as many countries as there are for particular region, max 20
nro_latest_cases <- nrow(dat1[dat1$Year == max(dat1$Year),])
if (nro_latest_cases < 20) {ncases <- nro_latest_cases} else ncases <- 20

dat_plot <- dat1 %>% dplyr::mutate(color = minYr)


# levels based on newest year
dat_plot$AreaName <- factor(dat_plot$AreaName, levels=arrange(dat_plot,Value)$AreaName)
###############

# To make the latest point on top
dat_plot <- arrange(dat_plot, color)

p <- ggplot(dat_plot, aes(x=AreaName,y=Value))
p <- p + geom_segment(aes(y = min(dat_plot$Value), xend = AreaName, 
                          yend = Value, color=color), alpha=.5)
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75) + theme(panel.grid.major.y = element_blank())
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="\ng/cap/day")
if (rulang) p <- p + labs(x="",y="\nг/чел/день")
#p <- p + guides(color = guide_legend(nrow = 2))
p <- p + theme(legend.position = "none")
p


yr = dat1$Year[1]
# Caption
caption_text <- paste("Average protein supply, top ",nrow(dat_plot)," countries in ",yr, sep = "")
if (rulang) caption_text <- paste("Средний объем получаемых белков, ",nrow(dat_plot)," стран с самыми высокими значениями в ",yr," гг.", sep = "")


## ---- P2availabBOTTOM ----
dat1 <- subset(temp, subset=Part %in% "P2availab")
dat1 <- subset(dat1, subset=Position %in% "BOTTOM")
dat1 <- subset(dat1, select = c(AreaName,Year,Indicator,Value))
dat1 <- dat1 %>% 
  dplyr::mutate(Yr = substr(dat1$Year,1,4))
dat1$Yr <- as.integer((dat1$Yr)) + 1

dat_plot <- dat1

p <- ggplot(data = dat_plot, aes(x = Yr, y = Value,group=AreaName,color=AreaName))
p <- p + geom_line(size=1.1, alpha=.7)
p <- p + scale_color_manual(values = plot_colors(part = 1, length(unique(dat_plot$AreaName)))[["Sub"]])
p <- p + labs(y="g/cap/day\n", x="")
if (rulang) p <- p + labs(x="",y="г/чел/день\n")
p <- p + guides(color = guide_legend(nrow = 2))
p <- p + scale_x_continuous(breaks = c(2001, 2006, 2010,2012),
                            labels = c("2000-02", "2005-07", "2009-11","2011-13"))
p

minYr <- min(dat1$Year)
maxYr <- max(dat1$Year)

# Caption
caption_text <- paste("Average supply of protein of animal origin",dag_char)
if (rulang) caption_text <- paste("Средний объем получаемых белков животного происхождения",dag_char)


## ---- P2availabMAP ----

dat1 <- subset(temp, subset=Part %in% "P2availab")
dat1 <- subset(dat1, subset=Position %in% "MAP")
dat1 <- subset(dat1, select = c(AreaCode,Value,Year))
dat1$AreaCode <- as.integer(dat1$AreaCode)

map.plot <- left_join(map.df,dat1, by = c("FAOST_CODE" = "AreaCode")) # so that each country in the region will be filled (value/NA)

# Add region key and subset

map.plot <- map.plot[which(map.plot[[region_to_report]]),]

cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","Value")]
cat_data$value_cat <- categories(x=cat_data$Value, n=5, method="jenks",decimals=2)

map.plot <- left_join(map.plot,cat_data[c("FAOST_CODE","value_cat")])

# define map unit
map_unit <- "percent"
if (rulang) map_unit <- "проценты"


p <- create_map_here()
p

yr = dat1$Year[1]
# Caption
caption_text <- paste("Average value of food production, constant 2004-2006 I\\$ per person (3 year average, ",yr,")", sep = "")
if (rulang) caption_text <- paste("Средний объем производства продовольствия в стоимостном выражении, в постоянных межд. долларах 2004-2006 гг. на душу населения (средние значения за три года, ",yr," гг.)", sep = "")

#   _____                       _
#  |  ___|   ___     ___     __| |     __ _    ___    ___    ___   ___   ___
#  | |_     / _ \   / _ \   / _` |    / _` |  / __|  / __|  / _ \ / __| / __|
#  |  _|   | (_) | | (_) | | (_| |   | (_| | | (__  | (__  |  __/ \__ \ \__ \
#  |_|      \___/   \___/   \__,_|    \__,_|  \___|  \___|  \___| |___/ |___/
#


## ---- P2accessTEXT ----
spread_title <- "Food access"
if (region_to_report == "RAF") short_text <- "An adequate supply of food does not in itself guarantee household level food security. Access to food is primarily determined by incomes, food prices and the ability of households and individuals to obtain access to social support. Individuals’ access to food is also heavily influenced by social variables, including gender positioning and power hierarchies within households. In addition to economic affordability, physical access to food is also facilitated by adequate infrastructure, such as railway lines and paved roads."
if (region_to_report == "RAP") short_text <- "An adequate supply of food does not in itself guarantee household level food security. Access to food is primarily determined by incomes, food prices and the ability of households and individuals to obtain access to social support. Individuals’ access to food is also heavily influenced by social variables, including gender positioning and power hierarchies within households. In addition to economic affordability, physical access to food is also facilitated by adequate infrastructure, such as railway lines and paved roads."
if (region_to_report == "REU") short_text <- "An adequate supply of food does not in itself guarantee household level food security. Access to food is primarily determined by incomes, food prices and the ability of households and individuals to obtain access to social support. Individuals’ access to food is also heavily influenced by social variables, including gender positioning and power hierarchies within households. In addition to economic affordability, physical access to food is also facilitated by adequate infrastructure, such as railway lines and paved roads."
if (region_to_report == "RNE") short_text <- "An adequate supply of food does not in itself guarantee household level food security. Access to food is primarily determined by incomes, food prices and the ability of households and individuals to obtain access to social support. Individuals’ access to food is also heavily influenced by social variables, including gender positioning and power hierarchies within households. In addition to economic affordability, physical access to food is also facilitated by adequate infrastructure, such as railway lines and paved roads."
if (region_to_report == "GLO") short_text <- "An adequate supply of food does not in itself guarantee household level food security. Access to food is primarily determined by incomes, food prices and the ability of households and individuals to obtain access to social support. Individuals’ access to food is also heavily influenced by social variables, including gender positioning and power hierarchies within households. In addition to economic affordability, physical access to food is also facilitated by adequate infrastructure, such as railway lines and paved roads."
if (rulang) spread_title <- "Доступность продовольствия"
if (region_to_report == "REU" & rulang) short_text <- "Наличие достаточного количества продуктов питания само по себе не гарантирует уровень продовольственной безопасности на уровне домохозяйств. Доступ к продуктам питания в первую очередь определяется уровнем доходов, цен на продовольствие и способностью домохозяйств и индивидуумов получить доступ к социальной поддержке. Доступ индивидуумов к продуктам питания также сильно зависит от социальных факторов, включая гендерное позиционирование и иерархию власти внутри домохозяйства. В дополнение к экономической доступности, физическая доступность продовольствия также обусловливается адекватной инфраструктурой, в том числе густотой железнодорожной сети и наличием дорог с твердым покрытием."

## ---- P2accessData ----
dat1 <- subset(temp, subset=Part %in% "P2access")
dat1 <- subset(dat1, subset=Position %in% "TOPRIGHT")
dat1 <- subset(dat1, select = c(AreaName,Year,Indicator,Value))
dat1 <- dat1 %>% 
  dplyr::mutate(Yr = substr(dat1$Year,1,4))
dat1$Yr <- as.integer((dat1$Yr)) + 1

## ---- P2accessTOPRIGHT ----
dat_plot <- dat1

p <- ggplot(data = dat_plot, aes(x = Yr, y = Value,group=AreaName,color=AreaName))
p <- p + geom_line(size=1.1, alpha=.7)
p <- p + scale_color_manual(values = plot_colors(part = 1, length(unique(dat_plot$AreaName)))[["Sub"]])
p <- p + labs(y="kcal/cap/day\n", x="")
if (rulang) p <- p + labs(x="",y="ккал/чел/день\n")
p <- p + guides(color = guide_legend(nrow = 5))
p <- p + scale_x_continuous(breaks = c(2000, 2006, 2015),
                            labels = c("1999-2001",  "2005-07", "2014-16"))
# p <- p + theme(axis.text.x = element_text(angle = 45))
p

# Caption
caption_text <- paste("Depth of food deficit (kcal/capita/day) (3 year averages)",dag_char)
if (rulang) caption_text <- paste("Масштабы дефицита продовольствия (ккал/чел/день) (средние значения за 3 года)",dag_char)


## ---- P2accessLEFT ----
dat1 <- subset(temp, subset=Part %in% "P2access")
dat1 <- subset(dat1, subset=Position %in% "LEFT")
dat1 <- subset(dat1, select = c(AreaName,Year,Indicator,Value))
dat1$Year <- as.integer((dat1$Year))

minYr <- min(dat1$Year)
maxYr <- max(dat1$Year)

# semi-standard data munging for two year dot-plots
# Plot only as many countries as there are for particular region, max 20
nro_latest_cases <- nrow(dat1[dat1$Year == max(dat1$Year),])
if (nro_latest_cases < 20) {ncases <- nro_latest_cases} else ncases <- 20
dat1 <- arrange(dat1, -Year, -Value)
# slice the data for both years
top2015 <- dat1 %>% slice(1:ncases) %>% dplyr::mutate(color = maxYr)
top2000 <- dat1 %>% filter(AreaName %in% top2015$AreaName, Year == minYr) %>% dplyr::mutate(color = minYr)
dat_plot <- rbind(top2015,top2000)
# levels based on newest year
dat_plot$AreaName <- factor(dat_plot$AreaName, levels=arrange(top2015,Value)$AreaName)
###############

if (rulang){
  dat_plot$color <- paste(dat_plot$color," г.")
}

# To make the latest point on top
dat_plot <- arrange(dat_plot, color)
dat_plot$color <- as.character(dat_plot$color)

p <- ggplot(data=dat_plot, aes(x=AreaName, y= Value, fill=color))
p <- p + geom_segment(data=dat_plot %>% select(Year,AreaName,Value) %>%
                        spread(key = Year, value = Value) %>% 
                        mutate(color=NA), 
                      aes(y = `2000`, xend = AreaName,
                          yend = `2014`), color="grey80")
p <- p + geom_point(aes(fill=color),size = 4, alpha = 0.75, pch=21, color="white") + theme(panel.grid.major.y = element_blank())
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="\nindex")
if (rulang) p <- p + labs(x="",y="\nиндекс")
p <- p + guides(fill = guide_legend(nrow = 1))
p


# Caption
caption_text <- paste("Domestic food price level index, top ",ncases," countries in ",maxYr," (",minYr," to ",maxYr,")", sep = "")
if (rulang) caption_text <- paste("Индекс внутренних цен на продовольствие, ",ncases," стран с самыми высокими значениями в ",maxYr," году (с ",minYr," по ",maxYr," гг.)", sep = "")


## ---- P2accessRIGHT ----
dat1 <- subset(temp, subset=Part %in% "P2access")
dat1 <- subset(dat1, subset=Position %in% "RIGHT")
dat1 <- subset(dat1, select = c(AreaName,Year,Indicator,Value))
dat1 <- dat1 %>% 
  dplyr::mutate(Yr = substr(dat1$Year,1,4))
dat1$Yr <- as.integer((dat1$Yr))

minYr <- min(dat1$Year)
maxYr <- max(dat1$Year)

# semi-standard data munging for two year dot-plots
# Plot only as many countries as there are for particular region, max 20
nro_latest_cases <- nrow(dat1[dat1$Yr == max(dat1$Yr),])
if (nro_latest_cases < 20) {ncases <- nro_latest_cases} else ncases <- 20
dat1 <- arrange(dat1, -Yr, -Value)
# slice the data for both years
top2015 <- dat1 %>% slice(1:ncases) %>% dplyr::mutate(color = maxYr)
top2000 <- dat1 %>% filter(AreaName %in% top2015$AreaName, Year == minYr) %>% dplyr::mutate(color = minYr)
dat_plot <- rbind(top2015,top2000)
# levels based on newest year
dat_plot$AreaName <- factor(dat_plot$AreaName, levels=arrange(top2015,Value)$AreaName)
###############

if (rulang){
  dat_plot$color <- paste(dat_plot$color," гг.")
}

# To make the latest point on top
dat_plot <- arrange(dat_plot, color)

p <- ggplot(data=dat_plot, aes(x=AreaName, y= Value, fill=color))
p <- p + geom_segment(data=dat_plot %>% select(Year,AreaName,Value) %>%
                        spread(key = Year, value = Value) %>% 
                        mutate(color=NA), 
                      aes(y = `1999-2001`, xend = AreaName,
                          yend = `2014-2016`), color="grey80")
p <- p + geom_point(aes(fill=color),size = 4, alpha = 0.75, pch=21, color="white") + theme(panel.grid.major.y = element_blank())
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="\npercent")
if (rulang) p <- p + labs(x="",y="\nпроценты")
p <- p + guides(fill = guide_legend(nrow = 1))
p


# Caption
caption_text <- paste("Prevalence of undernourishment, highest ",ncases," countries in ",maxYr," (3 year averages)", sep = "")
if (rulang) caption_text <- paste("Распространенность недоедания,",ncases,"стран мира с самими высокими показателями в ",maxYr," гг. (средние показатели за 3 года)", sep = "")

## ---- P2accessBOTTOM ----
dat1 <- subset(temp, subset=Part %in% "P2access")
dat1 <- subset(dat1, subset=Position %in% "BOTTOM")
dat1 <- subset(dat1, select = c(AreaName,Year,Indicator,Value))
dat1$Year <- as.integer((dat1$Year))

p <- ggplot(data = dat1, aes(x = Year, y = Value,group=AreaName,color=AreaName))
p <- p + geom_line(size=1.1, alpha=.7)
p <- p + scale_color_manual(values = plot_colors(part = 1, length(unique(dat1$AreaName)))[["Sub"]])
p <- p + labs(y="US$\n", x="")
if (rulang) p <- p + labs(x="",y="доллары США\n")
p <- p + guides(color = guide_legend(nrow = 3))
p <- p + scale_y_continuous(labels=space)
p

# Caption
caption_text <- paste("GDP per capita, PPP, constant 2011 international \\$", sep = "")
if (rulang) caption_text <- paste("ВВП на душу населения по ППС, в постоянных межд. долл. 2011 г.", sep = "")


## ---- P2accessMAP ----
dat1 <- subset(temp, subset=Part %in% "P2access")
dat1 <- subset(dat1, subset=Position %in% "MAP")
dat1 <- subset(dat1, select = c(AreaCode,Value,Year))
dat1$AreaCode <- as.integer(dat1$AreaCode)

map.plot <- left_join(map.df,dat1, by = c("FAOST_CODE" = "AreaCode")) # so that each country in the region will be filled (value/NA)

cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","Value")]
cat_data$value_cat <- categories(x=cat_data$Value, n=5) # manualBreaks = c(0, 5, 15, 25, 35, 100),

map.plot <- left_join(map.plot,cat_data[c("FAOST_CODE","value_cat")])


# define map unit
map_unit <- "km per 100 km² of land"
if (rulang) map_unit <- "км на 100 км² суши"

p <- create_map_here()
p

yr = dat1$Year[1]
# Caption
caption_text <- paste("Road density, km per 100 km\\textsuperscript{2} of land area (",yr,"*)", sep = "")
if (rulang) caption_text <- paste("Густота дорожной сети, в км на 100 км2 площади суши (",yr," гг.*)", sep = "")

#   _____                       _           _             _       _   _   _   _
#  |  ___|   ___     ___     __| |    ___  | |_    __ _  | |__   (_) | | (_) | |_   _   _
#  | |_     / _ \   / _ \   / _` |   / __| | __|  / _` | | '_ \  | | | | | | | __| | | | |
#  |  _|   | (_) | | (_) | | (_| |   \__ \ | |_  | (_| | | |_) | | | | | | | | |_  | |_| |
#  |_|      \___/   \___/   \__,_|   |___/  \__|  \__,_| |_.__/  |_| |_| |_|  \__|  \__, |
#                                                                                   |___/

## ---- P2stabilityTEXT ----
spread_title <- "Economic and political stability"
if (region_to_report == "RAF") short_text <- "Over the last ten years, food and agricultural markets have entered an unexpectedly turbulent phase, characterized by large supply shortfalls, price swings. Increased political unrests and economic downturns, coupled with extreme weather conditions in Africa, have had direct and adverse impacts on food security. The poorer the household, the stronger the impact of external shocks, as poor households spend a proportionally higher share of their incomes on food."
if (region_to_report == "RAP") short_text <- "Over the last ten years, food and agricultural markets have entered an unexpectedly turbulent phase, characterized by large supply shortfalls, price swings. Political and economic uncertainties, coupled with extreme weather conditions, can have direct and adverse impacts on food security. The poorer the household, the stronger the impact of external shocks, as poor households spend a proportionally higher share of their incomes on food."
if (region_to_report == "REU") short_text <- "Over the last ten years, food and agricultural markets have entered an unexpectedly turbulent phase, characterized by large supply shortfalls, price swings. Political and economic uncertainties, coupled with extreme weather conditions, can have direct and adverse impacts on food security. The poorer the household, the stronger the impact of external shocks, as poor households spend a proportionally higher share of their incomes on food."
if (region_to_report == "RNE") short_text <- "Over the last ten years, food and agricultural markets have entered an unexpectedly turbulent phase, characterized by large supply shortfalls, price swings. Recent political and economic uncertainties in several countries in the region, coupled with extreme weather conditions , have imposed direct and adverse impacts on food security. The poorer the household, the stronger the impact of external shocks, as poor households spend a proportionally higher share of their incomes on food."
if (region_to_report == "GLO") short_text <- "Over the last ten years, food and agricultural markets have entered an unexpectedly turbulent phase, characterized by large supply shortfalls, price swings. Political and economic uncertainties, coupled with extreme weather conditions, can have direct and adverse impacts on food security. The poorer the household, the stronger the impact of external shocks, as poor households spend a proportionally higher share of their incomes on food."
if (rulang) spread_title <- "Экономическая и политическая стабильность"
if (region_to_report == "REU" & rulang) short_text <- "В течение последних десяти лет продовольственный и сельскохозяйственный рынки переживают неспокойные времена, характеризующиеся большими дефицитами предложения и колебаниями цен. Политическая и экономическая неопределенность в сочетании с экстремальными погодными условиями могут иметь прямое и неблагоприятное воздействие на продовольственную безопасность. Чем беднее домохозяйство, тем сильнее влияние внешних факторов, поскольку бедные домохозяйства тратят пропорционально более высокую долю своих доходов на питание."

## ---- P2stabilityData ----
dat1 <- subset(temp, subset=Part %in% "P2stability")
dat1 <- subset(dat1, subset=Position %in% "TOPRIGHT")
dat1 <- subset(dat1, select = c(AreaName,Year,Indicator,Value))
dat1$Year <- as.integer(dat1$Year)

## ---- P2stabilityTOPRIGHT ----
dat_plot <- dat1
p <- ggplot(data = dat_plot, aes(x = Year, y = Value,group=AreaName,color=AreaName))
p <- p + geom_line(size=1.1, alpha=.7)
p <- p + scale_color_manual(values = plot_colors(part = 1, length(unique(dat_plot$AreaName)))[["Sub"]])
p <- p + labs(y="index\n", x="")
if (rulang) p <- p + labs(x="",y="индекс\n")
p <- p + guides(color = guide_legend(nrow = 5))
p <- p + scale_x_continuous(breaks = c(2000, 2005,2010, 2014))
p


# Caption
caption_text <- paste("Per capita food production variability, constant 2004-2006 thousand international \\$",dag_char)
if (rulang) caption_text <- paste("Вариативность производства продовольствия на душу населения, в постоянных тыс. межд. долларах  2004-2006 гг.",dag_char)


## ---- P2stabilityLEFT ----
dat1 <- subset(temp, subset=Part %in% "P2stability")
dat1 <- subset(dat1, subset=Position %in% "LEFT")
dat1 <- subset(dat1, select = c(AreaName,Year,Indicator,Value))
dat1 <- dat1 %>% 
  dplyr::mutate(Yr = substr(dat1$Year,1,4))
dat1$Yr <- as.integer((dat1$Yr))

minYr <- min(dat1$Year)
maxYr <- max(dat1$Year)

# Plot only as many countries as there are for particular region, max 20
nro_latest_cases <- nrow(dat1[dat1$Year == max(dat1$Year),])
if (nro_latest_cases < 20) {ncases <- nro_latest_cases} else ncases <- 20
dat1 <- arrange(dat1, -Yr, -Value)
# slice the data for both years
top2015 <- dat1 %>% slice(1:ncases) %>% dplyr::mutate(color = maxYr)
top2000 <- dat1 %>% filter(AreaName %in% top2015$AreaName, Year == minYr) %>% dplyr::mutate(color = minYr)
dat_plot <- rbind(top2015,top2000)
# levels based on newest year
dat_plot$AreaName <- factor(dat_plot$AreaName, levels=arrange(top2015,Value)$AreaName)
###############

if (rulang){
  dat_plot$color <- paste(dat_plot$color," г.")
}

# To make the latest point on top
dat_plot <- arrange(dat_plot, Year)

p <- ggplot(data=dat_plot, aes(x=AreaName, y= Value, fill=color))
p <- p + geom_segment(data=dat_plot %>% select(Year,AreaName,Value) %>%
                        spread(key = Year, value = Value) %>% 
                        mutate(color=NA), 
                      aes(y = `2000`, xend = AreaName,
                          yend = `2013`), color="grey80")
p <- p + geom_point(aes(fill=color),size = 4, alpha = 0.75, pch=21, color="white") + theme(panel.grid.major.y = element_blank())
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="\nkcal/capita/day")
if (rulang) p <- p + labs(x="",y="\nккал/чел/день")
p <- p + guides(color = guide_legend(nrow = 1))
p



# Caption
caption_text <- paste("Per capita food supply variability, top ",ncases," countries in ",maxYr,", kcal/capita/day", sep = "")
if (rulang) caption_text <- paste("Вариативность продовольственного снабжения на душу населения, ",ncases," стран с самыми высокими значениями в ",maxYr," году, ккал/чел/день", sep = "")

## ---- P2stabilityRIGHT ----
dat1 <- subset(temp, subset=Part %in% "P2stability")
dat1 <- subset(dat1, subset=Position %in% "RIGHT")
dat1 <- subset(dat1, select = c(AreaName,Year,Indicator,Value))
dat1 <- dat1 %>% 
  dplyr::mutate(Yr = substr(dat1$Year,1,4))
dat1$Yr <- as.integer((dat1$Yr))

minYr <- min(dat1$Year)
maxYr <- max(dat1$Year)

# Plot only as many countries as there are for particular region, max 20
nro_latest_cases <- nrow(dat1[dat1$Year == max(dat1$Year),])
if (nro_latest_cases < 20) {ncases <- nro_latest_cases} else ncases <- 20
dat1 <- arrange(dat1, -Yr, -Value)
# slice the data for both years
top2015 <- dat1 %>% slice(1:ncases) %>% dplyr::mutate(color = maxYr)
top2000 <- dat1 %>% filter(AreaName %in% top2015$AreaName, Year == minYr) %>% dplyr::mutate(color = minYr)
dat_plot <- rbind(top2015,top2000)
# levels based on newest year
dat_plot$AreaName <- factor(dat_plot$AreaName, levels=arrange(top2015,Value)$AreaName)
###############

if (rulang){
  dat_plot$color <- paste(dat_plot$color," г.")
}

# To make the latest point on top
dat_plot <- arrange(dat_plot, Year)

p <- ggplot(data=dat_plot, aes(x=AreaName, y= Value, fill=color))
p <- p + geom_segment(data=dat_plot %>% select(Year,AreaName,Value) %>%
                        spread(key = Year, value = Value) %>% 
                        mutate(color=NA), 
                      aes(y = `2000`, xend = AreaName,
                          yend = `2014`), color="grey80")
p <- p + geom_point(aes(fill=color),size = 4, alpha = 0.75, pch=21, color="white") + theme(panel.grid.major.y = element_blank())
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="\nindex")
if (rulang) p <- p + labs(x="",y="\nиндекс")
p <- p + guides(color = guide_legend(nrow = 1))
p


# Caption
caption_text <- paste("Domestic food price volatility index, top ",ncases," countries in ",maxYr,"", sep = "")
if (rulang) caption_text <- paste("Индекс волатильности внутренних цен на продовольствие, ",ncases," стран с самыми высокими значениями в ",maxYr," году", sep = "")

## ---- P2stabilityBOTTOM ----
dat1 <- subset(temp, subset=Part %in% "P2stability")
dat1 <- subset(dat1, subset=Position %in% "BOTTOM")
dat1 <- subset(dat1, select = c(AreaName,Year,Indicator,Value))

minYr <- min(dat1$Year)
maxYr <- max(dat1$Year)
dat_plot <- dat1
dat_plot$AreaName <- factor(dat_plot$AreaName, levels=(dat_plot %>% filter(Year == maxYr) %>% arrange(-Value))$AreaName)

if (rulang){
  dat_plot$Year <- paste(dat_plot$Year," гг.")
}


p <- ggplot(dat_plot, aes(x=AreaName,y=Value,fill=Year))
p <- p + geom_bar(stat="identity",position="dodge")
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + labs(x=NULL,y="percent\n")
if (rulang) p <- p + labs(x="",y="проценты\n")
p <- p + theme(axis.text.x = element_text(angle=45))
p


caption_text <- paste("Value of food imports as a share of total merchandise exports (3 year averages)",dag_char)
if (rulang) caption_text <- paste("Стоимость импорта продовольствия по отношению к стоимости экспорта всех товаров (средние значения за 3 года)",dag_char)

## ---- P2stabilityMAP ----
dat1 <- subset(temp, subset=Part %in% "P2stability")
dat1 <- subset(dat1, subset=Position %in% "MAP")
dat1 <- subset(dat1, select = c(AreaCode,Value,Year))
dat1$AreaCode <- as.integer(dat1$AreaCode)

map.plot <- left_join(map.df,dat1, by = c("FAOST_CODE" = "AreaCode")) # so that each country in the region will be filled (value/NA)

# Add region key and subset

map.plot <- map.plot[which(map.plot[[region_to_report]]),]

cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","Value")]
cat_data$value_cat <- categories(x=cat_data$Value, n=5,decimals = 1) # manualBreaks = c(0, 5, 15, 25, 35, 100),

map.plot <- left_join(map.plot,cat_data[c("FAOST_CODE","value_cat")])

# define map unit
map_unit <- "index"
if (rulang) map_unit <- "индекс"

p <- create_map_here()
p

yr = dat1$Year[1]
# Caption
caption_text <- paste("Political stability and absence of violence/terrorism, index (",yr,")", sep = "")
if (rulang) caption_text <- paste("Политическая стабильность и отсутствие проявлений насилия/терроризма, индекс (",yr," г.)", sep = "")


#   _____                       _             _     _   _   _                 _     _
#  |  ___|   ___     ___     __| |    _   _  | |_  (_) | | (_)  ____   __ _  | |_  (_)   ___    _ __
#  | |_     / _ \   / _ \   / _` |   | | | | | __| | | | | | | |_  /  / _` | | __| | |  / _ \  | '_ \
#  |  _|   | (_) | | (_) | | (_| |   | |_| | | |_  | | | | | |  / /  | (_| | | |_  | | | (_) | | | | |
#  |_|      \___/   \___/   \__,_|    \__,_|  \__| |_| |_| |_| /___|  \__,_|  \__| |_|  \___/  |_| |_|


## ---- P2utilizaTEXT ----
spread_title <- "Food utilization"
if (region_to_report == "RAF") short_text <- "Utilization emphasizes the nutritional aspects of food security. It is commonly understood as the way the body makes the most of nutrients from food. Sufficient energy and nutrient intake includes nutritious and safe diets, a clean environment, access to health care, diversity of a diet and intra-household distribution of food. Poor utilization within a population can impose economic and social costs in countries at all economic levels. The effects of poor utilization are even worse on children who tend to suffer nutrition conditions as stunting, wasting and underweight. The situation is more apparent in countries that are experiencing political instability and terrorism like Nigeria. Access to sanitation facilities is still a problem as just about 30\\% of Africa's population enjoy these facilities."
if (region_to_report == "RAP") short_text <- "Utilization emphasizes the nutritional aspects of food security. It is commonly understood as the way the body makes the most of nutrients from food. Sufficient energy and nutrient intake includes nutritious and safe diets, a clean environment, access to health care, diversity of a diet and intra-household distribution of food. Poor utilization within a population can impose economic and social costs in countries at all economic levels."
if (region_to_report == "REU") short_text <- "Utilization emphasizes the nutritional aspects of food security. It is commonly understood as the way the body makes the most of nutrients from food. Sufficient energy and nutrient intake includes nutritious and safe diets, a clean environment, access to health care, diversity of a diet and intra-household distribution of food. Poor utilization within a population can impose economic and social costs in countries at all economic levels."
if (region_to_report == "RNE") short_text <- "Utilization emphasizes the nutritional aspects of food security. It is commonly understood as the way the body makes the most of nutrients from food. Sufficient energy and nutrient intake includes nutritious and safe diets, a clean environment, access to health care, diversity of a diet and intra-household distribution of food. Poor utilization within a population can impose economic and social costs in countries at all economic levels."
if (region_to_report == "GLO") short_text <- "Utilization emphasizes the nutritional aspects of food security. It is commonly understood as the way the body makes the most of nutrients from food. Sufficient energy and nutrient intake includes nutritious and safe diets, a clean environment, access to health care, diversity of a diet and intra-household distribution of food. Poor utilization within a population can impose economic and social costs in countries at all economic levels."
if (rulang) spread_title <- "Использование продовольствия"
if (region_to_report == "REU" & rulang) short_text <- "Использование делает акцент на питательные аспекты продовольственной безопасности. Под использованием продовольствия обычно понимается способность организма переваривать, усваивать и использовать питательные вещества из пищи. Потребление достаточного количества энергии и питательных веществ подразумевает питательный и безопасный рацион питания, чистоту окружающей среды, доступ к здравоохранению, разнообразие рациона питания и распределение потребления продуктов питания внутри домохозяйства. Плохое использование продовольствия среди населения может привести к экономическим и социальным издержкам во странах на всех уровнях экономического развития."

## ---- P2utilizaData ----
dat1 <- subset(temp, subset=Part %in% "P2utiliza")
dat1 <- subset(dat1, subset=Position %in% "TOPRIGHT")
dat1 <- subset(dat1, select = c(AreaName,Year,Indicator,Value))

## ---- P2utilizaTOPRIGHT ----
tbl <- arrange(dat1, -Value)[1:5,]

if (rulang){
  tbl <- tbl[c(1,4)]
  names(tbl) <- c("","год","%")
  print.xtable(xtable(tbl, caption = "\\large{Страны с самой высокой долей детей в возрасте до пяти лет, имеющих пониженную массу тела, в процентах}, в процентах", digits = c(0,0,0,1),
                      align= "l{\raggedright\arraybackslash}p{1.6cm}rr"),
               type = table_type, table.placement = NULL, booktabs = TRUE, include.rownames = FALSE, size = "footnotesize", caption.placement = "top",
               html.table.attributes = 'class="table table-striped table-hover"')
} else{
  tbl <- tbl[c(1,2,4)]
  names(tbl) <- c("","Year","%")
  print.xtable(xtable(tbl, caption = "\\large{Countries with highest share of children under 5 who are underweight}, percent", digits = c(0,0,0,1),
                      align= "l{\raggedright\arraybackslash}p{1.6cm}rr"),
               type = table_type, table.placement = NULL, booktabs = TRUE, include.rownames = FALSE, size = "footnotesize", caption.placement = "top",
               html.table.attributes = 'class="table table-striped table-hover"')
}



## ---- P2utilizaLEFT ----
dat1 <- subset(temp, subset=Part %in% "P2utiliza")
dat1 <- subset(dat1, subset=Position %in% "LEFT")
dat1 <- subset(dat1, select = c(AreaName,Year,Indicator,Value))
yr = dat1$Year[1]

dat_plot <- arrange(dat1, -Value) %>% dplyr::mutate(color = yr)
dat_plot$AreaName <- factor(dat_plot$AreaName, levels=arrange(dat1, Value)$AreaName)

p <- ggplot(dat_plot, aes(x=AreaName,y=Value))
p <- p + geom_segment(aes(y = min(dat_plot$Value), xend = AreaName, 
                          yend = Value, color=color), alpha=.5)
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75) + theme(panel.grid.major.y = element_blank())
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + theme(legend.position = "none")
p <- p + labs(x="",y="\npercent")
if (rulang) p <- p + labs(x="",y="\nпроценты")
p


# Caption
caption_text <- paste("Percentage of children under 5 who are stunted, highest ",nrow(dat1)," countries (",yr,"*)", sep = "")
if (rulang) caption_text <- paste("Процентная доля детей в возрасте до пяти лет, отстающих в росте, ",nrow(dat1)," стран с самыми высокими показателями (",yr," гг.*)", sep = "")

## ---- P2utilizaRIGHT ----
dat1 <- subset(temp, subset=Part %in% "P2utiliza")
dat1 <- subset(dat1, subset=Position %in% "RIGHT")
dat1 <- subset(dat1, select = c(AreaName,Year,Indicator,Value))
yr = dat1$Year[1]

dat_plot <- arrange(dat1, -Value) %>% dplyr::mutate(color = yr)
dat_plot$AreaName <- factor(dat_plot$AreaName, levels=arrange(dat1, Value)$AreaName)

p <- ggplot(dat_plot, aes(x=AreaName,y=Value))
p <- p + geom_segment(aes(y = min(dat_plot$Value), xend = AreaName, 
                          yend = Value, color=color), alpha=.5)
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75) + theme(panel.grid.major.y = element_blank())
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + theme(legend.position = "none")
p <- p + labs(x="",y="\npercent")
if (rulang) p <- p + labs(x="",y="\nпроценты")
p

# Caption
caption_text <- paste("Percentage of children under 5 affected by wasting, highest ",nrow(dat1)," countries (",yr,"*)", sep = "")
if (rulang) caption_text <- paste("Процентная доля детей до пяти лет, страдающих от истощения, ",nrow(dat1)," стран с самыми высокими показателями (",yr," гг.*)", sep = "")

## ---- P2utilizaBOTTOM ----
dat1 <- subset(temp, subset=Part %in% "P2utiliza")
dat1 <- subset(dat1, subset=Position %in% "BOTTOM")
dat1 <- subset(dat1, select = c(AreaName,Year,Indicator,Value))
dat1$Year <- as.integer(dat1$Year)

dat_plot <- dat1

p <- ggplot(dat_plot, aes(x=Year,y=Value,color=Indicator))
p <- p + geom_line(size=1.1, alpha=.7)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + labs(x=NULL,y="percent of population\n")
if (rulang) p <- p + labs(x="",y="процент населения\n")
if (region_to_report == "RAP") p <- p + scale_x_continuous(breaks=c(2008,2010,2012))
if (region_to_report != "RAP") p <- p + scale_x_continuous(breaks=c(2000,2005,2010,2015))
p

caption_text <- "Access to improved water source and sanitation facilities"
if (rulang) caption_text <- "Доступ к улучшенным источникам водоснабжения и санитарно-техническим сооружениям "

## ---- P2utilizaMAP ----

dat1 <- subset(temp, subset=Part %in% "P2utiliza")
dat1 <- subset(dat1, subset=Position %in% "MAP")
dat1 <- subset(dat1, select = c(AreaCode,Value,Year))
dat1$AreaCode <- as.integer(dat1$AreaCode)

map.plot <- left_join(map.df,dat1, by = c("FAOST_CODE" = "AreaCode")) # so that each country in the region will be filled (value/NA)

# Add region key and subset

map.plot <- map.plot[which(map.plot[[region_to_report]]),]

cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","Value")]
cat_data$value_cat <- categories(x=cat_data$Value, n=5) # manualBreaks = c(0, 5, 15, 25, 35, 100),

map.plot <- left_join(map.plot,cat_data[c("FAOST_CODE","value_cat")])

# define map unit
map_unit <- "percent"
if (rulang) map_unit <- "проценты"


p <- create_map_here()
p

# Caption
caption_text <- paste("Percentage of anemia among children under 5, percent (",dat1$Year[1],")", sep = "")
if (rulang) caption_text <- paste("Масштабы распространения анемии среди детей до пяти лет, в процентах (",dat1$Year[1]," г.)", sep = "")
