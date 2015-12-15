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
if (region_to_report == "RAF") short_text <- "Undernourishment is a state, lasting for at least one year, of inability to acquire enough food, defined as a level of food intake insufficient to meet dietary energy requirements. About 793 million people – just over one in every nine people – in the world still lack sufficient food for conducting an active and healthy life. Yet progress has been made, even in the presence of significant population growth. Two hundred and sixteen million million fewer people suffer from undernourishment than 25 years ago and 167 million fewer than a decade ago."
if (region_to_report == "RAP") short_text <- "Undernourishment is a state, lasting for at least one year, of inability to acquire enough food, defined as a level of food intake insufficient to meet dietary energy requirements. About 793 million people – just over one in every nine people – in the world still lack sufficient food for conducting an active and healthy life. Yet progress has been made, even in the presence of significant population growth. Two hundred and sixteen million million fewer people suffer from undernourishment than 25 years ago and 167 million fewer than a decade ago."
if (region_to_report == "REU") short_text <- "Undernourishment is a state, lasting for at least one year, of inability to acquire enough food, defined as a level of food intake insufficient to meet dietary energy requirements. About 793 million people – just over one in every nine people – in the world still lack sufficient food for conducting an active and healthy life. Yet progress has been made, even in the presence of significant population growth. Two hundred and sixteen million million fewer people suffer from undernourishment than 25 years ago and 167 million fewer than a decade ago."
if (region_to_report == "RNE") short_text <- "Undernourishment is a state, lasting for at least one year, of inability to acquire enough food, defined as a level of food intake insufficient to meet dietary energy requirements. About 793 million people – just over one in every nine people – in the world still lack sufficient food for conducting an active and healthy life. Yet progress has been made, even in the presence of significant population growth. Two hundred and sixteen million million fewer people suffer from undernourishment than 25 years ago and 167 million fewer than a decade ago."
if (region_to_report == "GLO") short_text <- "Undernourishment is a state, lasting for at least one year, of inability to acquire enough food, defined as a level of food intake insufficient to meet dietary energy requirements. About 793 million people – just over one in every nine people – in the world still lack sufficient food for conducting an active and healthy life. Yet progress has been made, even in the presence of significant population growth. Two hundred and sixteen million million fewer people suffer from undernourishment than 25 years ago and 167 million fewer than a decade ago."

## ---- P2undernuData ----

if (!file.exists(paste0(data.dir,"/fsi_data.RData"))){
  dat <- read.csv(paste0(data.dir,"/DisseminationDatasetRYB.csv"), stringsAsFactors=FALSE)

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
  dat$FAOST_CODE[dat$FAOST_CODE == "RAPDeveloped"] <- 13006  #	Australia New Zealand ??
  dat$FAOST_CODE[dat$FAOST_CODE == "5502"] <- 13008  #	Melanesia ??
  dat$FAOST_CODE[dat$FAOST_CODE == "5503"] <- 13009  #	Micronesia ??
  dat$FAOST_CODE[dat$FAOST_CODE == "5504"] <- 13010  #	Polynesia ??
  dat$FAOST_CODE[dat$FAOST_CODE == "5303"] <- 13012  #	Southern Asia
  dat$FAOST_CODE[dat$FAOST_CODE == "5856"] <- 13014  #	Western Asia ??
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
  dat <- merge(dat,tmp[c("FAOST_CODE","Area")],by="FAOST_CODE")
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

  aggregates_to_censore <- c( 13006,  #	Australia New Zealand ??
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


  save(dat, file=paste0(data.dir,"/fsi_data.RData"))
}

load(paste0(data.dir,"/fsi_data.RData"))

# M49LatinAmericaAndCaribbean
dat$Area[dat$FAOST_CODE == 5205] <- "M49macroReg"
# dat$FS.OA.NOU.P3D1[dat$FS.OA.NOU.P3D1 == "<0.1"] <- 0.01
# dat$FS.OA.NOU.P3D1[dat$FS.OA.NOU.P3D1 == "ns"] <- 0
dat$FS.OA.NOU.P3D1 <- as.factor(dat$FS.OA.NOU.P3D1)
dat$FS.OA.NOU.P3D1 <- as.numeric(levels(dat$FS.OA.NOU.P3D1))[dat$FS.OA.NOU.P3D1]
dat$FS.OA.POU.PCT3D1[dat$FS.OA.POU.PCT3D1 == "<5.0"] <- 0.1
dat$FS.OA.POU.PCT3D1 <- as.factor(dat$FS.OA.POU.PCT3D1)
dat$FS.OA.POU.PCT3D1 <- as.numeric(levels(dat$FS.OA.POU.PCT3D1))[dat$FS.OA.POU.PCT3D1]

df <- dat[!duplicated(dat[c("FAOST_CODE","Year")]),]



## ---- P2undernuTOPRIGHT ----

# This should be thought twice how to produce it for regional books!

dw <- df %>% filter(FAOST_CODE %in% c(5000,12000,13000,14000,15000),Year %in% c(1991,2015)) %>%
  mutate(Year = paste0("X",Year)) %>%
  select(Year,FAO_TABLE_NAME,FS.OA.POU.PCT3D1) %>%
  spread(key = Year,value = FS.OA.POU.PCT3D1)

dw$FAO_TABLE_NAME[dw$FAO_TABLE_NAME == "Latin America and the Caribbean"] <- "Latin America and \n the Caribbean"
dw$X2015[dw$X2015 == "20"] <- "20.0"
names(dw) <- c("","1990-92","2014-16")
tbl_data <- dw
#dw <- dw[c(7,3,4,1,2,5,6),]
# Chiaras comments
if (table_type == "latex") cap <- "\\large{Prevalence of undernourishment (percent)}"
if (table_type == "html")  cap <- "<b>Table: Prevalence of undernourishment (percent)</b>"
caption_text <- cap

print.xtable(xtable(dw, caption = cap, digits = c(0,0,0,0),
                    align= "l{\raggedright\arraybackslash}p{1.7cm}rr"),
             type = table_type, table.placement = NULL,
             comment=FALSE,
             booktabs = TRUE, include.rownames = FALSE, size = "footnotesize", caption.placement = "top",
             html.table.attributes = 'class="table table-striped table-hover"')



## ---- P2undernuLEFT ----
# data
dat <- df[df$Year %in%  c(1991,2015) & df$FAOST_CODE < 5000,c("FAOST_CODE","Year","FAO_TABLE_NAME","FS.OA.NOU.P3D1")]

dat <- dat[!is.na(dat$FS.OA.NOU.P3D1),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat <- dat[dat$FAOST_CODE != 357,]
dat <- dat[dat$FAOST_CODE != 41,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

#dat <- dat[which(dat[[region_to_report]]),]

dat <- arrange(dat, -Year, -FS.OA.NOU.P3D1)
top15 <- dat %>% slice(1:20) %>% dplyr::mutate(color = "2014-2016")
top91 <- dat %>% filter(FAOST_CODE %in% top15$FAOST_CODE, Year == 1991) %>% dplyr::mutate(color = "1990-1992")
dat_plot <- rbind(top15,top91)

# semi-standard data munging for two year dot-plots
# give name Value for value-col
names(dat)[names(dat)=="FS.OA.NOU.P3D1"] <- "Value"
# Plot only as many countries as there are for particular region, max 20
nro_latest_cases <- nrow(dat[dat$Year == max(dat$Year),])
if (nro_latest_cases < 20) {ncases <- nro_latest_cases} else ncases <- 20
dat <- arrange(dat, -Year, -Value)
# slice the data for both years
top2015 <- dat %>% slice(1:ncases) %>% dplyr::mutate(color = "2014-2016")
top2000 <- dat %>% filter(FAOST_CODE %in% top2015$FAOST_CODE, Year == 1991) %>% dplyr::mutate(color = "1990-1992")
dat_plot <- rbind(top2015,top2000)
# levels based on newest year
dat_plot$SHORT_NAME <- factor(dat_plot$SHORT_NAME, levels=arrange(top2015,Value)$SHORT_NAME)
###############

p <- ggplot(dat_plot, aes(x=SHORT_NAME,y=Value))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="million people")
p <- p + guides(color = guide_legend(nrow = 2))
p

caption_text <- "Countries with the highest number of undernourished in 2014-16"


## ---- P2undernuRIGHT ----

dat <- df[df$Year %in%  c(1991,2015) & df$FAOST_CODE < 5000,c("FAOST_CODE","Year","FAO_TABLE_NAME","FS.OA.NOU.P3D1")]

dat <- dat[!is.na(dat$FS.OA.NOU.P3D1),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

dat <- dat[which(dat[[region_to_report]]),]

dat <- arrange(dat, -Year, -FS.OA.NOU.P3D1)

# semi-standard data munging for two year dot-plots
# give name Value for value-col
names(dat)[names(dat)=="FS.OA.NOU.P3D1"] <- "Value"
# Plot only as many countries as there are for particular region, max 20
nro_latest_cases <- nrow(dat[dat$Year == max(dat$Year),])
if (nro_latest_cases < 20) {ncases <- nro_latest_cases} else ncases <- 20
dat <- arrange(dat, -Year, -Value)
# slice the data for both years
top2015 <- dat %>% slice(1:ncases) %>% dplyr::mutate(color = "2014-2016")
top2000 <- dat %>% filter(FAOST_CODE %in% top2015$FAOST_CODE, Year == 1991) %>% dplyr::mutate(color = "1990-1992")
dat_plot <- rbind(top2015,top2000)
# levels based on newest year
dat_plot$SHORT_NAME <- factor(dat_plot$SHORT_NAME, levels=arrange(top2015,Value)$SHORT_NAME)
###############

p <- ggplot(dat_plot, aes(x=SHORT_NAME,y=Value))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="million people")
p <- p + guides(color = guide_legend(nrow = 2))
p

# Caption
caption_text <- "Countries with the highest number of undernourished in 2014-16"
if (region_to_report == "RAF") caption_text <- "African countries with the highest number of undernourished in 2014-16"
if (region_to_report == "RAP") caption_text <- "Asian and the Pacific countries with the highest number of undernourished in 2014-16"
if (region_to_report == "REU") caption_text <- "European countries with the highest number of undernourished in 2014-16"
if (region_to_report == "RNE") caption_text <- "North Aftican and Near East countries with the highest number of undernourished in 2014-16"
if (region_to_report == "GLO") caption_text <- "Countries with the highest number of undernourished in 2014-16"


## ---- P2undernuBOTTOM ----

dat <- df[df$Year %in%  c(1991:2015) & df$FAOST_CODE < 5000,c("FAOST_CODE","Year","FAO_TABLE_NAME","FS.OA.POU.PCT3D1")]

dat <- dat[!is.na(dat$FS.OA.POU.PCT3D1),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

dat <- dat[which(dat[[region_to_report]]),]

dat <- arrange(dat, -Year, -FS.OA.POU.PCT3D1)
top5_FAOST_CODE <- head(dat$FAOST_CODE, 5)
dat_plot <- dat %>%  filter(FAOST_CODE %in% top5_FAOST_CODE)

p <- ggplot(dat_plot, aes(x=Year,y=FS.OA.POU.PCT3D1,color=SHORT_NAME))
p <- p + geom_line(size=1.1, alpha=.7)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 5)[["Sub"]])
p <- p + labs(x="",y="percent")
p <- p + scale_x_continuous(breaks = c(1991, 2000, 2005, 2010, 2015),
                            labels = c("1990-92", "1999-2001", "2004-06", "2009-11", "2014-16"))
p

# Caption
caption_text <- "Prevalence of undernourishment, top 5 countries"


## ---- P2undernuMAP ----
# dat <- syb.df %>% filter(Year %in% 2014) %>% select(FAOST_CODE,SHORT_NAME,OA.TPR.POP.PPL.SHP)

dat <- df[df$Year %in%  c(1991:2015) & df$FAOST_CODE < 5000,c("Year","FAOST_CODE","FS.OA.POU.PCT3D1")]

#dat <- dat[!is.na(dat$FS.OA.POU.PCT3D1),]

map.plot <- left_join(map.df,dat)

# Add region key and subset
map.plot <- map.plot[which(map.plot[[region_to_report]]),]

cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","FS.OA.POU.PCT3D1")]
cat_data$value_cat <- categories(x=cat_data$FS.OA.POU.PCT3D1, n=5, manual = TRUE, manual_breaks = c(0, 5, 15, 25, 35, 100), method="sd") # manualBreaks = c(0, 5, 15, 25, 35, 100),

map.plot <- left_join(map.plot,cat_data[c("FAOST_CODE","value_cat")])

# define map unit
map_unit <- "Percent"


p <- create_map_here()
p

# Caption
caption_text <- "Prevalence of undernourishment (percent, 2014-16)"





 #
 #   ___  _               _ _
 #  / _ \| |__   ___  ___(_) |_ _   _
 # | | | | '_ \ / _ \/ __| | __| | | |
 # | |_| | |_) |  __/\__ \ | |_| |_| |
 #  \___/|_.__/ \___||___/_|\__|\__, |
 #                              |___/


## ---- P2obesityTEXT ----
spread_title <- "Obesity/overweight"
if (region_to_report == "RAF") short_text <- "Overweight and obesity are defined as abnormal or excessive fat accumulation that may impair health. These phenomena are measured using BMI, with overweight greater than 25 and obesity higher than 30. A high body mass index is recognized as increasing the likelihood of incurring various non-communicable diseases and health problems, including cardiovascular disease, diabetes, various cancers and osteoarthritis. The global prevalence of overweight and obesity has risen in all regions and is also increasing in nearly all countries."
if (region_to_report == "RAP") short_text <- "Overweight and obesity are defined as abnormal or excessive fat accumulation that may impair health. These phenomena are measured using BMI, with overweight greater than 25 and obesity higher than 30. A high body mass index is recognized as increasing the likelihood of incurring various non-communicable diseases and health problems, including cardiovascular disease, diabetes, various cancers and osteoarthritis. The global prevalence of overweight and obesity has risen in all regions and is also increasing in nearly all countries."
if (region_to_report == "REU") short_text <- "Overweight and obesity are defined as abnormal or excessive fat accumulation that may impair health. These phenomena are measured using BMI, with overweight greater than 25 and obesity higher than 30. A high body mass index is recognized as increasing the likelihood of incurring various non-communicable diseases and health problems, including cardiovascular disease, diabetes, various cancers and osteoarthritis. The global prevalence of overweight and obesity has risen in all regions and is also increasing in nearly all countries."
if (region_to_report == "RNE") short_text <- "Overweight and obesity are defined as abnormal or excessive fat accumulation that may impair health. These phenomena are measured using BMI, with overweight greater than 25 and obesity higher than 30. A high body mass index is recognized as increasing the likelihood of incurring various non-communicable diseases and health problems, including cardiovascular disease, diabetes, various cancers and osteoarthritis. The global prevalence of overweight and obesity has risen in all regions and is also increasing in nearly all countries."
if (region_to_report == "GLO") short_text <- "Overweight and obesity are defined as abnormal or excessive fat accumulation that may impair health. These phenomena are measured using BMI, with overweight greater than 25 and obesity higher than 30. A high body mass index is recognized as increasing the likelihood of incurring various non-communicable diseases and health problems, including cardiovascular disease, diabetes, various cancers and osteoarthritis. The global prevalence of overweight and obesity has risen in all regions and is also increasing in nearly all countries."

## ---- P2obesityTOPRIGHT
dat <- read.csv(paste0(data.dir,"/FSI2015_disseminationData_A_11.csv"), stringsAsFactors=FALSE)
over_acq <- gather(dat, Year_range, value, 3:27)
over_acq$Year_range <- as.character(over_acq$Year_range)

over_acq$Year[over_acq$Year_range == "X1990.92"] <- 1991
over_acq$Year[over_acq$Year_range == "X1991.93"] <- 1992
over_acq$Year[over_acq$Year_range == "X1992.94"] <- 1993
over_acq$Year[over_acq$Year_range == "X1993.95"] <- 1994
over_acq$Year[over_acq$Year_range == "X1994.96"] <- 1995
over_acq$Year[over_acq$Year_range == "X1995.97"] <- 1996
over_acq$Year[over_acq$Year_range == "X1996.98"] <- 1997
over_acq$Year[over_acq$Year_range == "X1997.99"] <- 1998
over_acq$Year[over_acq$Year_range == "X1998.00"] <- 1999
over_acq$Year[over_acq$Year_range == "X1999.01"] <- 2000
over_acq$Year[over_acq$Year_range == "X2000.02"] <- 2001
over_acq$Year[over_acq$Year_range == "X2001.03"] <- 2002
over_acq$Year[over_acq$Year_range == "X2002.04"] <- 2003
over_acq$Year[over_acq$Year_range == "X2003.05"] <- 2004
over_acq$Year[over_acq$Year_range == "X2004.06"] <- 2005
over_acq$Year[over_acq$Year_range == "X2005.07"] <- 2006
over_acq$Year[over_acq$Year_range == "X2006.08"] <- 2007
over_acq$Year[over_acq$Year_range == "X2007.09"] <- 2008
over_acq$Year[over_acq$Year_range == "X2008.10"] <- 2009
over_acq$Year[over_acq$Year_range == "X2009.11"] <- 2010
over_acq$Year[over_acq$Year_range == "X2010.12"] <- 2011
over_acq$Year[over_acq$Year_range == "X2011.13"] <- 2012
over_acq$Year[over_acq$Year_range == "X2012.14."] <- 2013
over_acq$Year[over_acq$Year_range == "X2013.15."] <- 2014
over_acq$Year[over_acq$Year_range == "X2014.16."] <- 2015

over_acq$Year <- factor(over_acq$Year)
over_acq$Year <- as.numeric(levels(over_acq$Year))[over_acq$Year]

names(over_acq) <- c("FAOST_CODE","FAO_TABLE_NAME","Year_range","value","Year")

dat <- over_acq %>% filter(FAOST_CODE %in% c(5001,5851,5852), Year %in% c(1992,2015)) %>%  select(FAOST_CODE,FAO_TABLE_NAME,Year,value) %>% 
    dplyr::rename(SHORT_NAME = FAO_TABLE_NAME)

dat$fill[dat$Year == 1992] <- "1991-93"
dat$fill[dat$Year == 2015] <- "2014-16"

dat_plot <- dat
# reorder
# dat_plot$SHORT_NAME <- factor(dat_plot$SHORT_NAME, levels=(dat_plot %>% filter(fill == "2014-16") %>% arrange(-value))$SHORT_NAME)
dat_plot$SHORT_NAME <- factor(dat_plot$SHORT_NAME, levels=c("Developed countries","Developing countries","World"))

p <- ggplot(dat_plot, aes(x=SHORT_NAME, y=value, fill=fill))
p <- p + geom_bar(stat="identity", position="dodge")
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + labs(x="",y="percent")
p <- p + theme(axis.text.x = element_text(angle=45))
p

# Caption
caption_text <- "Prevalence of over-acquisition (1991-93 and 2014-16)"
# caption_text <- "text"

## ---- P2obesityLEFT ----
dat <- syb.df[syb.df$Year %in%  2005:2013 ,c("FAOST_CODE","Year","SHORT_NAME","SH.STA.OWGH.MA.ZS")]

dat <- dat[!is.na(dat$SH.STA.OWGH.MA.ZS),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

#and subset
dat <- dat[which(dat[[region_to_report]]),]

dat_plot <- dat %>% group_by(SHORT_NAME) %>% dplyr::filter(Year == max(Year)) %>% ungroup() %>% arrange(-SH.STA.OWGH.MA.ZS) %>% slice(1:20) %>% dplyr::mutate(color = "2013")

p <- ggplot(dat_plot, aes(x=reorder(SHORT_NAME, SH.STA.OWGH.MA.ZS),y=SH.STA.OWGH.MA.ZS))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 1)[["Sub"]])
p <- p + theme(legend.position = "none") # hide legend as only one year plotted
p <- p + coord_flip()
p <- p + labs(x="",y="percent")
p <- p + guides(color = guide_legend(nrow = 2))
p

# Caption
caption_text <- "Prevalence of overweigth among children under 5, countries with the highest values, male (percent 2005-2013*)"


## ---- P2obesityRIGHT ----
dat <- syb.df[syb.df$Year %in%  2003:2013 ,c("FAOST_CODE","Year","SHORT_NAME","SH.STA.OWGH.FE.ZS")]

dat <- dat[!is.na(dat$SH.STA.OWGH.FE.ZS),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

#and subset
dat <- dat[which(dat[[region_to_report]]),]

dat_plot <- dat %>% group_by(SHORT_NAME) %>% dplyr::filter(Year == max(Year)) %>% ungroup() %>% arrange(-SH.STA.OWGH.FE.ZS) %>% slice(1:20) %>% dplyr::mutate(color = "2013")


p <- ggplot(dat_plot, aes(x=reorder(SHORT_NAME, SH.STA.OWGH.FE.ZS),y=SH.STA.OWGH.FE.ZS))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 1)[["Sub"]])
p <- p + theme(legend.position = "none") # hide legend as only one year plotted
p <- p + coord_flip()
p <- p + labs(x="",y="percent")
p <- p + guides(color = guide_legend(nrow = 2))
p

# Caption
caption_text <- "Prevalence of overweigth among children under 5, countries with the highest values, female (percent 2005-2013*)"
# caption_text <- "text"
# 
## ---- P2obesityBOTTOM ----
dat <- over_acq %>% filter(FAOST_CODE %in% c(5001,5100,5853,5500,5205), Year >= 1990) %>%  select(FAOST_CODE,FAO_TABLE_NAME,Year,value) %>% 
  dplyr::rename(SHORT_NAME = FAO_TABLE_NAME)


dat_plot <- dat

p <- ggplot(data = dat_plot, aes(x = Year, y = value,group=SHORT_NAME,color=SHORT_NAME))
p <- p + geom_line(size=1.1, alpha=.7)
p <- p + scale_color_manual(values = plot_colors(part = 1, length(unique(dat_plot$SHORT_NAME)))[["Sub"]])
p <- p + labs(y="percent", x="")
p <- p + guides(color = guide_legend(nrow = 2))
p <- p + scale_x_continuous(breaks = c(1991, 2001, 2006, 2010,2015),
                            labels = c("1990-92", "2000-02", "2005-07", "2009-11","2014-16"))
p

# Caption
caption_text <- "Prevalence of over-acquisition (1990-92 to 2014-16)"
# caption_text <- "text"

## ---- P2obesityMAP ----
dat <- syb.df %>% filter(Year == 2014) %>%
                select(Year,FAOST_CODE,SHORT_NAME,overweight_BOTH)
dat <- dat[!is.na(dat$overweight_BOTH),]
dat <- dat %>% group_by(FAOST_CODE) %>% filter(Year == max(Year))


map.plot <- left_join(map.df,dat) # so that each country in the region will be filled (value/NA)

# Add region key and subset

map.plot <- map.plot[which(map.plot[[region_to_report]]),]

cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","overweight_BOTH")]
cat_data$value_cat <- categories(x=cat_data$overweight_BOTH, n=5, method="jenks")

map.plot <- left_join(map.plot,cat_data[c("FAOST_CODE","value_cat")])

# define map unit
map_unit <- "Percent"

p <- create_map_here()
p

# Caption
caption_text <- "Prevalence of overweight and obesity, adults (percent, 2014)"



#   _____                       _                             _   _           _       _   _   _   _
#  |  ___|   ___     ___     __| |     __ _  __   __   __ _  (_) | |   __ _  | |__   (_) | | (_) | |_   _   _
#  |  _|   | (_) | | (_) | | (_| |   | (_| |  \ V /  | (_| | | | | | | (_| | | |_) | | | | | | | | |_  | |_| |
#  | |_     / _ \   / _ \   / _` |    / _` | \ \ / /  / _` | | | | |  / _` | | '_ \  | | | | | | | __| | | | |
#  |_|      \___/   \___/   \__,_|    \__,_|   \_/    \__,_| |_| |_|  \__,_| |_.__/  |_| |_| |_|  \__|  \__, |
#                                                                                                       |___/

## ---- P2availabTEXT ----
spread_title <- "Food availability"
if (region_to_report == "RAF") short_text <- "Availability is an important dimension of food security. Supplying enough food to the reference population is a necessary, but insufficient, condition for ensuring adequate access for individuals. Over recent decades, trends in food production per capita have been generally positive across most regions. However, growth rates in Africa have been lower for the last 20 years, despite notable exceptions. In most countries and regions, high food availability is associated with relatively low prevalence of undernourishment. However, outcome indicators show that high food availability does not always guarantee high food security."
if (region_to_report == "RAP") short_text <- "Availability is an important dimension of food security. Supplying enough food to the reference population is a necessary, but insufficient, condition for ensuring adequate access for individuals. Over recent decades, trends in food production per capita have been generally positive across most regions. However, growth rates in Africa have been lower for the last 20 years, despite notable exceptions. In most countries and regions, high food availability is associated with relatively low prevalence of undernourishment. However, outcome indicators show that high food availability does not always guarantee high food security."
if (region_to_report == "REU") short_text <- "Availability is an important dimension of food security. Supplying enough food to the reference population is a necessary, but insufficient, condition for ensuring adequate access for individuals. Over recent decades, trends in food production per capita have been generally positive across most regions. However, growth rates in Africa have been lower for the last 20 years, despite notable exceptions. In most countries and regions, high food availability is associated with relatively low prevalence of undernourishment. However, outcome indicators show that high food availability does not always guarantee high food security."
if (region_to_report == "RNE") short_text <- "Availability is an important dimension of food security. Supplying enough food to the reference population is a necessary, but insufficient, condition for ensuring adequate access for individuals. Over recent decades, trends in food production per capita have been generally positive across most regions. However, growth rates in Africa have been lower for the last 20 years, despite notable exceptions. In most countries and regions, high food availability is associated with relatively low prevalence of undernourishment. However, outcome indicators show that high food availability does not always guarantee high food security."
if (region_to_report == "GLO") short_text <- "Availability is an important dimension of food security. Supplying enough food to the reference population is a necessary, but insufficient, condition for ensuring adequate access for individuals. Over recent decades, trends in food production per capita have been generally positive across most regions. However, growth rates in Africa have been lower for the last 20 years, despite notable exceptions. In most countries and regions, high food availability is associated with relatively low prevalence of undernourishment. However, outcome indicators show that high food availability does not always guarantee high food security."

## ---- P2availabData ----



## ---- P2availabTOPRIGHT ----
dat_plot <- df %>% filter(FAOST_CODE %in% c(5000,12000,13000,14000,15000)) %>%  select(FAOST_CODE,Year,FAO_TABLE_NAME,FS.DA.ADESA.PCT3D)

dat_plot$FAO_TABLE_NAME <- factor(dat_plot$FAO_TABLE_NAME, levels=c("Near East and North Africa",
                                                                    "Europe and Central Asia",
                                                                    "Asia and the Pacific",
                                                                    "Africa",
                                                                    "World"))

p <- ggplot(data = dat_plot, aes(x = Year, y = FS.DA.ADESA.PCT3D,group=FAO_TABLE_NAME,color=FAO_TABLE_NAME))
p <- p + geom_line(size=1.1, alpha=.7)
p <- p + scale_color_manual(values = plot_colors(part = 1, length(unique(dat_plot$FAO_TABLE_NAME)))[["Sub"]])
p <- p + labs(y="percent", x="")
p <- p + guides(color = guide_legend(nrow = 3))
p <- p + scale_x_continuous(breaks = c(1991, 2001, 2006, 2013, 2015),
                            labels = c("1990-92", "2000-02", "2005-07", "2012-14", "2014-16"))
p <- p + theme(axis.text.x = element_text(angle = 45))
p


# Caption
caption_text <- "Average dietary energy supply adequacy, 3 year average (1990 to 2015)"


## ---- P2availabLEFT ----

dat <- df[df$Year %in%  c(2000,2010) & df$FAOST_CODE < 5000,c("FAOST_CODE","Year","FAO_TABLE_NAME","FBS.PCSS.CSR.PCT3D")]

dat <- dat[!is.na(dat$FBS.PCSS.CSR.PCT3D),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

dat <- dat[which(dat[[region_to_report]]),]

# semi-standard data munging for two year dot-plots
# give name Value for value-col
names(dat)[names(dat)=="FBS.PCSS.CSR.PCT3D"] <- "Value"
# Plot only as many countries as there are for particular region, max 20
nro_latest_cases <- nrow(dat[dat$Year == max(dat$Year),])
if (nro_latest_cases < 20) {ncases <- nro_latest_cases} else ncases <- 20
dat <- arrange(dat, -Year, -Value)
# slice the data for both years
top2015 <- dat %>% slice(1:ncases) %>% dplyr::mutate(color = "2009-2011")
top2000 <- dat %>% filter(FAOST_CODE %in% top2015$FAOST_CODE, Year == 2000) %>% dplyr::mutate(color = "1999-2001")
dat_plot <- rbind(top2015,top2000)
# levels based on newest year
dat_plot$SHORT_NAME <- factor(dat_plot$SHORT_NAME, levels=arrange(top2015,Value)$SHORT_NAME)
###############

p <- ggplot(dat_plot, aes(x=SHORT_NAME,y=Value))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="percent")
p <- p + guides(color = guide_legend(nrow = 2))
p

# Caption
caption_text <- "Energy supply derived from cereals, roots and tubers, top 20 countries in 2009-2011"
if (region_to_report == "RAF") caption_text <- "Energy supply derived from cereals, roots and tubers, top 20 African countries in 2009-2011"
if (region_to_report == "RAP") caption_text <- "Energy supply derived from cereals, roots and tubers, top 20 Asian countries in 2009-2011"
if (region_to_report == "REU") caption_text <- "Energy supply derived from cereals, roots and tubers, top 6 European countries in 2009-2011"
if (region_to_report == "RNE") caption_text <- "Energy supply derived from cereals, roots and tubers, top 6 North African countries in 2009-2011"
if (region_to_report == "GLO") caption_text <- "Energy supply derived from cereals, roots and tubers, top 20 countries in 2009-2011"


## ---- P2availabRIGHT ----


dat <- df[df$Year %in%  c(2010) & df$FAOST_CODE < 5000,c("FAOST_CODE","Year","FAO_TABLE_NAME","FBS.PPCS.GT.GCD3D")]

dat <- dat[!is.na(dat$FBS.PPCS.GT.GCD3D),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

dat <- dat[which(dat[[region_to_report]]),]

dat <- arrange(dat, -Year, -FBS.PPCS.GT.GCD3D)

# limit the nro of printed for REU/RNE countries
if (nrow(dat) < 20){
  max_nro_countries <- nrow(dat)
} else max_nro_countries <- 20


top15 <- dat %>% slice(1:max_nro_countries) %>% dplyr::mutate(color = "2009-2011")
# top91 <- dat %>% filter(FAOST_CODE %in% top15$FAOST_CODE, Year == 2000) %>% dplyr::mutate(color = "1999-2001")
# dat_plot <- rbind(top15,top91)
dat_plot <- top15

dat_plot$SHORT_NAME <- factor(dat_plot$SHORT_NAME, levels=arrange(top15, FBS.PPCS.GT.GCD3D)$SHORT_NAME)

p <- ggplot(dat_plot, aes(x=SHORT_NAME,y=FBS.PPCS.GT.GCD3D))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="g/cap/day")
#p <- p + guides(color = guide_legend(nrow = 2))
p <- p + theme(legend.position = "none")
p

# Caption
caption_text <- "Average protein supply, top 20 countries in 2009-2011"
if (region_to_report == "RAF") caption_text <- "Average protein supply, top 20 African countries in 2009-2011"
if (region_to_report == "RAP") caption_text <- "Average protein supply, top 20 Asian countries in 2009-2011"
if (region_to_report == "REU") caption_text <- "Average protein supply, top 6 European countries in 2009-2011"
if (region_to_report == "RNE") caption_text <- "Average protein supply, top 6 North African countries in 2009-2011"
if (region_to_report == "GLO") caption_text <- "Average protein supply, top 20 countries in 2009-2011"



## ---- P2availabBOTTOM ----

dat_plot <- df %>% filter(FAOST_CODE %in% c(5000,12000,13000,14000,15000)) %>%  select(FAOST_CODE,Year,FAO_TABLE_NAME,FBS.PPCS.AO.GCD3D)

dat_plot$FAO_TABLE_NAME <- factor(dat_plot$FAO_TABLE_NAME, levels=c("Near East and North Africa",
                                                                    "Europe and Central Asia",
                                                                    "Asia and the Pacific",
                                                                    "Africa",
                                                                    "World"))

dat_plot <- dat_plot[!is.na(dat_plot$FBS.PPCS.AO.GCD3D),]
dat_plot$FAO_TABLE_NAME[dat_plot$FAO_TABLE_NAME == "Latin America and the Caribbean"] <- "Latin Am. and the Carib."

p <- ggplot(data = dat_plot, aes(x = Year, y = FBS.PPCS.AO.GCD3D,group=FAO_TABLE_NAME,color=FAO_TABLE_NAME))
p <- p + geom_line(size=1.1, alpha=.7)
p <- p + scale_color_manual(values = plot_colors(part = 1, length(unique(dat_plot$FAO_TABLE_NAME)))[["Sub"]])
p <- p + labs(y="g/cap/day", x="")
p <- p + guides(color = guide_legend(nrow = 2))
p <- p + scale_x_continuous(breaks = c(1991, 2001, 2006, 2010),
                            labels = c("1990-92", "2000-02", "2005-07", "2009-11"))
p <- p + theme(axis.text.x = element_text(angle = 45))
p

# Caption
caption_text <- "Average supply of protein of animal origin"


## ---- P2availabMAP ----

dat <- df[df$Year %in%  2012 & df$FAOST_CODE < 5000,c("Year","FAOST_CODE","QV.PCNPV.FOOD.ID3D")]

map.plot <- left_join(map.df,dat)

# Add region key and subset
map.plot <- map.plot[which(map.plot[[region_to_report]]),]

cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","QV.PCNPV.FOOD.ID3D")]
cat_data$value_cat <- categories(x=cat_data$QV.PCNPV.FOOD.ID3D, n=5) # manualBreaks = c(0, 5, 15, 25, 35, 100),

map.plot <- left_join(map.plot,cat_data[c("FAOST_CODE","value_cat")])

# define map unit
map_unit <- "Percent"


p <- create_map_here()
p

# Caption
caption_text <- "Average value of food production, constant 2004-2006 I\\$ per person (3 year average, 2011-13)"


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


## ---- P2accessData ----



## ---- P2accessTOPRIGHT ----
dat_plot <- df %>% filter(FAOST_CODE %in% c(5000,12000,13000,14000,15000)) %>%  select(FAOST_CODE,Year,FAO_TABLE_NAME,FS.OA.DOFD.KCD3D)

dat_plot$FAO_TABLE_NAME <- factor(dat_plot$FAO_TABLE_NAME, levels=c("Near East and North Africa",
                                                                    "Europe and Central Asia",
                                                                    "Asia and the Pacific",
                                                                    "Africa",
                                                                    "World"))

dat_plot$FAO_TABLE_NAME[dat_plot$FAO_TABLE_NAME == "Latin America and the Caribbean"] <- "Latin Am. and the Carib."

p <- ggplot(data = dat_plot, aes(x = Year, y = FS.OA.DOFD.KCD3D,group=FAO_TABLE_NAME,color=FAO_TABLE_NAME))
p <- p + geom_line(size=1.1, alpha=.7)
p <- p + scale_color_manual(values = plot_colors(part = 1, length(unique(dat_plot$FAO_TABLE_NAME)))[["Sub"]])
p <- p + labs(y="kcal/cap/day", x="")
p <- p + guides(color = guide_legend(nrow = 3))
p <- p + scale_x_continuous(breaks = c(1991, 2001, 2006, 2013, 2015),
                            labels = c("1990-92", "2000-02", "2005-07", "2012-14", "2014-16"))
p <- p + theme(axis.text.x = element_text(angle = 45))
p


# Caption
caption_text <- "Depth of food decifit (kcal/capita/day) (3 year averages)"



## ---- P2accessLEFT ----


dat <- df[df$Year %in%  c(2000,2014) & df$FAOST_CODE < 5000,c("FAOST_CODE","Year","FAO_TABLE_NAME","FS.DEA.DFPLI.IND")]

dat <- dat[!is.na(dat$FS.DEA.DFPLI.IND),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

dat <- dat[which(dat[[region_to_report]]),]

# semi-standard data munging for two year dot-plots
# give name Value for value-col
names(dat)[names(dat)=="FS.DEA.DFPLI.IND"] <- "Value"
# Plot only as many countries as there are for particular region, max 20
nro_latest_cases <- nrow(dat[dat$Year == max(dat$Year),])
if (nro_latest_cases < 20) {ncases <- nro_latest_cases} else ncases <- 20
dat <- arrange(dat, -Year, -Value)
# slice the data for both years
top2015 <- dat %>% slice(1:ncases) %>% dplyr::mutate(color = "2014")
top2000 <- dat %>% filter(FAOST_CODE %in% top2015$FAOST_CODE, Year == 2000) %>% dplyr::mutate(color = "2000")
dat_plot <- rbind(top2015,top2000)
# levels based on newest year
dat_plot$SHORT_NAME <- factor(dat_plot$SHORT_NAME, levels=arrange(top2015,Value)$SHORT_NAME)
###############

p <- ggplot(dat_plot, aes(x=SHORT_NAME,y=Value))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="percent")
p <- p + guides(color = guide_legend(nrow = 1))
p

# Caption
caption_text <- "Domestic food price level index, top 20 countries in 2014 (2000 to 2014)"
if (region_to_report == "RAF") caption_text <- "Domestic food price level index, top 20 countries in 2014 (2000 to 2014)"
if (region_to_report == "RAP") caption_text <- "Domestic food price level index, top 20 countries in 2014 (2000 to 2014)"
if (region_to_report == "REU") caption_text <- "Domestic food price level index, top 20 countries in 2014 (2000 to 2014)"
if (region_to_report == "RNE") caption_text <- "Domestic food price level index, top 20 countries in 2014 (2000 to 2014)"
if (region_to_report == "GLO") caption_text <- "Domestic food price level index, top 20 countries in 2014 (2000 to 2014)"



## ---- P2accessRIGHT ----

dat <- df[df$Year %in%  c(2000,2015) & df$FAOST_CODE < 5000,c("FAOST_CODE","Year","FAO_TABLE_NAME","FS.OA.POU.PCT3D1")]

dat <- dat[!is.na(dat$FS.OA.POU.PCT3D1),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

dat <- dat[which(dat[[region_to_report]]),]

# semi-standard data munging for two year dot-plots
# give name Value for value-col
names(dat)[names(dat)=="FS.OA.POU.PCT3D1"] <- "Value"
# Plot only as many countries as there are for particular region, max 20
nro_latest_cases <- nrow(dat[dat$Year == max(dat$Year),])
if (nro_latest_cases < 20) {ncases <- nro_latest_cases} else ncases <- 20
dat <- arrange(dat, -Year, -Value)
# slice the data for both years
top2015 <- dat %>% slice(1:ncases) %>% dplyr::mutate(color = "2014-2016")
top2000 <- dat %>% filter(FAOST_CODE %in% top2015$FAOST_CODE, Year == 2000) %>% dplyr::mutate(color = "1999-2001")
dat_plot <- rbind(top2015,top2000)
# levels based on newest year
dat_plot$SHORT_NAME <- factor(dat_plot$SHORT_NAME, levels=arrange(top2015,Value)$SHORT_NAME)
###############

p <- ggplot(dat_plot, aes(x=SHORT_NAME,y=Value))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="percent")
p <- p + guides(color = guide_legend(nrow = 2))
p


# Caption
caption_text <- "Prevalence of undernourishment, highest 20 countries in 2014-16 (3 year averages)"
if (region_to_report == "RAF") caption_text <- "Prevalence of undernourishment, highest 20 countries in 2014-16 (3 year averages)"
if (region_to_report == "RAP") caption_text <- "Prevalence of undernourishment, highest 20 countries in 2014-16 (3 year averages)"
if (region_to_report == "REU") caption_text <- "Prevalence of undernourishment, highest 20 countries in 2014-16 (3 year averages)"
if (region_to_report == "RNE") caption_text <- "Prevalence of undernourishment, highest 20 countries in 2014-16 (3 year averages)"
if (region_to_report == "GLO") caption_text <- "Prevalence of undernourishment, highest 20 countries in 2014-16 (3 year averages)"


## ---- P2accessBOTTOM ----

dat_plot <- df %>% filter(FAOST_CODE %in% c(5000,12000,13000,14000,15000)) %>%  select(FAOST_CODE,Year,FAO_TABLE_NAME,NY.GDP.PCAP.PP.KD)

dat_plot$FAO_TABLE_NAME <- factor(dat_plot$FAO_TABLE_NAME, levels=c("Near East and North Africa",
                                                                    "Europe and Central Asia",
                                                                    "Asia and the Pacific",
                                                                    "Africa",
                                                                    "World"))

dat_plot$FAO_TABLE_NAME[dat_plot$FAO_TABLE_NAME == "Latin America and the Caribbean"] <- "Latin Am. and the Carib."

p <- ggplot(data = dat_plot, aes(x = Year, y = NY.GDP.PCAP.PP.KD,group=FAO_TABLE_NAME,color=FAO_TABLE_NAME))
p <- p + geom_line(size=1.1, alpha=.7)
p <- p + scale_color_manual(values = plot_colors(part = 1, length(unique(dat_plot$FAO_TABLE_NAME)))[["Sub"]])
p <- p + labs(y="US$", x="")
p <- p + guides(color = guide_legend(nrow = 2))
p <- p + scale_y_continuous(labels=space)
p

# Caption
caption_text <- "GDP per capita, PPP, constant 2011 international \\$"



## ---- P2accessMAP ----
dat <- df[df$Year %in%  2007:2011 & df$FAOST_CODE < 5000,c("Year","FAOST_CODE","IS.ROD.DNST.K2D")]


dat <- dat[!is.na(dat$IS.ROD.DNST.K2D),]
dat <- dat %>% group_by(FAOST_CODE) %>% filter(Year == max(Year))

map.plot <- left_join(map.df,dat)

# Add region key and subset
map.plot <- map.plot[which(map.plot[[region_to_report]]),]

cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","IS.ROD.DNST.K2D")]
cat_data$value_cat <- categories(x=cat_data$IS.ROD.DNST.K2D, n=5) # manualBreaks = c(0, 5, 15, 25, 35, 100),

map.plot <- left_join(map.plot,cat_data[c("FAOST_CODE","value_cat")])

# define map unit
map_unit <- "per 100 km² of land"


p <- create_map_here()
p

# Caption
caption_text <- "Road density, per 100 km\\textsuperscript{2} of land area (2007 to 2011*)"


#   _____                       _           _             _       _   _   _   _
#  |  ___|   ___     ___     __| |    ___  | |_    __ _  | |__   (_) | | (_) | |_   _   _
#  | |_     / _ \   / _ \   / _` |   / __| | __|  / _` | | '_ \  | | | | | | | __| | | | |
#  |  _|   | (_) | | (_) | | (_| |   \__ \ | |_  | (_| | | |_) | | | | | | | | |_  | |_| |
#  |_|      \___/   \___/   \__,_|   |___/  \__|  \__,_| |_.__/  |_| |_| |_|  \__|  \__, |
#                                                                                   |___/

## ---- P2stabilityTEXT ----
spread_title <- "Economic and political stability"
if (region_to_report == "RAF") short_text <- "Over the last ten years, food and agricultural markets have entered an unexpectedly turbulent phase, characterized by large supply shortfalls, price swings. Political and economic uncertainties, coupled with extreme weather conditions, can have direct and adverse impacts on food security. The poorer the household, the stronger the impact of external shocks, as poor households spend a proportionally higher share of their incomes on food."
if (region_to_report == "RAP") short_text <- "Over the last ten years, food and agricultural markets have entered an unexpectedly turbulent phase, characterized by large supply shortfalls, price swings. Political and economic uncertainties, coupled with extreme weather conditions, can have direct and adverse impacts on food security. The poorer the household, the stronger the impact of external shocks, as poor households spend a proportionally higher share of their incomes on food."
if (region_to_report == "REU") short_text <- "Over the last ten years, food and agricultural markets have entered an unexpectedly turbulent phase, characterized by large supply shortfalls, price swings. Political and economic uncertainties, coupled with extreme weather conditions, can have direct and adverse impacts on food security. The poorer the household, the stronger the impact of external shocks, as poor households spend a proportionally higher share of their incomes on food."
if (region_to_report == "RNE") short_text <- "Over the last ten years, food and agricultural markets have entered an unexpectedly turbulent phase, characterized by large supply shortfalls, price swings. Political and economic uncertainties, coupled with extreme weather conditions, can have direct and adverse impacts on food security. The poorer the household, the stronger the impact of external shocks, as poor households spend a proportionally higher share of their incomes on food."
if (region_to_report == "GLO") short_text <- "Over the last ten years, food and agricultural markets have entered an unexpectedly turbulent phase, characterized by large supply shortfalls, price swings. Political and economic uncertainties, coupled with extreme weather conditions, can have direct and adverse impacts on food security. The poorer the household, the stronger the impact of external shocks, as poor households spend a proportionally higher share of their incomes on food."

## ---- P2stabilityData ----


## ---- P2stabilityTOPRIGHT ----
dat_plot <- df %>% filter(FAOST_CODE %in% c(5000,12000,13000,14000,15000)) %>%  select(FAOST_CODE,Year,FAO_TABLE_NAME,FS.DEA.PCFPV.IDD)

dat_plot$FAO_TABLE_NAME <- factor(dat_plot$FAO_TABLE_NAME, levels=c("Near East and North Africa",
                                                                    "Europe and Central Asia",
                                                                    "Asia and the Pacific",
                                                                    "Africa",
                                                                    "World"))

dat_plot$FAO_TABLE_NAME[dat_plot$FAO_TABLE_NAME == "Latin America and the Caribbean"] <- "Latin Am. and the Carib."

p <- ggplot(data = dat_plot, aes(x = Year, y = FS.DEA.PCFPV.IDD,group=FAO_TABLE_NAME,color=FAO_TABLE_NAME))
p <- p + geom_line(size=1.1, alpha=.7)
p <- p + scale_color_manual(values = plot_colors(part = 1, length(unique(dat_plot$FAO_TABLE_NAME)))[["Sub"]])
p <- p + labs(y="index", x="")
p <- p + guides(color = guide_legend(nrow = 3))
p <- p + theme(axis.text.x = element_text(angle = 45))
p


# Caption
caption_text <- "Per capita food production variability, constant 2004-2006 thousand international \\$"



## ---- P2stabilityLEFT ----

dat <- df[df$Year %in%  c(2000,2011) & df$FAOST_CODE < 5000,c("FAOST_CODE","Year","FAO_TABLE_NAME","FS.DS.PCFSV.KCDD")]

dat <- dat[!is.na(dat$FS.DS.PCFSV.KCDD),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

dat <- dat[which(dat[[region_to_report]]),]

# semi-standard data munging for two year dot-plots
# give name Value for value-col
names(dat)[names(dat)=="FS.DS.PCFSV.KCDD"] <- "Value"
# Plot only as many countries as there are for particular region, max 20
nro_latest_cases <- nrow(dat[dat$Year == max(dat$Year),])
if (nro_latest_cases < 20) {ncases <- nro_latest_cases} else ncases <- 20
dat <- arrange(dat, -Year, -Value)
# slice the data for both years
top2015 <- dat %>% slice(1:ncases) %>% dplyr::mutate(color = "2011")
top2000 <- dat %>% filter(FAOST_CODE %in% top2015$FAOST_CODE, Year == 2000) %>% dplyr::mutate(color = "2000")
dat_plot <- rbind(top2015,top2000)
# levels based on newest year
dat_plot$SHORT_NAME <- factor(dat_plot$SHORT_NAME, levels=arrange(top2015,Value)$SHORT_NAME)
###############

p <- ggplot(dat_plot, aes(x=SHORT_NAME,y=Value))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="kcal/capita/day")
p <- p + guides(color = guide_legend(nrow = 1))
p

# Caption
caption_text <- "Per capita food supply variability, top 20 countries in 2011, kcal/capita/day"
if (region_to_report == "RAF") caption_text <- "Per capita food supply variability, top 20 countries in 2011, kcal/capita/day"
if (region_to_report == "RAP") caption_text <- "Per capita food supply variability, top 20 countries in 2011, kcal/capita/day"
if (region_to_report == "REU") caption_text <- "Per capita food supply variability, top 20 countries in 2011, kcal/capita/day"
if (region_to_report == "RNE") caption_text <- "Per capita food supply variability, top 20 countries in 2011, kcal/capita/day"
if (region_to_report == "GLO") caption_text <- "Per capita food supply variability, top 20 countries in 2011, kcal/capita/day"


## ---- P2stabilityRIGHT ----
dat <- df[df$Year %in%  c(2000,2014) & df$FAOST_CODE < 5000,c("FAOST_CODE","Year","FAO_TABLE_NAME","FS.DEA.DFPLIV.IND")]

dat <- dat[!is.na(dat$FS.DEA.DFPLIV.IND),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

dat <- dat[which(dat[[region_to_report]]),]

# semi-standard data munging for two year dot-plots
# give name Value for value-col
names(dat)[names(dat)=="FS.DEA.DFPLIV.IND"] <- "Value"
# Plot only as many countries as there are for particular region, max 20
nro_latest_cases <- nrow(dat[dat$Year == max(dat$Year),])
if (nro_latest_cases < 20) {ncases <- nro_latest_cases} else ncases <- 20
dat <- arrange(dat, -Year, -Value)
# slice the data for both years
top2015 <- dat %>% slice(1:ncases) %>% dplyr::mutate(color = "2011")
top2000 <- dat %>% filter(FAOST_CODE %in% top2015$FAOST_CODE, Year == 2000) %>% dplyr::mutate(color = "2000")
dat_plot <- rbind(top2015,top2000)
# levels based on newest year
dat_plot$SHORT_NAME <- factor(dat_plot$SHORT_NAME, levels=arrange(top2015,Value)$SHORT_NAME)
###############

p <- ggplot(dat_plot, aes(x=SHORT_NAME,y=Value))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="index")
p <- p + guides(color = guide_legend(nrow = 1))
p

# Caption
caption_text <- "Domestic food price volatility index, top 20 countriesin 2014"
if (region_to_report == "RAF") caption_text <- "Domestic food price volatility index, top 20 countriesin 2014"
if (region_to_report == "RAP") caption_text <- "Domestic food price volatility index, top 20 countriesin 2014"
if (region_to_report == "REU") caption_text <- "Domestic food price volatility index, top 20 countriesin 2014"
if (region_to_report == "RNE") caption_text <- "Domestic food price volatility index, top 20 countriesin 2014"
if (region_to_report == "GLO") caption_text <- "Domestic food price volatility index, top 20 countriesin 2014"

## ---- P2stabilityBOTTOM ----
dat <- df %>% filter(FAOST_CODE %in% c(5000,12000,13000,14000,15000), Year %in% c(2000,2010)) %>%  select(FAOST_CODE,Year,FAO_TABLE_NAME,T.V.FEFS.PCT3D)

dat_plot <- dat[!is.na(dat$T.V.FEFS.PCT3D),]

# top ten

dat_plot$year_range[dat$Year == 2000] <- "1999-2001"
dat_plot$year_range[dat$Year == 2010] <- "2009-2011"

dat_plot$FAO_TABLE_NAME <- factor(dat_plot$FAO_TABLE_NAME, levels=(dat_plot %>% filter(year_range == "2009-2011") %>% arrange(-T.V.FEFS.PCT3D))$FAO_TABLE_NAME)

p <- ggplot(dat_plot, aes(x=FAO_TABLE_NAME,y=T.V.FEFS.PCT3D,fill=year_range))
p <- p + geom_bar(stat="identity",position="dodge")
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + labs(x=NULL,y="percent")
p <- p + theme(axis.text.x = element_text(angle=45))
p

caption_text <- "Value of food imports as a share of total merchandise exports (3 year averages)"


## ---- P2stabilityMAP ----
dat <- df[df$Year %in%  2013 & df$FAOST_CODE < 5000,c("Year","FAOST_CODE","G.GD.PSAVT.IN")]

dat <- dat[!is.na(dat$G.GD.PSAVT.IN),]

map.plot <- left_join(map.df,dat)

# Add region key and subset
map.plot <- map.plot[which(map.plot[[region_to_report]]),]

cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","G.GD.PSAVT.IN")]
cat_data$value_cat <- categories(x=cat_data$G.GD.PSAVT.IN, n=5,decimals = 1) # manualBreaks = c(0, 5, 15, 25, 35, 100),

map.plot <- left_join(map.plot,cat_data[c("FAOST_CODE","value_cat")])

# define map unit
map_unit <- "index"


p <- create_map_here()
p

# Caption
caption_text <- "Political stability and absence of violence/terrorism, index (2013)"



#   _____                       _             _     _   _   _                 _     _
#  |  ___|   ___     ___     __| |    _   _  | |_  (_) | | (_)  ____   __ _  | |_  (_)   ___    _ __
#  | |_     / _ \   / _ \   / _` |   | | | | | __| | | | | | | |_  /  / _` | | __| | |  / _ \  | '_ \
#  |  _|   | (_) | | (_) | | (_| |   | |_| | | |_  | | | | | |  / /  | (_| | | |_  | | | (_) | | | | |
#  |_|      \___/   \___/   \__,_|    \__,_|  \__| |_| |_| |_| /___|  \__,_|  \__| |_|  \___/  |_| |_|


## ---- P2utilizaTEXT ----
spread_title <- "Food utilization"
if (region_to_report == "RAF") short_text <- "Utilization emphasizes the nutritional aspects of food security. It is commonly understood as the way the body makes the most of nutrients from food. Sufficient energy and nutrient intake includes nutritious and safe diets, a clean environment, access to health care, diversity of a diet and intra-household distribution of food. Poor utilization within a population can impose economic and social costs in countries at all economic levels."
if (region_to_report == "RAP") short_text <- "Utilization emphasizes the nutritional aspects of food security. It is commonly understood as the way the body makes the most of nutrients from food. Sufficient energy and nutrient intake includes nutritious and safe diets, a clean environment, access to health care, diversity of a diet and intra-household distribution of food. Poor utilization within a population can impose economic and social costs in countries at all economic levels."
if (region_to_report == "REU") short_text <- "Utilization emphasizes the nutritional aspects of food security. It is commonly understood as the way the body makes the most of nutrients from food. Sufficient energy and nutrient intake includes nutritious and safe diets, a clean environment, access to health care, diversity of a diet and intra-household distribution of food. Poor utilization within a population can impose economic and social costs in countries at all economic levels."
if (region_to_report == "RNE") short_text <- "Utilization emphasizes the nutritional aspects of food security. It is commonly understood as the way the body makes the most of nutrients from food. Sufficient energy and nutrient intake includes nutritious and safe diets, a clean environment, access to health care, diversity of a diet and intra-household distribution of food. Poor utilization within a population can impose economic and social costs in countries at all economic levels."
if (region_to_report == "GLO") short_text <- "Utilization emphasizes the nutritional aspects of food security. It is commonly understood as the way the body makes the most of nutrients from food. Sufficient energy and nutrient intake includes nutritious and safe diets, a clean environment, access to health care, diversity of a diet and intra-household distribution of food. Poor utilization within a population can impose economic and social costs in countries at all economic levels."

## ---- P2utilizaData ----


## ---- P2utilizaTOPRIGHT ----
dat <- df[df$Year %in%  2008:2015 & df$FAOST_CODE < 5000,c("FAOST_CODE","Year","FAO_TABLE_NAME","SH.STA.MALN.ZS")]

dat <- dat[!is.na(dat$SH.STA.MALN.ZS),]

dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

dat <- dat[which(dat[[region_to_report]]),]

tbl <- dat %>% group_by(FAOST_CODE) %>% filter(Year == max(Year)) %>% ungroup()
tbl <- arrange(tbl, -SH.STA.MALN.ZS)[1:5,]
tbl <- left_join(tbl,FAOcountryProfile[c("FAOST_CODE","SHORT_NAME")])
tbl <- tbl[c(5,2,4)]
names(tbl) <- c("","Year","%")

print.xtable(xtable(tbl, caption = "\\large{Countries with highest share of children under 5 who are underweight}, percent", digits = c(0,0,0,1),
                    align= "l{\raggedright\arraybackslash}p{1.6cm}rr"),
             type = table_type, table.placement = NULL, booktabs = TRUE, include.rownames = FALSE, size = "footnotesize", caption.placement = "top",
             html.table.attributes = 'class="table table-striped table-hover"')



## ---- P2utilizaLEFT ----
dat <- df[df$Year %in%  2006:2014 & df$FAOST_CODE < 5000,c("FAOST_CODE","Year","FAO_TABLE_NAME","SH.STA.STNT.ZS")]

dat <- dat[!is.na(dat$SH.STA.STNT.ZS),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

dat <- dat[which(dat[[region_to_report]]),]

dat <- dat %>% group_by(FAOST_CODE) %>% filter(Year == max(Year)) %>% ungroup()

dat <- arrange(dat, -Year, -SH.STA.STNT.ZS)

# limit the nro of printed for REU/RNE countries
if (region_to_report %in% c("REU","RNE")){
  max_nro_countries <- nrow(dat)
} else max_nro_countries <- 20


top15 <- dat %>% slice(1:max_nro_countries) %>% dplyr::mutate(color = "2009-2011")
# top91 <- dat %>% filter(FAOST_CODE %in% top15$FAOST_CODE, Year == 2000) %>% dplyr::mutate(color = "1999-2001")
# dat_plot <- rbind(top15,top91)
dat_plot <- top15

dat_plot$SHORT_NAME <- factor(dat_plot$SHORT_NAME, levels=arrange(top15, SH.STA.STNT.ZS)$SHORT_NAME)

p <- ggplot(dat_plot, aes(x=SHORT_NAME,y=SH.STA.STNT.ZS))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + theme(legend.position = "none")
p <- p + labs(x="",y="percent")
# p <- p + guides(color = guide_legend(nrow = 2))
p

# Caption
caption_text <- "Percentage of children under 5 who are stunted, highest 20 countries (2006 - 2014*)"
if (region_to_report == "RAF") caption_text <- "Percentage of children under 5 who are stunted, highest 20 countries (2006 - 2014*)"
if (region_to_report == "RAP") caption_text <- "Percentage of children under 5 who are stunted, highest 20 countries (2006 - 2014*)"
if (region_to_report == "REU") caption_text <- "Percentage of children under 5 who are stunted, highest 20 countries (2006 - 2014*)"
if (region_to_report == "RNE") caption_text <- "Percentage of children under 5 who are stunted, highest 20 countries (2006 - 2014*)"
if (region_to_report == "GLO") caption_text <- "Percentage of children under 5 who are stunted, highest 20 countries (2006 - 2014*)"

## ---- P2utilizaRIGHT ----
dat <- df[df$Year %in%  2006:2014 & df$FAOST_CODE < 5000,c("FAOST_CODE","Year","FAO_TABLE_NAME","SH.STA.WAST.ZS")]

dat <- dat[!is.na(dat$SH.STA.WAST.ZS),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

dat <- dat[which(dat[[region_to_report]]),]

dat <- dat %>% group_by(FAOST_CODE) %>% filter(Year == max(Year)) %>% ungroup()

dat <- arrange(dat, -Year, -SH.STA.WAST.ZS)

# limit the nro of printed for REU/RNE countries
if (region_to_report %in% c("REU","RNE")){
  max_nro_countries <- nrow(dat)
} else max_nro_countries <- 20


top15 <- dat %>% slice(1:max_nro_countries) %>% dplyr::mutate(color = "2009-2011")
# top91 <- dat %>% filter(FAOST_CODE %in% top15$FAOST_CODE, Year == 2000) %>% dplyr::mutate(color = "1999-2001")
# dat_plot <- rbind(top15,top91)
dat_plot <- top15

dat_plot$SHORT_NAME <- factor(dat_plot$SHORT_NAME, levels=arrange(top15, SH.STA.WAST.ZS)$SHORT_NAME)

p <- ggplot(dat_plot, aes(x=SHORT_NAME,y=SH.STA.WAST.ZS))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + theme(legend.position = "none")
p <- p + labs(x="",y="percent")
# p <- p + guides(color = guide_legend(nrow = 2))
p

# Caption
caption_text <- "Percentage of children under 5 affected by wasting, highest 20 countries (2006 - 2014*)"
if (region_to_report == "RAF") caption_text <- "Percentage of children under 5 affected by wasting, highest 20 countries (2006 - 2014*)"
if (region_to_report == "RAP") caption_text <- "Percentage of children under 5 affected by wasting, highest 20 countries (2006 - 2014*)"
if (region_to_report == "REU") caption_text <- "Percentage of children under 5 affected by wasting, highest 20 countries (2006 - 2014*)"
if (region_to_report == "RNE") caption_text <- "Percentage of children under 5 affected by wasting, highest 20 countries (2006 - 2014*)"
if (region_to_report == "GLO") caption_text <- "Percentage of children under 5 affected by wasting, highest 20 countries (2006 - 2014*)"

## ---- P2utilizaBOTTOM ----
dat <- df %>% filter(FAOST_CODE %in% c(5000), Year >= 2000) %>%  select(FAOST_CODE,Year,FAO_TABLE_NAME,SH.H2O.SAFE.ZS,SH.STA.ACSN)

dat <- dat[!is.na(dat$SH.STA.ACSN),]
dat <- dat[!is.na(dat$SH.H2O.SAFE.ZS),]

dat_plot <- gather(dat, variable, value, 4:5)

dat_plot$variable <- as.character(dat_plot$variable)
dat_plot$variable[dat_plot$variable == "SH.H2O.SAFE.ZS"] <- "Water source"
dat_plot$variable[dat_plot$variable == "SH.STA.ACSN"] <- "Sanitation facilities"


p <- ggplot(dat_plot, aes(x=Year,y=value,color=variable))
p <- p + geom_line(size=1.1, alpha=.7)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + labs(x=NULL,y="percent of population")
p <- p + scale_x_continuous(breaks=c(2000,2005,2010))
p

caption_text <- "Access to improved water source and sanitation facilities"

## ---- P2utilizaMAP ----

dat <- df[df$Year %in%  2008:2011 & df$FAOST_CODE < 5000,c("Year","FAOST_CODE","SH.ANM.CHLD.ZS")]

# dat <- dat[dat$FAOST_CODE != 41,]
dat$FAOST_CODE[dat$FAOST_CODE == 351] <- 41

dat <- dat[!is.na(dat$SH.ANM.CHLD.ZS),]

map.plot <- left_join(map.df,dat)

# Add region key and subset
map.plot <- map.plot[which(map.plot[[region_to_report]]),]

cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","SH.ANM.CHLD.ZS")]
cat_data$value_cat <- categories(x=cat_data$SH.ANM.CHLD.ZS, n=5) # manualBreaks = c(0, 5, 15, 25, 35, 100),

map.plot <- left_join(map.plot,cat_data[c("FAOST_CODE","value_cat")])

# define map unit
map_unit <- "percent"


p <- create_map_here()
p

# Caption
caption_text <- "Percentage of anaemia among children under 5, percent (2011)"
