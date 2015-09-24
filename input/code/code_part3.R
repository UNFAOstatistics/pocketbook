## ---- part3_setup ----

source(paste0(root.dir,'/input/code/plot/plot_color.R'))

syb_part <- 3

## Part 3
colPart3 <- plot_colors(part = syb_part, 12)
col.main1 <- colPart3[["Main"]][1]
## color for the grid
col.main2 <- colPart3[["Main"]][2]

source(paste0(root.dir,"/input/code/plot/theme.R"))

# map functions
source(paste0(root.dir,'/input/code/plot/map_categories.R'))



#   ____   _        _                                                                                         _
#  |  _ \ (_)  ___ | |_  __ _  _ __  _   _    ___  _ __    ___  _ __  __ _  _   _   ___  _   _  _ __   _ __  | | _   _
#  | | | || | / _ \| __|/ _` || '__|| | | |  / _ \| '_ \  / _ \| '__|/ _` || | | | / __|| | | || '_ \ | '_ \ | || | | |
#  | |_| || ||  __/| |_| (_| || |   | |_| | |  __/| | | ||  __/| |  | (_| || |_| | \__ \| |_| || |_) || |_) || || |_| |
#  |____/ |_| \___| \__|\__,_||_|    \__, |  \___||_| |_| \___||_|   \__, | \__, | |___/ \__,_|| .__/ | .__/ |_| \__, |
#                                    |___/                           |___/  |___/              |_|    |_|        |___/


## ---- P3desTEXT ----
spread_title <- "Dietary energy supply"
short_text <- "The dietary energy supply (DES) is the food available for human consumption, expressed in kilocalories per person per day. At the country level, it is calculated as a measure of food available for human use after taking out all non-food utilization, including exports, industrial use, animal feed, seed, wastage and changes in stocks. In 1961 the average global calorie availability was as low as 2 196 kcal/cap/day; by 2011, it had reached 2 870 kcal/cap/day, and was centered more around a narrow base of staple grains as well as meat and dairy products."


## ---- P3desData ----
# Retrieve data
dat <- read.csv(paste0(data.dir,"/FSI2015_DisseminationDataset.csv"), stringsAsFactors=FALSE)
metdat <- read.csv(paste0(data.dir,"/FSI2015_DisseminationMetadata.csv"), stringsAsFactors=FALSE)
dat$FAOST_CODE <- as.factor(dat$FAOST_CODE)
dat$FAOST_CODE <- as.numeric(levels(dat$FAOST_CODE))[dat$FAOST_CODE]
# SOFI to M49 conversions
# Asia
dat$FAOST_CODE[dat$FAOST_CODE == 5853] <- 5300
dat$FAOST_CODE[dat$FAOST_CODE == 5001] <- 5000

# Add Area var from sybdata.df
tmp <- syb.df[!duplicated(syb.df[c("FAOST_CODE","Area")]),]
dat <- merge(dat,tmp[c("FAOST_CODE","Area")],by="FAOST_CODE")
dat <- merge(dat,FAOcountryProfile[c("FAOST_CODE","SHORT_NAME")],by="FAOST_CODE", all.x=TRUE)
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

# For despie graphs icn2.df
load(paste0(root.dir,"../ICN2PB14/Data/Processed/icn2.RData"))



## ---- P3desTOPRIGHT ----


## Plot
despie <- icn2.df[icn2.df$Year %in% c(2009:2011), c("FAOST_CODE","Year","FAO_TABLE_NAME","FBS.SDES.CRLS.PCT3D","FBS.SDES.SR.PCT3D","FBS.SDES.SS.PCT3D","FBS.SDES.MO.PCT3D","FBS.SDES.VOAF.PCT3D","FBS.SDES.MEB.PCT3D")]
#despie <- despie[despie$FAOST_CODE %in% "5000",]

dw <- gather(despie,
             "var",
             "value",
             4:9)
d <- dw %>% group_by(FAOST_CODE,var) %>% dplyr::summarise(mean = mean(value))

d$var <- as.character(d$var)
d$var[d$var == "FBS.SDES.CRLS.PCT3D"] <- "Cereals\n(excl. beer)"
d$var[d$var == "FBS.SDES.SR.PCT3D"] <- "Starchy roots"
d$var[d$var == "FBS.SDES.SS.PCT3D"] <- "Sugar and\nsweeteners"
d$var[d$var == "FBS.SDES.MO.PCT3D"] <- "Meat and offals"
d$var[d$var == "FBS.SDES.VOAF.PCT3D"] <- "Milk\n(excl. butter)"
d$var[d$var == "FBS.SDES.MEB.PCT3D"] <- "Veg. oils and\nanimal fats"

d$FAOST_CODE <- factor(d$FAOST_CODE)
d$FAOST_CODE <- as.numeric(levels(d$FAOST_CODE))[d$FAOST_CODE]

dat <- left_join(d,region_key)

## option 1
pop <- syb.df %>% select(FAOST_CODE,Year,OA.TPBS.POP.PPL.NO) %>%  group_by(FAOST_CODE) %>% filter(Year == 2014)

dat <- dat[which(dat[[region_to_report]]),]

dat <- left_join(dat[c("FAOST_CODE","var","mean")],pop)
dat <- dat[!is.na(dat$mean),]
dat <- dat[!is.na(dat$OA.TPBS.POP.PPL.NO),]

dat <- dat %>% group_by(var) %>%  dplyr::summarise(wmean = weighted.mean(mean, OA.TPBS.POP.PPL.NO, na.rm=FALSE)) %>%
             dplyr::mutate(mean = wmean/sum(wmean)*100)

dat_plot <- dat  %>% dplyr::mutate(sum = sum(mean))

p <- ggplot(dat_plot, aes(x=sum/2, y = mean, fill = var, width = sum))
p <- p + geom_bar(position="fill", stat="identity")
p <- p + coord_polar("y")
p <- p + theme_minimal()
p <- p + theme(legend.position = "right")
p <- p + theme(text = element_text(size=11, family="PT Sans"))
p <- p + theme(axis.text = element_blank())
p <- p + theme(axis.title = element_blank())
p <- p + theme(axis.ticks = element_blank())
p <- p + theme(panel.grid.minor = element_blank())
p <- p + theme(panel.grid.major = element_blank())
p <- p + scale_fill_manual(values=rev(colPart3$Sub))
p <- p + theme(legend.title = element_blank())
p <- p + theme(legend.key.height = unit(7, "mm"))
p <- p + theme(legend.key.width = unit(3, "mm"))
p <- p + theme(panel.grid=element_blank(), panel.border=element_blank())
p <- p + labs(x=NULL, y=NULL)
p <- p + theme(plot.margin=unit(c(0,0,0,0),"mm"))
p

# Caption
caption_text <- "This is the default caption if no region spesific is defined"


## ---- P3desLEFT ----
# data

dat <- df[df$Year %in%  c(2000,2015) & df$FAOST_CODE < 5000,c("FAOST_CODE","Year","FAO_TABLE_NAME","FBS.PCS.PDES.KCD3D")]

dat <- dat[!is.na(dat$FBS.PCS.PDES.KCD3D),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

dat <- dat[which(dat[[region_to_report]]),]

dat <- arrange(dat, -Year, -FBS.PCS.PDES.KCD3D)
top2015 <- dat %>% slice(1:20) %>% dplyr::mutate(color = "2015")
top2000 <- dat %>% filter(FAOST_CODE %in% top2015$FAOST_CODE, Year == 2000) %>% dplyr::mutate(color = "2000")
dat_plot <- rbind(top2015,top2000)

p <- ggplot(dat_plot, aes(x=reorder(SHORT_NAME, FBS.PCS.PDES.KCD3D),y=FBS.PCS.PDES.KCD3D))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="kcal/cap/day")
p <- p + guides(color = guide_legend(nrow = 1))
p

# Caption
caption_text <- "Dietary energy supply, top 20 countries in 2015"

## ---- P3desRIGHT ----

dat <- df[df$Year %in%  c(2000,2015) & df$FAOST_CODE < 5000,c("FAOST_CODE","Year","FAO_TABLE_NAME","FBS.PCS.PDES.KCD3D")]

dat <- dat[!is.na(dat$FBS.PCS.PDES.KCD3D),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

dat <- dat[which(dat[[region_to_report]]),]

dat <- arrange(dat, -Year, FBS.PCS.PDES.KCD3D)
bottom2015 <- dat %>% slice(1:20) %>% dplyr::mutate(color = "2015")
bottom2000 <- dat %>% filter(FAOST_CODE %in% bottom2015$FAOST_CODE, Year == 2000) %>% dplyr::mutate(color = "2000")
dat_plot <- rbind(bottom2015,bottom2000)

p <- ggplot(dat_plot, aes(x=reorder(SHORT_NAME, FBS.PCS.PDES.KCD3D),y=FBS.PCS.PDES.KCD3D))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="kcal/cap/day")
p <- p + guides(color = guide_legend(nrow = 1))
p

# Caption
caption_text <- "Dietary energy supply, bottom 20 countries in 2015"


## ---- P3desBOTTOM ----
dat <- df[df$Year %in%  c(2000:2015) & df$FAOST_CODE < 5000,c("FAOST_CODE","Year","FAO_TABLE_NAME","FBS.PCS.PDES.KCD3D")]

dat <- dat[!is.na(dat$FBS.PCS.PDES.KCD3D),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"


dat <- dat[which(dat[[region_to_report]]),]

dat <- arrange(dat, -Year, -FBS.PCS.PDES.KCD3D)
top5_FAOST_CODE <- head(dat$FAOST_CODE, 5)
dat_plot <- dat %>%  filter(FAOST_CODE %in% top5_FAOST_CODE)


p <- ggplot(dat_plot, aes(x=Year,y=FBS.PCS.PDES.KCD3D,color=SHORT_NAME))
p <- p + geom_point() + geom_line()
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 5)[["Sub"]])
p <- p + labs(x="",y="kcal/cap/day")
p

# Caption
caption_text <- "Dietary energy supply"

## ---- P3desMAP ----

dat <- df[df$Year %in%  2015 & df$FAOST_CODE < 5000,c("Year","FAOST_CODE","FS.DA.ADESA.PCT3D")]

dat <- dat[dat$FAOST_CODE != 41,]
dat$FAOST_CODE[dat$FAOST_CODE == 351] <- 41

map.plot <- left_join(map.df,dat) # so that each country in the region will be filled (value/NA)

# Add region key and subset

map.plot <- map.plot[which(map.plot[[region_to_report]]),]


cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","FS.DA.ADESA.PCT3D")]
cat_data$value_cat <- categories(x=cat_data$FS.DA.ADESA.PCT3D, n=5, manual=FALSE, method="jenks") # manualBreaks = c(0, 5, 15, 25, 35, 100),

map.plot <- left_join(map.plot,cat_data[c("FAOST_CODE","value_cat")])

# define map unit
map_unit <- "Percent"

create_map_here()

# Caption
caption_text <- "This is the default caption if no region spesific is defined"
if (region_to_report == "RAF") caption_text <- "Average dietary energy supply adequacy"
if (region_to_report == "RAP") caption_text <- "Average dietary energy supply adequacy"
if (region_to_report == "REU") caption_text <- "Average dietary energy supply adequacy"
if (region_to_report == "RNE") caption_text <- "Average dietary energy supply adequacy"
if (region_to_report == "GLO") caption_text <- "Average dietary energy supply adequacy"

#    ____                                             _               _    _
#   / ___| _ __  ___   _ __    _ __   _ __  ___    __| | _   _   ___ | |_ (_)  ___   _ __
#  | |    | '__|/ _ \ | '_ \  | '_ \ | '__|/ _ \  / _` || | | | / __|| __|| | / _ \ | '_ \
#  | |___ | |  | (_) || |_) | | |_) || |  | (_) || (_| || |_| || (__ | |_ | || (_) || | | |
#   \____||_|   \___/ | .__/  | .__/ |_|   \___/  \__,_| \__,_| \___| \__||_| \___/ |_| |_|
#                     |_|     |_|


## ---- P3cropproTEXT ----
spread_title <- "Crop production"
short_text <- "The majority of people in developing countries live in rural areas, and most of them depend on agriculture for their livelihoods. Over the past 50 years, growth in crop production has been driven largely by higher yields per unit of land, and crop intensification. Trends are not uniform across regions, however. Most of the growth in wheat and rice production in Asia and Northern Africa has been from gains in yield, while expansion of harvested land has led to production growth of maize in Latin America and in sub-Saharan Africa."


## ---- P3cropproData ----

# This should be thought twice how to produce it for regional books!
load(paste0(data.dir,"/Production_Crops_E_All_Data.RData"))
names(dat)[names(dat)=="CountryCode"] <- "FAOST_CODE"
# Add region key and subset
dat <- left_join(dat,region_key)




## ---- P3cropproTOPRIGHT ----
# rc <- dat %>%  filter(Year >= 2000, Element == "Production") %>% group_by(Item,Year) %>% dplyr::summarise(Value = sum(Value, na.rm = TRUE)) %>%
#   dplyr::mutate(Growth=c(NA,exp(diff(log(Value)))-1)) %>%
#   dplyr::summarise(mean_growth = mean(Growth, na.rm = TRUE)*100) %>%  filter(!is.infinite(mean_growth)) %>%
#   arrange(-mean_growth) %>% slice(1:5) %>% select(Item,mean_growth)

dat <- dat[which(dat[[region_to_report]]),]

growth <- data.frame()

gr_dat <- dat %>% filter(Year >= 2000,Element == "Production")
gr_dat <- gr_dat[!is.na(gr_dat$Value),]
gr_dat$Item <- as.character(gr_dat$Item)
for (fs in unique(gr_dat$Item)){
  d <- gr_dat[gr_dat$Item %in% fs,]
  if (sum(d$Value) == 0) next
  d <- d[d$Value > 0,]
  grate <- as.numeric((exp(coef(lm(log(d$Value) ~ Year, d))[2]) - 1) * 100)
  row <- data.frame(FAOST_CODE = fs,
                    growth_rate = grate)
  growth <- rbind(growth,row)
}
# items to exclude
growth <- growth[growth[[1]] != "Fruit, pome nes",] # leave the 1st, Fruit, pome nes , out from the table as pointed by Amy sep 18, 2015
rc <- growth %>% arrange(-growth_rate) %>% slice(1:5) 


names(rc) <- c("","%")

print.xtable(xtable(rc, caption = "Fastest growing products based on quantities (average anual growth rate, 2000 to 2013) (COMPUTE NEW RATES!!)", digits = c(0,0,0),
                    align= "l{\raggedright\arraybackslash}p{2.2cm}r"),
             type = "latex", table.placement = NULL, booktabs = TRUE,
             include.rownames = FALSE, size = "footnotesize", caption.placement = "top")



## ---- P3cropproLEFT ----
# data
dat <- syb.df %>% filter(Year %in% c(2000,2012)) %>%  select(FAOST_CODE,Year,QV.NPCPV.CRPS.ID.SHP)

dat <- dat[!is.na(dat$QV.NPCPV.CRPS.ID.SHP),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

dat <- dat[which(dat[[region_to_report]]),]

dat <- arrange(dat, -Year, -QV.NPCPV.CRPS.ID.SHP)
top12 <- dat %>% slice(1:20) %>% dplyr::mutate(color = "2012")
top00 <- dat %>% filter(FAOST_CODE %in% top12$FAOST_CODE, Year == 2000) %>% dplyr::mutate(color = "2000")
dat_plot <- rbind(top12,top00)

p <- ggplot(dat_plot, aes(x=reorder(SHORT_NAME, QV.NPCPV.CRPS.ID.SHP),y=QV.NPCPV.CRPS.ID.SHP))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="percent")
p <- p + guides(color = guide_legend(nrow = 2))
p

# Caption
caption_text <- "Top 20 crop producing countries in 2012 based on net per capita crop production value"


## ---- P3cropproRIGHT ----

# data
dat <- syb.df %>% filter(Year %in% c(2000,2012)) %>%  select(FAOST_CODE,Year,QV.GPCPV.FOOD.ID.SHP)

dat <- dat[!is.na(dat$QV.GPCPV.FOOD.ID.SHP),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

dat <- dat[which(dat[[region_to_report]]),]

dat <- arrange(dat, -Year, -QV.GPCPV.FOOD.ID.SHP)
top12 <- dat %>% slice(1:20) %>% dplyr::mutate(color = "2012")
top00 <- dat %>% filter(FAOST_CODE %in% top12$FAOST_CODE, Year == 2000) %>% dplyr::mutate(color = "2000")
dat_plot <- rbind(top12,top00)

p <- ggplot(dat_plot, aes(x=reorder(SHORT_NAME, QV.GPCPV.FOOD.ID.SHP),y=QV.GPCPV.FOOD.ID.SHP))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="percent")
p <- p + guides(color = guide_legend(nrow = 2))
p

# Caption
caption_text <- "Top 20 food producing countries in 2012 based on net food per capita production value"

## ---- P3cropproBOTTOM ----
dat <- syb.df %>% filter(Year >= 2000) %>%
  select(FAOST_CODE,Area,Year,
                         QC.PRD.CRLS.TN.NO,   # Cereals production (tonnes)
                         QC.RHRV.CRLS.HA.NO,  # Cereals harvested area (ha)
                         QC.YIELD.CRLS.HG.NO) # Cereals yield (hg/ha)

# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

dat <- dat[which(dat[[region_to_report]]),]

dat <- gather(dat,
                variable,
                value,
                4:6)
dat <- dat %>% filter(!is.na(value)) %>% arrange(Year,variable)

dat_plot <- dat %>% group_by(variable,Year) %>% dplyr::summarise(value = sum(value, na.rm = TRUE)) %>%
  dplyr::mutate(Growth=c(NA,exp(diff(log(value)))-1)) %>%
  dplyr::summarise(mean_growth = mean(Growth, na.rm = TRUE)*100)

dat_plot$variable <- as.character(dat_plot$variable)

dat_plot$variable[dat_plot$variable == "QC.PRD.CRLS.TN.NO"] <- "Production"
dat_plot$variable[dat_plot$variable == "QC.RHRV.CRLS.HA.NO"] <- "Harvested area"
dat_plot$variable[dat_plot$variable == "QC.YIELD.CRLS.HG.NO"] <- "Yield"

p <- ggplot(dat_plot, aes(x=variable,y=mean_growth,fill=variable))
p <- p + geom_bar(stat="identity",position="dodge")
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, length(unique(dat_plot$variable)))[["Sub"]])
p <- p + labs(x="",y="percent")
p <- p + theme(legend.position = "none")
p

# Caption
caption_text <- "Number of people undernourished, top 5 countries from region"


## ---- P3cropproMAP ----
dat <- syb.df %>% filter(Year %in% 2013) %>% select(FAOST_CODE,
                                                    QV.NPCPV.CRPS.ID.SHP)
map.plot <- left_join(map.df,dat) # so that each country in the region will be filled (value/NA)

# Add region key and subset

map.plot <- map.plot[which(map.plot[[region_to_report]]),]

cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","QV.NPCPV.CRPS.ID.SHP")]
cat_data$value_cat <- categories(x=cat_data$QV.NPCPV.CRPS.ID.SHP, n=5, method="jenks")

map.plot <- left_join(map.plot,cat_data[c("FAOST_CODE","value_cat")])

# define map unit
map_unit <- "Percent"

create_map_here()

# Caption
caption_text <- "Crops, gross per capita production index (2004-06 = 100, 2013)"

#
#    ____
#   / ___| _ __  ___   _ __
#  | |    | '__|/ _ \ | '_ \
#  | |___ | |  | (_) || |_) |
#   \____||_|   \___/ | .__/
#                     |_|



## ---- P3cropTEXT ----
spread_title <- "Crop"
short_text <- "Cereals, which include wheat, rice, barley, maize, rye, oats and millet, make up the majority of the production of the crop sector. They continue to be the most important food source for human consumption. Yet external factors, such as rising incomes and urbanization, are causing diets to shift towards diets that are higher in protein, fats and sugar. In addition, livestock and biofuel production have and will most likely grow at a faster rate than crop production. This is causing a shift away from crops, like wheat and rice, towards coarse grains and oilseeds to meet demands for food, feed and biofuel."


## ---- P3cropData ----

# This should be thought twice how to produce it for regional books!
load(paste0(data.dir,"/Production_Crops_E_All_Data.RData"))
names(dat)[names(dat)=="CountryCode"] <- "FAOST_CODE"
# Add region key and subset
dat <- left_join(dat,region_key)



## ---- P3cropTOPRIGHT ----
dat <- dat[which(dat[[region_to_report]]),]
d13 <- dat %>%  filter(Year == 2013, Element == "Production", FAOST_CODE < 5000) %>%
  filter(!grepl("Total",Item)) %>%
  group_by(Item) %>%
  dplyr::summarise(Value = sum(Value, na.rm = TRUE)) %>%
  arrange(-Value) %>%
  slice(1:5)
d00 <- dat %>% filter(Year == 2000, Element == "Production", Item %in% d13$Item ) %>%
  filter(!grepl("Total",Item)) %>%
  group_by(Item) %>%
  dplyr::summarise(Value = sum(Value, na.rm = TRUE)) %>%
  arrange(-Value) %>%
  slice(1:5)
gg <- merge(d00,d13,by="Item")
gg$Value.x <- gg$Value.x/1000
gg$Value.y <- gg$Value.y/1000
gg <- arrange(gg, -gg$Value.y)
names(gg) <- c("","2000", "2013")
gg[[2]] <- round(gg[[2]],0)
gg[[3]] <- round(gg[[3]],0)
gg[[2]]<- prettyNum(gg[[2]], big.mark=" ")
gg[[3]]<- prettyNum(gg[[3]], big.mark=" ")

print(xtable(gg, caption = "Top five items produced in 2013, thousand tonnes", digits = c(0,0,0,0),
             align= "l{\raggedright\arraybackslash}p{1.0cm}rr"),
      type = "latex", table.placement = NULL,
      booktabs = TRUE, include.rownames = FALSE,
      size = "footnotesize", caption.placement = "top")




## ---- P3cropLEFT ----
# data
dat <- syb.df %>% filter(Year %in% c(2000,2012)) %>%  select(FAOST_CODE,Year,QC.PRD.RICE.TN.SHP)

dat <- dat[!is.na(dat$QC.PRD.RICE.TN.SHP),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

dat <- dat[which(dat[[region_to_report]]),]

dat <- arrange(dat, -Year, -QC.PRD.RICE.TN.SHP)
top12 <- dat %>% slice(1:20) %>% dplyr::mutate(color = "2012")
top00 <- dat %>% filter(FAOST_CODE %in% top12$FAOST_CODE, Year == 2000) %>% dplyr::mutate(color = "2000")
dat_plot <- rbind(top12,top00)

p <- ggplot(dat_plot, aes(x=reorder(SHORT_NAME, QC.PRD.RICE.TN.SHP),y=QC.PRD.RICE.TN.SHP))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="percent")
p <- p + guides(color = guide_legend(nrow = 2))
p

# Caption
caption_text <- "Top 20 rice producing countries, per capita"


## ---- P3cropRIGHT ----

# data
dat <- syb.df %>% filter(Year %in% c(2000,2012)) %>%  select(FAOST_CODE,Year,QC.PRD.WHT.TN.SHP)

dat <- dat[!is.na(dat$QC.PRD.WHT.TN.SHP),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

dat <- dat[which(dat[[region_to_report]]),]

dat <- arrange(dat, -Year, -QC.PRD.WHT.TN.SHP)
top12 <- dat %>% slice(1:20) %>% dplyr::mutate(color = "2012")
top00 <- dat %>% filter(FAOST_CODE %in% top12$FAOST_CODE, Year == 2000) %>% dplyr::mutate(color = "2000")
dat_plot <- rbind(top12,top00)

p <- ggplot(dat_plot, aes(x=reorder(SHORT_NAME, QC.PRD.WHT.TN.SHP),y=QC.PRD.WHT.TN.SHP))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="percent")
p <- p + guides(color = guide_legend(nrow = 2))
p

# Caption
caption_text <- "Top 20 wheat producing countries, per capita"


## ---- P3cropBOTTOM ----
dat <- syb.df %>% filter(Year >= 2000) %>%
  select(FAOST_CODE,
         Year,
         QC.YIELD.CRLS.HG.NO,
         OA.TPBS.POP.PPL.NO)

# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

dat <- dat[which(dat[[region_to_report]]),]

# DEFAULT GROUPING
df <- subgrouping(region_to_report = region_to_report)

# merge data with the region info
dat <- merge(dat,df[c("FAOST_CODE","subgroup")],by="FAOST_CODE")

# AGREGATE
dat <- dat[!is.na(dat$OA.TPBS.POP.PPL.NO),]
dat_plot <- dat %>% group_by(subgroup,Year) %>% dplyr::summarise(value = weighted.mean(QC.YIELD.CRLS.HG.NO, OA.TPBS.POP.PPL.NO, na.rm=TRUE)) %>% ungroup()

p <- ggplot(data = dat_plot, aes(x = Year, y = value,group=subgroup,color=subgroup))
p <- p + geom_line()
p <- p + scale_color_manual(values = plot_colors(part = 1, length(unique(dat_plot$subgroup)))[["Sub"]])
p <- p + labs(y="tonnes/cap", x="")
p <- p + guides(color = guide_legend(nrow = 2))
p

# Caption
caption_text <- "Cereals, yield"


## ---- P3cropMAP ----
dat <- syb.df %>% filter(Year %in% 2013) %>% select(FAOST_CODE,
                                                    QC.PRD.CRLS.TN.SHP)
map.plot <- left_join(map.df,dat) # so that each country in the region will be filled (value/NA)

# Add region key and subset

map.plot <- map.plot[which(map.plot[[region_to_report]]),]

cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","QC.PRD.CRLS.TN.SHP")]
cat_data$value_cat <- categories(x=cat_data$QC.PRD.CRLS.TN.SHP, n=5, method="jenks")

map.plot <- left_join(map.plot,cat_data[c("FAOST_CODE","value_cat")])

# define map unit
map_unit <- "tonne/cap"

create_map_here()

# Caption
caption_text <- "Cereal production, tonnes/cap (2013)"



#   _      _                   _                _
#  | |    (_)__   __ ___  ___ | |_  ___    ___ | | __
#  | |    | |\ \ / // _ \/ __|| __|/ _ \  / __|| |/ /
#  | |___ | | \ V /|  __/\__ \| |_| (_) || (__ |   <
#  |_____||_|  \_/  \___||___/ \__|\___/  \___||_|\_\
#

## ---- P3livestockTEXT ----
spread_title <- "Livestock"
short_text <- "The world food economy is being increasingly driven by the shift of diets towards animal-based products such as meat, milk and dairy. As a result, agriculture is being affected, not only through growth of livestock production, but also through linkages to other sectors that supply feeding stuffs, such as crops and fisheries. Globally livestock production is the largest user of agricultural land and therefore also leaves a significant imprint on the environment."


## ---- P3livestockData ----

# This should be thought twice how to produce it for regional books!
load(paste0(data.dir,"/Production_Livestock_E_All_Data.RData"))
names(dat)[names(dat)=="CountryCode"] <- "FAOST_CODE"
# Add region key and subset
dat <- left_join(dat,region_key)



## ---- P3livestockTOPRIGHT ----
dat <- dat[which(dat[[region_to_report]]),]
d13 <- dat %>%  filter(Year %in% 2013, Unit %in% "Head") %>%
  filter(!grepl("Total",Item)) %>%
  group_by(Item) %>%
  dplyr::summarise(Value = sum(Value, na.rm = TRUE)) %>%
  arrange(-Value) %>%
  slice(1:5)
d00 <- dat %>% filter(Year == 2000, Unit %in% "Head") %>%
  filter(!grepl("Total",Item)) %>%
  group_by(Item) %>%
  dplyr::summarise(Value = sum(Value, na.rm = TRUE)) %>%
  arrange(-Value) %>%
  slice(1:5)
gg <- merge(d00,d13,by="Item")
gg$Value.x <- gg$Value.x/1000
gg$Value.y <- gg$Value.y/1000
gg <- arrange(gg, -gg$Value.y)
names(gg) <- c("","2000", "2013")
gg[[2]] <- round(gg[[2]],0)
gg[[3]] <- round(gg[[3]],0)
gg[[2]]<- prettyNum(gg[[2]], big.mark=" ")
gg[[3]]<- prettyNum(gg[[3]], big.mark=" ")

print.xtable(xtable(gg, caption = "Live animal production, top 5 in 2013 (thousand heads)", digits = c(0,0,0,0),
                    align= "l{\raggedright\arraybackslash}p{1.0cm}rr"),
             type = "latex", table.placement = NULL, booktabs = TRUE,
             include.rownames = FALSE, size = "footnotesize", caption.placement = "top")




## ---- P3livestockLEFT ----
# data
dat <- filter(syb.df, Year %in% 2012) %>% select(FAOST_CODE,Year,QL.PRD.MILK.TN.NO)

# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[!is.na(dat$QL.PRD.MILK.TN.NO),]

dat <- dat[which(dat[[region_to_report]]),]

dat <- arrange(dat, -QL.PRD.MILK.TN.NO)
top10 <- dat %>% slice(1:10) %>% dplyr::mutate(color = "Countries with highest values")
bot10 <- dat %>% slice( (nrow(dat)-9):nrow(dat)) %>% dplyr::mutate(color = "Countries with lowest values")
dat_plot <- rbind(top10,bot10)

p <- ggplot(dat_plot, aes(x=reorder(SHORT_NAME, QL.PRD.MILK.TN.NO),y=QL.PRD.MILK.TN.NO))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="percent")
p <- p + guides(color = guide_legend(nrow = 2))
p

# Caption
caption_text <- "Total milk production, top and bottom 10 countries (2012)"

## ---- P3livestockRIGHT ----

dat <- filter(syb.df, Year %in% 2012) %>% select(FAOST_CODE,Year,QL.PRD.EGG.TN.NO)

# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[!is.na(dat$QL.PRD.EGG.TN.NO),]

dat <- dat[which(dat[[region_to_report]]),]

dat <- arrange(dat, -QL.PRD.EGG.TN.NO)
top10 <- dat %>% slice(1:10) %>% dplyr::mutate(color = "Countries with highest values")
bot10 <- dat %>% slice( (nrow(dat)-9):nrow(dat)) %>% dplyr::mutate(color = "Countries with lowest values")
dat_plot <- rbind(top10,bot10)

p <- ggplot(dat_plot, aes(x=reorder(SHORT_NAME, QL.PRD.EGG.TN.NO),y=QL.PRD.EGG.TN.NO))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="percent")
p <- p + guides(color = guide_legend(nrow = 2))
p

# Caption
caption_text <- "Total egg production, top and bottom 10 countries (2012)"


## ---- P3livestockBOTTOM ----
load(paste0(data.dir,"/Production_Livestock_E_All_Data.RData"))
names(dat)[names(dat)=="CountryCode"] <- "FAOST_CODE"
# Add region key and subset
dat <- left_join(dat,region_key)
dat <- dat[which(dat[[region_to_report]]),]

# DEFAULT GROUPING
df <- subgrouping(region_to_report = region_to_report)

# merge data with the region info
dat <- merge(dat,df[c("FAOST_CODE","subgroup")],by="FAOST_CODE")

d <- dat %>% filter(Item == "Pigs", Year %in% c(2000,2013)) %>% group_by(Year,subgroup) %>%
  dplyr::summarise(Value = sum(Value)) %>%
  dplyr::mutate(sum = sum(Value)) %>%
  dplyr::mutate(share = round(Value/sum*100,0)) %>%
  ungroup() %>%
  dplyr::mutate(subgroup = str_replace_all(subgroup, "\\ \\+\\ \\(Total\\)","")) %>%
  group_by(Year) %>%
  dplyr::mutate(pos = cumsum(share)- share/2)

p <- ggplot(d, aes(x=sum/2, y = share, fill = subgroup, width = sum))
p <- p + geom_bar(position="fill", stat="identity")
p <- p + facet_wrap(~Year)
p <- p + coord_polar("y")
p <- p + theme_minimal()
p <- p + theme(legend.position = "top")
p <- p + theme(text = element_text(size=11, family="PT Sans"))
p <- p + theme(axis.text = element_blank())
p <- p + theme(axis.title = element_blank())
p <- p + theme(axis.ticks = element_blank())
p <- p + theme(panel.grid.minor = element_blank())
p <- p + theme(panel.grid.major = element_blank())
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, length(unique(d$subgroup)))[["Sub"]])
p <- p + theme(legend.title = element_blank())
p <- p + theme(legend.key.size = unit(3, "mm"))
p <- p + labs(x=NULL, y=NULL)
p <- p + theme(plot.margin=unit(c(0,0,0,0),"mm"))
p <- p + guides(fill = guide_legend(nrow = 2))
p


# Caption
caption_text <- "Pig production (heads)"



## ---- P3livestockMAP ----
dat <- syb.df %>% filter(Year %in% 2012) %>% select(FAOST_CODE,
                                                    QA.STCK.CB.HD.SHL)
map.plot <- left_join(map.df,dat) # so that each country in the region will be filled (value/NA)

# Add region key and subset

map.plot <- map.plot[which(map.plot[[region_to_report]]),]

cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","QA.STCK.CB.HD.SHL")]
cat_data$value_cat <- categories(x=cat_data$QA.STCK.CB.HD.SHL, n=5, method="jenks",decimals = 1)

map.plot <- left_join(map.plot,cat_data[c("FAOST_CODE","value_cat")])

# define map unit
map_unit <- "head/ha"

# graticule
grat_robin <- spTransform(graticule, CRS("+proj=robin"))  # reproject graticule
gr_rob <- fortify(grat_robin)
# crop the grid
if (!(region_to_report %in% c("GLO","COF"))) {
  gr_rob <- gr_rob[gr_rob$lat >= min(map.plot$lat) & gr_rob$lat <= max(map.plot$lat),]
  gr_rob <- gr_rob[gr_rob$long >= min(map.plot$long) & gr_rob$long <= max(map.plot$long),]
} else gr_rob <- gr_rob

create_map_here()

# Caption
caption_text <- "Cattle and buffaloes per ha of agricultural area, heads per ha (2012)"


#   _____  _       _                  _
#  |  ___|(_) ___ | |__    ___  _ __ (_)  ___  ___
#  | |_   | |/ __|| '_ \  / _ \| '__|| | / _ \/ __|
#  |  _|  | |\__ \| | | ||  __/| |   | ||  __/\__ \
#  |_|    |_||___/|_| |_| \___||_|   |_| \___||___/
#

## ---- P3fisheriesTEXT ----
spread_title <- "Fisheries"
short_text <- "Fish is an important component in people’s diets, providing about 3.1 billion people with almost 20 percent of their average intake of animal protein. Capture fisheries continue to dominate world output, but aquaculture accounts for a growing percentage of total fish supply. Fishery sectors are particularly important in developing countries, providing both food and livelihoods"


## ---- P3fisheriesData ----






## ---- P3fisheriesTOPRIGHT ----
dat <- read_excel(paste0(data.dir,"/FISH_percapita_production2015.xlsx"))
dat[[1]] <- as.character(dat[[1]])
dat[1,] <- c("Year",1990:2013)
names(dat) <- dat[1,]
dat <- dat[-1,]
dl <- gather(dat,
             "Year1",
             "capture",
             2:25)
dl <- spread(dl, Year, capture)
names(dl) <- c("Year","per_capita_aquaculture","per_capita_catch")
dl[[2]] <- factor(dl[[2]])
dl[[3]] <- factor(dl[[3]])
dl[[1]] <- as.numeric(levels(dl[[1]]))[dl[[1]]]
dl[[2]] <- as.numeric(levels(dl[[2]]))[dl[[2]]]
dl[[3]] <- as.numeric(levels(dl[[3]]))[dl[[3]]]

dat <- gather(dl, variable, value, 2:3)

dat$variable <- as.character(dat$variable)
dat$variable[dat$variable == "per_capita_aquaculture"] <- "From aquaculture"
dat$variable[dat$variable == "per_capita_catch"] <- "From capture fisheries"

# Draw the plot
p <- ggplot(dat, aes(x = Year, y = value))
p <- p + geom_area(aes(fill=variable), stat = "identity",position = "stack")
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + theme(axis.text.x = element_text(angle = 45))
p <- p + labs(y="kg/cap")
p <- p + scale_x_continuous(breaks=c(1990,1995,2000,2005,2010,2013))
p

# Caption
caption_text <- "Per capita fish food supply ONLY GLOBAL LEVEL DATA"


## ---- P3fisheriesLEFT ----
dat <- syb.df[syb.df$Year %in%  2012 & syb.df$FAOST_CODE < 5000,c("FAOST_CODE","Year","RF.FERT.NI.TN.SH")]

dat <- dat[!is.na(dat$RF.FERT.NI.TN.SH),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

dat <- dat[which(dat[[region_to_report]]),]

# top for this plot
dat <- arrange(dat, -RF.FERT.NI.TN.SH)
top20 <- dat %>% slice(1:20) %>% dplyr::mutate(color = "2012")


dat_plot <- top20

p <- ggplot(dat_plot, aes(x=reorder(SHORT_NAME, RF.FERT.NI.TN.SH),y=RF.FERT.NI.TN.SH))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 1)[["Sub"]])
p <- p + theme(legend.position = "none") # hide legend as only one year plotted
p <- p + coord_flip()
p <- p + labs(x="",y="kg/ha")
p <- p + guides(color = guide_legend(nrow = 2))
p

# Caption
caption_text <- "Nitrogen fertilizers consumption in nutrients per ha of arable land"


## ---- P3fisheriesRIGHT ----

dat <- syb.df[syb.df$Year %in%  2012 & syb.df$FAOST_CODE < 5000,c("FAOST_CODE","Year","RF.FERT.PH.TN.SH")]

dat <- dat[!is.na(dat$RF.FERT.PH.TN.SH),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

dat <- dat[which(dat[[region_to_report]]),]

# top for this plot
dat <- arrange(dat, -RF.FERT.PH.TN.SH)
dat_plot <- dat %>% slice(1:20) %>% dplyr::mutate(color = "2012")


p <- ggplot(dat_plot, aes(x=reorder(SHORT_NAME, RF.FERT.PH.TN.SH),y=RF.FERT.PH.TN.SH))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 1)[["Sub"]])
p <- p + theme(legend.position = "none") # hide legend as only one year plotted
p <- p + coord_flip()
p <- p + labs(x="",y="kg/ha")
p <- p + guides(color = guide_legend(nrow = 2))
p

# Caption

caption_text <- "Phosphate fertilizers consumption in nutrients per ha of arable land"


## ---- P3fisheriesBOTTOM ----
dat <- filter(syb.df, Year %in% 2012) %>% select(FAOST_CODE,
                                                 RF.FERT.NI.TN.NO,
                                                 RF.FERT.PH.TN.NO,
                                                 RF.FERT.PO.TN.NO,
                                                 RL.AREA.ARBLPRMN.HA.NO)

# Add region key and subset
dat <- left_join(dat,region_key)
dat <- dat[which(dat[[region_to_report]]),]

dat <- gather(dat, variable, value, 2:4)
dat$fill[dat$variable == "RF.FERT.NI.TN.NO"] <- "Nitrogen"
dat$fill[dat$variable == "RF.FERT.PH.TN.NO"] <- "Phosphate"
dat$fill[dat$variable == "RF.FERT.PO.TN.NO"] <- "Potash"

# DEFAULT GROUPING
df <- subgrouping(region_to_report = region_to_report)

# merge data with the region info
dat_plot <- merge(dat,df[c("FAOST_CODE","subgroup")],by="FAOST_CODE")

# AGREGATE
dat_plot <- dat_plot %>% group_by(subgroup,fill) %>%
              dplyr::summarise(value  = sum(value, na.rm=TRUE)*1000,
                        area  = sum(RL.AREA.ARBLPRMN.HA.NO, na.rm=TRUE)) %>%
  dplyr::mutate(share = value / area) %>% dplyr::mutate(sum = sum(share)) %>%  ungroup() 

# reorder regions by the share of agricultural land
dat_plot$subgroup <- factor(dat_plot$subgroup, 
                                  levels=unique(arrange(dat_plot,-sum)$subgroup))

p <- ggplot(dat_plot, aes(x=subgroup, y=share, fill=fill))
p <- p + geom_bar(stat="identity", position="stack")
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, 3)[["Sub"]])
p <- p + labs(x="",y="percent")
p <- p + theme(axis.text.x = element_text(angle=45))
p

# Caption
caption_text <- "Fertilizer consumption in nutrients per ha of arable land (2012)"



## ---- P3fisheriesMAP ----
dat <- filter(syb.df, Year %in% 2007:2012) %>% select(FAOST_CODE, RP.PEST.TOT.TN.SH) %>%
        group_by(FAOST_CODE) %>%  dplyr::summarise(RP.PEST.TOT.TN.SH = mean(RP.PEST.TOT.TN.SH, na.rm=TRUE))

# dat <- dat[dat$FAOST_CODE != 41,]
dat$FAOST_CODE[dat$FAOST_CODE == 41] <- 351

# set Robinson projection
map.plot <- left_join(map.df,dat) # so that each country in the region will be filled (value/NA)

# Subset
map.plot <- map.plot[which(map.plot[[region_to_report]]),]

cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","RP.PEST.TOT.TN.SH")]
cat_data$value_cat <- categories(x=cat_data$RP.PEST.TOT.TN.SH, n=5,decimals = 1)

map.plot <- left_join(map.plot,cat_data[c("FAOST_CODE","value_cat")])

# define map unit
map_unit <- "kg/ha"

create_map_here()

# Caption
caption_text <- "Pesticides per ha of arable land (kg/ha, 2007 to 2012*)"


#      _                 _               _  _                       _   _                    _
#     / \    __ _  _ __ (_)  ___  _   _ | || |_  _   _  _ __  __ _ | | | |_  _ __  __ _   __| |  ___
#    / _ \  / _` || '__|| | / __|| | | || || __|| | | || '__|/ _` || | | __|| '__|/ _` | / _` | / _ \
#   / ___ \| (_| || |   | || (__ | |_| || || |_ | |_| || |  | (_| || | | |_ | |  | (_| || (_| ||  __/
#  /_/   \_\\__, ||_|   |_| \___| \__,_||_| \__| \__,_||_|   \__,_||_|  \__||_|   \__,_| \__,_| \___|
#           |___/
#

## ---- P3tradeTEXT ----
spread_title <- "Agricultural trade"
short_text <- "Most of the food consumed worldwide is grown locally. Where there is not enough local production to meet demand, trade has been instrumental in filling the gap. The scale of food and agricultural trade today is unprecedented. In real terms, the value of international flows has increased around fivefold over the past 50 years, reflecting global trends in the overall volume of trade. However, this expansion has been unevenly distributed across regions. High-income countries have generally outpaced developing regions, although several of the latter have comparative advantages in food and agricultural production."


## ---- P3tradeData ----
# This should be thought twice how to produce it for regional books!
# Retrieve data
library(FAOSTAT)
dat <- getFAOtoSYB(domainCode = "TP",
                   elementCode = 5622,
                   itemCode = 1883)
dat1 <- dat$aggregates
names(dat1) <- c("FAOST_CODE","Year","value")
dat1$variable <- "Import value"
dat <- getFAOtoSYB(domainCode = "TP",
                   elementCode = 5922,
                   itemCode = 1883)
dat2 <- dat$aggregates
names(dat2) <- c("FAOST_CODE","Year","value")
dat2$variable <- "Export value"
df <- rbind(dat1,dat2)



## ---- P3tradeTOPRIGHT ----
# Add region key and subset
dat <- left_join(df,region_key)
dat <- dat[which(dat[[region_to_report]]),]

df <- subgrouping(region_to_report = region_to_report)

# merge data with the region info
dat <- merge(dat,df[c("FAOST_CODE","subgroup")],by="FAOST_CODE")

dat <- dat %>% filter(Year == 2012) %>% group_by(subgroup,variable) %>%
  dplyr::summarise(value = sum(value, na.rm = TRUE)) %>%
  dplyr::mutate(value = value/1000000)

dw <- spread(dat, variable, value)
dw <- dw[order(-dw$'Import value'),]

names(dw) <- c("","Export value", "Import value")

# Only top 4 not to break the pagination (whole table to be changed though!!)
dw <- head(dw, 4)

print.xtable(xtable(dw, caption = " Exports and Imports of food, million US\\$ (2012)", digits = c(0,0,0,0),
                    align= "l{\raggedright\arraybackslash}p{1.0cm}rr"),
             type = "latex", table.placement = NULL, booktabs = TRUE, include.rownames = FALSE,
             size = "footnotesize", caption.placement = "top")




## ---- P3tradeLEFT ----
# data
dat <- syb.df %>% filter(Year %in% c(2000,2012)) %>%  select(FAOST_CODE,Year,TP.IMVAL.FOOD.USD.NO)

dat <- dat[!is.na(dat$TP.IMVAL.FOOD.USD.NO),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

dat <- dat[which(dat[[region_to_report]]),]

dat <- arrange(dat, -Year, -TP.IMVAL.FOOD.USD.NO)
top12 <- dat %>% slice(1:20) %>% dplyr::mutate(color = "2012")
top00 <- dat %>% filter(FAOST_CODE %in% top12$FAOST_CODE, Year == 2000) %>% dplyr::mutate(color = "2000")
dat_plot <- rbind(top12,top00)

p <- ggplot(dat_plot, aes(x=reorder(SHORT_NAME, TP.IMVAL.FOOD.USD.NO),y=TP.IMVAL.FOOD.USD.NO))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="percent")
p <- p + guides(color = guide_legend(nrow = 2))
p

# Caption
caption_text <- "Top food importing countries in 2012"

## ---- P3tradeRIGHT ----

# data
dat <- syb.df %>% filter(Year %in% c(2000,2012)) %>%  select(FAOST_CODE,Year,TP.EXVAL.FOOD.USD.NO)

dat <- dat[!is.na(dat$TP.EXVAL.FOOD.USD.NO),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

dat <- dat[which(dat[[region_to_report]]),]

dat <- arrange(dat, -Year, -TP.EXVAL.FOOD.USD.NO)
top12 <- dat %>% slice(1:20) %>% dplyr::mutate(color = "2012")
top00 <- dat %>% filter(FAOST_CODE %in% top12$FAOST_CODE, Year == 2000) %>% dplyr::mutate(color = "2000")
dat_plot <- rbind(top12,top00)

p <- ggplot(dat_plot, aes(x=reorder(SHORT_NAME, TP.EXVAL.FOOD.USD.NO),y=TP.EXVAL.FOOD.USD.NO))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="percent")
p <- p + guides(color = guide_legend(nrow = 2))
p

# Caption
caption_text <- "Top food exporting countries in 2012"


## ---- P3tradeBOTTOM ----
dat <- syb.df %>% filter(Year %in% 2000:2012) %>%
  select(FAOST_CODE,
         Year,
         TP.EXVAL.CRLS.USD.NO)

# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

dat <- dat[which(dat[[region_to_report]]),]

# DEFAULT GROUPING
df <- subgrouping(region_to_report = region_to_report)

# merge data with the region info
dat <- merge(dat,df[c("FAOST_CODE","subgroup")],by="FAOST_CODE")

# AGREGATE
dat_plot <- dat %>% group_by(subgroup,Year) %>%
  dplyr::summarise(value = sum(TP.EXVAL.CRLS.USD.NO, na.rm=TRUE)) %>%
  dplyr::mutate(value = value/1000000000)

p <- ggplot(data = dat_plot, aes(x = Year, y = value,group=subgroup,color=subgroup))
p <- p + geom_line()
p <- p + scale_color_manual(values = plot_colors(part = 1, length(unique(dat_plot$subgroup)))[["Sub"]])
p <- p + labs(y="billion constant 2005 US$", x="")
p <- p + guides(color = guide_legend(nrow = 2))
p

# Caption
caption_text <- "Exports of cereals"

## ---- P3tradeMAP ----
dat <- syb.df %>% filter(Year %in% 2011) %>% select(FAOST_CODE,
                                                    TI.IMVAL.FOOD.IN.NO)
map.plot <- left_join(map.df,dat) # so that each country in the region will be filled (value/NA)

# Add region key and subset

map.plot <- map.plot[which(map.plot[[region_to_report]]),]

cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","TI.IMVAL.FOOD.IN.NO")]
cat_data$value_cat <- categories(x=cat_data$TI.IMVAL.FOOD.IN.NO, n=5, method="jenks")

map.plot <- left_join(map.plot,cat_data[c("FAOST_CODE","value_cat")])

# define map unit
map_unit <- "index"


create_map_here()

# Caption
caption_text <- "Import value index (2004-2006 = 100, 2011)"
