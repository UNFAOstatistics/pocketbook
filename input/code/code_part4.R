## ---- part4_setup ----

source(paste0(root.dir,'./input/code/plot/plot_color.R'))

syb_part <- 4

## Part 4
colPart4 <- plot_colors(part = syb_part, 12)
col.main1 <- colPart4[["Main"]][1]
## color for the grid
col.main2 <- colPart4[["Main"]][2]

source(paste0(root.dir,"./input/code/plot/theme.R"))

# map functions
source(paste0(root.dir,'./input/code/plot/map_categories.R'))




#
#   _                       _
#  | |     __ _  _ __    __| |
#  | |    / _` || '_ \  / _` |
#  | |___| (_| || | | || (_| |
#  |_____|\__,_||_| |_| \__,_|
#
#



## ---- P4landTEXT ----
spread_title <- "Land"
short_text <- "Land is necessary for sustainable agricultural development, essential ecosystem functions and food security. More than 1.5 billion hectares – about 12 percent of the world’s land area – are used for crop production. Although large amounts of land are potentially suitable for agriculture, much of it is covered by forests, protected for environmental reasons or are part of urban areas. Some 90 percent of agricultural land is in Latin America and sub-Saharan Africa. At the other extreme, there is almost none available for agricultural expansion in Southern Asia, the Western Asia and Northern Africa."


## ---- P4landData ----




## ---- P4landTOPRIGHT ----
dat <- filter(syb.df, Year %in% 2012) %>% select(FAOST_CODE,
                                                 RL.AREA.AGR.HA.SH,
                                                 RL.AREA.FOR.HA.SH,
                                                 RL.AREA.OTH.HA.SH)

# Add region key and subset
dat <- left_join(dat,region_key)
dat <- dat[which(dat[[region_to_report]]),]

dat <- gather(dat, variable, value, 2:4)
dat$fill[dat$variable == "RL.AREA.AGR.HA.SH"] <- "Agricultural"
dat$fill[dat$variable == "RL.AREA.FOR.HA.SH"] <- "Forest"
dat$fill[dat$variable == "RL.AREA.OTH.HA.SH"] <- "Other"

# DEFAULT GROUPING
df <- subgrouping(region_to_report = region_to_report)

# merge data with the region info
dat_plot <- merge(dat,df[c("FAOST_CODE","subgroup")],by="FAOST_CODE")

# AGREGATE
dat_plot <- dat_plot %>% group_by(subgroup,fill) %>% summarise(value  = mean(value, na.rm=TRUE)) %>% ungroup()

# reorder regions by the share of agricultural land
dat_plot$subgroup <- factor(dat_plot$subgroup,
                                  levels=arrange(dat_plot[dat_plot$fill == "Agricultural",],-value)$subgroup )


p <- ggplot(dat_plot, aes(x=subgroup, y=value, fill=fill))
p <- p + geom_bar(stat="identity", position="stack")
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, 3)[["Sub"]])
p <- p + labs(x="",y="percent")
p <- p + theme(axis.text.x = element_text(angle=45))
p

# Caption
caption_text <- "Land area"


## ---- P4landLEFT ----
dat <- syb.df[syb.df$Year %in%  2012 & syb.df$FAOST_CODE < 5000,c("FAOST_CODE","Year","RL.AREA.ARBL.HA.SHP")]

dat <- dat[!is.na(dat$RL.AREA.ARBL.HA.SHP),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

dat <- dat[which(dat[[region_to_report]]),]

# top for this plot
dat <- arrange(dat, -RL.AREA.ARBL.HA.SHP)
top20 <- dat %>% slice(1:20) %>% mutate(color = "2012")
# bottom for the next plot
dat <- arrange(dat, RL.AREA.ARBL.HA.SHP)
bottom20 <- dat %>% slice(1:20) %>% mutate(color = "2012")

dat_plot <- top20

p <- ggplot(dat_plot, aes(x=reorder(SHORT_NAME, RL.AREA.ARBL.HA.SHP),y=RL.AREA.ARBL.HA.SHP))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 1)[["Sub"]])
p <- p + theme(legend.position = "none") # hide legend as only one year plotted
p <- p + coord_flip()
p <- p + labs(x="",y="ha/cap")
p <- p + guides(color = guide_legend(nrow = 2))
p

# Caption
caption_text <- "Arable land per capita, top 20 countries"
if (region_to_report == "RAF") caption_text <- "Arable land per capita, top 20 African countries"
if (region_to_report == "RAP") caption_text <- "Arable land per capita, top 20 Asian and the Pacific countries"
if (region_to_report == "REU") caption_text <- "Arable land per capita, top 20 European and Central Asian countries"
if (region_to_report == "RNE") caption_text <- "Arable land per capita, top 20 North Africa and Near East countries"
if (region_to_report == "GLO") caption_text <- "Arable land per capita, top 20 countries"



## ---- P4landRIGHT ----
dat_plot <- bottom20

p <- ggplot(dat_plot, aes(x=reorder(SHORT_NAME, RL.AREA.ARBL.HA.SHP),y=RL.AREA.ARBL.HA.SHP))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 1)[["Sub"]])
p <- p + theme(legend.position = "none") # hide legend as only one year plotted
p <- p + coord_flip()
p <- p + labs(x="",y="ha/cap")
p <- p + guides(color = guide_legend(nrow = 2))
p

# Caption
caption_text <- "Arable land per capita, bottom 20 countries"
if (region_to_report == "RAF") caption_text <- "Arable land per capita, bottom 20 African countries"
if (region_to_report == "RAP") caption_text <- "Arable land per capita, bottom 20 Asian and the Pacific countries"
if (region_to_report == "REU") caption_text <- "Arable land per capita, bottom 20 European and Central Asian countries"
if (region_to_report == "RNE") caption_text <- "Arable land per capita, bottom 20 North Africa and Near East countries"
if (region_to_report == "GLO") caption_text <- "Arable land per capita, bottom 20 countries"



## ---- P4landBOTTOM ----
dat <- filter(syb.df, Year %in% 2012) %>% select(FAOST_CODE,RL.AREA.ARBL.HA.SH,RL.AREA.PRMNCR.HA.SH,RL.AREA.PRMNMP.HA.SH)

# Add region key and subset
dat <- left_join(dat,region_key)
dat <- dat[which(dat[[region_to_report]]),]

dat <- gather(dat, variable, value, 2:4)
dat$fill[dat$variable == "RL.AREA.ARBL.HA.SH"]   <- "Arable"
dat$fill[dat$variable == "RL.AREA.PRMNCR.HA.SH"] <- "Permanent crops"
dat$fill[dat$variable == "RL.AREA.PRMNMP.HA.SH"] <- "Permanent meadows and pastures"

# DEFAULT GROUPING
df <- subgrouping(region_to_report = region_to_report)

# merge data with the region info
dat_plot <- merge(dat,df[c("FAOST_CODE","subgroup")],by="FAOST_CODE")

# AGREGATE
dat_plot <- dat_plot %>% group_by(subgroup,fill) %>% summarise(value  = mean(value, na.rm=TRUE)) %>% ungroup()

# reorder regions by the share of agricultural land
dat_plot$subgroup <- factor(dat_plot$subgroup,
                                  levels=arrange(dat_plot[dat_plot$fill == "Arable",],-value)$subgroup )

p <- ggplot(dat_plot, aes(x=subgroup, y=value, fill=fill))
p <- p + geom_bar(stat="identity", position="stack")
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, 3)[["Sub"]])
p <- p + labs(x="",y="percent")
p <- p + theme(axis.text.x = element_text(angle=45))
p

# Caption
caption_text <- "Agricultural area"



## ---- P4landMAP ----
dat <- filter(syb.df, Year %in% 2012) %>% select(FAOST_CODE,
                                                                    RL.AREA.ARBLPRMN.HA.SH)

# dat <- dat[dat$FAOST_CODE != 41,]
dat$FAOST_CODE[dat$FAOST_CODE == 41] <- 351

# set Robinson projection
map.plot <- left_join(dat,map.df)

# Subset
map.plot <- map.plot[which(map.plot[[region_to_report]]),]

cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","RL.AREA.ARBLPRMN.HA.SH")]
cat_data$value_cat <- categories(x=cat_data$RL.AREA.ARBLPRMN.HA.SH, n=5)

map.plot <- left_join(map.plot,cat_data[c("FAOST_CODE","value_cat")])

# define map unit
map_unit <- "Percent"

# graticule
grat_robin <- spTransform(graticule, CRS("+proj=robin"))  # reproject graticule
gr_rob <- fortify(grat_robin)
# crop the grid
if (region_to_report != "GLO") {
  gr_rob <- gr_rob[gr_rob$lat >= min(map.plot$lat) & gr_rob$lat <= max(map.plot$lat),]
  gr_rob <- gr_rob[gr_rob$long >= min(map.plot$long) & gr_rob$long <= max(map.plot$long),]
} else gr_rob <- gr_rob

create_map_here()

# Caption
caption_text <- "Cropland per capita, ha/cap"


#
#  __        __          _
#  \ \      / /   __ _  | |_    ___   _ __
#   \ \ /\ / /   / _` | | __|  / _ \ | '__|
#    \ V  V /   | (_| | | |_  |  __/ | |
#     \_/\_/     \__,_|  \__|  \___| |_|
#




## ---- P4waterTEXT ----
spread_title <- "Water"
short_text <- "Global demand for water has risen sharply over the last century. Total annual water withdrawal from agriculture, municipalities and industries rose from less than 580 km\\textsuperscript{3} in 1900 to more than 3 900 km\\textsuperscript{3} in 2010. Agriculture accounts for approximately 70 percent of total freshwater withdrawal in the world, mostly through irrigation. This has been crucial for gains in food production since irrigation reduces drought risk and encourages crop diversification, thus also enhancing rural incomes. While irrigated agriculture represents about 20 percent of the cultivated land, it contributes to 40 percent of global food production."



## ---- P4waterData, cache=TRUE,results='hide', eval=P4water ----
g <- read_excel(paste0(data.dir,"/Data/Raw/UPDATEDWATER_WR_Capita_2000-2010.xlsx"))
names(g) <- c("FAO_TABLE_NAME","Year2000","Year2010")
g <- gather(g, "Year", "per_capita_water_resources", 2:3)
g$Year <- as.character(g$Year)
g$Year[g$Year == "Year2000"] <- 2000
g$Year[g$Year == "Year2010"] <- 2010

g$Year <- factor(g$Year)
g$Year <- as.numeric(levels(g$Year))[g$Year]

# change country names for the merge to succeed
g$FAO_TABLE_NAME[g$FAO_TABLE_NAME == "United Arab Emirates"] <- "the United Arab Emirates"

g$FAO_TABLE_NAME[g$FAO_TABLE_NAME == "Bahamas"] <- "the Bahamas"
g$FAO_TABLE_NAME[g$FAO_TABLE_NAME == "Philippines"] <- "the Philippines"
g$FAO_TABLE_NAME[g$FAO_TABLE_NAME == "Bahamas"] <- "the Bahamas"
g$FAO_TABLE_NAME[g$FAO_TABLE_NAME == "Cape Verde"] <- "Cabo Verde"
#g$FAO_TABLE_NAME[g$FAO_TABLE_NAME == "Côte d'Ivoire"] <- "C\xf4te d'Ivoire"
g$FAO_TABLE_NAME[g$FAO_TABLE_NAME == "Comoros"] <- "the Comoros"
g$FAO_TABLE_NAME[g$FAO_TABLE_NAME == "Czech Republic"] <- "the Czech Republic"
g$FAO_TABLE_NAME[g$FAO_TABLE_NAME == "Democratic Republic of the Congo"] <- "the Democratic Republic of the Congo"
g$FAO_TABLE_NAME[g$FAO_TABLE_NAME == "Dominican Republic"] <- "the Dominican Republic"
g$FAO_TABLE_NAME[g$FAO_TABLE_NAME == "Gambia"] <- "the Gambia"
g$FAO_TABLE_NAME[g$FAO_TABLE_NAME == "Lao People's Democratic Republic"] <- "the Lao People's Democratic Republic"
g$FAO_TABLE_NAME[g$FAO_TABLE_NAME == "Netherlands"] <- "the Netherlands"
g$FAO_TABLE_NAME[g$FAO_TABLE_NAME == "Niger"] <- "the Niger"
g$FAO_TABLE_NAME[g$FAO_TABLE_NAME == "Russian Federation"] <- "the Russian Federation"
g$FAO_TABLE_NAME[g$FAO_TABLE_NAME == "Syrian Arab Republic"] <- "the Syrian Arab Republic"
g$FAO_TABLE_NAME[g$FAO_TABLE_NAME == "United Kingdom"] <- "the United Kingdom of Great Britain and Northern Ireland"
g$FAO_TABLE_NAME[g$FAO_TABLE_NAME == "United Republic of Tanzania"] <- "the United Republic of Tanzania"
g$FAO_TABLE_NAME[g$FAO_TABLE_NAME == "United States of America"] <- "the United States of America"

gg <- left_join(g,region_key)




## ---- P4waterTOPRIGHT, eval=P4water, top_right_plot=P4water, fig.height=top_right_plot_height, fig.width=top_right_plot_width ----
gg <- gg[which(gg[[region_to_report]]),]


# bottom five
bottom_5 <- head(arrange(filter(gg, Year == 2010), per_capita_water_resources),5)
bottom_5_00 <- gg[gg$Year == 2000 & gg$FAOST_CODE %in% unique(bottom_5$FAOST_CODE),]
bottomdata <- rbind(bottom_5,bottom_5_00)

bottomdata$FAO_TABLE_NAME <- factor(bottomdata$FAO_TABLE_NAME, levels=arrange(bottomdata[bottomdata$Year == 2010,], -per_capita_water_resources)$FAO_TABLE_NAME)

p <- ggplot(bottomdata, aes(x=FAO_TABLE_NAME,y=per_capita_water_resources,fill=factor(Year)))
p <- p + geom_bar(stat="identity",position="dodge")
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + labs(x=NULL,y=expression(m^"3"/yr/person))
p <- p + theme(axis.text.x = element_text(angle=45))
p


# Caption
caption_text <- "Countries with the lowest renewable water resources per capita"

## ---- P4waterLEFT ----

dat <- syb.df %>% group_by(FAOST_CODE) %>% dplyr::summarise(AQ.WAT.WWIND.MC.SH = mean(AQ.WAT.WWIND.MC.SH, na.rm=TRUE))
dat$Year <- 2014
names(dat) <- c("FAOST_CODE","new_var","Year")
# remove NA
dat <- dat[!is.na(dat$new_var),]
# China
dat$FAOST_CODE[dat$FAOST_CODE == 357] <- 351
#

dat <- left_join(dat,region_key)
dat <- dat[which(dat[[region_to_report]]),]


dat <- arrange(dat, -new_var)
dat_plot <- dat %>% slice(1:20) %>% mutate(color = "2013")

p <- ggplot(dat_plot, aes(x=reorder(SHORT_NAME, new_var),y=new_var))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 1)[["Sub"]])
p <- p + theme(legend.position = "none") # hide legend as only one year plotted
p <- p + coord_flip()
p <- p + labs(x="",y="percent")
p <- p + guides(color = guide_legend(nrow = 2))
p

# Caption
caption_text <- "Freshwater withdrawal by industrial sector, share of total, highest 20 (1999 to 2013)"


## ---- P4waterRIGHT ----

dat <- syb.df %>% group_by(FAOST_CODE) %>% dplyr::summarise(AQ.WAT.WWAGR.MC.SH = mean(AQ.WAT.WWAGR.MC.SH, na.rm=TRUE))
dat$Year <- 2014
names(dat) <- c("FAOST_CODE","new_var","Year")
# remove NA
dat <- dat[!is.na(dat$new_var),]
# China
dat$FAOST_CODE[dat$FAOST_CODE == 357] <- 351
#

dat <- left_join(dat,region_key)
dat <- dat[which(dat[[region_to_report]]),]


dat <- arrange(dat, -new_var)
dat_plot <- dat %>% slice(1:20) %>% mutate(color = "2013")

p <- ggplot(dat_plot, aes(x=reorder(SHORT_NAME, new_var),y=new_var))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 1)[["Sub"]])
p <- p + theme(legend.position = "none") # hide legend as only one year plotted
p <- p + coord_flip()
p <- p + labs(x="",y="percent")
p <- p + guides(color = guide_legend(nrow = 2))
p

# Caption
caption_text <- "Freshwater withdrawal by agricultural sector, share of total, highest 20 (1999 to 2013)"


## ---- P4waterBOTTOM ----

gg <- gg[which(gg[[region_to_report]]),]

# top ten
top_10 <- head(arrange(filter(gg, Year == 2010), -per_capita_water_resources),10)
top_10_00 <- gg[gg$Year == 2000 & gg$FAOST_CODE %in% unique(top_10$FAOST_CODE),]
topdata <- rbind(top_10,top_10_00)

topdata$FAO_TABLE_NAME <- factor(topdata$FAO_TABLE_NAME, levels=arrange(topdata[topdata$Year == 2010,], -per_capita_water_resources)$FAO_TABLE_NAME)

p <- ggplot(topdata, aes(x=FAO_TABLE_NAME,y=per_capita_water_resources,fill=factor(Year)))
p <- p + geom_bar(stat="identity",position="dodge")
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + labs(x=NULL,y=expression(m^"3"/yr/person))
p <- p + theme(axis.text.x = element_text(angle=45))
p


# Caption
caption_text <- "Countries with the lowest renewable water resources per capita"


## ---- P4waterMAP ----
dat <- syb.df %>% filter(Year %in% c(2007:2012)) %>%
                select(FAOST_CODE,SHORT_NAME,SL.AGR.EMPL.ZS) %>%
                group_by(FAOST_CODE) %>% summarise(SL.AGR.EMPL.ZS = max(SL.AGR.EMPL.ZS, na.rm = TRUE)) %>%
                #filter(!is.na(SL.AGR.EMPL.ZS)) %>%
                ungroup()

water <- syb.df[c("FAOST_CODE","Year","AQ.WAT.RFRWAGR.MC.SH")]
water <- water[!is.na(water$AQ.WAT.RFRWAGR.MC.SH),]
dat <- water %>% group_by(FAOST_CODE) %>% dplyr::summarise(pooled.freshwater = mean(AQ.WAT.RFRWAGR.MC.SH, na.rm = TRUE))

map.plot <- left_join(dat,map.df)

# Add region key and subset

map.plot <- map.plot[which(map.plot[[region_to_report]]),]

cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","pooled.freshwater")]
cat_data$value_cat <- categories(x=cat_data$pooled.freshwater, n=5, method="jenks")

map.plot <- left_join(map.plot,cat_data[c("FAOST_CODE","value_cat")])

# define map unit
map_unit <- "Percent"

# graticule
grat_robin <- spTransform(graticule, CRS("+proj=robin"))  # reproject graticule
gr_rob <- fortify(grat_robin)
# crop the grid
if (region_to_report != "GLO") {
  gr_rob <- gr_rob[gr_rob$lat >= min(map.plot$lat) & gr_rob$lat <= max(map.plot$lat),]
  gr_rob <- gr_rob[gr_rob$long >= min(map.plot$long) & gr_rob$long <= max(map.plot$long),]
} else gr_rob <- gr_rob

create_map_here()

# Caption
caption_text <- "Freshwater resources withdrawn by agriculture (percent, 1999-2013*)"


#   _____
#  | ____|  _ __     ___   _ __    __ _   _   _
#  |  _|   | '_ \   / _ \ | '__|  / _` | | | | |
#  | |___  | | | | |  __/ | |    | (_| | | |_| |
#  |_____| |_| |_|  \___| |_|     \__, |  \__, |
#                                 |___/   |___/



## ---- P4energyTEXT ----
spread_title <- "Energy"
short_text <- "Energy is an important input for the agri-food chain and is used to power agricultural machinery, heat greenhouses, power irrigation systems, but also to manufacture equipment, fertilizers, pesticides and other agro-chemicals. The amount of energy consumed by agriculture is increasing worldwide as mechanization, especially in developing countries, increases. At the same time agriculture produces energy in the form of bioenergy. Bioenergy production increased sharply over the last years to meet the new demand for liquid biofuels for transport (e.g., ethanol and biodiesel) and solid biomass for power such as pellets or wood chips."



## ---- P4energyData ----






## ---- P4energyTOPRIGHT ----

plot(cars)
# Caption
caption_text <- "Countries with the lowest renewable water resources per capita"


## ---- P4energyLEFT ----

plot(cars)
# Caption
caption_text <- "Freshwater withdrawal by industrial sector, share of total, highest 20 (1999 to 2013)"


## ---- P4energyRIGHT ----

plot(cars)
# Caption
caption_text <- "Freshwater withdrawal by agricultural sector, share of total, highest 20 (1999 to 2013)"


## ---- P4energyBOTTOM ----

plot(cars)
# Caption
caption_text <- "Countries with the lowest renewable water resources per capita"


## ---- P4energyMAP ----

plot(cars)
# Caption
caption_text <- "Freshwater resources withdrawn by agriculture (percent, 1999-2013*)"


#   _____                               _
#  |  ___|   ___    _ __    ___   ___  | |_   _ __   _   _
#  | |_     / _ \  | '__|  / _ \ / __| | __| | '__| | | | |
#  |  _|   | (_) | | |    |  __/ \__ \ | |_  | |    | |_| |
#  |_|      \___/  |_|     \___| |___/  \__| |_|     \__, |
#                                                    |___/

## ---- P4forestryTEXT ----
spread_title <- "Forestry"
short_text <- "Forests make vital contributions to biodiversity. They also sustain a range of economic activities and act as a source of food, medicine and fuel for more than a billion people. The latest estimate of the world’s total forest area is more than 4 billion hectares, corresponding to about 30 percent of total land area. But today forests face unprecedented pressures. Changes in land cover have caused the most pressing environmental issue in recent decades. The impact of deforestation and land use intensification, especially on soil degradation, have been significant."


## ---- P4forestryData ----


## ---- P4forestryTOPRIGHT ----

dat <- syb.df %>% select(FAOST_CODE,Year,
                         FO.PRD.RP.M3.NO,
                         FO.PRD.WP.M3.NO,
                         FO.PRD.PPB.M3.NO) %>%
  mutate(FO.PRD.RP.M3.NO = FO.PRD.RP.M3.NO / 1000000,
         FO.PRD.WP.M3.NO = FO.PRD.WP.M3.NO / 1000000,
         FO.PRD.PPB.M3.NO = FO.PRD.PPB.M3.NO / 1000000)


# Add region key and subset
dat <- left_join(dat,region_key)
dat <- dat[which(dat[[region_to_report]]),]

dat <- gather(dat, variable, value, 3:5)
dat$fill[dat$variable == "FO.PRD.RP.M3.NO"] <- "Recovered paper"
dat$fill[dat$variable == "FO.PRD.WP.M3.NO"] <- "Wood pulp"
dat$fill[dat$variable == "FO.PRD.PPB.M3.NO"] <- "Paper and paperboard"

dat <- dat[!is.na(dat$value),]

# DEFAULT GROUPING
df <- subgrouping(region_to_report = region_to_report)

# merge data with the region info
dat_plot <- merge(dat,df[c("FAOST_CODE","subgroup")],by="FAOST_CODE")

# AGREGATE
dat_plot <- dat_plot %>% group_by(Year,fill) %>%
          summarise(value  = sum(value, na.rm=TRUE)) %>%  ungroup()

p <- ggplot(dat_plot, aes(x=Year, y=value, color=fill))
p <- p + geom_line()
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 3)[["Sub"]])
p <- p + labs(x="",y="million tonnes")
p <- p + theme(axis.text.x = element_text(angle=45))
p <- p + guides(color = guide_legend(nrow = 2))
p

# Caption
caption_text <- "Production of selected forest products"


## ---- P4forestryLEFT ----
dat <- syb.df %>%  filter(Year %in%  2012) %>%  select(FAOST_CODE,Year,FO.EXVAL.TOT.USD.NO) %>%
  mutate(FO.EXVAL.TOT.USD.NO = FO.EXVAL.TOT.USD.NO / 1000000 )

dat <- dat[!is.na(dat$FO.EXVAL.TOT.USD.NO),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

dat <- dat[which(dat[[region_to_report]]),]

# top for this plot
dat <- arrange(dat, -FO.EXVAL.TOT.USD.NO)
dat_plot <- dat %>% slice(1:20) %>% mutate(color = "2012")


p <- ggplot(dat_plot, aes(x=reorder(SHORT_NAME, FO.EXVAL.TOT.USD.NO),y=FO.EXVAL.TOT.USD.NO))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 1)[["Sub"]])
p <- p + theme(legend.position = "none") # hide legend as only one year plotted
p <- p + coord_flip()
p <- p + labs(x="",y="billion US$")
p

# Caption
caption_text <- "Top 20 exporters of forest products (2012)"


## ---- P4forestryRIGHT ----

dat <- syb.df %>%  filter(Year %in%  2012) %>%  select(FAOST_CODE,Year,FO.IMVAL.TOT.USD.NO) %>%
  mutate(FO.IMVAL.TOT.USD.NO = FO.IMVAL.TOT.USD.NO / 1000000 )

dat <- dat[!is.na(dat$FO.IMVAL.TOT.USD.NO),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

dat <- dat[which(dat[[region_to_report]]),]

# top for this plot
dat <- arrange(dat, -FO.IMVAL.TOT.USD.NO)
dat_plot <- dat %>% slice(1:20) %>% mutate(color = "2012")


p <- ggplot(dat_plot, aes(x=reorder(SHORT_NAME, FO.IMVAL.TOT.USD.NO),y=FO.IMVAL.TOT.USD.NO))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 1)[["Sub"]])
p <- p + theme(legend.position = "none") # hide legend as only one year plotted
p <- p + coord_flip()
p <- p + labs(x="",y="billion US$")
p

# Caption
caption_text <- "Top 20 importers of forest products (2012)"


## ---- P4forestryBOTTOM ----
dat <- syb.df %>%  filter(Year %in% 2010) %>% select(FAOST_CODE,
                                             GFRA.TOT.PF.HA.NO,
                                             GFRA.TOT.PLF.HA.NO,
                                             GFRA.TOT.ONRF.HA.NO) %>%
  mutate(GFRA.TOT.PF.HA.NO   = GFRA.TOT.PF.HA.NO   / 1000000,
         GFRA.TOT.PLF.HA.NO  = GFRA.TOT.PLF.HA.NO  / 1000000,
         GFRA.TOT.ONRF.HA.NO = GFRA.TOT.ONRF.HA.NO / 1000000)

# Add region key and subset
dat <- left_join(dat,region_key)
dat <- dat[which(dat[[region_to_report]]),]

dat <- gather(dat, variable, value, 2:4)
dat$fill[dat$variable == "GFRA.TOT.PF.HA.NO"] <- "primary forest"
dat$fill[dat$variable == "GFRA.TOT.PLF.HA.NO"] <- "planted forest"
dat$fill[dat$variable == "GFRA.TOT.ONRF.HA.NO"] <- "other naturally regenerated forest"

dat <- dat[!is.na(dat$value),]

# DEFAULT GROUPING
df <- subgrouping(region_to_report = region_to_report)

# merge data with the region info
dat_plot <- merge(dat,df[c("FAOST_CODE","subgroup")],by="FAOST_CODE")

# AGREGATE
dat_plot <- dat_plot %>% group_by(subgroup,fill) %>%
          summarise(value  = sum(value, na.rm=TRUE)) %>% ungroup()

# reorder regions by the share of agricultural land
dat_plot$subgroup <- factor(dat_plot$subgroup,
                              levels=arrange(dat_plot[dat_plot$fill == "primary forest",],-value)$subgroup )

p <- ggplot(dat_plot, aes(x=subgroup, y=value, fill=fill))
p <- p + geom_bar(stat="identity", position="stack")
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, 3)[["Sub"]])
p <- p + labs(x="",y="percent")
p <- p + theme(axis.text.x = element_text(angle=45))
p

# Caption
caption_text <- "Forest characteristics (2010)"


## ---- P4forestryMAP ----
dat <- filter(syb.df, Year %in% 2012) %>% select(FAOST_CODE, RL.AREA.FOR.HA.SH)

# dat <- dat[dat$FAOST_CODE != 41,]
dat$FAOST_CODE[dat$FAOST_CODE == 41] <- 351

# set Robinson projection
map.plot <- left_join(dat,map.df)

# Subset
map.plot <- map.plot[which(map.plot[[region_to_report]]),]

cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","RL.AREA.FOR.HA.SH")]
cat_data$value_cat <- categories(x=cat_data$RL.AREA.FOR.HA.SH, n=5)

map.plot <- left_join(map.plot,cat_data[c("FAOST_CODE","value_cat")])

# define map unit
map_unit <- "percent"

# graticule
grat_robin <- spTransform(graticule, CRS("+proj=robin"))  # reproject graticule
gr_rob <- fortify(grat_robin)
# crop the grid
if (region_to_report != "GLO") {
gr_rob <- gr_rob[gr_rob$lat >= min(map.plot$lat) & gr_rob$lat <= max(map.plot$lat),]
gr_rob <- gr_rob[gr_rob$long >= min(map.plot$long) & gr_rob$long <= max(map.plot$long),]
} else gr_rob <- gr_rob

create_map_here()

# Caption
caption_text <- "Forest area as share of total land area"


#    ____   _   _                       _                     _
#   / ___| | | (_)  _ __ ___     __ _  | |_    ___      ___  | |__     __ _   _ __     __ _    ___
#  | |     | | | | | '_ ` _ \   / _` | | __|  / _ \    / __| | '_ \   / _` | | '_ \   / _` |  / _ \
#  | |___  | | | | | | | | | | | (_| | | |_  |  __/   | (__  | | | | | (_| | | | | | | (_| | |  __/
#   \____| |_| |_| |_| |_| |_|  \__,_|  \__|  \___|    \___| |_| |_|  \__,_| |_| |_|  \__, |  \___|
#                                                                                     |___/

## ---- P4climateTEXT ----
spread_title <- "Climate change"
short_text <- "The severity and speed of climate change is presenting an unprecedented challenge. Current global surface temperatures are now about 0.6 degrees Celsius higher than the average for the last century. This increase is consistent with model predictions of the effects of rising atmospheric concentrations of carbon dioxide (CO\\textsubscript{2}) and other GHGs, which are a result of human activity. The poorest and most food-insecure regions around the globe are the most vulnerable. Already scarce land and water resources will likely become even more scarce, and insufficient technical and financial means will make adaptation to a changing climate very difficult."


## ---- P4climateData ----


## ---- P4climateTOPRIGHT ----
dat <- filter(syb.df, Year %in% 2000:2012) %>% select(FAOST_CODE,Year,
                                                 GHG.TOT.ALL.GG.NO)

dat <- dat[!is.na(dat$GHG.TOT.ALL.GG.NO),]
# Add region key and subset
dat <- left_join(dat,region_key)
dat <- dat[which(dat[[region_to_report]]),]

# DEFAULT GROUPING
df <- subgrouping(region_to_report = region_to_report)

# merge data with the region info
dat_plot <- merge(dat,df[c("FAOST_CODE","subgroup")],by="FAOST_CODE")

# AGREGATE
dat_plot <- dat_plot %>% group_by(Year,subgroup) %>%
  summarise(GHG.TOT.ALL.GG.NO = sum(GHG.TOT.ALL.GG.NO, na.rm=TRUE)) %>%
  mutate(GHG.TOT.ALL.GG.NO = GHG.TOT.ALL.GG.NO /1000)

p <- ggplot(dat_plot, aes(x=Year, y=GHG.TOT.ALL.GG.NO, color=subgroup))
p <- p + geom_line()
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, 3)[["Sub"]])
p <- p + labs(x="",y=expression("thousand gigagrams CO"[2] * "eq"))
p <- p + theme(axis.text.x = element_text(angle=45))
p

# Caption
caption_text <- "Greenhouse gas emissions in agriculture"


## ---- P4climateLEFT ----
dat <- syb.df %>% filter(Year %in% c(2000,2012)) %>%  select(FAOST_CODE,Year,GHG.TOT.ALL.GG.NO) %>%
  mutate(GHG.TOT.ALL.GG.NO = GHG.TOT.ALL.GG.NO / 1000)

dat <- dat[!is.na(dat$GHG.TOT.ALL.GG.NO),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

dat <- dat[which(dat[[region_to_report]]),]

dat <- arrange(dat, -Year, -GHG.TOT.ALL.GG.NO)
top12 <- dat %>% slice(1:20) %>% mutate(color = "2012")
top00 <- dat %>% filter(FAOST_CODE %in% top12$FAOST_CODE, Year == 2000) %>% mutate(color = "2000")
dat_plot <- rbind(top12,top00)

p <- ggplot(dat_plot, aes(x=reorder(SHORT_NAME, GHG.TOT.ALL.GG.NO),y=GHG.TOT.ALL.GG.NO))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y=expression("Mt CO"[2] * "eq"))
p <- p + guides(color = guide_legend(nrow = 2))
p

# Caption
caption_text <- "Greehouse gas emissions in agriculture, highest 20 countries in 2012"



## ---- P4climateRIGHT ----
dat <- syb.df %>% filter(Year %in% c(2000,2012)) %>%  select(FAOST_CODE,Year,GL.LU.TOT.NERCO2EQ.NO) %>%
  mutate(GL.LU.TOT.NERCO2EQ.NO = GL.LU.TOT.NERCO2EQ.NO / 1000)

dat <- dat[!is.na(dat$GL.LU.TOT.NERCO2EQ.NO),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

dat <- dat[which(dat[[region_to_report]]),]

dat <- arrange(dat, -Year, -GL.LU.TOT.NERCO2EQ.NO)
top12 <- dat %>% slice(1:20) %>% mutate(color = "2012")
top00 <- dat %>% filter(FAOST_CODE %in% top12$FAOST_CODE, Year == 2000) %>% mutate(color = "2000")
dat_plot <- rbind(top12,top00)

p <- ggplot(dat_plot, aes(x=reorder(SHORT_NAME, GL.LU.TOT.NERCO2EQ.NO),y=GL.LU.TOT.NERCO2EQ.NO))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y=expression("Mt CO"[2] * "eq"))
p <- p + guides(color = guide_legend(nrow = 2))
p

# Caption
caption_text <- "Land use total emissions, highest 20 countries in 2012"



## ---- P4climateBOTTOM ----
dat <- filter(syb.df, Year %in% 2012) %>% select(FAOST_CODE,
                                                 GHG.TOT.ALL.GG.NO,
                                                 GL.FL.NFC.NERCO2EQ.NO,
                                                 GLI.CHPF.TOT.ECO2EQ.NO,
                                                 GHG.BS.TECO2EQ.GG.NO,
                                                 GL.FL.F.NERCO2EQ.NO)

# Add region key and subset
dat <- left_join(dat,region_key)
dat <- dat[which(dat[[region_to_report]]),]

dat <- gather(dat, variable, value, 2:6)
dat$fill[dat$variable == "GHG.TOT.ALL.GG.NO"]   <- "All GHG agricultural sectors"
dat$fill[dat$variable == "GL.FL.NFC.NERCO2EQ.NO"] <- "Net forest conversion"
dat$fill[dat$variable == "GLI.CHPF.TOT.ECO2EQ.NO"] <- "Cultivation histoils and peat fires"
dat$fill[dat$variable == "GHG.BS.TECO2EQ.GG.NO"] <- "Burning savanna"
dat$fill[dat$variable == "GL.FL.F.NERCO2EQ.NO"] <- "Forest"


# AGREGATE
dat_plot <- dat %>% group_by(fill) %>% summarise(value  = mean(value, na.rm=TRUE) / 1000) %>% ungroup()


p <- ggplot(dat_plot, aes(x=reorder(fill, -value), y=value, fill=fill))
p <- p + geom_bar(stat="identity", position="dodge")
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, 5)[["Sub"]])
p <- p + labs(x="",y=expression("thousand gigagrams CO"[2] * "eq"))
p <- p + theme(axis.text.x = element_text(angle=45))
p

# Caption
caption_text <- "Emissions by subsectors in 2012 - figure differs, check!!"



## ---- P4climateMAP ----
dat <- filter(syb.df, Year %in% 2012) %>% select(FAOST_CODE,GHG.AFOLU.TOT.ECO2EQ.NO)

# dat <- dat[dat$FAOST_CODE != 41,]
dat$FAOST_CODE[dat$FAOST_CODE == 41] <- 351

# set Robinson projection
map.plot <- left_join(dat,map.df)

# Subset
map.plot <- map.plot[which(map.plot[[region_to_report]]),]

cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","GHG.AFOLU.TOT.ECO2EQ.NO")]
cat_data$value_cat <- categories(x=cat_data$GHG.AFOLU.TOT.ECO2EQ.NO, n=5)

map.plot <- left_join(map.plot,cat_data[c("FAOST_CODE","value_cat")])

# define map unit
map_unit <- expression("thousand gigagrams CO"[2] * "eq")

# graticule
grat_robin <- spTransform(graticule, CRS("+proj=robin"))  # reproject graticule
gr_rob <- fortify(grat_robin)
# crop the grid
if (region_to_report != "GLO") {
  gr_rob <- gr_rob[gr_rob$lat >= min(map.plot$lat) & gr_rob$lat <= max(map.plot$lat),]
  gr_rob <- gr_rob[gr_rob$long >= min(map.plot$long) & gr_rob$long <= max(map.plot$long),]
} else gr_rob <- gr_rob

create_map_here()

# Caption
caption_text <- "Total greenhouse gas emissions from agriculture, forestry and other land use, gigagrams CO2 eq (2012)"
