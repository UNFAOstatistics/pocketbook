## ---- part1_setup ----
source(paste0(root.dir,'./input/code/plot/plot_color.R'))

syb_part <- 1

## Part 1
colPart1 <- plot_colors(part = syb_part, 12)
col.main1 <- colPart1[["Main"]][1]
## color for the grid
col.main2 <- colPart1[["Main"]][2]

source(paste0(root.dir,"./input/code/plot/theme.R"))

nCol <- 5
colPart = plot_colors(part = syb_part, 12)
mapColFun = colorRampPalette(c("white", colPart[["Main"]][1]))
tmpCol = mapColFun(nCol)[2]
mapColFun = colorRampPalette(c(tmpCol, colPart[["Main"]][1]))
mapColors = mapColFun(nCol)

# map functions
source(paste0(root.dir,'./input/code/plot/map_categories.R'))



#    ___                                  _
#   / _ \  __   __   ___   _ __  __   __ (_)   ___  __      __
#  | | | | \ \ / /  / _ \ | '__| \ \ / / | |  / _ \ \ \ /\ / /
#  | |_| |  \ V /  |  __/ | |     \ V /  | | |  __/  \ V  V /
#   \___/    \_/    \___| |_|      \_/   |_|  \___|   \_/\_/
#



## ---- P1overTEXT ----
spread_title <- "Overview"
short_text <- "A combination of declining mortality rates, prolonged life expectancy and younger populations in regions with high fertility contributes to population growth in the world. While growth rates have been slowing since the late 1960s, the world’s population has nevertheless doubled since then, to over 7 billion people. Population growth is generally highest where income levels are low. This is especially true in cities. Since 2008, there have been more people living in cities than in rural areas."


## ---- P1overData ----
# Retrieve data
# library(FAOSTAT)
# dat <- getFAOtoSYB(domainCode = "OA",
#                    elementCode = 551,
#                    itemCode = 3010)
# dat1 <- dat$aggregates
# dat <- getFAOtoSYB(domainCode = "OA",
#                    elementCode = 561,
#                    itemCode = 3010)
# dat2 <- dat$aggregates
# dat <- left_join(dat1,dat2)
# df <- gather(dat, variable, value, 3:4)



## ---- P1overTOPRIGHT ----


# If you could aggrate the population by summing up the countries you would do it like this
# dat <- df %>% select(FAOST_CODE,Year,variable,value)
# dat <- left_join(dat,region_key)
# dat <- dat[which(dat[[region_to_report]]),]
# dat <- dat %>% group_by(Year,variable) %>%  summarise(value = sum(value, na.rm=TRUE)/1000000)

# But as you cant in the case of population at least, we need to use the specific aggregates from FAOSTAT

dat <- syb.df %>% select(FAOST_CODE,Year,OA.TPU.POP.PPL.NO,OA.TPR.POP.PPL.NO)
dat <- dat[!is.na(dat$OA.TPR.POP.PPL.NO),]
dat <- left_join(dat,region_key)


if (region_to_report == "REU")  dat <- dat %>% filter(FAOST_CODE %in% c(5400, # Europe
                                                                        5301 # Central Asia
                                                                        ))
if (region_to_report == "RAF")  dat <- dat %>% filter(FAOST_CODE %in% c(5100 # Africa
                                                                        ))
if (region_to_report == "RNE")  dat <- dat[which(dat[[region_to_report]]),]
if (region_to_report == "RAP")  dat <- dat[which(dat[[region_to_report]]),]

dat <- gather(dat, variable, value, 3:4)

dat$variable <- as.character(dat$variable)
dat$variable[dat$variable == "OA.TPR.POP.PPL.NO"] <- "Rural population"
dat$variable[dat$variable == "OA.TPU.POP.PPL.NO"] <- "Urban population"

dat <- dat %>% group_by(Year,variable) %>%  summarise(value = sum(value, na.rm=TRUE)/1000000000)

# print data for technical report
#datatable(dat)

# Draw the plot
p <- ggplot(dat, aes(x = Year, y = value))
p <- p + geom_area(aes(fill=variable), stat = "identity",position = "stack")
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + theme(axis.text.x = element_text(angle = 45))
p <- p + labs(x="",y="billion people")
p

# Caption

caption_text <- "rural and urban population"
if (region_to_report == "RAF") caption_text <- "Africa rural and urban population"
if (region_to_report == "RAP") caption_text <- "Asia and Pacific rural and urban population"
if (region_to_report == "REU") caption_text <- "Europe and Central Asia rural and urban population"
if (region_to_report == "RNE") caption_text <- "North Africa and Near East rural and urban population"
if (region_to_report == "GLO") caption_text <- "World rural and urban population"



## ---- P1overLEFT ----
# data
dat <- filter(syb.df, Year %in%
                   c(2004:2014)) %>%
                  group_by(FAOST_CODE,SHORT_NAME) %>%
                  dplyr::summarise(OA.TPBS.POP.PPL.GR10 = mean(OA.TPBS.POP.PPL.GR10, na.rm=TRUE))
dat <- ungroup(dat)
dat <- dat[!is.na(dat$OA.TPBS.POP.PPL.GR10),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[which(dat[[region_to_report]]),]

dat <- arrange(dat, -OA.TPBS.POP.PPL.GR10)
top10 <- dat %>% slice(1:10) %>% mutate(color = "Countries with highest values")
bot10 <- dat %>% slice( (nrow(dat)-9):nrow(dat)) %>% mutate(color = "Countries with lowest values")
dat_plot <- rbind(top10,bot10)

p <- ggplot(dat_plot, aes(x=reorder(SHORT_NAME, OA.TPBS.POP.PPL.GR10),y=OA.TPBS.POP.PPL.GR10))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="percent")
p <- p + guides(color = guide_legend(nrow = 2))
p

# Caption
caption_text <- "Population, average annual growth (2004-2014)"


## ---- P1overRIGHT ----

# data
dat <- syb.df %>% filter(Year %in% 2013) %>% select(FAOST_CODE,SP.DYN.LE00.IN)
dat <- dat[!is.na(dat$SP.DYN.LE00.IN),]

# Add region key and subset
dat <- left_join(dat,region_key)
dat <- dat[which(dat[[region_to_report]]),]

dat <- arrange(dat, -SP.DYN.LE00.IN)
top10 <- dat %>% slice(1:10) %>% mutate(color = "Countries with highest values")
bot10 <- dat %>% slice( (nrow(dat)-9):nrow(dat)) %>% mutate(color = "Countries with lowest values")
dat_plot <- rbind(top10,bot10)

p <- ggplot(dat_plot, aes(x=reorder(SHORT_NAME, SP.DYN.LE00.IN),y=SP.DYN.LE00.IN))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="percent")
p <- p + guides(color = guide_legend(nrow = 2))
p

# Caption
caption_text <- "Life expectancy at birth, countries with the highest and lowest values (2013)"

## ---- P1overBOTTOM ----
# data
dat <- syb.df %>% filter(Year %in% c(2000:2014)) %>%
  select(FAOST_CODE,Year,SHORT_NAME,OA.TEAPT.POP.PPL.NO)
dat <- dat[!is.na(dat$OA.TEAPT.POP.PPL.NO),]
dat <- dat[!is.na(dat$SHORT_NAME),]

# DEFAULT GROUPING
df <- subgrouping(region_to_report = region_to_report)

# merge data with the region info
dat <- merge(dat,df[c("FAOST_CODE","subgroup")],by="FAOST_CODE")

# AGREGATE
dat_plot <- dat %>% group_by(subgroup,Year) %>%
  summarise(OA.TEAPT.POP.PPL.NO = sum(OA.TEAPT.POP.PPL.NO, na.rm=TRUE)) %>%
  mutate(OA.TEAPT.POP.PPL.NO = OA.TEAPT.POP.PPL.NO / 1000000)

p <- ggplot(dat_plot, aes(x=Year,y=OA.TEAPT.POP.PPL.NO,color=subgroup))
p <- p + geom_point() + geom_line()
p <- p + scale_color_manual(values=plot_colors(part = syb_part, length(unique(dat_plot$subgroup)))[["Sub"]])
p <- p + labs(x="",y="million people")
p <- p + guides(color = guide_legend(nrow = 3))
p

# Caption
caption_text <- "Total economically active population (2000 to 2014)"




## ---- P1overMAP ----
dat <- syb.df %>% filter(Year %in% 2014, FAOST_CODE < 5000) %>% select(FAOST_CODE,SHORT_NAME,OA.TPR.POP.PPL.SHP)

map.plot <- left_join(dat,map.df)

# Add region key and subset

map.plot <- map.plot[which(map.plot[[region_to_report]]),]

cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","OA.TPR.POP.PPL.SHP")]
cat_data$value_cat <- categories(x=cat_data$OA.TPR.POP.PPL.SHP, n=5, method="jenks")

map.plot <- left_join(map.plot,cat_data[c("FAOST_CODE","value_cat")])

# define map unit
map_unit <- "Percent"

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
caption_text <- "Rural population, share of total population (2014)"



#   _____
#  | ____|   ___    ___    _ __     ___    _ __ ___    _   _
#  |  _|    / __|  / _ \  | '_ \   / _ \  | '_ ` _ \  | | | |
#  | |___  | (__  | (_) | | | | | | (_) | | | | | | | | |_| |
#  |_____|  \___|  \___/  |_| |_|  \___/  |_| |_| |_|  \__, |
#                                                      |___/
#


## ---- P1econTEXT ----
spread_title <- "Economy"
short_text <- "While some sectors have been hard hit, agriculture has demonstrated resilience during the recent economic downturn.  Changes in the wider economy, including growing global integration, affect the performance of the agriculture sector.  Higher overall economic growth also raises consumers’ incomes and hence food demand. Changing interest rates influence capital investments, land values and storage levels, while inflation affects input prices, revenues and credit costs. Fluctuations in exchange rates have an important bearing on international competitiveness and trade flows."


## ---- P1econTOPRIGHT ----
dat <- filter(syb.df, Year %in% 2013) %>% select(FAOST_CODE,
                                                 NV.AGR.TOTL.ZS,
                                                 NV.IND.TOTL.ZS,
                                                 NV.SRV.TETC.ZS)

# Add region key and subset
dat <- left_join(dat,region_key)
dat <- dat[which(dat[[region_to_report]]),]

dat <- gather(dat, variable, value, 2:4)
dat$fill[dat$variable == "NV.AGR.TOTL.ZS"] <- "Agriculture"
dat$fill[dat$variable == "NV.IND.TOTL.ZS"] <- "Indurstry"
dat$fill[dat$variable == "NV.SRV.TETC.ZS"] <- "Services"


# DEFAULT GROUPING
df <- subgrouping(region_to_report = region_to_report)

# merge data with the region info
dat_plot <- merge(dat,df[c("FAOST_CODE","subgroup")],by="FAOST_CODE")

# AGREGATE
dat_plot <- dat_plot %>% group_by(subgroup,fill) %>% summarise(value  = mean(value, na.rm=TRUE)) %>% ungroup()

# reorder regions by the share of agricultural land
dat_plot$subgroup <- factor(dat_plot$subgroup,
                                  levels=arrange(dat_plot[dat_plot$fill == "Agriculture",],-value)$subgroup )

p <- ggplot(dat_plot, aes(x=subgroup, y=value, fill=fill))
p <- p + geom_bar(stat="identity", position="stack")
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, 3)[["Sub"]])
p <- p + labs(x="",y="percent")
p <- p + theme(axis.text.x = element_text(angle=45))
p

# Caption
caption_text <- "Value added in agriculture, industry and services, share of GDP (2013)"


## ---- P1econLEFT ----
dat <- syb.df[syb.df$Year %in%  2003:2013 & syb.df$FAOST_CODE < 5000,c("FAOST_CODE","Year","SHORT_NAME","EA.PRD.AGRI.KD")]

dat <- dat[!is.na(dat$EA.PRD.AGRI.KD),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

#and subset
dat <- dat[which(dat[[region_to_report]]),]

# top for this plot
dat_plot <- dat %>% group_by(SHORT_NAME) %>% dplyr::filter(Year == max(Year)) %>% ungroup() %>% arrange(-EA.PRD.AGRI.KD) %>% slice(1:20) %>% mutate(color = "2013")

p <- ggplot(dat_plot, aes(x=reorder(SHORT_NAME, EA.PRD.AGRI.KD),y=EA.PRD.AGRI.KD))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 1)[["Sub"]])
p <- p + theme(legend.position = "none") # hide legend as only one year plotted
p <- p + coord_flip()
p <- p + labs(x="",y="US$")
p <- p + guides(color = guide_legend(nrow = 2))
p

# Caption
caption_text <- "Agriculture value added per worker, countries with the highest values (2003-2013*)"

## ---- P1econRIGHT ----

# data
dat <- syb.df %>% filter(Year %in% c(2003:2013)) %>% select(FAOST_CODE,SHORT_NAME,Year,NV.AGR.TOTL.KD)
dat <- dat[!is.na(dat$NV.AGR.TOTL.KD),]
dat <- dat[!is.na(dat$SHORT_NAME),]
# Add region key and subset
dat <- left_join(dat,region_key)
dat <- dat[which(dat[[region_to_report]]),]

top10 <- dat %>% arrange(FAOST_CODE,Year) %>%
              group_by(FAOST_CODE) %>% dplyr::mutate(Growth=c(NA,exp(diff(log(NV.AGR.TOTL.KD)))-1)) %>%
              #ddply("FAOST_CODE",transform,Growth=c(NA,exp(diff(log(NV.AGR.TOTL.KD)))-1)) %>%
              group_by(SHORT_NAME) %>%
              dplyr::summarise(growth_NV.AGR.TOTL.KD = mean(Growth, na.rm = TRUE)*100) %>%
              arrange(-growth_NV.AGR.TOTL.KD) %>%
              slice(1:10) %>%
              mutate(color = "Countries with highest values")

bot10 <- dat %>% arrange(FAOST_CODE,Year) %>%
              group_by(FAOST_CODE) %>% dplyr::mutate(Growth=c(NA,exp(diff(log(NV.AGR.TOTL.KD)))-1)) %>%
              group_by(SHORT_NAME) %>%
              dplyr::summarise(growth_NV.AGR.TOTL.KD = mean(Growth, na.rm = TRUE)*100) %>%
              arrange(growth_NV.AGR.TOTL.KD) %>%
              slice(1:10) %>%
              mutate(color = "Countries with lowest values")
dat_plot <- rbind(top10,bot10)

p <- ggplot(dat_plot, aes(x=reorder(SHORT_NAME, growth_NV.AGR.TOTL.KD),y=growth_NV.AGR.TOTL.KD))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="percent")
p <- p + guides(color = guide_legend(nrow = 2))
p

# Caption
caption_text <- "Value added in agriculture, average annual growth (2003-2013)"


## ---- P1econBOTTOM_data ----
# data
# Constant GDP from World Bank
library(WDI)
dl <- WDI(indicator = c("NY.GDP.MKTP.KD","iso3Code"), start=2000, end=2013)
names(dl)[names(dl)=="year"] <- "Year"
dl <- merge(dl,FAOcountryProfile[c("ISO2_WB_CODE","FAOST_CODE","UNSD_MACRO_REG_CODE","UNSD_SUB_REG_CODE")],
            by.x="iso2c",by.y="ISO2_WB_CODE",all.x=TRUE)
dl <- na.omit(dl)

# nominator from syb FAOSTAT
nomin <- syb.df[c("FAOST_CODE","SHORT_NAME","Year","NV.AGR.TOTL.KD")]




## ---- P1econBOTTOM ----

# DEFAULT GROUPING
df <- subgrouping(region_to_report = region_to_report)

# merge data with the region info
dat <- merge(dl,df[c("FAOST_CODE","subgroup")],by="FAOST_CODE")

#dat_plot <- dat %>% group_by(subgroup,Year) %>% dplyr::summarise(constant_gdp = sum(NY.GDP.MKTP.KD,na.rm=TRUE))

dat <- merge(nomin,dat,by=c("FAOST_CODE","Year"))

dat_plot <- dat %>%  group_by(subgroup,Year) %>%
    summarise(constant_gdp    = sum(NY.GDP.MKTP.KD,na.rm=TRUE),
              agr_value_added = sum(NV.AGR.TOTL.KD,na.rm=TRUE)) %>%
    mutate(share = agr_value_added/constant_gdp*100) %>%
    ungroup() %>%
    arrange(-share)

p <- ggplot(data = dat_plot, aes(x = Year, y = share,group=subgroup,color=subgroup))
p <- p + geom_line()
p <- p + scale_color_manual(values = plot_colors(part = 1, length(unique(dat_plot$subgroup)))[["Sub"]])
p <- p + labs(y="percent", x="")
p <- p + guides(color = guide_legend(nrow = 2))
p

# Caption
caption_text <- "Value added in agriculture as share of GDP"

## ---- P1econMAP ----
dat <- syb.df %>% filter(Year %in% c(2010:2013), FAOST_CODE < 5000) %>%
                select(FAOST_CODE,SHORT_NAME,NV.AGR.TOTL.ZS) %>%
                group_by(FAOST_CODE) %>% dplyr::summarise(NV.AGR.TOTL.ZS = max(NV.AGR.TOTL.ZS)) %>%
                ungroup()

map.plot <- left_join(dat,map.df)

# Add region key and subset

map.plot <- map.plot[which(map.plot[[region_to_report]]),]

cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","NV.AGR.TOTL.ZS")]
cat_data$value_cat <- categories(x=cat_data$NV.AGR.TOTL.ZS, n=5, method="jenks")

map.plot <- left_join(map.plot,cat_data[c("FAOST_CODE","value_cat")])

# define map unit
map_unit <- "Percent"

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
caption_text <- "Value added in agriculture, share of GDP (percent, 2010 to 2013*)"





#   _               _
#  | |       __ _  | |__     ___    _   _   _ __
#  | |      / _` | | '_ \   / _ \  | | | | | '__|
#  | |___  | (_| | | |_) | | (_) | | |_| | | |
#  |_____|  \__,_| |_.__/   \___/   \__,_| |_|
#


## ---- P1laboTEXT ----
spread_title <- "Labour"
short_text <- "A strong labour market is the foundation of sustained well-being and economic growth, inclusion and social cohesion. Therefore access to safe, productive and remunerated work is essential. Yet many workers, especially the most vulnerable, do not enter into formal wage employment but are instead self-employed or participate in unpaid family work, such as in agriculture. This is especially the case with subsistence farming. As a large share of the working poor are involved in agriculture, developments in this sector have a major impact on welfare."


## ---- P1laboTOPRIGHT, eval=P1labo, top_right_plot=P1labo, fig.height=top_right_plot_height, fig.width=top_right_plot_width ----
dat <- filter(syb.df, Year %in% 2013) %>% select(FAOST_CODE,
                                                 SL.TLF.CACT.MA.ZS,
                                                 SL.TLF.CACT.FE.ZS,
                                                 OA.TPBS.POP.PPL.NO)

# Add region key and subset
dat <- left_join(dat,region_key)
dat <- dat[which(dat[[region_to_report]]),]

dat <- gather(dat, variable, value, 2:3)
dat$fill[dat$variable == "SL.TLF.CACT.MA.ZS"] <- "Male"
dat$fill[dat$variable == "SL.TLF.CACT.FE.ZS"] <- "Female"

# DEFAULT GROUPING
df <- subgrouping(region_to_report = region_to_report)

# merge data with the region info
dat <- merge(dat,df[c("FAOST_CODE","subgroup")],by="FAOST_CODE")

# AGREGATE
dat <- dat[!is.na(dat$OA.TPBS.POP.PPL.NO),]
dat_plot <- dat %>% group_by(subgroup,fill) %>% summarise(value = weighted.mean(value, OA.TPBS.POP.PPL.NO, na.rm=TRUE)) %>% ungroup()

# reorder
dat_plot$subgroup <- factor(dat_plot$subgroup,
                                  levels=arrange(dat_plot[dat_plot$fill == "Female",],-value)$subgroup )

p <- ggplot(dat_plot, aes(x=subgroup, y=value, fill=fill))
p <- p + geom_bar(stat="identity", position="dodge")
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + labs(x="",y="percent")
p <- p + theme(axis.text.x = element_text(angle=45))
p

# Caption
caption_text <- "Labour force participation rate by gender, ages 15+ (2013)"


## ---- P1laboLEFT ----
dat <- syb.df[syb.df$Year %in%  2003:2013 ,c("FAOST_CODE","Year","SHORT_NAME","SL.AGR.EMPL.FE.ZS")]

dat <- dat[!is.na(dat$SL.AGR.EMPL.FE.ZS),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

#and subset
dat <- dat[which(dat[[region_to_report]]),]

dat_plot <- dat %>% group_by(SHORT_NAME) %>% dplyr::filter(Year == max(Year)) %>% ungroup() %>% arrange(-SL.AGR.EMPL.FE.ZS) %>% slice(1:20) %>% mutate(color = "2013")

p <- ggplot(dat_plot, aes(x=reorder(SHORT_NAME, SL.AGR.EMPL.FE.ZS),y=SL.AGR.EMPL.FE.ZS))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 1)[["Sub"]])
p <- p + theme(legend.position = "none") # hide legend as only one year plotted
p <- p + coord_flip()
p <- p + labs(x="",y="percent")
p <- p + guides(color = guide_legend(nrow = 2))
p

# Caption
caption_text <- "Female employment in agriculture, share of female employment (2003-2013*)"

## ---- P1laboRIGHT ----
dat <- syb.df[syb.df$Year %in%  2003:2013 ,c("FAOST_CODE","Year","SHORT_NAME","SL.AGR.EMPL.MA.ZS")]

dat <- dat[!is.na(dat$SL.AGR.EMPL.MA.ZS),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

#and subset
dat <- dat[which(dat[[region_to_report]]),]

dat_plot <- dat %>% group_by(SHORT_NAME) %>% dplyr::filter(Year == max(Year)) %>% ungroup() %>% arrange(-SL.AGR.EMPL.MA.ZS) %>% slice(1:20) %>% mutate(color = "2013")


p <- ggplot(dat_plot, aes(x=reorder(SHORT_NAME, SL.AGR.EMPL.MA.ZS),y=SL.AGR.EMPL.MA.ZS))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 1)[["Sub"]])
p <- p + theme(legend.position = "none") # hide legend as only one year plotted
p <- p + coord_flip()
p <- p + labs(x="",y="percent")
p <- p + guides(color = guide_legend(nrow = 2))
p

# Caption
caption_text <- "Male employment in agriculture, share of Male employment (2003 - 2013*)"


## ---- P1laboBOTTOM_data ----
# data

# Retrieve data
library(FAOSTAT)
dat <- getFAOtoSYB(domainCode = "OA", # male economically active population
                   elementCode = 592,
                   itemCode = 3010)
dat1 <- dat$aggregates
dat <- getFAOtoSYB(domainCode = "OA", # female economically active population
                   elementCode = 593,
                   itemCode = 3010)
dat2 <- dat$aggregates
dat <- getFAOtoSYB(domainCode = "OA", # male economically active population IN AGRICULTURE
                   elementCode = 602,
                   itemCode = 3010)
dat3 <- dat$aggregates
dat <- getFAOtoSYB(domainCode = "OA", # female economically active population IN AGRICULTURE
                   elementCode = 603,
                   itemCode = 3010)
dat4 <- dat$aggregates
dat <- left_join(dat1,dat2)
dat <- left_join(dat,dat3)
datx <- left_join(dat,dat4)





## ---- P1laboBOTTOM ----
dat <- datx %>%  filter(Year %in% 2000:2014) %>% select(-OA_3010_592,-OA_3010_602)

# DEFAULT GROUPING
df <- subgrouping(region_to_report = region_to_report)

# merge data with the region info
dat <- merge(dat,df[c("FAOST_CODE","subgroup")],by="FAOST_CODE")

dat_plot <- dat %>% group_by(subgroup,Year) %>% summarise(sum_593 = sum(OA_3010_593, na.rm=TRUE),
                                                          sum_603 = sum(OA_3010_603, na.rm=TRUE)) %>%
                                                mutate(share = sum_603 / sum_593 * 100) %>%
                                                ungroup()

p <- ggplot(data = dat_plot, aes(x = Year, y = share,group=subgroup,color=subgroup))
p <- p + geom_line()
p <- p + scale_color_manual(values = plot_colors(part = 1, length(unique(dat_plot$subgroup)))[["Sub"]])
p <- p + labs(y="percent", x="")
p <- p + guides(color = guide_legend(nrow = 2))
p

# Caption
caption_text <- "Female employment in agriculture, share of female employment (2000-2014)"

## ---- P1laboMAP ----
dat <- syb.df %>% filter(Year %in% c(2007:2012)) %>%
                select(FAOST_CODE,SHORT_NAME,SL.AGR.EMPL.ZS) %>%
                group_by(FAOST_CODE) %>% summarise(SL.AGR.EMPL.ZS = max(SL.AGR.EMPL.ZS, na.rm = TRUE)) %>%
                #filter(!is.na(SL.AGR.EMPL.ZS)) %>%
                ungroup()

map.plot <- left_join(dat,map.df)

# Add region key and subset

map.plot <- map.plot[which(map.plot[[region_to_report]]),]

cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","SL.AGR.EMPL.ZS")]
cat_data$value_cat <- categories(x=cat_data$SL.AGR.EMPL.ZS, n=5, method="jenks")

map.plot <- left_join(map.plot,cat_data[c("FAOST_CODE","value_cat")])

# define map unit
map_unit <- "Percent"

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
caption_text <- "Employment in agriculture, share of total employment"



#   ___                           _
#  |_ _|  _ __    _ __    _   _  | |_   ___
#   | |  | '_ \  | '_ \  | | | | | __| / __|
#   | |  | | | | | |_) | | |_| | | |_  \__ \
#  |___| |_| |_| | .__/   \__,_|  \__| |___/
#                |_|
#



## ---- P1inputTEXT ----
spread_title <- "Inputs"
short_text <- "Adequate access to inputs, including land, pesticides and fertilizers, is vital for agricultural production and growth. Throughout Asia and in parts of Latin America, expanding seed and fertilizer use has been accompanied by investments in irrigation, rural roads, marketing infrastructure and financial services, paving the way for dynamic commercial input markets. In other regions, such as sub-Saharan Africa, the uptake of agricultural inputs is relatively low because it is often cheaper to expand cropland to have higher production."


## ---- P1inputData ----


## ---- P1inputTOPRIGHT ----
dat <- filter(syb.df, Year %in% 2002:2012) %>% select(FAOST_CODE,
                                                      Year,
                                                 RF.FERT.NI.TN.NO,
                                                 RF.FERT.PH.TN.NO,
                                                 RF.FERT.PO.TN.NO,
                                                 RL.AREA.ARBLPRMN.HA.NO)

# Add region key and subset
dat <- left_join(dat,region_key)
dat <- dat[which(dat[[region_to_report]]),]

dat <- gather(dat, variable, value, 3:5)
dat$fill[dat$variable == "RF.FERT.NI.TN.NO"] <- "Nitrogen"
dat$fill[dat$variable == "RF.FERT.PH.TN.NO"] <- "Phosphate"
dat$fill[dat$variable == "RF.FERT.PO.TN.NO"] <- "Potash"

# DEFAULT GROUPING
df <- subgrouping(region_to_report = region_to_report)

# merge data with the region info
dat_plot <- merge(dat,df[c("FAOST_CODE","subgroup")],by="FAOST_CODE")

# AGREGATE
dat_plot <- dat_plot %>% group_by(Year,fill) %>%
              summarise(value  = sum(value, na.rm=TRUE)*1000,
                        area  = sum(RL.AREA.ARBLPRMN.HA.NO, na.rm=TRUE)) %>%
              mutate(share = value / area) %>% ungroup()

p <- ggplot(dat_plot, aes(x=Year, y=share, fill=fill))
p <- p + geom_area(stat="identity", position="stack")
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, 3)[["Sub"]])
p <- p + labs(x="",y="kg/haś")
p <- p + scale_x_continuous(breaks=c(2002,2004,2006,2008,2010,2012))
p

# Caption
caption_text <- "Fertilizer consumption in nutrients per ha of arable land (2002 to 2012)"


## ---- P1inputLEFT ----
dat <- syb.df[syb.df$Year %in%  2012 & syb.df$FAOST_CODE < 5000,c("FAOST_CODE","Year","RF.FERT.NI.TN.SH")]

dat <- dat[!is.na(dat$RF.FERT.NI.TN.SH),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

dat <- dat[which(dat[[region_to_report]]),]

# top for this plot
dat <- arrange(dat, -RF.FERT.NI.TN.SH)
top20 <- dat %>% slice(1:20) %>% mutate(color = "2012")


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
caption_text <- "Nitrogen fertilizers consumption in nutrients per ha of arable land (2012)"


## ---- P1inputRIGHT ----

dat <- syb.df[syb.df$Year %in%  2012 & syb.df$FAOST_CODE < 5000,c("FAOST_CODE","Year","RF.FERT.PH.TN.SH")]

dat <- dat[!is.na(dat$RF.FERT.PH.TN.SH),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

dat <- dat[which(dat[[region_to_report]]),]

# top for this plot
dat <- arrange(dat, -RF.FERT.PH.TN.SH)
dat_plot <- dat %>% slice(1:20) %>% mutate(color = "2012")


p <- ggplot(dat_plot, aes(x=reorder(SHORT_NAME, RF.FERT.PH.TN.SH),y=RF.FERT.PH.TN.SH))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 1)[["Sub"]])
p <- p + theme(legend.position = "none") # hide legend as only one year plotted
p <- p + coord_flip()
p <- p + labs(x="",y="kg/ha")
p <- p + guides(color = guide_legend(nrow = 2))
p

# Caption
caption_text <- "Phosphate fertilizers consumption in nutrients per ha of arable land (2012)"


## ---- P1inputBOTTOM ----
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
              summarise(value  = sum(value, na.rm=TRUE)*1000,
                        area  = sum(RL.AREA.ARBLPRMN.HA.NO, na.rm=TRUE)) %>%
              mutate(share = value / area) %>% mutate(sum = sum(share)) %>%  ungroup()

# reorder regions by the share of agricultural land
dat_plot$subgroup <- factor(dat_plot$subgroup, levels=unique(arrange(dat_plot, -sum)$subgroup))

p <- ggplot(dat_plot, aes(x=subgroup, y=share, fill=fill))
p <- p + geom_bar(stat="identity", position="stack")
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, 3)[["Sub"]])
p <- p + labs(x="",y="kg/ha")
p <- p + theme(axis.text.x = element_text(angle=45))
p

# Caption
caption_text <- "Fertilizer consumption in nutrients per ha of arable land (2012)"


## ---- P1inputMAP ----
dat <- filter(syb.df, Year %in% 2007:2012) %>% select(FAOST_CODE, Year, RP.PEST.TOT.TN.SH) %>%  mutate(RP.PEST.TOT.TN.SH = RP.PEST.TOT.TN.SH*1000)

dat <- dat[!is.na(dat$RP.PEST.TOT.TN.SH),]
  
dat <- dat %>% group_by(FAOST_CODE) %>% dplyr::filter(Year == max(Year)) %>% ungroup()


# dat <- dat[dat$FAOST_CODE != 41,]
dat$FAOST_CODE[dat$FAOST_CODE == 41] <- 351

# set Robinson projection
map.plot <- left_join(dat,map.df)

# Subset
map.plot <- map.plot[which(map.plot[[region_to_report]]),]

cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","RP.PEST.TOT.TN.SH")]
cat_data$value_cat <- categories(x=cat_data$RP.PEST.TOT.TN.SH, n=5)

map.plot <- left_join(map.plot,cat_data[c("FAOST_CODE","value_cat")])

# define map unit
map_unit <- "g/ha"

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
caption_text <- "Pesticides per ha of arable land (kg/ha, 2007 to 2012*)"

#   ___                                _                                _
#  |_ _|  _ __   __   __   ___   ___  | |_   _ __ ___     ___   _ __   | |_
#   | |  | '_ \  \ \ / /  / _ \ / __| | __| | '_ ` _ \   / _ \ | '_ \  | __|
#   | |  | | | |  \ V /  |  __/ \__ \ | |_  | | | | | | |  __/ | | | | | |_
#  |___| |_| |_|   \_/    \___| |___/  \__| |_| |_| |_|  \___| |_| |_|  \__|
#


## ---- P1investTEXT ----
spread_title <- "Investments"
short_text <- "Investing in agriculture is one of the most effective strategies for reducing poverty and hunger, and promoting sustainability. The regions of the world where hunger and extreme poverty are most widespread today – South Asia and sub-Saharan Africa – have seen flat or declining rates of investment per worker in agriculture over the past thirty years. Farmers tend to be the largest investors in developing country agriculture, and therefore their investment decisions are paramount for any strategy aimed at improving agricultural investment."


## ---- P1investData ----


## ---- P1investTOPRIGHT ----

dat <- read_excel(paste0(data.dir,"/database/Data/Raw/Stat Pocketbook_Investment ODA 09 Sep 2015.xlsx"), sheet=1)
dat <- dat[1:3]
names(dat) <- c("Year","agriculture_broad","agriculture_narrow")

dat$Year <- factor(dat$Year)
dat$Year <- as.numeric(levels(dat$Year))[dat$Year]

dat <- dat[dat$Year >= 1995,]

dat <- gather(dat, variable, value, 2:3)
dat$variable <- as.character(dat$variable)
dat$variable[dat$variable == "agriculture_narrow"] <- "Agriculture, narrow"
dat$variable[dat$variable == "agriculture_broad"] <- "Agriculture, broad"

# print data for technical report
#datatable(dat)
dat_plot <- dat

# Draw the plot
p <- ggplot(dat, aes(x = Year, y = value, color=variable))
p <- p + geom_line()
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + labs(x="",y="percent")
p

# Caption
caption_text <- "Aid flows to agriculture, share of total aid (1995-2013)"

## ---- P1investLEFT ----
# data
gg <- read.csv(paste0(data.dir, "/Data/Raw/credit_to_agriculture.csv"))
gg <- gg[gg$ElementName == "Value US$",]
gg <- gg[gg$ItemName == "Total Credit",]
# into millions
names(gg)[names(gg)=="AreaCode"] <- "FAOST_CODE"

dat1 <- gg %>%  filter(Year %in% c(1999:2001)) %>% group_by(FAOST_CODE) %>% dplyr::summarise(value = mean(Value, na.rm=TRUE)/1000000) %>%
  mutate(Year = 2000)
dat2 <- gg %>%  filter(Year %in% c(2010:2012)) %>% group_by(FAOST_CODE) %>% dplyr::summarise(value = mean(Value, na.rm=TRUE)/1000000) %>%
  mutate(Year = 2011)

dat <- rbind(dat1,dat2)

dat <- left_join(dat,region_key)

dat <- dat[which(dat[[region_to_report]]),]

dat <- arrange(dat, -Year, -value)
top2010 <- dat %>% slice(1:20) %>% mutate(color = "2010−2012")
top2000 <- dat %>% filter(FAOST_CODE %in% top2010$FAOST_CODE, Year == 2000) %>% mutate(color = "1999−2001")
dat_plot <- rbind(top2010,top2000)

p <- ggplot(dat_plot, aes(x=reorder(SHORT_NAME, value),y=value))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="kcal/cap/day")
p <- p + guides(color = guide_legend(nrow = 1))
p

# Caption
caption_text <- "Total credit to agriculture, top 20 countries in 2014 (2000 and 2012)"

## ---- P1investRIGHT ----

# data
gg <- read_excel(paste0(data.dir,"/Data/Raw/investments/Lowest and Top 20 AOI GEA_final_Stat Pocketbook.xlsx"))
gg <- gg[c(3,5)]
gg$Year <- 2010
names(gg)[names(gg)=="AOI average (2008-2012)"] <- "agri_orientation_index"
names(gg)[names(gg)=="countrycode"] <- "FAOST_CODE"


# Add region key and subset
dat <- left_join(gg,region_key)
dat <- dat[which(dat[[region_to_report]]),]

dat <- arrange(dat, -agri_orientation_index)
top10 <- dat %>% slice(1:10) %>% mutate(color = "Countries with highest values")
bot10 <- dat %>% slice( (nrow(dat)-9):nrow(dat)) %>% mutate(color = "Countries with lowest values")
dat_plot <- rbind(top10,bot10)

p <- ggplot(dat_plot, aes(x=reorder(SHORT_NAME, agri_orientation_index),y=agri_orientation_index))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="index")
p <- p + guides(color = guide_legend(nrow = 2))
p

# Caption
caption_text <- "Agri-Orientation Index, highest and lowest values (average 2008-2012)"


## ---- P1investBOTTOM ----
# data
dat <- read_excel(paste0(data.dir,"/Data/Raw/InvestmentDataforStatPocketbook_28May2015.xlsx"), sheet=2, skip=3)
dat <- dat[1:3]
names(dat) <- c("Year","Bilateral","Multilateral")


dat$Year <- str_replace_all(dat$Year, "\\*", "")

dat$Year <- factor(dat$Year)
dat$Year <- as.numeric(levels(dat$Year))[dat$Year]

dat <- dat[dat$Year >= 1995,]
dat <- dat[!is.na(dat$Year),]

dat <- gather(dat, variable, value, 2:3)
dat$variable <- as.character(dat$variable)

dat_plot <- dat

p <- ggplot(dat_plot, aes(x=Year, y=value, fill=variable))
p <- p + geom_area(stat="identity", position="stack")
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + labs(x="",y="million constant US$")
p

# Caption
caption_text <- "Aid flows to agriculture, broad (1995-2013) - NO COUNTRY LEVEL DATA???"

## ---- p1investMAPdata ----
dat <- getFAOtoSYB(domainCode = "IG",
                   elementCode = 6111,
                   itemCode = 23101)
dat <- dat[["aggregates"]]
dat <- dat %>% filter(Year %in% 2008:2012) %>% group_by(FAOST_CODE) %>% mutate(maxyear = max(Year)) %>% ungroup () %>% filter(Year == maxyear)




## ---- P1investMAP ----
map.plot <- right_join(dat,map.df)

# Add region key and subset

map.plot <- map.plot[which(map.plot[[region_to_report]]),]

cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","IG_23101_6111")]
cat_data$value_cat <- categories(x=cat_data$IG_23101_6111, n=5, method="jenks")

map.plot <- left_join(map.plot,cat_data[c("FAOST_CODE","value_cat")])

# define map unit
map_unit <- "Percent"

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
caption_text <- "Rural population, share of total population (2014)"
