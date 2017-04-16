## ---- part4_setup ----

source(paste0(root.dir,'/input/code/plot/plot_color.R'))

syb_part <- 4

## Part 4
colPart4 <- plot_colors(part = syb_part, 12)
col.main1 <- colPart4[["Main"]][1]
## color for the grid
col.main2 <- colPart4[["Main"]][2]

source(paste0(root.dir,"/input/code/plot/theme.R"))

# map functions
source(paste0(root.dir,'/input/code/plot/map_categories.R'))

#   _                       _
#  | |     __ _  _ __    __| |
#  | |    / _` || '_ \  / _` |
#  | |___| (_| || | | || (_| |
#  |_____|\__,_||_| |_| \__,_|
#

## ---- P4landTEXT ----
spread_title <- "Land"
if (region_to_report == "RAF") short_text <- "Land is necessary for sustainable agricultural development, essential ecosystem functions and food security. More than 1.5 billion hectares – about 12 percent of the world’s land area – are used for crop production. Although large amounts of land are potentially suitable for agriculture, much of it is covered by forests, protected for environmental reasons or is part of urban areas. Some 90 percent of agricultural land is in Latin America and sub-Saharan Africa. At the other extreme, there is almost none available for agricultural expansion in Southern Asia, the Western Asia and Northern Africa."
if (region_to_report == "RAP") short_text <- "Land is necessary for sustainable agricultural development, essential ecosystem functions and food security. More than 1.5 billion hectares – about 12 percent of the world’s land area – are used for crop production. Although large amounts of land are potentially suitable for agriculture, much of it is covered by forests, protected for environmental reasons or are part of urban areas. Some 90 percent of agricultural land is in Latin America and sub-Saharan Africa. At the other extreme, there is almost none available for agricultural expansion in Southern Asia, the Western Asia and Northern Africa."
if (region_to_report == "REU") short_text <- "The Europe and Central Asia region accounts for 21 percent of the world’s land area and 16 percent of its agricultural land. In the region agricultural land makes about 30\\% of the total land area, of which about 45 percent is used for crop production and the remaining part is used as permanent meadows and pastures. The country in the region with the highest share of agricultural land is Kazakhstan, with 77.5 percent of its 270 million hectares used for farming and stock-raising."
if (region_to_report == "RNE") short_text <- "Land is necessary for sustainable agricultural development, essential ecosystem functions and food security. More than 1.5 billion hectares – about 12 percent of the world’s land area – are used for crop production. The agricultural in the region makes 25 to more than 50 percent of the total area, but it has, however, a very small share of arable land while the rest is permanent meadows and pastures. Sudan has the higher value of arable land per capita while Gulf State, in general, have the smallest values."
if (region_to_report == "GLO") short_text <- "Land is necessary for sustainable agricultural development, essential ecosystem functions and food security. More than 1.5 billion hectares – about 12 percent of the world’s land area – are used for crop production. Although large amounts of land are potentially suitable for agriculture, much of it is covered by forests, protected for environmental reasons or are part of urban areas. Some 90 percent of agricultural land is in Latin America and sub-Saharan Africa. At the other extreme, there is almost none available for agricultural expansion in Southern Asia, the Western Asia and Northern Africa."
if (rulang) spread_title <- "Земля"
if (region_to_report == "REU" & rulang) short_text <- "В регионе Европы и Центральной Азии находится 21 процент мировой площади земли и 16 процентов мировых сельскохозяйственных угодий. Сельскохозяйственные угодья в регионе составляют около 30 процентов от общей площади земель, из которых около 45 процентов используется для производства сельскохозяйственных культур, а остальная часть в качестве постоянных лугов и пастбищ. Казахстан – самая большая страна в регионе по площади сельскохозяйственных угодий: 77,5 процентов от 270 млн гектаров используется для земледелия и животноводства."


## ---- P4landData ----




## ---- P4landTOPRIGHT ----
if (region_to_report == "RAF") dat <- syb.df %>% filter(Year %in% 2014, FAOST_CODE %in% 12001:12005) %>%
  select(SHORT_NAME,
         RL.AREA.AGR.HA.SH,
         RL.AREA.FOR.HA.SH,
         RL.AREA.OTH.HA.SH)
if (region_to_report == "RAP") dat <- syb.df %>% filter(Year %in% 2014, FAOST_CODE %in% 13001:13014) %>%
  select(SHORT_NAME,
         RL.AREA.AGR.HA.SH,
         RL.AREA.FOR.HA.SH,
         RL.AREA.OTH.HA.SH)
if (region_to_report == "REU") dat <- syb.df %>% filter(Year %in% 2014, FAOST_CODE %in% 14001:14007) %>%
  select(SHORT_NAME,
         RL.AREA.AGR.HA.SH,
         RL.AREA.FOR.HA.SH,
         RL.AREA.OTH.HA.SH)
if (region_to_report == "RNE") dat <- syb.df %>% filter(Year %in% 2014, FAOST_CODE %in% 15001:15003) %>%
  select(SHORT_NAME,
         RL.AREA.AGR.HA.SH,
         RL.AREA.FOR.HA.SH,
         RL.AREA.OTH.HA.SH)
if (region_to_report == "GLO") dat <- syb.df %>% filter(Year %in% 2014, FAOST_CODE %in% c(5100,5200,5300,5400,5500)) %>%
  select(SHORT_NAME,
         RL.AREA.AGR.HA.SH,
         RL.AREA.FOR.HA.SH,
         RL.AREA.OTH.HA.SH)
dat <- na.omit(dat)


dat <- gather(dat, variable, value, 2:4)


dat$fill[dat$variable == "RL.AREA.AGR.HA.SH"] <- "Agricultural"
dat$fill[dat$variable == "RL.AREA.FOR.HA.SH"] <- "Forest"
dat$fill[dat$variable == "RL.AREA.OTH.HA.SH"] <- "Other"
dat$fill <- factor(dat$fill, levels=c("Agricultural",
                                      "Forest",
                                      "Other"))


dat$value <- dat$value * 100 # into percents

dat_plot <- dat

dat_plot$SHORT_NAME <- factor(dat_plot$SHORT_NAME, levels=(dat_plot %>% filter(fill == "Agricultural") %>% arrange(-value))$SHORT_NAME)

if (rulang){
  dat_plot$fill <- as.character(dat_plot$fill)
  dat_plot$fill[dat_plot$fill == "Agricultural"] <- "Сельскохозяйственные \nземли"
  dat_plot$fill[dat_plot$fill == "Forest"] <- "Леса"
  dat_plot$fill[dat_plot$fill == "Other"] <- "Другие \nземли"
  dat_plot$fill <- factor(dat_plot$fill, levels=c("Сельскохозяйственные \nземли",
                                                  "Леса",
                                                  "Другие \nземли"
  ))
  dat_plot$SHORT_NAME <- translate_subgroups(dat_plot$SHORT_NAME, isfactor = TRUE, add_row_breaks = TRUE)
} 



p <- ggplot(dat_plot, aes(x=SHORT_NAME, y=value, fill=fill))
p <- p + geom_bar(stat="identity", position="stack")
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, 3)[["Sub"]])
p <- p + labs(x="",y="percent\n")
if (rulang) p <- p + labs(x="",y="проценты\n")
p <- p + theme(axis.text.x = element_text(angle=45))
p <- p + coord_cartesian(ylim=c(0,100))
if (rulang) p <- p + guides(fill = guide_legend(nrow = 2))
p

# Caption
caption_text <- "Land area (2014)"
if (rulang) caption_text <- "Земельные площади (2014 г.)"


## ---- P4landLEFT ----
dat <- syb.df[syb.df$Year %in%  2014 & syb.df$FAOST_CODE < 5000,c("FAOST_CODE","Year","RL.AREA.ARBL.HA.SHP")]

dat <- dat[!is.na(dat$RL.AREA.ARBL.HA.SHP),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

dat <- dat[which(dat[[region_to_report]]),]

# top for this plot
dat <- arrange(dat, -RL.AREA.ARBL.HA.SHP)
if (region_to_report == "RNE") top20 <- dat %>% slice(1:10) %>% dplyr::mutate(color = "2012")
if (region_to_report != "RNE") top20 <- dat %>% slice(1:20) %>% dplyr::mutate(color = "2012")
# bottom for the next plot
dat <- arrange(dat, RL.AREA.ARBL.HA.SHP)
if (region_to_report == "RNE") bottom20 <- dat %>% slice(1:10) %>% dplyr::mutate(color = "2012")
if (region_to_report != "RNE") bottom20 <- dat %>% slice(1:20) %>% dplyr::mutate(color = "2012")


dat_plot <- top20

if (rulang) dat_plot$SHORT_NAME <- countrycode.multilang::countrycode(dat_plot$FAOST_CODE, origin = "fao", destination = "country.name.russian.fao")

dat_plot$SHORT_NAME <- fct_reorder(dat_plot$SHORT_NAME, dat_plot$RL.AREA.ARBL.HA.SHP) 

p <- ggplot(dat_plot, aes(x=SHORT_NAME,y=RL.AREA.ARBL.HA.SHP))
p <- p + geom_segment(aes(y = min(dat_plot$RL.AREA.ARBL.HA.SHP), xend = SHORT_NAME, 
                          yend = RL.AREA.ARBL.HA.SHP, color=color), alpha=.5)
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75) + theme(panel.grid.major.y = element_blank())
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 1)[["Sub"]])
p <- p + theme(legend.position = "none") # hide legend as only one year plotted
p <- p + coord_flip()
p <- p + labs(x="",y="\n\nha/cap")
if (rulang) p <- p + labs(x="",y="\nга на душу населения")
p <- p + guides(color = guide_legend(nrow = 2))
p

# Caption
caption_text <- paste("Arable land per capita, top",nrow(dat_plot),"countries (2014)")
if (rulang) caption_text <- paste("Пахотные земли на душу населения,",nrow(dat_plot),"стран с самыми высокими значениями (2014 г.)")


## ---- P4landRIGHT ----
dat_plot <- bottom20

if (rulang) dat_plot$SHORT_NAME <- countrycode.multilang::countrycode(dat_plot$FAOST_CODE, origin = "fao", destination = "country.name.russian.fao")

dat_plot$SHORT_NAME <- fct_reorder(dat_plot$SHORT_NAME, dat_plot$RL.AREA.ARBL.HA.SHP) 

p <- ggplot(dat_plot, aes(x=SHORT_NAME,y=RL.AREA.ARBL.HA.SHP))
p <- p + geom_segment(aes(y = min(dat_plot$RL.AREA.ARBL.HA.SHP), xend = SHORT_NAME, 
                          yend = RL.AREA.ARBL.HA.SHP, color=color), alpha=.5)
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75) + theme(panel.grid.major.y = element_blank())
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 1)[["Sub"]])
p <- p + theme(legend.position = "none") # hide legend as only one year plotted
p <- p + coord_flip()
p <- p + labs(x="",y="\n\nha/cap")
if (rulang) p <- p + labs(x="",y="\nга на душу населения")
p <- p + guides(color = guide_legend(nrow = 2))
p

# Caption
caption_text <- paste("Arable land per capita, bottom",nrow(dat_plot),"countries (2014)")
if (rulang) caption_text <- paste("Пахотные земли на душу населения,",nrow(dat_plot),"стран с самыми низкими значениями (2014 г.)")



## ---- P4landBOTTOM ----

if (region_to_report == "RAF") dat <- syb.df %>% filter(Year %in% 2014, FAOST_CODE %in% 12001:12005) %>%
  select(SHORT_NAME,
         RL.AREA.ARBL.HA.SH,
         RL.AREA.PRMNCR.HA.SH,
         RL.AREA.PRMNMP.HA.SH)
if (region_to_report == "RAP") dat <- syb.df %>% filter(Year %in% 2014, FAOST_CODE %in% 13001:13014) %>%
  select(SHORT_NAME,
         RL.AREA.ARBL.HA.SH,
         RL.AREA.PRMNCR.HA.SH,
         RL.AREA.PRMNMP.HA.SH)
if (region_to_report == "REU") dat <- syb.df %>% filter(Year %in% 2014, FAOST_CODE %in% 14001:14007) %>%
  select(SHORT_NAME,
         RL.AREA.ARBL.HA.SH,
         RL.AREA.PRMNCR.HA.SH,
         RL.AREA.PRMNMP.HA.SH)
if (region_to_report == "RNE") dat <- syb.df %>% filter(Year %in% 2014, FAOST_CODE %in% 15001:15003) %>%
  select(SHORT_NAME,
         RL.AREA.ARBL.HA.SH,
         RL.AREA.PRMNCR.HA.SH,
         RL.AREA.PRMNMP.HA.SH)
if (region_to_report == "GLO") dat <- syb.df %>% filter(Year %in% 2014, FAOST_CODE %in% c(5100,5200,5300,5400,5500)) %>%
  select(SHORT_NAME,
         RL.AREA.ARBL.HA.SH,
         RL.AREA.PRMNCR.HA.SH,
         RL.AREA.PRMNMP.HA.SH)



dat <- na.omit(dat)

dat <- gather(dat, variable, value, 2:4)
dat$fill[dat$variable == "RL.AREA.ARBL.HA.SH"]   <- "Arable"
dat$fill[dat$variable == "RL.AREA.PRMNCR.HA.SH"] <- "Permanent crops"
dat$fill[dat$variable == "RL.AREA.PRMNMP.HA.SH"] <- "Permanent meadows and pastures"
dat$fill <- factor(dat$fill, levels=c("Arable",
                                      "Permanent crops",
                                      "Permanent meadows and pastures"))

dat$value <- dat$value * 100 # into percents

dat_plot <- dat

dat_plot$SHORT_NAME <- factor(dat_plot$SHORT_NAME, levels=(dat_plot %>% filter(fill == "Arable") %>% arrange(-value))$SHORT_NAME)

if (rulang){
  dat_plot$SHORT_NAME <- translate_subgroups(dat_plot$SHORT_NAME, isfactor = TRUE, add_row_breaks =  TRUE)
  dat_plot$fill <- as.character(dat_plot$fill)
  dat_plot$fill[dat_plot$fill == "Arable"]   <- "Пахотные \nземли"
  dat_plot$fill[dat_plot$fill == "Permanent crops"]   <- "Многолетние \nсельскохозяйственные культуры"
  dat_plot$fill[dat_plot$fill == "Permanent meadows and pastures"]   <- "Постоянные луга \nи пастбища"
  dat_plot$fill <- factor(dat_plot$fill, levels=c("Пахотные \nземли",
                                                  "Многолетние \nсельскохозяйственные культуры",
                                                  "Постоянные луга \nи пастбища"))
}


p <- ggplot(dat_plot, aes(x=SHORT_NAME, y=value, fill=fill))
p <- p + geom_bar(stat="identity", position="stack")
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, 3)[["Sub"]])
p <- p + labs(x="",y="percent\n")
if (rulang) p <- p + labs(x="",y="проценты\n")
p <- p + theme(axis.text.x = element_text(angle=45))
p <- p + coord_cartesian(ylim=c(0,100))
p

# Caption
caption_text <- "Agricultural area (2014)"
if (rulang) caption_text <- "Сельскохозяйственные земли (2014 г.)"



## ---- P4landMAP ----
dat <- syb.df %>% 
  filter(Year %in% 2014) %>% 
  select(FAOST_CODE,RL.AREA.ARBLPRMN.HA.SH) #%>% mutate(RL.AREA.ARBLPRMN.HA.SH = RL.AREA.ARBLPRMN.HA.SH * 10000)
  
# dat <- dat[dat$FAOST_CODE != 41,]
dat$FAOST_CODE[dat$FAOST_CODE == 41] <- 351

# set Robinson projection
map.plot <- left_join(map.df,dat) # so that each country in the region will be filled (value/NA)

# Subset
map.plot <- map.plot[which(map.plot[[region_to_report]]),]

cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","RL.AREA.ARBLPRMN.HA.SH")]
cat_data$value_cat <- categories(x=cat_data$RL.AREA.ARBLPRMN.HA.SH, n=5,decimals = 2)

map.plot <- left_join(map.plot,cat_data[c("FAOST_CODE","value_cat")])

# define map unit
# map_unit <- "m² per capita"
map_unit <- "ha per capita"
if (rulang) map_unit <- "га/чел"

p <- create_map_here()
p

# Caption
caption_text <- "Cropland per capita, ha/cap (2014)"
if (rulang) caption_text <- "Земли под сельскохозяйственными культурами на душу населения, га/чел (2014 г.)"


#
#  __        __          _
#  \ \      / /   __ _  | |_    ___   _ __
#   \ \ /\ / /   / _` | | __|  / _ \ | '__|
#    \ V  V /   | (_| | | |_  |  __/ | |
#     \_/\_/     \__,_|  \__|  \___| |_|
#




## ---- P4waterTEXT ----
spread_title <- "Water"
if (region_to_report == "RAF") short_text <- "Global demand for water has risen sharply over the last century. Total annual water withdrawal from agriculture, municipalities and industries rose from less than 580 km3 in 1900 to more than 3 900 km3 in 2010. Some countries in the African region use as high as 80\\% of total freshwater withdrawn in the agriculture sector. The industrial sector , however uses just about 40\\% of total freshwater withdrawn in the country with the highest consumption.  Agriculture accounts for approximately 70 percent of total freshwater withdrawal in the world, mostly through irrigation. This has been crucial for gains in food production since irrigation reduces drought risk and encourages crop diversification, thus also enhancing rural incomes. While irrigated agriculture represents about 20 percent of the cultivated land, it contributes to 40 percent of global food production."
if (region_to_report == "RAP") short_text <- "Global demand for water has risen sharply over the last century. Total annual water withdrawal from agriculture, municipalities and industries rose from less than 580 km\\textsuperscript{3} in 1900 to more than 3 900 km\\textsuperscript{3} in 2010. Agriculture accounts for approximately 70 percent of total freshwater withdrawal in the world, mostly through irrigation. This has been crucial for gains in food production since irrigation reduces drought risk and encourages crop diversification, thus also enhancing rural incomes. While irrigated agriculture represents about 20 percent of the cultivated land, it contributes to 40 percent of global food production."
if (region_to_report == "REU") short_text <- "Global demand for water has risen sharply over the last century. Total annual water withdrawal from agriculture, municipalities and industries worldwide rose from less than 580 km3 in 1900 to more than 3 900 km3 in 2010. Agriculture accounts for approximately 45 percent of total freshwater withdrawal in the region, mostly through irrigation. In Central Asia, where irrigation is especially important, this indicator is as high as over 85 percent."
if (region_to_report == "RNE") short_text <- "Global demand for water has risen sharply over the last century. Total annual water withdrawal from agriculture, municipalities and industries rose from less than 580 km3 in 1900 to more than 3 900 km3 2010. Renewable water is a very scarce resource in the region, in general, and especially in the Gulf Stets. Fresh water withdrawal by agriculture compared to total withdrawal is higher than 90\\% for countries like Sudan, Iran, Yemen and Mauritania."
if (region_to_report == "GLO") short_text <- "Global demand for water has risen sharply over the last century. Total annual water withdrawal from agriculture, municipalities and industries rose from less than 580 km\\textsuperscript{3} in 1900 to more than 3 900 km\\textsuperscript{3} in 2010. Agriculture accounts for approximately 70 percent of total freshwater withdrawal in the world, mostly through irrigation. This has been crucial for gains in food production since irrigation reduces drought risk and encourages crop diversification, thus also enhancing rural incomes. While irrigated agriculture represents about 20 percent of the cultivated land, it contributes to 40 percent of global food production."
if (rulang) spread_title <- "Вода"
if (region_to_report == "REU" & rulang) short_text <- "За последнее столетие резко возрос глобальный спрос на воду. Общий годовой забор воды на нужды сельского хозяйства, промышленности и хозяйственно-питьевые (муниципальные) нужды по всему миру увеличился с менее 580 км3 в 1900 году до более 3 900 км3 в 2010 году. На сельское хозяйство приходится около 45 процентов от общего забора пресной воды в регионе, в основном за счет орошения. В Центральной Азии, где орошение играет первостепенную роль, этот показатель составляет более 85 процентов."


## ---- P4waterData, cache=TRUE,results='hide', eval=P4water ----
library(readxl)
g <- read_excel(paste0(data.dir,"/UPDATEDWATER_WR_Capita_2000-2010.xlsx"))
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

bottomdata$FAO_TABLE_NAME <- factor(bottomdata$FAO_TABLE_NAME, levels=arrange(bottomdata[bottomdata$Year == 2010,], per_capita_water_resources)$FAO_TABLE_NAME)

if (rulang) levels(bottomdata$FAO_TABLE_NAME) <- countrycode.multilang::countrycode(levels(bottomdata$FAO_TABLE_NAME), origin = "country.name", destination = "country.name.russian.fao")
if (rulang) bottomdata$Year[bottomdata$Year == 2000] <- "2000 г."
if (rulang) bottomdata$Year[bottomdata$Year == 2010] <- "2010 г."

p <- ggplot(bottomdata, aes(x=FAO_TABLE_NAME,y=per_capita_water_resources,fill=factor(Year)))
p <- p + geom_bar(stat="identity",position="dodge")
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + labs(x=NULL,y=expression(m^"3"/yr/person))
if (rulang) p <- p + labs(x="",y=expression(м^"3"/год/чел))
p <- p + theme(axis.text.x = element_text(angle=45))
p



# Caption
caption_text <- "Countries with the lowest renewable water resources per capita"
if (rulang) caption_text <- "Страны с самыми низкими показателями возобновляемых водных ресурсов на душу населения"

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
dat_plot <- dat %>% slice(1:20) %>% dplyr::mutate(color = "2013")

if (rulang) dat_plot$SHORT_NAME <- countrycode.multilang::countrycode(dat_plot$FAOST_CODE, origin = "fao", destination = "country.name.russian.fao")

dat_plot$SHORT_NAME <- fct_reorder(dat_plot$SHORT_NAME, dat_plot$new_var) 

p <- ggplot(dat_plot, aes(x=SHORT_NAME,y=new_var))
p <- p + geom_segment(aes(y = min(dat_plot$new_var), xend = SHORT_NAME, 
                          yend = new_var, color=color), alpha=.5, show.legend = FALSE)
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75) + theme(panel.grid.major.y = element_blank())
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 1)[["Sub"]])
p <- p + theme(legend.position = "none") # hide legend as only one year plotted
p <- p + coord_flip()
p <- p + labs(x="",y="\npercent")
if (rulang) p <- p + labs(x="",y="\nпроценты")
p <- p + guides(color = guide_legend(nrow = 2))
p

# Caption
caption_text <- paste("Freshwater withdrawal by industrial sector, share of total, highest",nrow(dat_plot),"(1999 to 2013)")
if (rulang) caption_text <- paste("Забор пресной воды на промышленные нужды как процентная доля от общего водозабора,",nrow(dat_plot),"стран с самыми высокими значениями (с 1999 по 2013 гг.)")


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
dat_plot <- dat %>% slice(1:20) %>% dplyr::mutate(color = "2013")

if (rulang) dat_plot$SHORT_NAME <- countrycode.multilang::countrycode(dat_plot$FAOST_CODE, origin = "fao", destination = "country.name.russian.fao")

dat_plot$SHORT_NAME <- fct_reorder(dat_plot$SHORT_NAME, dat_plot$new_var) 

p <- ggplot(dat_plot, aes(x=SHORT_NAME,y=new_var))
p <- p + geom_segment(aes(y = min(dat_plot$new_var), xend = SHORT_NAME, 
                          yend = new_var, color=color), alpha=.5, show.legend = FALSE)
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75) + theme(panel.grid.major.y = element_blank())
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 1)[["Sub"]])
p <- p + theme(legend.position = "none") # hide legend as only one year plotted
p <- p + coord_flip()
p <- p + labs(x="",y="\npercent")
if (rulang) p <- p + labs(x="",y="\nпроценты")
p <- p + guides(color = guide_legend(nrow = 2))
p

# Caption
caption_text <- paste("Freshwater withdrawal by agricultural sector, share of total, highest",nrow(dat_plot),"(1999 to 2013)")
if (rulang) caption_text <- paste("Забор пресной воды на сельскохозяйственные нужды как процентная доля от общего водозабора,",nrow(dat_plot),"стран с самыми высокими значениями (с 1999 по 2013 гг.)")


## ---- P4waterBOTTOM ----

gg <- gg[which(gg[[region_to_report]]),]

# top ten
top_10 <- head(arrange(filter(gg, Year == 2010), -per_capita_water_resources),10)
top_10_00 <- gg[gg$Year == 2000 & gg$FAOST_CODE %in% unique(top_10$FAOST_CODE),]
topdata <- rbind(top_10,top_10_00)

topdata$FAO_TABLE_NAME <- factor(topdata$FAO_TABLE_NAME, levels=arrange(topdata[topdata$Year == 2010,], -per_capita_water_resources)$FAO_TABLE_NAME)

if (rulang) levels(topdata$FAO_TABLE_NAME) <- countrycode.multilang::countrycode(levels(topdata$FAO_TABLE_NAME), origin = "country.name", destination = "country.name.russian.fao")
if (rulang) topdata$Year[topdata$Year == 2000] <- "2000 г."
if (rulang) topdata$Year[topdata$Year == 2010] <- "2010 г."

if (!rulang) levels(topdata$FAO_TABLE_NAME) <- gsub('(.{1,30})(\\s|$)', '\\1\n', levels(topdata$FAO_TABLE_NAME))

p <- ggplot(topdata, aes(x=FAO_TABLE_NAME,y=per_capita_water_resources,fill=factor(Year)))
p <- p + geom_bar(stat="identity",position="dodge")
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + labs(x=NULL,y=expression(m^"3"/yr/person))
if (rulang) p <- p + labs(x="",y=expression(м^"3"/год/человек))
p <- p + theme(axis.text.x = element_text(angle=45))
p <- p + scale_y_continuous(labels=space)
p


# Caption
caption_text <- "Countries with the highest renewable water resources per capita"
if (rulang) caption_text <- "Страны с самыми высокими показателями возобновляемых водных ресурсов на душу населения"


## ---- P4waterMAP ----
dat <- syb.df %>% filter(Year %in% c(2007:2012)) %>%
  select(FAOST_CODE,SHORT_NAME,SL.AGR.EMPL.ZS) %>%
  group_by(FAOST_CODE) %>% dplyr::summarise(SL.AGR.EMPL.ZS = max(SL.AGR.EMPL.ZS, na.rm = TRUE)) %>%
  #filter(!is.na(SL.AGR.EMPL.ZS)) %>%
  ungroup()

water <- syb.df[c("FAOST_CODE","Year","AQ.WAT.RFRWAGR.MC.SH")]
water <- water[!is.na(water$AQ.WAT.RFRWAGR.MC.SH),]
dat <- water %>% group_by(FAOST_CODE) %>% dplyr::summarise(pooled.freshwater = mean(AQ.WAT.RFRWAGR.MC.SH, na.rm = TRUE))

map.plot <- left_join(map.df,dat) # so that each country in the region will be filled (value/NA)

# Add region key and subset

map.plot <- map.plot[which(map.plot[[region_to_report]]),]

cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","pooled.freshwater")]
cat_data$value_cat <- categories(x=cat_data$pooled.freshwater, n=5, method="jenks")

map.plot <- left_join(map.plot,cat_data[c("FAOST_CODE","value_cat")])

# define map unit
map_unit <- "percent"
if (rulang) map_unit <- "проценты"

p <- create_map_here()
p

# Caption
caption_text <- "Freshwater resources withdrawn by agriculture (percent, 1999-2013*)"
if (rulang) caption_text <- "Забор воды из пресноводных ресурсов на сельскохозяйственные нужды (в процентах, 1999-2013 гг.*)"


#   _____
#  | ____|  _ __     ___   _ __    __ _   _   _
#  |  _|   | '_ \   / _ \ | '__|  / _` | | | | |
#  | |___  | | | | |  __/ | |    | (_| | | |_| |
#  |_____| |_| |_|  \___| |_|     \__, |  \__, |
#                                 |___/   |___/



## ---- P4energyTEXT ----
spread_title <- "Energy"
if (region_to_report == "RAF") short_text <- "Energy is an important input for the agri-food chain and is used to power agricultural machinery, heat greenhouses, power irrigation systems, but also to manufacture equipment, fertilizers, pesticides and other agro-chemicals. The amount of energy consumed by agriculture is increasing especially in Northern Africa as mechanization, especially increases. At the same time agriculture produces energy in the form of bioenergy. Bioenergy production made up about 96\\% of total renewable energy produced in 2009 in Africa and has increased sharply over the last years to meet the new demand for liquid biofuels for transport (e.g., ethanol and biodiesel) and solid biomass for power such as pellets or wood chips."
if (region_to_report == "RAP") short_text <- "Energy is an important input for the agri-food chain and is used to power agricultural machinery, heat greenhouses, power irrigation systems, but also to manufacture equipment, fertilizers, pesticides and other agro-chemicals. The amount of energy consumed by agriculture is increasing worldwide as mechanization, especially in developing countries, increases. At the same time agriculture produces energy in the form of bioenergy. Bioenergy production increased sharply over the last years to meet the new demand for liquid biofuels for transport (e.g., ethanol and biodiesel) and solid biomass for power such as pellets or wood chips."
if (region_to_report == "REU") short_text <- "Energy is an important input for the agrifood chain and is used to power agricultural machinery, heat greenhouses, power irrigation systems, but also to manufacture equipment, fertilizers, pesticides and other agro-chemicals. The amount of energy consumed by agriculture is increasing in the region as mechanization, especially in countries in transition, increases. At the same time agriculture produces energy in the form of bioenergy. Bioenergy production increased sharply over the last years to meet the new demand for liquid biofuels for transport (e.g., ethanol and biodiesel) and solid biomass for power such as pellets or wood chips."
if (region_to_report == "RNE") short_text <- "Energy is an important input for the agri-food chain and is used to power agricultural machinery, heat greenhouses, power irrigation systems, but also to manufacture equipment, fertilizers, pesticides and other agro-chemicals. The amount of energy consumed by agriculture has been increasing in Other Near east countries since 2000 as mechanization increases. At the same time agriculture produces energy in the form of bioenergy. Bioenergy production in North Africa and Other Near East countries, however, has been slightly decreasing until 2009"
if (region_to_report == "GLO") short_text <- "Energy is an important input for the agri-food chain and is used to power agricultural machinery, heat greenhouses, power irrigation systems, but also to manufacture equipment, fertilizers, pesticides and other agro-chemicals. The amount of energy consumed by agriculture is increasing worldwide as mechanization, especially in developing countries, increases. At the same time agriculture produces energy in the form of bioenergy. Bioenergy production increased sharply over the last years to meet the new demand for liquid biofuels for transport (e.g., ethanol and biodiesel) and solid biomass for power such as pellets or wood chips."
if (rulang) spread_title <- "Энергия"
if (region_to_report == "REU" & rulang) short_text <- "Энергия является важным компонентом аграрной цепочки и используется в качестве источника питания сельскохозяйственной техники, систем полива, для обогрева теплиц, а также для производства техники, удобрений, пестицидов и других химических веществ. Количество энергии, потребляемой сельским хозяйством в регионе, растет, поскольку увеличивается уровень механизации, особенно в странах с переходной экономикой. В то же время сельское хозяйство производит энергию в форме биоэнергии. Производство биотоплива резко возросло за последние годы, чтобы удовлетворить новый спрос на жидкое биотопливо для транспорта (например, этанол и биодизельное топливо) и твердую биомассу для производства энергии (например, древесные гранулы или опилки)."

## ---- P4energyData ----


## ---- P4energyTOPRIGHT ----
if (region_to_report == "RAF") dat <- syb.df %>% filter(Year %in% 2000:2012, FAOST_CODE %in% 12001:12005) %>%
  select(SHORT_NAME,Year,
         EE_6740_72041)
if (region_to_report == "RAP") dat <- syb.df %>% filter(Year %in% 2000:2012, FAOST_CODE %in% 13001:13014) %>%
  select(SHORT_NAME,Year,
         EE_6740_72041)
if (region_to_report == "REU") dat <- syb.df %>% filter(Year %in% 2000:2012, FAOST_CODE %in% 14001:14007) %>%
  select(SHORT_NAME,Year,
         EE_6740_72041) %>% filter(!is.na(EE_6740_72041))
if (region_to_report == "RNE") dat <- syb.df %>% filter(Year %in% 2000:2012, FAOST_CODE %in% 15001:15003) %>%
  select(SHORT_NAME,Year,
         EE_6740_72041)
if (region_to_report == "GLO") dat <- syb.df %>% filter(Year %in% 2000:2012, FAOST_CODE %in% c(5100,5200,5300,5400,5500)) %>%
  select(SHORT_NAME,Year,
         EE_6740_72041)

dat <- na.omit(dat)

dat_plot <- dat

if (rulang) dat_plot$SHORT_NAME <- translate_subgroups(dat_plot$SHORT_NAME, isfactor = FALSE, add_row_breaks = FALSE)

p <- ggplot(dat_plot, aes(x=Year, y=EE_6740_72041, color=SHORT_NAME))
p <- p + geom_line(size=1.1, alpha=.7)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, length(unique(dat_plot$SHORT_NAME)))[["Sub"]])
p <- p + labs(x="",y="% of tot energy production\n")
if (rulang) p <- p + labs(x="",y="% в общем объеме \nпроизводства энергии\n")
p <- p + guides(color = guide_legend(nrow = length(unique(dat_plot$SHORT_NAME))))
p <- p + scale_x_continuous(breaks=c(2000,2003,2006,2009))
p



# Caption
caption_text <- "Bioenergy production, share of total energy production"
if (rulang) caption_text <- "Производство биоэнергии, доля в общем объеме производства энергии"




## ---- P4energyLEFT ----
dat <- syb.df %>% filter(Year %in% 2009) %>% select(SHORT_NAME,Year,EE_6740_72041)

dat <- dat[!is.na(dat$EE_6740_72041),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

dat <- dat[which(dat[[region_to_report]]),]

# top for this plot
dat <- arrange(dat, -EE_6740_72041)
top20 <- dat %>% slice(1:20) %>% dplyr::mutate(color = "2012")

dat_plot <- top20

if (rulang) dat_plot$SHORT_NAME <- countrycode.multilang::countrycode(dat_plot$FAOST_CODE, origin = "fao", destination = "country.name.russian.fao")

dat_plot$SHORT_NAME <- fct_reorder(dat_plot$SHORT_NAME, dat_plot$EE_6740_72041) 

p <- ggplot(dat_plot, aes(x=SHORT_NAME,y=EE_6740_72041))
p <- p + geom_segment(aes(y = min(dat_plot$EE_6740_72041), xend = SHORT_NAME, 
                          yend = EE_6740_72041, color=color), alpha=.5, show.legend = FALSE)
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75) + theme(panel.grid.major.y = element_blank())
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 1)[["Sub"]])
p <- p + theme(legend.position = "none") # hide legend as only one year plotted
p <- p + coord_flip()
p <- p + labs(x="",y="\n% of tot energy production")
if (rulang) p <- p + labs(x="",y="\n% в общ. объеме \nпроизв. энергии")
p <- p + guides(color = guide_legend(nrow = 2))
p

# Caption
caption_text <- paste("Bioenergy production, share of total energy production, top",nrow(dat_plot),"countries 2009")
if (rulang) caption_text <- paste("Производство биоэнергии, доля в общем производстве энергии,",nrow(dat_plot),"стран с самыми высокими показателями в 2009 году")

## ---- P4energyRIGHT ----
dat <- syb.df %>% filter(Year %in% 2009) %>% select(SHORT_NAME,Year,EE_6741_72040)


dat <- dat[!is.na(dat$EE_6741_72040),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

dat <- dat[which(dat[[region_to_report]]),]

# top for this plot
dat <- arrange(dat, -EE_6741_72040)
top20 <- dat %>% slice(1:20) %>% dplyr::mutate(color = "2012")

dat_plot <- top20

if (rulang) dat_plot$SHORT_NAME <- countrycode.multilang::countrycode(dat_plot$FAOST_CODE, origin = "fao", destination = "country.name.russian.fao")

dat_plot$SHORT_NAME <- fct_reorder(dat_plot$SHORT_NAME, dat_plot$EE_6741_72040) 

p <- ggplot(dat_plot, aes(x=SHORT_NAME,y=EE_6741_72040))
p <- p + geom_segment(aes(y = min(dat_plot$EE_6741_72040), xend = SHORT_NAME, 
                          yend = EE_6741_72040, color=color), alpha=.5, show.legend = FALSE)
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75) + theme(panel.grid.major.y = element_blank())
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 1)[["Sub"]])
p <- p + theme(legend.position = "none") # hide legend as only one year plotted
p <- p + coord_flip()
p <- p + labs(x="",y="\n% of tot energy consumption")
if (rulang) p <- p + labs(x="",y="\n% в общ. объеме \nпотр. энергии")
p <- p + guides(color = guide_legend(nrow = 2))
p


# Caption
caption_text <- paste("Energy use in agriculture and forestry, share of total energy consumption, top",nrow(dat_plot),"countries 2009")
if (rulang) caption_text <- paste("Потребление энергии в сельском и лесном хозяйстве, доля в общем объеме потребления энергии,",nrow(dat_plot),"стран с самыми высокими показателями в 2009 г.")


## ---- P4energyBOTTOM ----
if (region_to_report == "RAF") dat <- syb.df %>% filter(Year %in% 2000:2012, FAOST_CODE %in% 12001:12005) %>%
  select(SHORT_NAME,Year,
         EE_6741_72040)
if (region_to_report == "RAP") dat <- syb.df %>% filter(Year %in% 2000:2012, FAOST_CODE %in% 13001:13014) %>%
  select(SHORT_NAME,Year,
         EE_6741_72040)
if (region_to_report == "REU") dat <- syb.df %>% filter(Year %in% 2000:2012, FAOST_CODE %in% 14001:14007) %>%
  select(SHORT_NAME,Year,
         EE_6741_72040)
if (region_to_report == "RNE") dat <- syb.df %>% filter(Year %in% 2000:2012, FAOST_CODE %in% 15001:15003) %>%
  select(SHORT_NAME,Year,
         EE_6741_72040)
if (region_to_report == "GLO") dat <- syb.df %>% filter(Year %in% 2000:2012, FAOST_CODE %in% c(5100,5200,5300,5400,5500)) %>%
  select(SHORT_NAME,Year,
         EE_6741_72040)

dat <- na.omit(dat)

dat_plot <- dat

if (rulang)dat_plot$SHORT_NAME <- translate_subgroups(dat_plot$SHORT_NAME, isfactor = FALSE, add_row_breaks = FALSE)

p <- ggplot(dat_plot, aes(x=Year, y=EE_6741_72040, color=SHORT_NAME))
p <- p + geom_line(size=1.1, alpha=.7)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, length(unique(dat_plot$SHORT_NAME)))[["Sub"]])
p <- p + labs(x="",y="% of total energy consumption\n")
if (rulang) p <- p + labs(x="",y="% в общем объеме производства энергии\n")
p <- p + guides(color = guide_legend(nrow = 3))
p <- p + scale_x_continuous(breaks=c(2000,2003,2006,2009))
p

# Caption
caption_text <- "Energy use in agriculture and forestry, share of total energy consumption"
if (rulang) caption_text <- "Потребление энергии в сельском и лесном хозяйстве, доля в общем объеме потребления энергии"

## ---- P4energyMAP ----
dat <- syb.df %>%
  filter(Year >= 2008) %>%
  select(FAOST_CODE,Year,GN_6808_72182) %>%
  mutate(GN_6808_72182 = GN_6808_72182 / 1000000)
dat <- na.omit(dat)

dat <- dat %>% group_by(FAOST_CODE) %>% filter(Year == max(Year)) %>%  ungroup()

dat$FAOST_CODE[dat$FAOST_CODE == 41] <- 351

# set Robinson projection
map.plot <- left_join(map.df,dat) # so that each country in the region will be filled (value/NA)

# Subset
map.plot <- map.plot[which(map.plot[[region_to_report]]),]

cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","GN_6808_72182")]
if (region_to_report %in% c("RAP","GLO"))  cat_data$value_cat <- categories(x=cat_data$GN_6808_72182,manual = TRUE, manual_breaks = c(5,25,50,500,7769))
if (!region_to_report %in% c("RAP","GLO"))  cat_data$value_cat <- categories(x=cat_data$GN_6808_72182)

map.plot <- left_join(map.plot,cat_data[c("FAOST_CODE","value_cat")])

# define map unit
map_unit <- "TJ"
if (rulang) map_unit <- "ТДж"

p <- create_map_here()
p

# Caption
caption_text <- "Energy consumption for power irrigation, TJ (2008-2011*)"
if (rulang) caption_text <- "Потребление энергии для орошения машинным способом, ТДж (2008-2011 гг.*)"


#   _____                               _
#  |  ___|   ___    _ __    ___   ___  | |_   _ __   _   _
#  | |_     / _ \  | '__|  / _ \ / __| | __| | '__| | | | |
#  |  _|   | (_) | | |    |  __/ \__ \ | |_  | |    | |_| |
#  |_|      \___/  |_|     \___| |___/  \__| |_|     \__, |
#                                                    |___/

## ---- P4forestryTEXT ----
spread_title <- "Forestry"
if (region_to_report == "RAF") short_text <- "Forests make vital contributions to biodiversity. They also sustain a range of economic activities and act as a source of food, medicine and fuel for more than a billion people. The latest estimate of Africa’s total forest area is more than 629 million hectares, corresponding to about 21 percent of its total land area. But today forests face unprecedented pressures. Changes in land cover have caused the most pressing environmental issue in recent decades. The impacts of deforestation and land use intensification, especially on soil degradation, have been significant."
if (region_to_report == "RAP") short_text <- "Forests make vital contributions to biodiversity. They also sustain a range of economic activities and act as a source of food, medicine and fuel for more than a billion people. The latest estimate of the world’s total forest area is more than 4 billion hectares, corresponding to about 30 percent of total land area. But today forests face unprecedented pressures. Changes in land cover have caused the most pressing environmental issue in recent decades. The impact of deforestation and land use intensification, especially on soil degradation, have been significant."
if (region_to_report == "REU") short_text <- "Forests make vital contributions to biodiversity. The latest estimate of the region’s total forest area is more than 1 billion hectares, corresponding to about 40 percent of total land area. Today forests face unprecedented pressures. Changes in land cover have caused the most pressing environmental issue in recent decades. The impact of deforestation and land use intensification, especially on soil degradation, has been significant."
if (region_to_report == "RNE") short_text <- "Forests make vital contributions to biodiversity. They also sustain a range of economic activities and act as a source of food, medicine and fuel for more than a billion people. The latest estimate of the world’s total forest area is more than 4 billion hectares, corresponding to about 30 percent of total land area. Within the region, however, the share of forests was merely 1.9 percent in 2014 due to the significant impact of deforestation and land use intensification on soil degradation."
if (region_to_report == "GLO") short_text <- "Forests make vital contributions to biodiversity. They also sustain a range of economic activities and act as a source of food, medicine and fuel for more than a billion people. The latest estimate of the world’s total forest area is more than 4 billion hectares, corresponding to about 30 percent of total land area. But today forests face unprecedented pressures. Changes in land cover have caused the most pressing environmental issue in recent decades. The impact of deforestation and land use intensification, especially on soil degradation, have been significant."
if (rulang) spread_title <- "Лесное хозяйство"
if (region_to_report == "REU" & rulang) short_text <- "Леса вносят существенный вклад в биоразнообразие. По последним оценкам, общая площадь лесов в регионе составляет более 1 млрд гектар, что соответствует приблизительно 40 процентам от общей площади земель. Сегодня леса оказываются под беспрецедентным давлением. За последние десятилетия изменения в растительном покрове вызвали наиболее острые экологические проблемы. Обезлесение и интенсивное землепользование оказывают сильное влияние на деградацию почвы."

## ---- P4forestryData ----




## ---- P4forestryTOPRIGHT ----

# syb.df %>% select(FAOST_CODE,Year,FO.PRD.WP.M3.NO) %>% filter(FAOST_CODE == 11)


dat <- syb.df %>% select(FAOST_CODE,Year,
                         FO.PRD.RP.M3.NO,
                         FO.PRD.WP.M3.NO,
                         FO.PRD.PPB.M3.NO) %>%
  dplyr::mutate(FO.PRD.RP.M3.NO = FO.PRD.RP.M3.NO / 1000000,
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
  dplyr::summarise(value  = sum(value, na.rm=TRUE)) %>%  ungroup()

if (rulang){
  dat_plot$fill[dat_plot$fill == "Paper and paperboard"] <- "Бумага и \nкартон"
  dat_plot$fill[dat_plot$fill == "Recovered paper"] <- "Рекуперированная \nбумага"
  dat_plot$fill[dat_plot$fill == "Wood pulp"] <- "Древесная \nцеллюлоза"
  
}


p <- ggplot(dat_plot, aes(x=Year, y=value, color=fill))
p <- p + geom_line(size=1.1, alpha=.7)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 3)[["Sub"]])
p <- p + labs(x="",y="million tonnes\n")
if (rulang) p <- p + labs(x="",y="млн тонн\n")
# p <- p + theme(axis.text.x = element_text(angle=45))
p <- p + guides(color = guide_legend(nrow = 2))
p <- p + scale_x_continuous(breaks=c(1960,1980,2000,2014))
p

# Caption
caption_text <- "Production of selected forest products"
if (rulang) caption_text <- "Производство отдельных лесопродуктов"


## ---- P4forestryLEFT ----
dat <- syb.df %>%  filter(Year %in%  2013) %>%  select(FAOST_CODE,Year,FO.EXVAL.TOT.USD.NO) %>%
  dplyr::mutate(FO.EXVAL.TOT.USD.NO = FO.EXVAL.TOT.USD.NO / 1000000000 ) #into trillion

dat <- dat[!is.na(dat$FO.EXVAL.TOT.USD.NO),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

dat <- dat[which(dat[[region_to_report]]),]

# top for this plot
dat <- arrange(dat, -FO.EXVAL.TOT.USD.NO)
dat_plot <- dat %>% slice(1:20) %>% dplyr::mutate(color = "2013")

if (rulang) dat_plot$SHORT_NAME <- countrycode.multilang::countrycode(dat_plot$FAOST_CODE, origin = "fao", destination = "country.name.russian.fao")

dat_plot$SHORT_NAME <- fct_reorder(dat_plot$SHORT_NAME, dat_plot$FO.EXVAL.TOT.USD.NO) 

p <- ggplot(dat_plot, aes(x=SHORT_NAME,y=FO.EXVAL.TOT.USD.NO))
p <- p + geom_segment(aes(y = 0, xend = SHORT_NAME, 
                          yend = FO.EXVAL.TOT.USD.NO, color=color), alpha=.5)
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75) + theme(panel.grid.major.y = element_blank())
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 1)[["Sub"]])
p <- p + theme(legend.position = "none") # hide legend as only one year plotted
p <- p + coord_flip()
p <- p + labs(x="",y="\ntrillion US$")
if (rulang) p <- p + labs(x="",y="\nтрлн долл. США")
p <- p + scale_y_continuous(labels=space)
# p <- p + scale_y_continuous(labels=space,breaks=c(0,10000,20000))
p

# Caption
caption_text <- paste("Top",nrow(dat_plot),"exporters of forest products (2013)")
if (rulang) caption_text <- caption_text <- paste(nrow(dat_plot),"самых крупных экспортеров лесопродукции (2013 г.)")


## ---- P4forestryRIGHT ----

dat <- syb.df %>%  filter(Year %in%  2013) %>%  select(FAOST_CODE,Year,FO.IMVAL.TOT.USD.NO) %>%
  dplyr::mutate(FO.IMVAL.TOT.USD.NO = FO.IMVAL.TOT.USD.NO / 1000000000 ) #into trillion

dat <- dat[!is.na(dat$FO.IMVAL.TOT.USD.NO),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

dat <- dat[which(dat[[region_to_report]]),]

# top for this plot
dat <- arrange(dat, -FO.IMVAL.TOT.USD.NO)
dat_plot <- dat %>% slice(1:20) %>% dplyr::mutate(color = "2012")

if (rulang) dat_plot$SHORT_NAME <- countrycode.multilang::countrycode(dat_plot$FAOST_CODE, origin = "fao", destination = "country.name.russian.fao")

dat_plot$SHORT_NAME <- fct_reorder(dat_plot$SHORT_NAME, dat_plot$FO.IMVAL.TOT.USD.NO) 

p <- ggplot(dat_plot, aes(x=SHORT_NAME,y=FO.IMVAL.TOT.USD.NO))
p <- p + geom_segment(aes(y = 0, xend = SHORT_NAME, 
                          yend = FO.IMVAL.TOT.USD.NO, color=color), alpha=.5)
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75) + theme(panel.grid.major.y = element_blank())
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 1)[["Sub"]])
p <- p + theme(legend.position = "none") # hide legend as only one year plotted
p <- p + coord_flip()
p <- p + labs(x="",y="\ntrillion US$")
if (rulang) p <- p + labs(x="",y="\nтрлн долл. США")
p <- p + scale_y_continuous(labels=space)
# p <- p + scale_y_continuous(labels=space,breaks=c(0,20000,40000))
p

# Caption
caption_text <- paste("Top",nrow(dat_plot),"importers of forest products (2013)")
if (rulang) caption_text <- paste(nrow(dat_plot),"самых крупных импортеров лесопродукции (2013 г.)")


## ---- P4forestryBOTTOM ----

if (region_to_report == "RAF") dat <- syb.df %>% filter(Year %in% 2015, FAOST_CODE %in% 12001:12005) %>%
  select(SHORT_NAME,Year,
         PrimFor,PlantFor,NatRegFor)
if (region_to_report == "RAP") dat <- syb.df %>% filter(Year %in% 2015, FAOST_CODE %in% 13001:13014) %>%
  select(SHORT_NAME,Year,
         PrimFor,PlantFor,NatRegFor)
if (region_to_report == "REU") dat <- syb.df %>% filter(Year %in% 2015, FAOST_CODE %in% 14001:14007) %>%
  select(SHORT_NAME,Year,
         PrimFor,PlantFor,NatRegFor)
if (region_to_report == "RNE") dat <- syb.df %>% filter(Year %in% 2015, FAOST_CODE %in% 15001:15003) %>%
  select(SHORT_NAME,Year,
         PrimFor,PlantFor,NatRegFor)
if (region_to_report == "GLO") dat <- syb.df %>% filter(Year %in% 2015, FAOST_CODE %in% c(5100,5200,5300,5400,5500)) %>%
  select(SHORT_NAME,Year,
         PrimFor,PlantFor,NatRegFor)

dat <- na.omit(dat)


dat <- gather(dat, variable, value, 3:5)
dat$fill[dat$variable == "PrimFor"] <- "primary forest"
dat$fill[dat$variable == "PlantFor"] <- "planted forest"
dat$fill[dat$variable == "NatRegFor"] <- "other naturally regenerated forest"

dat$value <- dat$value / 1000000

dat_plot <- dat

# # AGREGATE
dat_plot$SHORT_NAME <- factor(dat_plot$SHORT_NAME, levels=(dat_plot %>% filter(fill == "primary forest") %>% arrange(-value))$SHORT_NAME)
#

if (rulang){
  dat_plot$SHORT_NAME <- translate_subgroups(dat_plot$SHORT_NAME, isfactor = TRUE, add_row_breaks = TRUE)
  dat_plot$fill[dat_plot$fill == "primary forest"] <- "девственные леса"
  dat_plot$fill[dat_plot$fill == "planted forest"] <- "лесонасаждения"
  dat_plot$fill[dat_plot$fill == "other naturally regenerated forest"] <- "леса, восстанавливаемые \nестественным путем"
}


p <- ggplot(dat_plot, aes(x=SHORT_NAME, y=value, fill=fill))
p <- p + geom_bar(stat="identity", position="stack")
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, 3)[["Sub"]])
p <- p + labs(x="",y="million ha\n")
if (rulang) p <- p + labs(x="",y="млн га\n")
p <- p + theme(axis.text.x = element_text(angle=45))
p


# Caption
caption_text <- "Forest characteristics (2015)"
if (rulang) caption_text <- "Характеристики леса (2015 г.)"


## ---- P4forestryMAP ----
dat <- filter(syb.df, Year %in% 2013) %>% 
  select(FAOST_CODE, RL.AREA.FOR.HA.SH) %>% 
  mutate(RL.AREA.FOR.HA.SH = RL.AREA.FOR.HA.SH * 100) # we want percetanges

# dat <- dat[dat$FAOST_CODE != 41,]
dat$FAOST_CODE[dat$FAOST_CODE == 41] <- 351

# set Robinson projection
map.plot <- left_join(map.df,dat) # so that each country in the region will be filled (value/NA)

# Subset
map.plot <- map.plot[which(map.plot[[region_to_report]]),]

cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","RL.AREA.FOR.HA.SH")]
cat_data$value_cat <- categories(x=cat_data$RL.AREA.FOR.HA.SH, n=5)

map.plot <- left_join(map.plot,cat_data[c("FAOST_CODE","value_cat")])

# define map unit
map_unit <- "percent"
if (rulang) map_unit <- "проценты"

p <- create_map_here()
p

# Caption
caption_text <- "Forest area as share of total land area, percent (2013)"
if (rulang) caption_text <- "Доля площади лесов в общей площади суши, в процентах (2013 г.)"


#    ____   _   _                       _                     _
#   / ___| | | (_)  _ __ ___     __ _  | |_    ___      ___  | |__     __ _   _ __     __ _    ___
#  | |     | | | | | '_ ` _ \   / _` | | __|  / _ \    / __| | '_ \   / _` | | '_ \   / _` |  / _ \
#  | |___  | | | | | | | | | | | (_| | | |_  |  __/   | (__  | | | | | (_| | | | | | | (_| | |  __/
#   \____| |_| |_| |_| |_| |_|  \__,_|  \__|  \___|    \___| |_| |_|  \__,_| |_| |_|  \__, |  \___|
#                                                                                     |___/

## ---- P4climateTEXT ----
spread_title <- "Climate change"
if (region_to_report == "RAF") short_text <- "The severity and speed of climate change is presenting an unprecedented challenge. Current global surface temperatures are now about 0.6 degrees Celsius higher than the average for the last century. This increase is consistent with model predictions of the effects of rising atmospheric concentrations of carbon dioxide (CO\\textsubscript{2}) and other GHGs, which are a result of human activity. Compared to global figures, greenhouse gas emissions in agriculture from Africa are considerably low, the highest been about 300,000 gigagrams CO2eq as compared to over 2million gigagrams CO2eq globally in 2010.  The poorest and most food-insecure regions around the globe are the most vulnerable." # Already scarce land and water resources will likely become even more scarce, and insufficient technical and financial means will make adaptation to a changing climate very difficult."
if (region_to_report == "RAP") short_text <- "The severity and speed of climate change is presenting an unprecedented challenge. Current global surface temperatures are now about 0.6 degrees Celsius higher than the average for the last century. This increase is consistent with model predictions of the effects of rising atmospheric concentrations of carbon dioxide (CO\\textsubscript{2}) and other GHGs, which are a result of human activity. The poorest and most food-insecure regions around the globe are the most vulnerable. Already scarce land and water resources will likely become even more scarce, and insufficient technical and financial means will make adaptation to a changing climate very difficult."
if (region_to_report == "REU") short_text <- "The severity and speed of climate change is presenting an unprecedented challenge. Current global surface temperatures are now about 0.6 degrees Celsius higher than the average for the last century. This increase is consistent with model predictions of the effects of rising atmospheric concentrations of carbon dioxide (CO\\textsubscript{2}) and other GHGs, which are a result of human activity. The poorest and most food-insecure regions around the globe are the most vulnerable. Already scarce land and water resources will likely become even scarcer, and insufficient technical and financial means will make adaptation to a changing climate very difficult."
if (region_to_report == "RNE") short_text <- "The severity and speed of climate change is presenting an unprecedented challenge. Current global surface temperatures are now about 0.6 degrees Celsius higher than the average for the last century. This increase is consistent with model predictions of the effects of rising atmospheric concentrations of carbon dioxide (CO\\textsubscript{2}) and other GHGs, which are a result of human activity. The region contributes to the globally 2 million gigagrams CO\\textsubscript{2} eq of greenhouse gas emission in agriculture by less than 200,000 gigagrams CO\\textsubscript{2} eq."
if (region_to_report == "GLO") short_text <- "The severity and speed of climate change is presenting an unprecedented challenge. Current global surface temperatures are now about 0.6 degrees Celsius higher than the average for the last century. This increase is consistent with model predictions of the effects of rising atmospheric concentrations of carbon dioxide (CO\\textsubscript{2}) and other GHGs, which are a result of human activity. The poorest and most food-insecure regions around the globe are the most vulnerable. Already scarce land and water resources will likely become even more scarce, and insufficient technical and financial means will make adaptation to a changing climate very difficult."
if (rulang) spread_title <- "Изменение климата"
if (region_to_report == "REU" & rulang) short_text <- "Интенсивность и скорость изменения климата являются на сегодняшний день беспрецедентным вызовом всему миру. Текущие глобальные температуры земной поверхности примерно на 0,6 градуса по Цельсию выше, чем в среднем за последнее столетие. Этот рост соответствует модельным прогнозам последствий повышения концентрации в атмосфере углекислого газа (СО2) и других парниковых газов, которые являются результатом человеческой деятельности. Самые бедные регионы мира и регионы, в которых отсутствует продовольственная безопасность, являются наиболее уязвимыми. И так скудные земельные и водные ресурсы станут, вероятно, еще более ограниченными, а недостаточность технических и финансовых средств в значительной степени затруднит процесс приспособления к изменению климата."

## ---- P4climateData ----


## ---- P4climateTOPRIGHT ----
if (region_to_report == "RAF") dat <- syb.df %>% filter(Year >= 2000, FAOST_CODE %in% 12001:12005) %>%
  select(SHORT_NAME,Year,
         GHG.TOT.ALL.GG.NO)
if (region_to_report == "RAP") dat <- syb.df %>% filter(Year >= 2000, FAOST_CODE %in% 13001:13014) %>%
  select(SHORT_NAME,Year,
         GHG.TOT.ALL.GG.NO)
if (region_to_report == "REU") dat <- syb.df %>% filter(Year >= 2000, FAOST_CODE %in% 14001:14007) %>%
  select(SHORT_NAME,Year,
         GHG.TOT.ALL.GG.NO)
if (region_to_report == "RNE") dat <- syb.df %>% filter(Year >= 2000, FAOST_CODE %in% 15001:15003) %>%
  select(SHORT_NAME,Year,
         GHG.TOT.ALL.GG.NO)
if (region_to_report == "GLO") dat <- syb.df %>% filter(Year >= 2000, FAOST_CODE %in% c(5100,5200,5300,5400,5500)) %>%
  select(SHORT_NAME,Year,
         GHG.TOT.ALL.GG.NO)
dat <- na.omit(dat)

dat$GHG.TOT.ALL.GG.NO <- dat$GHG.TOT.ALL.GG.NO /1000

dat_plot <- dat

if (rulang) dat_plot$SHORT_NAME <- translate_subgroups(dat_plot$SHORT_NAME, isfactor = FALSE, add_row_breaks = FALSE)

p <- ggplot(dat_plot, aes(x=Year, y=GHG.TOT.ALL.GG.NO, color=SHORT_NAME))
p <- p + geom_line(size=1.1, alpha=.7)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, length(unique(dat_plot$SHORT_NAME)))[["Sub"]])
p <- p + labs(x="",y=expression("thousand gigagrams CO"[2] * "eq"))
if (rulang) p <- p + labs(x="",y=expression("тыс. гигаграммов CO"[2] * "-экв"))
# p <- p + theme(axis.text.x = element_text(angle=45))
p <- p + guides(color = guide_legend(nrow = length(unique(dat_plot$SHORT_NAME))))
p <- p + scale_x_continuous(breaks=c(2000,2006,2012))
p

# Caption
caption_text <- "Greenhouse gas emissions in agriculture"
if (rulang) caption_text <- "Выбросы парниковых газов в сельском хозяйстве"


## ---- P4climateLEFT ----
dat <- syb.df %>% 
  filter(Year %in% c(2000,2013)) %>% 
  select(FAOST_CODE,Year,GHG.TOT.ALL.GG.NO) %>%
  dplyr::mutate(GHG.TOT.ALL.GG.NO = GHG.TOT.ALL.GG.NO / 1000)

dat <- dat[!is.na(dat$GHG.TOT.ALL.GG.NO),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

dat <- dat[which(dat[[region_to_report]]),]

dat <- arrange(dat, -Year, -GHG.TOT.ALL.GG.NO)
top12 <- dat %>% filter(Year == 2013) %>% slice(1:20) %>% dplyr::mutate(color = "2013")
top00 <- dat %>% filter(FAOST_CODE %in% top12$FAOST_CODE, Year == 2000) %>% dplyr::mutate(color = "2000")
dat_plot <- rbind(top12,top00)

dat_plot$SHORT_NAME <- factor(dat_plot$SHORT_NAME, levels=unique(arrange(top12,GHG.TOT.ALL.GG.NO)$SHORT_NAME))
if (rulang) levels(dat_plot$SHORT_NAME) <- countrycode.multilang::countrycode(levels(dat_plot$SHORT_NAME), origin = "country.name", destination = "country.name.russian.fao")
if (rulang){
  dat_plot$color[dat_plot$color == "2013"] <- "2013 г."
  dat_plot$color[dat_plot$color == "2000"] <- "2000 г."
}

dat_plot$SHORT_NAME <- fct_reorder(dat_plot$SHORT_NAME, dat_plot$GHG.TOT.ALL.GG.NO) 

# To make the latest point on top
dat_plot <- arrange(dat_plot, color)

p <- ggplot(data=dat_plot, aes(x=SHORT_NAME, y= GHG.TOT.ALL.GG.NO, fill=color))
p <- p + geom_segment(data=dat_plot %>% select(Year,SHORT_NAME,GHG.TOT.ALL.GG.NO) %>%
                        spread(key = Year, value = GHG.TOT.ALL.GG.NO) %>% 
                        mutate(color=NA), 
                      aes(y = `2000`, xend = SHORT_NAME,
                          yend = `2013`), color="grey80")
p <- p + geom_point(aes(fill=color),size = 4, alpha = 0.75, pch=21, color="white") + theme(panel.grid.major.y = element_blank())
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])

p <- p + coord_flip()
p <- p + labs(x="",y=expression("Mt CO"[2] * "eq"))
if (rulang) p <- p + labs(x="",y=expression("млн тонн CO"[2] * "экв."))
p <- p + guides(color = guide_legend(nrow = 1)) 
p

# Caption
caption_text <- paste("Greehouse gas emissions in agriculture, highest",nrow(top12),"countries in 2013")
if (rulang) caption_text <- paste("Выбросы парниковых газов в сельском хозяйстве, ",nrow(top12),"стран с самыми высокими показателями в 2013 году")



## ---- P4climateRIGHT ----
dat <- syb.df %>% filter(Year %in% c(2000,2013)) %>%  select(FAOST_CODE,Year,GL.LU.TOT.NERCO2EQ.NO) %>%
  dplyr::mutate(GL.LU.TOT.NERCO2EQ.NO = GL.LU.TOT.NERCO2EQ.NO / 1000)

dat <- dat[!is.na(dat$GL.LU.TOT.NERCO2EQ.NO),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

dat <- dat[which(dat[[region_to_report]]),]

# give name Value for value-col
names(dat)[names(dat)=="GL.LU.TOT.NERCO2EQ.NO"] <- "Value"
# Plot only as many countries as there are for particular region, max 20
nro_latest_cases <- nrow(dat[dat$Year == max(dat$Year),])
if (nro_latest_cases < 20) {ncases <- nro_latest_cases} else ncases <- 20
dat <- arrange(dat, -Year, -Value)
# slice the data for both years
top2015 <- dat %>% slice(1:ncases) %>% dplyr::mutate(color = "2013")
top2000 <- dat %>% filter(FAOST_CODE %in% top2015$FAOST_CODE, Year == 2000) %>% dplyr::mutate(color = "2000")
dat_plot <- rbind(top2015,top2000)
# levels based on newest year
dat_plot$SHORT_NAME <- factor(dat_plot$SHORT_NAME, levels=arrange(top2015,Value)$SHORT_NAME)

if (rulang) levels(dat_plot$SHORT_NAME) <- countrycode.multilang::countrycode(levels(dat_plot$SHORT_NAME), origin = "country.name", destination = "country.name.russian.fao")
if (rulang){
  dat_plot$color[dat_plot$color == "2013"] <- "2013 г."
  dat_plot$color[dat_plot$color == "2000"] <- "2000 г."
}

p <- ggplot(dat_plot, aes(x=SHORT_NAME,y=Value))
p <- p + geom_segment(aes(y = 0, xend = SHORT_NAME, 
                          yend = Value, color=color), alpha=.5)
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75) + theme(panel.grid.major.y = element_blank())
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])

# To make the latest point on top
dat_plot <- arrange(dat_plot, color)

p <- ggplot(data=dat_plot, aes(x=SHORT_NAME, y= Value, fill=color))
p <- p + geom_segment(data=dat_plot %>% select(Year,SHORT_NAME,Value) %>%
                        spread(key = Year, value = Value) %>% 
                        mutate(color=NA), 
                      aes(y = `2000`, xend = SHORT_NAME,
                          yend = `2013`), color="grey80")
p <- p + geom_point(aes(fill=color),size = 4, alpha = 0.75, pch=21, color="white") + theme(panel.grid.major.y = element_blank())
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])


p <- p + coord_flip()
p <- p + labs(x="",y=expression("Mt CO"[2] * "eq"))
if (rulang) p <- p + labs(x="",y=expression("млн тонн CO"[2] * "экв."))
p <- p + guides(color = guide_legend(nrow = 1))
p

# Caption
caption_text <- paste("Land use total emissions, highest",ncases,"countries in 2013")
if (rulang) caption_text <- paste("Общий объем выбросов парниковых газов в результате землепользования,",
                                  ncases,"стран с самыми высокими показателями в 2013 году")



## ---- P4climateBOTTOM ----
if (region_to_report == "RAF")  dat <- syb.df %>% filter(FAOST_CODE %in% 12000, Year %in% 2013) %>% select(SHORT_NAME,GHG.TOT.ALL.GG.NO,GL.FL.NFC.NERCO2EQ.NO,GHG.BS.TECO2EQ.GG.NO,GL.FL.TOT.NERCO2EQ.NO)
if (region_to_report == "RAP")  dat <- syb.df %>% filter(FAOST_CODE %in% 13000, Year %in% 2013) %>% select(SHORT_NAME,GHG.TOT.ALL.GG.NO,GL.FL.NFC.NERCO2EQ.NO,GHG.BS.TECO2EQ.GG.NO,GL.FL.TOT.NERCO2EQ.NO)
if (region_to_report == "REU")  dat <- syb.df %>% filter(FAOST_CODE %in% 14000, Year %in% 2013) %>% select(SHORT_NAME,GHG.TOT.ALL.GG.NO,GL.FL.NFC.NERCO2EQ.NO,GHG.BS.TECO2EQ.GG.NO,GL.FL.TOT.NERCO2EQ.NO)

if (region_to_report == "RNE")  dat <- syb.df %>% filter(FAOST_CODE %in% 15000, Year %in% 2013) %>% select(SHORT_NAME,GHG.TOT.ALL.GG.NO,GL.FL.NFC.NERCO2EQ.NO,GHG.BS.TECO2EQ.GG.NO,GL.FL.TOT.NERCO2EQ.NO)
if (region_to_report == "GLO")  dat <- syb.df %>% filter(FAOST_CODE %in% 5000,  Year %in% 2013) %>% select(SHORT_NAME,GHG.TOT.ALL.GG.NO,GL.FL.NFC.NERCO2EQ.NO,GHG.BS.TECO2EQ.GG.NO,GL.FL.TOT.NERCO2EQ.NO)

dat <- gather(dat, variable, value, 2:5)
dat$fill[dat$variable == "GHG.TOT.ALL.GG.NO"]   <- "All GHG agricultural sectors"
dat$fill[dat$variable == "GL.FL.NFC.NERCO2EQ.NO"] <- "Net forest conversion"
# dat$fill[dat$variable == "GLI.CHPF.TOT.ECO2EQ.NO"] <- "Cultivation histoils and peat fires"
dat$fill[dat$variable == "GHG.BS.TECO2EQ.GG.NO"] <- "Burning savanna"
dat$fill[dat$variable == "GL.FL.TOT.NERCO2EQ.NO"] <- "Forest"


dat$value <- dat$value / 1000 # into thousand gigagrams

dat_plot <- dat

if (rulang){
  
  dat_plot$fill[dat_plot$fill == "All GHG agricultural sectors"]   <- "Выбросы парниковых \nгазов во всех \nсельскохозяйственных секторах"
  dat_plot$fill[dat_plot$fill == "Net forest conversion"]   <- "Чистая величина перевода лесов в нелесные земли"
  dat_plot$fill[dat_plot$fill == "Burning savanna"]   <- "Сжигание саванн"
  dat_plot$fill[dat_plot$fill == "Forest"]   <- "Лесное хозяйство"
  
}



p <- ggplot(dat_plot, aes(x=reorder(fill, -value), y=value, fill=fill))
p <- p + geom_bar(stat="identity", position="dodge")
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, 5)[["Sub"]])
p <- p + labs(x="",y=expression("thousand gigagrams CO"[2] * "eq"))
if (rulang) p <- p + labs(x="",y=expression("тыс. гигаграммов CO"[2] * "-экв."))
p <- p + theme(axis.text.x = element_blank())
p <- p + guides(fill = guide_legend(nrow = 2))
p <- p + scale_y_continuous(labels=space)
p

# Caption
caption_text <- "Emissions by subsectors in 2013"
if (rulang) caption_text <- "Выбросы парниковых газов в подсекторах в 2013 г."



## ---- P4climateMAP ----
dat <- filter(syb.df, Year %in% 2013) %>% select(FAOST_CODE,GHG.AFOLU.TOT.ECO2EQ.NO) %>%
  dplyr::mutate(GHG.AFOLU.TOT.ECO2EQ.NO = GHG.AFOLU.TOT.ECO2EQ.NO / 1000) # into million gigagrams

# dat <- dat[dat$FAOST_CODE != 41,]
dat$FAOST_CODE[dat$FAOST_CODE == 41] <- 351

# set Robinson projection
map.plot <- left_join(map.df,dat) # so that each country in the region will be filled (value/NA)

# Subset
map.plot <- map.plot[which(map.plot[[region_to_report]]),]

cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","GHG.AFOLU.TOT.ECO2EQ.NO")]
cat_data$value_cat <- categories(x=cat_data$GHG.AFOLU.TOT.ECO2EQ.NO, n=5)

map.plot <- left_join(map.plot,cat_data[c("FAOST_CODE","value_cat")])

# define map unit
map_unit <- expression("mln gigagrams CO"[2] * "eq")
if (rulang) map_unit <- expression("млн Гг CO"[2] * "-экв.")

p <- create_map_here()
p

# Caption
caption_text <- "Total greenhouse gas emissions from agriculture, forestry and other land use, mln gigagrams CO\\textsubscript{2} eq (2013)"
if (rulang) caption_text <- "Суммарные выбросы парниковых газов в сельском хозяйстве, лесном хозяйстве и других видах землепользования, в млн гигаграммов эквивалента CO\\textsubscript{2} (2013 г.)"
