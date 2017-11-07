## ---- part4_setup ----

## new data source
## can't get it to work with csv
library(readxl)
url <- paste0("http://fenixservices.fao.org/faostat/static/bulkdownloads/",region_to_report,"_Charts_data_final.xlsx")
destfile <- paste0(region_to_report,"_Charts_data_final.xlsx")
curl::curl_download(url, destfile)
temp <- read_excel(destfile, col_types = c("text", "text", "numeric", "text", 
                                           "text", "text", "numeric", "text", 
                                           "text", "text", "text", "text"))
## if RU, then remove EN names and rename RU columns
if (rulang) {
  temp <- subset(temp, select = -c(AreaName,ItemName))
  names(temp)[names(temp) == 'AreaNameRU'] <- 'AreaName'
  names(temp)[names(temp) == 'ItemNameRU'] <- 'ItemName'
}


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
dat1 <- subset(temp, subset=Part %in% "P4land")
dat1 <- subset(dat1, subset=Position %in% "TOPRIGHT")
dat1 <- subset(dat1, select = c(AreaName,Year,Indicator,Value,ItemName))

## ---- P4landTOPRIGHT ----
dat1$fill[dat1$Indicator == "RL.AREA.AGR.HA.SH"] <- "Agricultural"
dat1$fill[dat1$Indicator == "RL.AREA.FOR.HA.SH"] <- "Forest"
dat1$fill[dat1$Indicator == "RL.AREA.OTH.HA.SH"] <- "Other"
dat1$fill <- factor(dat1$fill, levels=c("Agricultural",
                                      "Forest",
                                      "Other"))


dat_plot <- dat1

dat_plot$AreaName <- factor(dat_plot$AreaName, levels=(dat_plot %>% filter(fill == "Agricultural") %>% arrange(-value))$AreaName)

if (rulang){
  dat_plot$fill <- as.character(dat_plot$fill)
  dat_plot$fill[dat_plot$fill == "Agricultural"] <- "Сельскохозяйственные \nземли"
  dat_plot$fill[dat_plot$fill == "Forest"] <- "Леса"
  dat_plot$fill[dat_plot$fill == "Other"] <- "Другие \nземли"
  dat_plot$fill <- factor(dat_plot$fill, levels=c("Сельскохозяйственные \nземли",
                                                  "Леса",
                                                  "Другие \nземли"
  ))
} 

p <- ggplot(dat_plot, aes(x=AreaName, y=Value, fill=fill))
p <- p + geom_bar(stat="identity", position="stack")
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, 3)[["Sub"]])
p <- p + labs(x="",y="percent\n")
if (rulang) p <- p + labs(x="",y="проценты\n")
p <- p + theme(axis.text.x = element_text(angle=45))
p <- p + coord_cartesian(ylim=c(0,100))
if (rulang) p <- p + guides(fill = guide_legend(nrow = 2))
p



# Caption
caption_text <- paste("Land area (",dat1$Year[1],")", sep = "")
if (rulang) caption_text <- paste("Земельные площади (",dat1$Year[1]," г.)", sep = "")


## ---- P4landLEFT ----
dat1 <- subset(temp, subset=Part %in% "P4land")
dat1 <- subset(dat1, subset=Position %in% "LEFT")
dat1 <- subset(dat1, select = c(AreaName,Value,Year))

# top for this plot
dat_plot1 <- dat1 %>% 
  group_by(AreaName) %>% 
  ungroup() %>% 
  arrange(-Value) %>% 
  slice(1:20) %>% 
  dplyr::mutate(color = "2015")

dat_plot1$AreaName <- fct_reorder(dat_plot1$AreaName, dat_plot1$Value) 

p <- ggplot(dat_plot1, aes(x=AreaName,y=Value))
p <- p + geom_segment(aes(y = min(dat_plot1$Value), xend = AreaName, 
                          yend = Value, color=color), alpha=.5)
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75) + theme(panel.grid.major.y = element_blank())
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 1)[["Sub"]])
p <- p + theme(legend.position = "none") # hide legend as only one year plotted
p <- p + coord_flip()
p <- p + labs(x="",y="\n\nha/cap")
if (rulang) p <- p + labs(x="",y="\nга на душу населения")
p <- p + guides(color = guide_legend(nrow = 2))
p


# Caption
caption_text <- paste("Arable land per capita, top ",nrow(dat_plot1)," countries (",dat1$Year[1],")", sep = "")
if (rulang) caption_text <- paste("Пахотные земли на душу населения, ",nrow(dat_plot1)," стран с самыми высокими значениями (",dat1$Year[1]," г.)", sep = "")


## ---- P4landRIGHT ----
dat1 <- subset(temp, subset=Part %in% "P4land")
dat1 <- subset(dat1, subset=Position %in% "RIGHT")
dat1 <- subset(dat1, select = c(AreaName,Value,Year))

# top for this plot
dat_plot1 <- dat1 %>% 
  group_by(AreaName) %>% 
  ungroup() %>% 
  arrange(-Value) %>% 
  slice(1:20) %>% 
  dplyr::mutate(color = "2015")

dat_plot1$AreaName <- fct_reorder(dat_plot1$AreaName, dat_plot1$Value) 

p <- ggplot(dat_plot1, aes(x=AreaName,y=Value))
p <- p + geom_segment(aes(y = min(dat_plot1$Value), xend = AreaName, 
                          yend = Value, color=color), alpha=.5)
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75) + theme(panel.grid.major.y = element_blank())
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 1)[["Sub"]])
p <- p + theme(legend.position = "none") # hide legend as only one year plotted
p <- p + coord_flip()
p <- p + labs(x="",y="\n\nha/cap")
if (rulang) p <- p + labs(x="",y="\nга на душу населения")
p <- p + guides(color = guide_legend(nrow = 2))
p


# Caption
caption_text <- paste("Arable land per capita, bottom ",nrow(dat_plot1)," countries (",dat1$Year[1],")", sep = "")
if (rulang) caption_text <- paste("Пахотные земли на душу населения, ",nrow(dat_plot1)," стран с самыми низкими значениями (",dat1$Year[1]," г.)", sep = "")



## ---- P4landBOTTOM ----
dat1 <- subset(temp, subset=Part %in% "P4land")
dat1 <- subset(dat1, subset=Position %in% "BOTTOM")
dat1 <- subset(dat1, select = c(AreaName,Indicator,Value,Year))

dat1$fill[dat1$Indicator == "RL.AREA.ARBL.HA.SH"]   <- "Arable"
dat1$fill[dat1$Indicator == "RL.AREA.PRMNCR.HA.SH"] <- "Permanent crops"
dat1$fill[dat1$Indicator == "RL.AREA.PRMNMP.HA.SH"] <- "Permanent meadows and pastures"
dat1$fill <- factor(dat1$fill, levels=c("Arable",
                                      "Permanent crops",
                                      "Permanent meadows and pastures"))
dat_plot <- dat1

dat_plot$AreaName <- factor(dat_plot$AreaName, levels=(dat_plot %>% filter(fill == "Arable") %>% arrange(-Value))$AreaName)

ncases <- nrow(dat_plot)

if (rulang){
  dat_plot$fill[dat_plot$fill == "Arable"]   <- "Пахотные \nземли"
  dat_plot$fill[dat_plot$fill == "Permanent crops"]   <- "Многолетние \nсельскохозяйственные культуры"
  dat_plot$fill[dat_plot$fill == "Permanent meadows and pastures"]   <- "Постоянные луга \nи пастбища"
  dat_plot$fill <- factor(dat_plot$fill, levels=c("Пахотные \nземли",
                                                  "Многолетние \nсельскохозяйственные культуры",
                                                  "Постоянные луга \nи пастбища"))
}

p <- ggplot(dat_plot, aes(x=AreaName, y=Value, fill=fill))
p <- p + geom_bar(stat="identity", position="stack")
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, 3)[["Sub"]])
p <- p + labs(x="",y="percent\n")
if (rulang) p <- p + labs(x="",y="проценты\n")
p <- p + theme(axis.text.x = element_text(angle=45))
p <- p + coord_cartesian(ylim=c(0,100))
p



# Caption
caption_text <- paste("Agricultural area (",dat1$Year[1],")", sep = "")
if (rulang) caption_text <- paste("Сельскохозяйственные земли (",dat1$Year[1]," г.)", sep = "")



## ---- P4landMAP ----
dat1 <- subset(temp, subset=Part %in% "P4land")
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
# map_unit <- "m² per capita"
map_unit <- "ha per capita"
if (rulang) map_unit <- "га/чел"

p <- create_map_here()
p



# Caption
caption_text <- paste("Cropland per capita, ha/cap (",dat1$Year[1],")", sep = "")
if (rulang) caption_text <- paste("Земли под сельскохозяйственными культурами на душу населения, га/чел (",dat1$Year[1]," г.)", sep = "")


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
dat1 <- subset(temp, subset=Part %in% "P4water")
dat1 <- subset(dat1, subset=Position %in% "TOPRIGHT")
dat1 <- subset(dat1, select = c(AreaName,Year,Indicator,Value))

## ---- P4waterTOPRIGHT, eval=P4water, top_right_plot=P4water, fig.height=top_right_plot_height, fig.width=top_right_plot_width ----
minYr <- min(dat1$Year)
maxYr <- max(dat1$Year)

# bottom five
b_5 <- head(arrange(filter(dat1, Year == maxYr), Value),5)
b_5_00 <- dat1[dat1$Year == minYr & dat1$AreaName %in% unique(b_5$AreaName),]
dat_plot <- rbind(b_5,b_5_00)

dat_plot$AreaName <- factor(dat_plot$AreaName, levels=arrange(dat_plot[dat_plot$Year == maxYr,], Value)$AreaName)

if (rulang){
  dat_plot$Year <- paste(dat_plot$Year,"r.")
}

p <- ggplot(dat_plot, aes(x=AreaName, y=Value, fill=Year))
p <- p + geom_bar(stat="identity", position="dodge")
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + labs(x=NULL,y=expression(m^"3"/yr/person))
if (rulang) p <- p + labs(x="",y=expression(м^"3"/год/чел))
p <- p + theme(axis.text.x = element_text(angle=45))
p


# Caption
caption_text <- "Countries with the lowest renewable water resources per capita"
if (rulang) caption_text <- "Страны с самыми низкими показателями возобновляемых водных ресурсов на душу населения"

## ---- P4waterLEFT ----
dat1 <- subset(temp, subset=Part %in% "P4water")
dat1 <- subset(dat1, subset=Position %in% "LEFT")
dat1 <- subset(dat1, select = c(AreaName,Value,Year))

# top for this plot
dat_plot1 <- dat1 %>% 
  group_by(AreaName) %>% 
  ungroup() %>% 
  arrange(-Value) %>% 
  slice(1:20) %>% 
  dplyr::mutate(color = "2015")

dat_plot1$AreaName <- fct_reorder(dat_plot1$AreaName, dat_plot1$Value) 

p <- ggplot(dat_plot1, aes(x=AreaName,y=Value))
p <- p + geom_segment(aes(y = min(dat_plot1$Value), xend = AreaName, 
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
caption_text <- paste("Freshwater withdrawal by industrial sector, share of total, top ",nrow(dat_plot1)," countries (",dat1$Year[1],")", sep = "")
if (rulang) caption_text <- paste("Забор пресной воды на промышленные нужды как процентная доля от общего водозабора, ",nrow(dat_plot1)," стран с самыми высокими значениями (с ",dat1$Year[1]," гг.)", sep = "")


## ---- P4waterRIGHT ----
dat1 <- subset(temp, subset=Part %in% "P4water")
dat1 <- subset(dat1, subset=Position %in% "RIGHT")
dat1 <- subset(dat1, select = c(AreaName,Value,Year))

# top for this plot
dat_plot1 <- dat1 %>% 
  group_by(AreaName) %>% 
  ungroup() %>% 
  arrange(-Value) %>% 
  slice(1:20) %>% 
  dplyr::mutate(color = "2015")

dat_plot1$AreaName <- fct_reorder(dat_plot1$AreaName, dat_plot1$Value) 

p <- ggplot(dat_plot1, aes(x=AreaName,y=Value))
p <- p + geom_segment(aes(y = min(dat_plot1$Value), xend = AreaName, 
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
caption_text <- paste("Freshwater withdrawal by agricultural sector, share of total, top ",nrow(dat_plot1)," countries (",dat1$Year[1],")", sep = "")
if (rulang) caption_text <- paste("Забор пресной воды на сельскохозяйственные нужды как процентная доля от общего водозабора,",nrow(dat_plot1),"стран с самыми высокими значениями (с ",dat1$Year[1]," гг.)", sep = "")


## ---- P4waterBOTTOM ----
dat1 <- subset(temp, subset=Part %in% "P4water")
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
p <- p + labs(x=NULL,y=expression(m^"3"/yr/person))
if (rulang) p <- p + labs(x="",y=expression(м^"3"/год/человек))
p <- p + theme(axis.text.x = element_text(angle=45))
p


# Caption
caption_text <- "Countries with the highest renewable water resources per capita"
if (rulang) caption_text <- "Страны с самыми высокими показателями возобновляемых водных ресурсов на душу населения"


## ---- P4waterMAP ----
dat1 <- subset(temp, subset=Part %in% "P4water")
dat1 <- subset(dat1, subset=Position %in% "MAP")
dat1 <- subset(dat1, select = c(AreaCode,Value,Year))
dat1$AreaCode <- as.integer(dat1$AreaCode)

map.plot <- left_join(map.df,dat1, by = c("FAOST_CODE" = "AreaCode")) # so that each country in the region will be filled (value/NA)

# Add region key and subset

map.plot <- map.plot[which(map.plot[[region_to_report]]),]

cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","Value")]
cat_data$value_cat <- categories(x=cat_data$Value, n=4, method="jenks",decimals=3)

map.plot <- left_join(map.plot,cat_data[c("FAOST_CODE","value_cat")])

# define map unit
map_unit <- "percent"
if (rulang) map_unit <- "проценты"

p <- create_map_here()
p

# Caption
caption_text <- paste("Freshwater resources withdrawn by agriculture (percent, ",dat1$Year[1],"*)", sep = "")
if (rulang) caption_text <- paste("Забор воды из пресноводных ресурсов на сельскохозяйственные нужды (в процентах, ",dat1$Year[1]," гг.*)", sep = "")


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
dat1 <- subset(temp, subset=Part %in% "P4energy")
dat1 <- subset(dat1, subset=Position %in% "TOPRIGHT")
dat1 <- subset(dat1, select = c(AreaName,Year,Indicator,Value))
dat1$Year <- as.integer(dat1$Year)

## ---- P4energyTOPRIGHT ----
dat_plot <- dat1
minYr <- min(dat1$Year)
maxYr <- max(dat1$Year)

p <- ggplot(data = dat_plot, aes(x = Year, y = Value,group=AreaName,color=AreaName))
p <- p + geom_line(size=1.1, alpha=.7)
p <- p + scale_color_manual(values = plot_colors(part = 1, length(unique(dat_plot$AreaName)))[["Sub"]])
p <- p + labs(x="",y="% of tot energy production\n")
if (rulang) p <- p + labs(x="",y="% в общем объеме \nпроизводства энергии\n")
p <- p + guides(color = guide_legend(nrow = length(unique(dat_plot$AreaName))))
p <- p + scale_x_continuous(breaks=c(minYr,2003,2006,maxYr))
p


# Caption
caption_text <- "Bioenergy production, share of total energy production"
if (rulang) caption_text <- "Производство биоэнергии, доля в общем объеме производства энергии"




## ---- P4energyLEFT ----
dat1 <- subset(temp, subset=Part %in% "P4energy")
dat1 <- subset(dat1, subset=Position %in% "LEFT")
dat1 <- subset(dat1, select = c(AreaName,Value,Year))

# top for this plot
dat_plot1 <- dat1 %>% 
  group_by(AreaName) %>% 
  ungroup() %>% 
  arrange(-Value) %>% 
  slice(1:20) %>% 
  dplyr::mutate(color = "2015")

dat_plot1$AreaName <- fct_reorder(dat_plot1$AreaName, dat_plot1$Value) 

p <- ggplot(dat_plot1, aes(x=AreaName,y=Value))
p <- p + geom_segment(aes(y = min(dat_plot1$Value), xend = AreaName, 
                          yend = Value, color=color), alpha=.5)
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75) + theme(panel.grid.major.y = element_blank())
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 1)[["Sub"]])
p <- p + theme(legend.position = "none") # hide legend as only one year plotted
p <- p + coord_flip()
p <- p + labs(x="",y="\n% of tot energy production")
if (rulang) p <- p + labs(x="",y="\n% в общ. объеме \nпроизв. энергии")
p <- p + guides(color = guide_legend(nrow = 2))
p


# Caption
caption_text <- paste("Bioenergy production, share of total energy production, top ",nrow(dat_plot1)," countries (",dat1$Year[1],")", sep = "")
if (rulang) caption_text <- paste("Производство биоэнергии, доля в общем производстве энергии, ",nrow(dat_plot1)," стран с самыми высокими показателями в (",dat1$Year[1]," году)", sep = "")

## ---- P4energyRIGHT ----
dat1 <- subset(temp, subset=Part %in% "P4energy")
dat1 <- subset(dat1, subset=Position %in% "RIGHT")
dat1 <- subset(dat1, select = c(AreaName,Value,Year))

# top for this plot
dat_plot1 <- dat1 %>% 
  group_by(AreaName) %>% 
  ungroup() %>% 
  arrange(-Value) %>% 
  slice(1:20) %>% 
  dplyr::mutate(color = "2015")

dat_plot1$AreaName <- fct_reorder(dat_plot1$AreaName, dat_plot1$Value) 

p <- ggplot(dat_plot1, aes(x=AreaName,y=Value))
p <- p + geom_segment(aes(y = min(dat_plot1$Value), xend = AreaName, 
                          yend = Value, color=color), alpha=.5)
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75) + theme(panel.grid.major.y = element_blank())
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 1)[["Sub"]])
p <- p + theme(legend.position = "none") # hide legend as only one year plotted
p <- p + coord_flip()
p <- p + labs(x="",y="\n% of tot energy production")
if (rulang) p <- p + labs(x="",y="\n% в общ. объеме \nпроизв. энергии")
p <- p + guides(color = guide_legend(nrow = 2))
p


# Caption
caption_text <- paste("Energy use in agriculture and forestry, share of total energy consumption, top ",nrow(dat_plot1)," countries (",dat1$Year[1],")", sep = "")
if (rulang) caption_text <- paste("Потребление энергии в сельском и лесном хозяйстве, доля в общем объеме потребления энергии, ",nrow(dat_plot1)," стран с самыми высокими показателями в (",dat1$Year[1]," г.)", sep = "")


## ---- P4energyBOTTOM ----
dat1 <- subset(temp, subset=Part %in% "P4energy")
dat1 <- subset(dat1, subset=Position %in% "BOTTOM")
dat1 <- subset(dat1, select = c(AreaName,Year,Value))
dat1$Year <- as.integer(dat1$Year)

minYr <- min(dat1$Year)
maxYr <- max(dat1$Year)

dat_plot <- dat1[!is.na(dat1$Value),]

p <- ggplot(dat_plot, aes(x=Year,y=Value,color=AreaName))
p <- p + geom_line(size=1.1, alpha=.7)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, length(unique(dat_plot$AreaName)))[["Sub"]])
p <- p + labs(x="",y="% of total energy consumption\n")
if (rulang) p <- p + labs(x="",y="% в общем объеме производства энергии\n")
p <- p + guides(color = guide_legend(nrow = 3))
p <- p + scale_x_continuous(breaks=c(minYr,2003,2006,maxYr))
p


# Caption
caption_text <- "Energy use in agriculture and forestry, share of total energy consumption"
if (rulang) caption_text <- "Потребление энергии в сельском и лесном хозяйстве, доля в общем объеме потребления энергии"

## ---- P4energyMAP ----
dat1 <- subset(temp, subset=Part %in% "P4energy")
dat1 <- subset(dat1, subset=Position %in% "MAP")
dat1 <- subset(dat1, select = c(AreaCode,Value,Year))
dat1$AreaCode <- as.integer(dat1$AreaCode)

map.plot <- left_join(map.df,dat1, by = c("FAOST_CODE" = "AreaCode")) # so that each country in the region will be filled (value/NA)

# Add region key and subset

map.plot <- map.plot[which(map.plot[[region_to_report]]),]

#if (region_to_report %in% c("RAP","GLO"))  cat_data$value_cat <- categories(x=cat_data$GN_6808_72182,manual = TRUE, manual_breaks = c(5,25,50,500,7769))
#if (!region_to_report %in% c("RAP","GLO"))  cat_data$value_cat <- categories(x=cat_data$GN_6808_72182)
cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","Value")]
cat_data$value_cat <- categories(x=cat_data$Value, n=5, method="jenks")

map.plot <- left_join(map.plot,cat_data[c("FAOST_CODE","value_cat")])

# define map unit
map_unit <- "TJ"
if (rulang) map_unit <- "ТДж"

p <- create_map_here()
p

# Caption
caption_text <- paste("Energy consumption for power irrigation, TJ (",dat1$Year[1],"*)", sep = "")
if (rulang) caption_text <- paste("Потребление энергии для орошения машинным способом, ТДж (",dat1$Year[1]," гг.*)", sep = "")


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
dat1 <- subset(temp, subset=Part %in% "P4forestry")
dat1 <- subset(dat1, subset=Position %in% "TOPRIGHT")
dat1 <- subset(dat1, select = c(AreaName,Year,Indicator,Value))

## ---- P4forestryTOPRIGHT ----
dat1$Year <- as.integer(dat1$Year)
minYr <- min(dat1$Year)
maxYr <- max(dat1$Year)

if (rulang){
  dat1$fill[dat1$Indicator == "Paper and paperboard"] <- "Бумага и \nкартон"
  dat1$fill[dat1$Indicator == "Recovered paper"] <- "Рекуперированная \nбумага"
  dat1$fill[dat1$Indicator == "Wood pulp"] <- "Древесная \nцеллюлоза"
} else {
  dat1$fill[dat1$Indicator == "FO.PRD.RP.M3.NO"] <- "Recovered paper"
  dat1$fill[dat1$Indicator == "FO.PRD.WP.M3.NO"] <- "Wood pulp"
  dat1$fill[dat1$Indicator == "FO.PRD.PPB.M3.NO"] <- "Paper and paperboard"}

p <- ggplot(dat1, aes(x=Year, y=Value, color=fill))
p <- p + geom_line(size=1.1, alpha=.7)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 3)[["Sub"]])
p <- p + labs(x="",y="million tonnes\n")
if (rulang) p <- p + labs(x="",y="млн тонн\n")
p <- p + guides(color = guide_legend(nrow = 2))
p <- p + scale_x_continuous(breaks=c(1960,1980,2000,maxYr))
p


# Caption
caption_text <- "Production of selected forest products"
if (rulang) caption_text <- "Производство отдельных лесопродуктов"


## ---- P4forestryLEFT ----
dat1 <- subset(temp, subset=Part %in% "P4forestry")
dat1 <- subset(dat1, subset=Position %in% "LEFT")
dat1 <- subset(dat1, select = c(AreaName,Value,Year))

# top for this plot
dat_plot1 <- dat1 %>% 
  group_by(AreaName) %>% 
  ungroup() %>% 
  arrange(-Value) %>% 
  slice(1:20) %>% 
  dplyr::mutate(color = "2015")

dat_plot1$AreaName <- fct_reorder(dat_plot1$AreaName, dat_plot1$Value) 

p <- ggplot(dat_plot1, aes(x=AreaName,y=Value))
p <- p + geom_segment(aes(y = min(dat_plot1$Value), xend = AreaName, 
                          yend = Value, color=color), alpha=.5)
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75) + theme(panel.grid.major.y = element_blank())
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 1)[["Sub"]])
p <- p + theme(legend.position = "none") # hide legend as only one year plotted
p <- p + coord_flip()
p <- p + labs(x="",y="\nbillion US$")
if (rulang) p <- p + labs(x="",y="\nмлрд долл. США")
p <- p + guides(color = guide_legend(nrow = 2))
p


# Caption
caption_text <- paste("Top ",nrow(dat_plot1)," exporters of forest products (",dat1$Year[1],")", sep = "")
if (rulang) caption_text <- caption_text <- paste(nrow(dat_plot1)," самых крупных экспортеров лесопродукции (",dat1$Year[1]," г.)", sep = "")


## ---- P4forestryRIGHT ----
dat1 <- subset(temp, subset=Part %in% "P4forestry")
dat1 <- subset(dat1, subset=Position %in% "RIGHT")
dat1 <- subset(dat1, select = c(AreaName,Value,Year))

# top for this plot
dat_plot1 <- dat1 %>% 
  group_by(AreaName) %>% 
  ungroup() %>% 
  arrange(-Value) %>% 
  slice(1:20) %>% 
  dplyr::mutate(color = "2015")

dat_plot1$AreaName <- fct_reorder(dat_plot1$AreaName, dat_plot1$Value) 

p <- ggplot(dat_plot1, aes(x=AreaName,y=Value))
p <- p + geom_segment(aes(y = min(dat_plot1$Value), xend = AreaName, 
                          yend = Value, color=color), alpha=.5)
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75) + theme(panel.grid.major.y = element_blank())
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 1)[["Sub"]])
p <- p + theme(legend.position = "none") # hide legend as only one year plotted
p <- p + coord_flip()
p <- p + labs(x="",y="\nbillion US$")
if (rulang) p <- p + labs(x="",y="\nмлрд долл. США")
p <- p + guides(color = guide_legend(nrow = 2))
p


# Caption
caption_text <- paste("Top ",nrow(dat_plot1)," importers of forest products (",dat1$Year[1],")", sep = "")
if (rulang) caption_text <- paste(nrow(dat_plot1)," самых крупных импортеров лесопродукции (",dat1$Year[1]," г.)", sep = "")


## ---- P4forestryBOTTOM ----

dat1 <- subset(temp, subset=Part %in% "P4forestry")
dat1 <- subset(dat1, subset=Position %in% "BOTTOM")
dat1 <- subset(dat1, select = c(AreaName,Indicator,Value,Year))

dat1$fill[dat1$Indicator == "PrimFor"] <- "primary forest"
dat1$fill[dat1$Indicator == "PlantFor"] <- "planted forest"
dat1$fill[dat1$Indicator == "NatRegFor"] <- "other naturally regenerated forest"

dat_plot <- dat1

dat_plot$AreaName <- factor(dat_plot$AreaName, levels=(dat_plot %>% filter(fill == "primary forest") %>% arrange(-Value))$AreaName)

ncases <- nrow(dat_plot)

if (rulang){
  dat_plot$fill[dat_plot$fill == "primary forest"] <- "девственные леса"
  dat_plot$fill[dat_plot$fill == "planted forest"] <- "лесонасаждения"
  dat_plot$fill[dat_plot$fill == "other naturally regenerated forest"] <- "леса, восстанавливаемые \nестественным путем"
}

p <- ggplot(dat_plot, aes(x=AreaName, y=Value, fill=fill))
p <- p + geom_bar(stat="identity", position="stack")
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, 3)[["Sub"]])
p <- p + labs(x="",y="million ha\n")
if (rulang) p <- p + labs(x="",y="млн га\n")
p <- p + theme(axis.text.x = element_text(angle=45))
p


# Caption
caption_text <- paste("Forest characteristics (",dat1$Year[1],")", sep = "")
if (rulang) caption_text <- paste("Характеристики леса (",dat1$Year[1]," г.)", sep = "")


## ---- P4forestryMAP ----

dat1 <- subset(temp, subset=Part %in% "P4forestry")
dat1 <- subset(dat1, subset=Position %in% "MAP")
dat1 <- subset(dat1, select = c(AreaCode,Value,Year))
dat1$AreaCode <- as.integer(dat1$AreaCode)

map.plot <- left_join(map.df,dat1, by = c("FAOST_CODE" = "AreaCode")) # so that each country in the region will be filled (value/NA)

# Add region key and subset

map.plot <- map.plot[which(map.plot[[region_to_report]]),]

cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","Value")]
cat_data$value_cat <- categories(x=cat_data$Value, n=5, method="jenks")

map.plot <- left_join(map.plot,cat_data[c("FAOST_CODE","value_cat")])
# define map unit
map_unit <- "percent"
if (rulang) map_unit <- "проценты"

p <- create_map_here()
p

# Caption
caption_text <- paste("Forest area as share of total land area, percent (",dat1$Year[1],")", sep = "")
if (rulang) caption_text <- paste("Доля площади лесов в общей площади суши, в процентах (",dat1$Year[1]," г.)", sep = "")


#    ____   _   _                       _                     _
#   / ___| | | (_)  _ __ ___     __ _  | |_    ___      ___  | |__     __ _   _ __     __ _    ___
#  | |     | | | | | '_ ` _ \   / _` | | __|  / _ \    / __| | '_ \   / _` | | '_ \   / _` |  / _ \
#  | |___  | | | | | | | | | | | (_| | | |_  |  __/   | (__  | | | | | (_| | | | | | | (_| | |  __/
#   \____| |_| |_| |_| |_| |_|  \__,_|  \__|  \___|    \___| |_| |_|  \__,_| |_| |_|  \__, |  \___|
#                                                                                     |___/

## ---- P4climateTEXT ----
spread_title <- "Climate change"
if (region_to_report == "RAF") short_text <- "The severity and speed of climate change is presenting an unprecedented challenge. Current global surface temperatures are now about 0.6 degrees Celsius higher than the average for the last century. This increase is consistent with model predictions of the effects of rising atmospheric concentrations of carbon dioxide (CO\\textsubscript{2}) and other greenhouse gases (GHGs), which are a result of human activity. Compared to global figures, greenhouse gas emissions in agriculture from Africa are considerably low, the highest been about 300,000 gigagrams CO2eq as compared to over 2million gigagrams CO2eq globally in 2010.  The poorest and most food-insecure regions around the globe are the most vulnerable." # Already scarce land and water resources will likely become even more scarce, and insufficient technical and financial means will make adaptation to a changing climate very difficult."
if (region_to_report == "RAP") short_text <- "The severity and speed of climate change is presenting an unprecedented challenge. Current global surface temperatures are now about 0.6 degrees Celsius higher than the average for the last century. This increase is consistent with model predictions of the effects of rising atmospheric concentrations of carbon dioxide (CO\\textsubscript{2}) and other greenhouse gases (GHGs), which are a result of human activity. The poorest and most food-insecure regions around the globe are the most vulnerable. Already scarce land and water resources will likely become even more scarce, and insufficient technical and financial means will make adaptation to a changing climate very difficult."
if (region_to_report == "REU") short_text <- "The severity and speed of climate change is presenting an unprecedented challenge. Current global surface temperatures are now about 0.6 degrees Celsius higher than the average for the last century. This increase is consistent with model predictions of the effects of rising atmospheric concentrations of carbon dioxide (CO\\textsubscript{2}) and other greenhouse gases (GHGs), which are a result of human activity. The poorest and most food-insecure regions around the globe are the most vulnerable. Already scarce land and water resources will likely become even scarcer, and insufficient technical and financial means will make adaptation to a changing climate very difficult."
if (region_to_report == "RNE") short_text <- "The severity and speed of climate change is presenting an unprecedented challenge. Current global surface temperatures are now about 0.6 degrees Celsius higher than the average for the last century. This increase is consistent with model predictions of the effects of rising atmospheric concentrations of carbon dioxide (CO\\textsubscript{2}) and other greenhouse gases (GHGs), which are a result of human activity. The region contributes to the globally 2 million gigagrams CO\\textsubscript{2} eq of greenhouse gas emission in agriculture by less than 200,000 gigagrams CO\\textsubscript{2} eq."
if (region_to_report == "GLO") short_text <- "The severity and speed of climate change is presenting an unprecedented challenge. Current global surface temperatures are now about 0.6 degrees Celsius higher than the average for the last century. This increase is consistent with model predictions of the effects of rising atmospheric concentrations of carbon dioxide (CO\\textsubscript{2}) and other greenhouse gases (GHGs), which are a result of human activity. The poorest and most food-insecure regions around the globe are the most vulnerable. Already scarce land and water resources will likely become even more scarce, and insufficient technical and financial means will make adaptation to a changing climate very difficult."
if (rulang) spread_title <- "Изменение климата"
if (region_to_report == "REU" & rulang) short_text <- "Интенсивность и скорость изменения климата являются на сегодняшний день беспрецедентным вызовом всему миру. Текущие глобальные температуры земной поверхности примерно на 0,6 градуса по Цельсию выше, чем в среднем за последнее столетие. Этот рост соответствует модельным прогнозам последствий повышения концентрации в атмосфере углекислого газа (СО2) и других парниковых газов, которые являются результатом человеческой деятельности. Самые бедные регионы мира и регионы, в которых отсутствует продовольственная безопасность, являются наиболее уязвимыми. И так скудные земельные и водные ресурсы станут, вероятно, еще более ограниченными, а недостаточность технических и финансовых средств в значительной степени затруднит процесс приспособления к изменению климата."

## ---- P4climateData ----
dat1 <- subset(temp, subset=Part %in% "P4climate")
dat1 <- subset(dat1, subset=Position %in% "TOPRIGHT")
dat1 <- subset(dat1, select = c(AreaName,Year,Indicator,Value))

## ---- P4climateTOPRIGHT ----
dat1$Year <- as.integer(dat1$Year)
dat_plot <- dat1
minYr <- min(dat1$Year)
maxYr <- max(dat1$Year)

p <- ggplot(data = dat_plot, aes(x = Year, y = Value,group=AreaName,color=AreaName))
p <- p + geom_line(size=1.1, alpha=.7)
p <- p + scale_color_manual(values = plot_colors(part = 1, length(unique(dat_plot$AreaName)))[["Sub"]])
p <- p + labs(x="",y=expression("thousand gigagrams CO"[2] * "eq"))
if (rulang) p <- p + labs(x="",y=expression("тыс. гигаграммов CO"[2] * "-экв"))
p <- p + guides(color = guide_legend(nrow = length(unique(dat_plot$AreaName))))
p <- p + scale_x_continuous(breaks=c(minYr,2005,2010,maxYr))
p


# Caption
caption_text <- "Greenhouse gas emissions in agriculture"
if (rulang) caption_text <- "Выбросы парниковых газов в сельском хозяйстве"


## ---- P4climateLEFT ----
dat1 <- subset(temp, subset=Part %in% "P4climate")
dat1 <- subset(dat1, subset=Position %in% "LEFT")
dat1 <- subset(dat1, select = c(AreaName,Year,Indicator,Value))
dat1 <- dat1 %>% 
  dplyr::mutate(Yr = substr(dat1$Year,1,4))
minYr <- min(dat1$Year)
maxYr <- max(dat1$Year)
dat1$Yr <- as.integer((dat1$Year))


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
                      aes_(y = as.name(minYr), xend = quote(AreaName),
                           yend = as.name(maxYr)), color="grey80")
p <- p + geom_point(aes(fill=color),size = 4, alpha = 0.75, pch=21, color="white") + theme(panel.grid.major.y = element_blank())
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y=expression("1000 gigagrams CO"[2] * "eq"))
if (rulang) p <- p + labs(x="",y=expression("тыс. гигаграммов CO"[2] * "экв."))
p <- p + guides(color = guide_legend(nrow = 1))
p


# Caption
caption_text <- paste("Greehouse gas emissions in agriculture, top ",ncases," countries in ",dat1$Year[1],"", sep = "")
if (rulang) caption_text <- paste("Выбросы парниковых газов в сельском хозяйстве, ",nrow(top12)," стран с самыми высокими показателями в ",dat1$Year[1]," году", sep = "")



## ---- P4climateRIGHT ----
dat1 <- subset(temp, subset=Part %in% "P4climate")
dat1 <- subset(dat1, subset=Position %in% "RIGHT")
dat1 <- subset(dat1, select = c(AreaName,Year,Indicator,Value))
dat1 <- dat1 %>% 
  dplyr::mutate(Yr = substr(dat1$Year,1,4))
minYr <- min(dat1$Year)
maxYr <- max(dat1$Year)
dat1$Yr <- as.integer((dat1$Year))


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
                      aes_(y = as.name(minYr), xend = quote(AreaName),
                           yend = as.name(maxYr)), color="grey80")
p <- p + geom_point(aes(fill=color),size = 4, alpha = 0.75, pch=21, color="white") + theme(panel.grid.major.y = element_blank())
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y=expression("1000 gigagrams CO"[2] * "eq"))
if (rulang) p <- p + labs(x="",y=expression("тыс. гигаграммов CO"[2] * "экв."))
p <- p + guides(color = guide_legend(nrow = 1))
p



# Caption
caption_text <- paste("Greehouse gas emissions in forestry and other land use, top ",ncases," countries in ",dat1$Year[1],"", sep = "")
if (rulang) caption_text <- paste("Общий объем выбросов парниковых газов в результате землепользования,",
                                  ncases,"стран с самыми высокими показателями в ",dat1$Year[1]," году", sep = "")



## ---- P4climateBOTTOM ----
dat1 <- subset(temp, subset=Part %in% "P4climate")
dat1 <- subset(dat1, subset=Position %in% "BOTTOM")
dat1 <- subset(dat1, select = c(AreaName,Year,Value,Indicator))
dat1$Year <- as.integer(dat1$Year)

dat1$fill[dat1$Indicator == "GHG.TOT.ALL.GG.NO"]   <- "All GHG agricultural sectors"
dat1$fill[dat1$Indicator == "GL.FL.NFC.NERCO2EQ.NO"] <- "Net forest conversion"
# dat1$fill[dat1$Indicator == "GLI.CHPF.TOT.ECO2EQ.NO"] <- "Cultivation histoils and peat fires"
dat1$fill[dat1$Indicator == "GHG.BS.TECO2EQ.GG.NO"] <- "Burning savanna"
dat1$fill[dat1$Indicator == "GL.FL.TOT.NERCO2EQ.NO"] <- "Forest"

dat_plot <- dat1

if (rulang){
  dat_plot$fill[dat_plot$fill == "All GHG agricultural sectors"]   <- "Выбросы парниковых \nгазов во всех \nсельскохозяйственных секторах"
  dat_plot$fill[dat_plot$fill == "Net forest conversion"]   <- "Чистая величина перевода лесов в нелесные земли"
  dat_plot$fill[dat_plot$fill == "Burning savanna"]   <- "Сжигание саванн"
  dat_plot$fill[dat_plot$fill == "Forest"]   <- "Лесное хозяйство"
}


p <- ggplot(dat_plot, aes(x=reorder(fill, -Value), y=Value, fill=fill))
p <- p + geom_bar(stat="identity", position="dodge")
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, 5)[["Sub"]])
p <- p + labs(x="",y=expression("thousand gigagrams CO"[2] * "eq"))
if (rulang) p <- p + labs(x="",y=expression("тыс. гигаграммов CO"[2] * "-экв."))
p <- p + theme(axis.text.x = element_blank())
p <- p + guides(fill = guide_legend(nrow = 2))
p <- p + scale_y_continuous(labels=space)
p

# Caption
caption_text <- paste("Emissions in agriculture, forestry and other land use in ",dat1$Year[1],"", sep = "")
if (rulang) caption_text <- paste("Выбросы парниковых газов в подсекторах в ",dat1$Year[1]," г.", sep = "")



## ---- P4climateMAP ----
dat1 <- subset(temp, subset=Part %in% "P4climate")
dat1 <- subset(dat1, subset=Position %in% "MAP")
dat1 <- subset(dat1, select = c(AreaCode,Value,Year))
dat1$AreaCode <- as.integer(dat1$AreaCode)

map.plot <- left_join(map.df,dat1, by = c("FAOST_CODE" = "AreaCode")) # so that each country in the region will be filled (value/NA)

# Add region key and subset

map.plot <- map.plot[which(map.plot[[region_to_report]]),]

cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","Value")]
cat_data$value_cat <- categories(x=cat_data$Value, n=5, method="jenks")

map.plot <- left_join(map.plot,cat_data[c("FAOST_CODE","value_cat")])


# define map unit
map_unit <- expression("1000 gigagrams CO"[2] * "eq")
if (rulang) map_unit <- expression("1000 Гг CO"[2] * "-экв.")

p <- create_map_here()
p

# Caption
caption_text <- paste("Total greenhouse gas emissions from agriculture, forestry and other land use (AFOLU), 1000 gigagrams CO\\textsubscript{2} eq (",dat1$Year[1],")", sep = "")
if (rulang) caption_text <- paste("Суммарные выбросы парниковых газов в сельском хозяйстве, лесном хозяйстве и других видах землепользования, в 1000 гигаграммов эквивалента CO\\textsubscript{2} (",dat1$Year[1]," г.)", sep = "")
