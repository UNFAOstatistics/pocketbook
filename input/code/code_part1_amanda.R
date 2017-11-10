
## ---- part1_setup ----
# new data source
# can't get it to work with csv
temp <- readxl::read_excel(paste0(root.dir,"/input_data/",region_to_report,"_Charts_data_final.xlsx"), 
                   col_types = c("text", "text", "numeric", "text",
                                 "text", "text", "numeric", "text",
                                  "text", "text", "text", "text"))

# if RU, then remove EN names and rename RU columns
if (rulang) {
  temp <- subset(temp, select = -c(AreaName,ItemName))
  names(temp)[names(temp) == 'AreaNameRU'] <- 'AreaName'
  names(temp)[names(temp) == 'ItemNameRU'] <- 'ItemName'
}


source(paste0(root.dir,'/input/code/plot/plot_color.R'))
source(paste0(root.dir,"input/code/knitr_hooks.R"))

syb_part <- 1

# Part 1
colPart1 <- plot_colors(part = syb_part, 12)
col.main1 <- colPart1[["Main"]][1]
# color for the grid
col.main2 <- colPart1[["Main"]][2]

source(paste0(root.dir,"/input/code/plot/theme.R"))

nCol <- 5
colPart = plot_colors(part = syb_part, 12)
mapColFun = colorRampPalette(c("white", colPart[["Main"]][1]))
tmpCol = mapColFun(nCol)[2]
mapColFun = colorRampPalette(c(tmpCol, colPart[["Main"]][1]))
mapColors = mapColFun(nCol)

# map functions
source(paste0(root.dir,'/input/code/plot/map_categories.R'))

# if (rulang){
#   syb.df$SHORT_NAME <- ifelse(syb.df$FAOST_CODE <= 351,
#                               countrycode.multilang::countrycode(syb.df$FAOST_CODE, origin = "fao", destination = "country.name.russian.fao"),
#                               syb.df$SHORT_NAME)
# }





#    ___                                  _
#   / _ \  __   __   ___   _ __  __   __ (_)   ___  __      __
#  | | | | \ \ / /  / _ \ | '__| \ \ / / | |  / _ \ \ \ /\ / /
#  | |_| |  \ V /  |  __/ | |     \ V /  | | |  __/  \ V  V /
#   \___/    \_/    \___| |_|      \_/   |_|  \___|   \_/\_/
#



## ---- P1overTEXT ----
spread_title <- "Population"
if (rulang) spread_title <- "Население"
if (region_to_report == "RAF") short_text <- "A combination of declining mortality rates, prolonged life expectancy and younger populations in regions with high fertility contributes to population growth in the world. Since the late 1960s, Africa has experienced unsteady population growth rate; nevertheless its population has tripled since then, to over 1 billion people. Population growth is generally highest where income levels are low. This is especially true in cities. Urbanization is progressing in Africa with two-fifths of people living in cities in 2015."
if (region_to_report == "RAP") short_text <- "A combination of declining mortality rates, prolonged life expectancy and younger populations in regions with high fertility contributes to population growth in the world. While growth rates have been slowing since the late 1960s, the world’s population has nevertheless doubled since then, to over 7 billion people. Population growth is generally highest where income levels are low. This is especially true in cities. Since 2008, there have been more people living in cities than in rural areas."
if (region_to_report == "REU") short_text <- "Declining mortality rates and prolonged life expectancy in the region are not supported by high fertility rates. Thus population growth rates have been slowing since the late 1960s to the practically flat rate at present. Population growth is distributed unevenly across the region, some areas like Central Asia, and Caucasus and Turkey showing population increase while some areas, like EU central and Eastern, and CIS Europe, population decline. About one seventh of the total population of the region lives in rural areas."
if (region_to_report == "REU" & rulang) short_text <- "Снижение показателей смертности и увеличение продолжительности жизни в регионе не сопровождается высоким коэффициентом рождаемости. Поэтому с конца 1960-х годов темпы роста населения замедлялись и в настоящее время достигли практически нулевого уровня. Рост населения неравномерно распределяется по территории региона: в некоторых субрегионах, как например «Центральная Азия», «Кавказ и Турция» наблюдается рост населения, а в некоторых субрегионах, как например «Центральная и Восточная часть ЕС» и «СНГ Европа» численность населения сокращается. Около одной седьмой части общей численности населения региона проживает в сельских районах."
if (region_to_report == "RNE") short_text <- "The declining mortality rates combined with a moderate life expectancy and supported by an extreme fertility contributes to population growth in the world. The region, in turn, have been showing steady population growth since the late 1960s in urban areas compared to lower rates in the rural ones. Population growth is generally higher in the Gulf countries like Qatar United Arab Emirates, Bahrain and Kuwait compared to middle income countries like Morocco, Tunisia and Iran. The last two decades show that there have been more people living in cities than in rural areas."
if (region_to_report == "GLO") short_text <- "A combination of declining mortality rates, prolonged life expectancy and younger populations in regions with high fertility contributes to population growth in the world. While growth rates have been slowing since the late 1960s, the world’s population has nevertheless doubled since then, to over 7 billion people. Population growth is generally highest where income levels are low. This is especially true in cities. Since 2008, there have been more people living in cities than in rural areas."

## ---- P1overData ----
# Retrieve data
dat1 <- subset(temp, subset=Part %in% "P1over")
dat1 <- subset(dat1, subset=Position %in% "TOPRIGHT")
dat1 <- subset(dat1, select = c(Year,Indicator,Value))

## ---- P1overTOPRIGHT ----

names(dat1)[names(dat1) == 'Indicator'] <- 'variable'
names(dat1)[names(dat1) == 'Value'] <- 'value'
dat1$Year <- as.integer(dat1$Year)
dat1$variable[dat1$variable == "OA.TPR.POP.PPL.NO"] <- "Rural population"
dat1$variable[dat1$variable == "OA.TPU.POP.PPL.NO"] <- "Urban population"

# print data for technical report
#datatable(dat1)

# Draw the plot
dat_plot <- dat1
p <- ggplot(dat_plot, aes(x = Year, y = value))
p <- p + geom_area(aes(fill=variable), stat = "identity",position = "stack")
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + labs(x="",y="billion people\n")
if (rulang) p <- p + labs(x="",y="млрд человек\n")
p <- p + geom_vline(aes(xintercept=2017), color="grey20", linetype="dashed")
if (region_to_report != "REU") p <- p + scale_x_continuous(breaks=c(1961,2000,2017))
if (region_to_report == "REU") p <- p + scale_x_continuous(breaks=c(1992,2000,2017))
p <- p + guides(fill = guide_legend(nrow = 2))
p

if (table_type == "latex"){
  if (!rulang){
    cat("\\footnotesize{\\textit{Data after 2017 are projections}}")
  } else cat("\\footnotesize{\\textit{Данные на период после 2017 года являются прогнозом}}")
  cat("\\vspace{1mm}")
} else cat("<br><i>Data after 2017 are projections</i>")

minYr <- min(dat1$Year)
maxYr <- max(dat1$Year)

# Caption
caption_text <- paste("Rural and urban population (",minYr," to ",maxYr,")", sep = "")
if (region_to_report == "REU" & rulang)  caption_text <- paste("Сельское и городское население  (с ",minYr," по ",minYr," гг.)", sep = "")

## ---- P1overLEFT ----
# data
dat1 <- subset(temp, subset=Part %in% "P1over")
dat1 <- subset(dat1, subset=Position %in% "LEFT")
dat1 <- subset(dat1, select = c(AreaName,Value,Year))
dat1$Year <- as.integer(dat1$Year)

dat1 <- arrange(dat1, -Value)

top10 <- dat1 %>% slice(1:10) %>% dplyr::mutate(color = "Highest values")
if (rulang) top10 <- dat1 %>% slice(1:10) %>% dplyr::mutate(color = "Самые высокие значения")

bot10 <- dat1 %>% slice( (nrow(dat1)-9):nrow(dat1)) %>% dplyr::mutate(color = "Lowest values")
if (rulang) bot10 <- dat1 %>% slice( (nrow(dat1)-9):nrow(dat1)) %>% dplyr::mutate(color = "Самые низкие значения")

overlap <- top10$AreaName[top10$AreaName %in% bot10$AreaName]
if (length(overlap)!=0) dat_plot <- rbind(top10[top10$AreaName %in% overlap,], 
                                          bot10[!bot10$AreaName %in% overlap,]) else dat_plot <- rbind(top10,bot10)

dat_plot$AreaName <- fct_reorder(dat_plot$AreaName, dat_plot$Value) 

p <- ggplot(dat_plot, aes(x=AreaName,y=Value))
p <- p + geom_segment(aes(y = 0, xend = AreaName, 
                          yend = Value, color=color), alpha=.5, show.legend = FALSE)
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)  + theme(panel.grid.major.y = element_blank())
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="\npercent")
if (rulang) p <- p + labs(x="",y="\nпроценты")
p <- p + guides(color = guide_legend(nrow = 2))
p

maxYr <- max(dat1$Year)
minYr <- maxYr - 10

# Caption
caption_text <- paste("Population, average annual growth, top and bottom ",nrow(dat_plot)/2," countries (",minYr,"-",maxYr,")",sep = "")
if (rulang) caption_text <- paste("Население, среднегодовые темпы роста, ",nrow(dat_plot)/2," стран с самыми высокими и самыми низкими значениями (",minYr,"-",maxYr," гг.)",sep = "")



## ---- P1overRIGHT ----

dat1 <- subset(temp, subset=Part %in% "P1over")
dat1 <- subset(dat1, subset=Position %in% "RIGHT")
dat1 <- subset(dat1, select = c(AreaName,Value,Year))
dat1$Year <- as.integer(dat1$Year)
dat1 <- arrange(dat1, -Value)

top10 <- dat1 %>% slice(1:10) %>% dplyr::mutate(color = "Highest values")
if (rulang) top10 <- dat1 %>% slice(1:10) %>% dplyr::mutate(color = "Самые высокие значения")

bot10 <- dat1 %>% slice( (nrow(dat1)-9):nrow(dat1)) %>% dplyr::mutate(color = "Lowest values")
if (rulang) bot10 <- dat1 %>% slice( (nrow(dat1)-9):nrow(dat1)) %>% dplyr::mutate(color = "Самые низкие значения")

overlap <- top10$AreaName[top10$AreaName %in% bot10$AreaName]
if (length(overlap)!=0) dat_plot <- rbind(top10[top10$AreaName %in% overlap,], 
                                          bot10[!bot10$AreaName %in% overlap,]) else dat_plot <- rbind(top10,bot10)

dat_plot$AreaName <- fct_reorder(dat_plot$AreaName, dat_plot$Value) 

p <- ggplot(dat_plot, aes(x=AreaName,y=Value))
p <- p + geom_segment(aes(y = min(dat_plot$Value), xend = AreaName, 
                          yend = Value, color=color), alpha=.5, show.legend = FALSE)
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75) + theme(panel.grid.major.y = element_blank())
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="\nyears")
if (rulang) p <- p + labs(x="",y="\nгоды")
p <- p + guides(color = guide_legend(nrow = 2))
p

maxYr <- max(dat1$Year)

# Caption
caption_text <- paste("Life expectancy at birth, top and bottom ",nrow(dat_plot)/2," countries (",maxYr,")", sep = "")
if (rulang) caption_text <- paste("Ожидаемая продолжительность жизни при рождении, ",nrow(dat_plot)/2," стран с самыми высокими самими низкими значениями (",maxYr," г.)", sep = "")


## ---- P1overBOTTOM ----
# data
dat1 <- subset(temp, subset=Part %in% "P1over")
dat1 <- subset(dat1, subset=Position %in% "BOTTOM")
dat1 <- subset(dat1, select = c(AreaName,Year,Value))
dat1$Year <- as.integer(dat1$Year)

dat_plot <- dat1[!is.na(dat1$Value),]

p <- ggplot(dat_plot, aes(x=Year,y=Value,color=AreaName))
p <- p + geom_line(size=1.1, alpha=.7)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, length(unique(dat_plot$AreaName)))[["Sub"]])
p <- p + labs(x="",y="million people\n")
if (rulang) p <- p + labs(x="",y="млн человек\n")
p <- p + guides(color = guide_legend(nrow = 3))
p <- p + scale_x_continuous(breaks=c(1992,2000,2010,2017))
p

minYr <- min(dat1$Year)
maxYr <- max(dat1$Year)

# Caption
caption_text <- paste("Total economically active population (",minYr," to ",maxYr,")", sep = "")
if (rulang) caption_text <- paste("Общая численность экономически активного населения (с ",minYr," по ",maxYr," гг.)", sep = "")


## ---- P1overMAP ----
dat1 <- subset(temp, subset=Part %in% "P1over")
dat1 <- subset(dat1, subset=Position %in% "MAP")
dat1 <- subset(dat1, select = c(AreaCode,Value,Year))
dat1$AreaCode <- as.integer(dat1$AreaCode)
dat1$Year <- as.integer(dat1$Year)

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

maxYr <- max(dat1$Year)

# Caption
caption_text <- paste("Rural population, share of total population (",maxYr,")", sep = "")
if (rulang) caption_text <- paste("Сельское население, доля сельского населения (",maxYr," г.)", sep = "")



#   _____
#  | ____|   ___    ___    _ __     ___    _ __ ___    _   _
#  |  _|    / __|  / _ \  | '_ \   / _ \  | '_ ` _ \  | | | |
#  | |___  | (__  | (_) | | | | | | (_) | | | | | | | | |_| |
#  |_____|  \___|  \___/  |_| |_|  \___/  |_| |_| |_|  \__, |
#                                                      |___/
#


## ---- P1econTEXT ----
spread_title <- "Economy"
if (region_to_report == "RAF") short_text <- "While some sectors have been hard hit, agriculture has demonstrated resilience during the recent economic downturn. In Africa, the agriculture’s contribution to GDP continues to be the highest compared to other regions and the sector is experiencing considerable annual growth across the region.  Changes in the wider economy, including growing global integration, affect the performance of the agriculture sector.  Higher overall economic growth also raises consumers’ incomes and hence food demand. Changing interest rates influence capital investments, land values and storage levels, while inflation affects input prices, revenues and credit costs. Fluctuations in exchange rates have an important bearing on international competitiveness and trade flows."
if (region_to_report == "RAP") short_text <- "While some sectors have been hard hit, agriculture has demonstrated resilience during the recent economic downturn.  Changes in the wider economy, including growing global integration, affect the performance of the agriculture sector.  Higher overall economic growth also raises consumers’ incomes and hence food demand. Changing interest rates influence capital investments, land values and storage levels, while inflation affects input prices, revenues and credit costs. Fluctuations in exchange rates have an important bearing on international competitiveness and trade flows."
if (region_to_report == "REU") short_text <- "Although its share in the total GDP has been steadily declining in the region over the last decade, the value added in agriculture has demonstrated a considerable increase in some countries, especially in Central Asia and Eastern Europe. However, in many of those countries agriculture engages large portions of the population, so the agricultural productivity, expressed in terms of the agriculture value added per worker, is still low. In terms of agricultural productivity the EU part of the region is clearly in lead."
if (region_to_report == "RNE") short_text <- "While it demonstrates very low share in Gulf Countries, the value added in agriculture has considerable share to GDP in Other Near East and North Africa countries, where it has been showing unsteady increase the last decade. It is also shows a very heterogeneous annual growth among different countries, with the highest positive value for Qatar and the lowest in United Arab Emirates. Agricultural value added per worker also shows big differences depending not only on different agricultural productivity but also on the proportion of the population engaged in agriculture in different countries."
if (region_to_report == "GLO") short_text <- "While some sectors have been hard hit, agriculture has demonstrated resilience during the recent economic downturn.  Changes in the wider economy, including growing global integration, affect the performance of the agriculture sector.  Higher overall economic growth also raises consumers’ incomes and hence food demand. Changing interest rates influence capital investments, land values and storage levels, while inflation affects input prices, revenues and credit costs. Fluctuations in exchange rates have an important bearing on international competitiveness and trade flows."
if (rulang) spread_title <- "Экономика"
if (region_to_report == "REU" & rulang) short_text <- "Несмотря на то что доля сельского хозяйства в общем объеме ВВП в регионе за последнее десятилетие неуклонно снижается, добавленная стоимость в сельском хозяйстве продемонстрировала значительный рост в некоторых странах, особенно в странах Центральной Азии и Восточной Европы. Однако во многих из этих стран сельское хозяйство привлекает значительную часть населения, поэтому производительность сельского хозяйства, выраженная в терминах добавленной стоимости в сельском хозяйстве на одного работника, остается по-прежнему низкой. По производительности сельского хозяйства страны ЕС являются бесспорными лидерами."


## ---- P1econTOPRIGHT ----

dat1 <- subset(temp, subset=Part %in% "P1econ")
dat1 <- subset(dat1, subset=Position %in% "TOPRIGHT")
dat1 <- subset(dat1, select = c(AreaName,Indicator,Value,Year))
dat1$Year <- as.integer(dat1$Year)

# Add region key and subset
dat1$fill[dat1$Indicator == "NV.AGR.TOTL.ZS"] <- "Agriculture"
dat1$fill[dat1$Indicator == "NV.IND.TOTL.ZS"] <- "Industry"
dat1$fill[dat1$Indicator == "NV.SRV.TETC.ZS"] <- "Services"

dat1$AreaName <- factor(dat1$AreaName, levels=(dat1 %>% 
                                       dplyr::filter(fill == "Agriculture") %>% 
                                       arrange(-Value))$AreaName)

if (rulang) dat1$fill[dat1$fill == "Agriculture"] <- "в сельском хозяйстве"
if (rulang) dat1$fill[dat1$fill == "Industry"] <- "в промышленности"
if (rulang) dat1$fill[dat1$fill == "Services"] <- "в сфере услуг"

dat_plot <- na.omit(dat1)

p <- ggplot(dat_plot, aes(x=AreaName, y=Value, fill=fill))
p <- p + geom_bar(stat="identity", position="stack")
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, 3)[["Sub"]])
p <- p + labs(x="",y="percent\n")
if (rulang) p <- p + labs(x="",y="проценты\n")
if (rulang) p <- p + guides(fill = guide_legend(nrow = 3))
p <- p + theme(axis.text.x = element_text(angle=45))
p

maxYr <- max(dat1$Year)
# Caption
caption_text <- paste("Value added in agriculture, industry and services, share of GDP (",maxYr,")", sep = "")
if (rulang) caption_text <- paste("Добавленная стоимость в сельском хозяйстве, промышленности и сфере услуг, доля от ВВП (",maxYr," г.)", sep = "")

## ---- P1econLEFT ----
dat1 <- subset(temp, subset=Part %in% "P1econ")
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
p <- p + labs(x="",y="\nthousand US$ (constant 2000)")
if (rulang) p <- p + labs(x="",y="\nтыс. долл. США \n(в пост. ценах 2000 г.)")
p <- p + guides(color = guide_legend(nrow = 2))
p

yr = dat1$Year[1]

# Caption
caption_text <- paste("Agriculture value added per worker, top ",nrow(dat_plot1)," countries with the highest values (",yr,"*)", sep = "")
if (rulang) caption_text <- paste("Добавленная стоимость в сельском хозяйстве на одного работника, ",nrow(dat_plot1)," стран с самыми высокими значениями (",yr," гг.*)", sep = "")


## ---- P1econRIGHT ----

# data
dat1 <- subset(temp, subset=Part %in% "P1econ")
dat1 <- subset(dat1, subset=Position %in% "RIGHT")
dat1 <- subset(dat1, select = c(AreaName,Value,Year))

dat1 <- arrange(dat1, -Value)

top10 <- dat1 %>% slice(1:10) %>% dplyr::mutate(color = "Highest values")
if (rulang) top10 <- dat1 %>% slice(1:10) %>% dplyr::mutate(color = "Самые высокие значения")

bot10 <- dat1 %>% slice( (nrow(dat1)-9):nrow(dat1)) %>% dplyr::mutate(color = "Lowest values")
if (rulang) bot10 <- dat1 %>% slice( (nrow(dat1)-9):nrow(dat1)) %>% dplyr::mutate(color = "Самые низкие значения")

overlap <- top10$AreaName[top10$AreaName %in% bot10$AreaName]
if (length(overlap)!=0) dat_plot <- rbind(top10[top10$AreaName %in% overlap,], 
                                          bot10[!bot10$AreaName %in% overlap,]) else dat_plot <- rbind(top10,bot10)

dat_plot$AreaName <- fct_reorder(dat_plot$AreaName, dat_plot$Value) 

p <- ggplot(dat_plot, aes(x=AreaName,y=Value))
p <- p + geom_segment(aes(y = 0, xend = AreaName, 
                          yend = Value, color=color), alpha=.5, show.legend = FALSE)
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75) + theme(panel.grid.major.y = element_blank())
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="\npercent")
if (rulang) p <- p + labs(x="",y="\nпроценты")
p <- p + guides(color = guide_legend(nrow = 2))
p

yr = dat1$Year[1]

# Caption
caption_text <- paste("Value added in agriculture, average annual growth, ",nrow(dat_plot)/2," countries with highest and lowest values (",yr,")", sep = "")
if (rulang) caption_text <- paste("Добавленная стоимость в сельском хозяйстве, среднегодовой прирост, ",nrow(dat_plot)/2," стран с самыми высокими и самыми низкими значениями (",yr," гг.)", sep = "")


## ---- P1econBOTTOM_data ----
# data

dat1 <- subset(temp, subset=Part %in% "P1econ")
dat1 <- subset(dat1, subset=Position %in% "BOTTOM")
dat1 <- subset(dat1, select = c(Year,AreaName,Value))

## ---- P1econBOTTOM ----
dat_plot <- dat1 %>%  
  # group needs to have at least 2 data points to show in this line plots
  na.omit() %>% 
  group_by(AreaName) %>% 
  mutate(n = n()) %>% 
  dplyr::filter(n >= 2) %>% 
  ungroup() %>% 
  arrange(-Value) 

p <- ggplot(data = dat_plot, aes(x = Year, y = Value,group=AreaName,color=AreaName))
p <- p + geom_line(size=1.1, alpha=.7)
p <- p + scale_color_manual(values = plot_colors(part = 1, length(unique(dat_plot$AreaName)))[["Sub"]])
p <- p + labs(y="percent\n", x="")
if (rulang) p <- p + labs(x="",y="проценты\n")
p <- p + guides(color = guide_legend(nrow = 3))
p


# Caption
caption_text <- "Value added in agriculture as share of GDP"
if (rulang) caption_text <- "Добавленная стоимость в сельском хозяйстве в процентах от ВВП"

## ---- P1econMAP ----
dat1 <- subset(temp, subset=Part %in% "P1econ")
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

# graticule
grat_robin <- spTransform(graticule, CRS("+proj=robin"))  # reproject graticule
gr_rob <- fortify(grat_robin)
# crop the grid
if (!(region_to_report %in% c("GLO","COF"))) {
  gr_rob <- gr_rob[gr_rob$lat >= min(map.plot$lat) & gr_rob$lat <= max(map.plot$lat),]
  gr_rob <- gr_rob[gr_rob$long >= min(map.plot$long) & gr_rob$long <= max(map.plot$long),]
} else gr_rob <- gr_rob

p <- create_map_here()
p

yr = dat1$Year[1]

# Caption
caption_text <- paste("Value added in agriculture, share of GDP (percent, ",yr,"*)", sep = "")
if (rulang) caption_text <- paste("Добавленная стоимость в сельском хозяйстве, доля в ВВП (в процентах, ",yr," гг.*)", sep = "")



#   _               _
#  | |       __ _  | |__     ___    _   _   _ __
#  | |      / _` | | '_ \   / _ \  | | | | | '__|
#  | |___  | (_| | | |_) | | (_) | | |_| | | |
#  |_____|  \__,_| |_.__/   \___/   \__,_| |_|
#


## ---- P1laboTEXT ----
spread_title <- "Labour"
if (region_to_report == "RAF") short_text <- "A strong labour market is the foundation of sustained well-being and economic growth, inclusion and social cohesion. Therefore access to safe, productive and remunerated work is essential. Yet many workers, especially the most vulnerable, do not enter into formal wage employment but are instead self-employed or participate in unpaid family work, such as in agriculture. This is especially the case with subsistence farming. Although about 20 countries record more than 50\\% of their share of female employment being in agriculture, across the region however, it is noted that female employment in agriculture has been on a steady decline since the year 2000." # As a large share of the working poor are involved in agriculture, developments in this sector have a major impact on welfare."
if (region_to_report == "RAP") short_text <- "A strong labour market is the foundation of sustained well-being and economic growth, inclusion and social cohesion. Therefore access to safe, productive and remunerated work is essential. Yet many workers, especially the most vulnerable, do not enter into formal wage employment but are instead self-employed or participate in unpaid family work, such as in agriculture. This is especially the case with subsistence farming. As a large share of the working poor are involved in agriculture, developments in this sector have a major impact on welfare."
if (region_to_report == "REU") short_text <- "A strong labour market is the foundation of sustained well-being and economic growth, inclusion and social cohesion. Therefore access to safe, productive and remunerated work is essential. Yet many workers, especially the most vulnerable, do not enter into formal wage employment but are instead self-employed or participate in unpaid family work, such as in agriculture. This is especially the case with subsistence farming. As a large share of the working poor are involved in agriculture, developments in this sector have a major impact on welfare."
if (region_to_report == "RNE") short_text <- "A strong labour market is the foundation of sustained well-being and economic growth, inclusion and social cohesion. Therefore access to safe, productive and remunerated work is essential. Yet many workers, especially the most vulnerable, do not enter into formal wage employment but are instead self-employed or participate in unpaid family work, such as in agriculture. This is especially the case with subsistence farming. Although labour force’s shares is biased towards males in the region, female employment rates are higher in agriculture and it has steadily been decreasing since 2000."
if (region_to_report == "GLO") short_text <- "A strong labour market is the foundation of sustained well-being and economic growth, inclusion and social cohesion. Therefore access to safe, productive and remunerated work is essential. Yet many workers, especially the most vulnerable, do not enter into formal wage employment but are instead self-employed or participate in unpaid family work, such as in agriculture. This is especially the case with subsistence farming. As a large share of the working poor are involved in agriculture, developments in this sector have a major impact on welfare."
if (rulang) spread_title <- "Занятость"
if (region_to_report == "REU" & rulang) short_text <- "Стабильный рынок труда является основой устойчивого благосостояния и экономического роста, инклюзивности и социальной сплоченности. Поэтому доступ к безопасной, продуктивной и оплачиваемой работе имеет первостепенное значение. Однако многие работники, в первую очередь наиболее уязвимые, не участвуют в формальной оплачиваемой занятости, а вместо этого являются самозанятыми или выполняют неоплачиваемую работу в семье, например, в сельском хозяйстве. Это в первую очередь распространяется на натуральное хозяйство. Поскольку значительная доля бедных слоев населения занята в сельском хозяйстве, изменения в этом секторе оказывают существенное влияние на уровень благосостояния населения."


## ---- P1laboTOPRIGHT ----
dat1 <- subset(temp, subset=Part %in% "P1labo")
dat1 <- subset(dat1, subset=Position %in% "TOPRIGHT")
dat1 <- subset(dat1, select = c(AreaName,Indicator,Value,Year))

dat1$fill[dat1$Indicator == "SL.TLF.CACT.MA.ZS"] <- "Male"
dat1$fill[dat1$Indicator == "SL.TLF.CACT.FE.ZS"] <- "Female"
dat1$fill <- factor(dat1$fill, levels=c("Male","Female"))

dat_plot <- dat1
# reorder
dat_plot$AreaName <- factor(dat_plot$AreaName, levels=(dat_plot %>% dplyr::filter(fill == "Male") %>% arrange(-Value))$AreaName)

if (rulang){
  levels(dat_plot$fill)[levels(dat_plot$fill) == "Male"] <- "Мужчины"
  levels(dat_plot$fill)[levels(dat_plot$fill) == "Female"] <- "Женщины"
}

p <- ggplot(dat_plot, aes(x=AreaName, y=Value, fill=fill))
p <- p + geom_bar(stat="identity", position="dodge")
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + labs(x="",y="percent\n")
if (rulang) p <- p + labs(x="",y="проценты\n")
p <- p + theme(axis.text.x = element_text(angle=45))
p

yr = dat1$Year[1]

# Caption
caption_text <- paste("Labour force participation rate by gender, ages 15+ (",yr,")", sep = "")
if (rulang) caption_text <- paste("Показатель экономической активности населения, с разбивкой по полу, в возрасте 15+ (",yr," г.)", sep = "")

## ---- P1laboLEFT ----
dat1 <- subset(temp, subset=Part %in% "P1labo")
dat1 <- subset(dat1, subset=Position %in% "LEFT")
dat1 <- subset(dat1, select = c(AreaName,Value,Year))

dat_plot <- dat1 %>% 
  arrange(-Value) %>% 
  slice(1:20) %>% 
  dplyr::mutate(color = "x")

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

yr = dat1$Year[1]
# Caption
ncases <- nrow(dat_plot)
caption_text <- paste("Female employment in agriculture in top ",ncases," countries, share of female employment (percent ",yr,"*)", sep = "")
if (rulang) caption_text <- paste("Доля женского населения, занятого в сельском хозяйстве среди экономически активного женского населения в ",ncases," странах с самыми высокими значениями, (в процентах, ",yr," гг.*)", sep = "")

## ---- P1laboRIGHT ----
dat1 <- subset(temp, subset=Part %in% "P1labo")
dat1 <- subset(dat1, subset=Position %in% "RIGHT")
dat1 <- subset(dat1, select = c(AreaName,Value,Year))

dat_plot <- dat1 %>% 
  arrange(-Value) %>% 
  slice(1:20) %>% 
  dplyr::mutate(color = "x")

dat_plot$AreaName <- fct_reorder(dat_plot$AreaName, dat_plot$Value) 

p <- ggplot(dat_plot, aes(x=AreaName,y=Value))
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

yr = dat1$Year[1]
# Caption
ncases <- nrow(dat_plot)
caption_text <- paste("Male employment in agriculture in top ",ncases," countries, share of male employment (percent ",yr,"*)", sep = "")
if (rulang) caption_text <- paste("Доля мужского населения, занятого в сельском хозяйстве среди экономически активного мужского населения в ",ncases," странах с самыми высокими значениями, (в процентах, ",yr," гг.*)", sep = "")

## ---- P1laboBOTTOM_data ----
dat1 <- subset(temp, subset=Part %in% "P1labo")
dat1 <- subset(dat1, subset=Position %in% "BOTTOM")
dat1 <- subset(dat1, select = c(Year,AreaName,Value))
dat1$Year <- as.integer(dat1$Year)

dat_plot <- dat1 %>% na.omit()

## ---- P1laboBOTTOM ----
# New variables from ILO based on "Employment by sex and economic activity -- ILO estimates and projections, Nov. 2016 (thousands)"
# ILO_female_emp_agri - female in agriculture share of total female

p <- ggplot(data = dat_plot, aes(x = Year, y = Value,group=AreaName,color=AreaName))
p <- p + geom_line(size=1.1, alpha=.7)
p <- p + scale_color_manual(values = plot_colors(part = 1, length(unique(dat_plot$AreaName)))[["Sub"]])
p <- p + labs(y="percent\n", x="")
if (rulang) p <- p + labs(x="",y="проценты\n")
p <- p + guides(color = guide_legend(nrow = 2))
p

minYr <- min(dat1$Year)
maxYr <- max(dat1$Year)
# Caption
caption_text <- paste("Female employment in agriculture, share of female employment (",minYr,"-",maxYr,")", sep = "")
if (rulang) caption_text <- paste("Доля женского населения, занятого в сельском хозяйстве, среди экономически активного женского населения (",minYr,"-",maxYr," гг.)", sep = "")

## ---- P1laboMAP ----
dat1 <- subset(temp, subset=Part %in% "P1labo")
dat1 <- subset(dat1, subset=Position %in% "MAP")
dat1 <- subset(dat1, select = c(AreaCode,Value,Year))
dat1$AreaCode <- as.integer(dat1$AreaCode)
# dat1$Year <- as.integer(dat1$Year)

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

minYr <- min(dat1$Year)
maxYr <- max(dat1$Year)
# Caption
caption_text <- paste("Employment in agriculture, share of total employment (percent, ",minYr," to ",maxYr,"*)", sep = "")
if (rulang) caption_text <- paste("Занятость в сельском хозяйстве, доля в общей занятости (в процентах, с ",minYr," по ",maxYr," гг.*)", sep = "")


#   ___                           _
#  |_ _|  _ __    _ __    _   _  | |_   ___
#   | |  | '_ \  | '_ \  | | | | | __| / __|
#   | |  | | | | | |_) | | |_| | | |_  \__ \
#  |___| |_| |_| | .__/   \__,_|  \__| |___/
#                |_|
#



## ---- P1inputTEXT ----
spread_title <- "Inputs"
if (region_to_report == "RAF") short_text <- "Adequate access to inputs, including land, pesticides and fertilizers, is vital for agricultural production and growth. Throughout Asia and in parts of Latin America, expanding seed and fertilizer use has been accompanied by investments in irrigation, rural roads, marketing infrastructure and financial services, paving the way for dynamic commercial input markets. In sub-Saharan Africa, the uptake of agricultural inputs especially fertilizers is extremely low- about 20kg/ha as compared to other regions in the world which consume as high as 170kg/ha. This can be attributed to the fact that it is often cheaper to expand cropland to have higher production."
if (region_to_report == "RAP") short_text <- "Adequate access to inputs, including land, pesticides and fertilizers, is vital for agricultural production and growth. Throughout Asia and in parts of Latin America, expanding seed and fertilizer use has been accompanied by investments in irrigation, rural roads, marketing infrastructure and financial services, paving the way for dynamic commercial input markets. In other regions, such as sub-Saharan Africa, the uptake of agricultural inputs is relatively low because it is often cheaper to expand cropland to have higher production."
if (region_to_report == "REU") short_text <- "Adequate access to inputs, including land, pesticides and fertilizers, is vital for agricultural production and growth. With respect to use of fertilizers and pesticides, the situation is non-homogenous in the region. In EU other and EFTA countries, due to stronger inclinations towards conservation and organic agriculture, per hectare use of the mineral fertilizers and pesticides has been declining over the last decade, while in the remaining part of the region it has been expanding. Nevertheless, in terms of the level of fertilizer use, EU other and EFTA countries still are in lead together with South Eastern Europe and EU Central and Eastern."
if (region_to_report == "RNE") short_text <- "Adequate access to inputs, including land, pesticides and fertilizers, is vital for agricultural production and growth. Within the region, it is evident the use of large quantities of fertilizers, nitrogen in specific. However, fertilizers use is not homogenous within the region as the Gulf countries consume more than five times nutrients per hectare than Near East and North Africa countries. This is mainly due to the difficult and costly expansion of arable land within the desert environment."
if (region_to_report == "GLO") short_text <- "Adequate access to inputs, including land, pesticides and fertilizers, is vital for agricultural production and growth. Throughout Asia and in parts of Latin America, expanding seed and fertilizer use has been accompanied by investments in irrigation, rural roads, marketing infrastructure and financial services, paving the way for dynamic commercial input markets. In other regions, such as sub-Saharan Africa, the uptake of agricultural inputs is relatively low because it is often cheaper to expand cropland to have higher production."
if (rulang) spread_title <- "Факторы производства"
if (region_to_report == "REU" & rulang) short_text <- "Адекватный доступ к факторам производства, таким как земля, пестициды и удобрения, имеет первостепенное значение для сельскохозяйственного производства и роста. Что касается применения удобрений и пестицидов, ситуация в регионе неоднородна. В субрегионе «Другие страны ЕС и ЕАСТ», благодаря усилению тенденций перехода к ресурсосберегающему сельскому хозяйству и к органическому сельскому хозяйству, использование минеральных удобрений и пестицидов на гектар пашни на протяжении последнего десятилетия снижается, в то время как в остальной части региона применение удобрений и пестицидов увеличивается." # Тем не менее, с точки зрения общего уровня применения удобрений, субрегион «Другие страны ЕС и ЕАСТ» по-прежнему лидирует, вместе со субрегионами «Юго-Восточноя Европа» и «Центральная и Восточная часть ЕС». "

## ---- P1inputData ----
dat1 <- subset(temp, subset=Part %in% "P1input")
dat1 <- subset(dat1, subset=Position %in% "TOPRIGHT")
dat1 <- subset(dat1, select = c(Year,Indicator,Value))

## ---- P1inputTOPRIGHT ----
dat1$Year <- as.integer(dat1$Year)

dat1$fill[dat1$Indicator == "RF.FERT.NI.TN.SH"] <- "Nitrogen"
dat1$fill[dat1$Indicator == "RF.FERT.PH.TN.SH"] <- "Phosphate"
dat1$fill[dat1$Indicator == "RF.FERT.PO.TN.SH"] <- "Potash"

dat_plot <- dat1

if (rulang){
  dat_plot$fill[dat_plot$fill == "Nitrogen"] <- "Азот"
  dat_plot$fill[dat_plot$fill == "Phosphate"] <- "Фосфат"
  dat_plot$fill[dat_plot$fill == "Potash"] <- "Поташ"
}


p <- ggplot(dat_plot, aes(x=Year, y=Value, fill=fill))
p <- p + geom_area(stat="identity", position="stack")
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, 3)[["Sub"]])
p <- p + labs(x="",y="kg/ha\n")
if (rulang) p <- p + labs(x="",y="кг/га\n")
p <- p + scale_x_continuous(breaks=c(2002,2006,2010,2014))
p


minYr <- min(dat1$Year)
maxYr <- max(dat1$Year)
# Caption
caption_text <- paste("Fertilizer consumption in nutrients per ha of arable land (",minYr," to ",maxYr,")", sep = "")
if (rulang) caption_text <- paste("Использование удобрений в переводе в питательные вещества, на гектар пашни, 20 стран с самыми высокими значениями (с ",minYr," по ",maxYr," гг.)", sep = "")

## ---- P1inputLEFT ----
dat1 <- subset(temp, subset=Part %in% "P1input")
dat1 <- subset(dat1, subset=Position %in% "LEFT")
dat1 <- subset(dat1, select = c(AreaName,Value,Year))

dat_plot <- dat1 %>% 
  arrange(-Value) %>% 
  slice(1:20) %>% 
  dplyr::mutate(color = "x")

dat_plot$AreaName <- fct_reorder(dat_plot$AreaName, dat_plot$Value) 

p <- ggplot(dat_plot, aes(x=AreaName,y=Value))
p <- p + geom_segment(aes(y = min(dat_plot1$Value), xend = AreaName, 
                          yend = Value, color=color), alpha=.5)
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75) + theme(panel.grid.major.y = element_blank())
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 1)[["Sub"]])
p <- p + theme(legend.position = "none") # hide legend as only one year plotted
p <- p + coord_flip()
p <- p + labs(x="",y="\nkg/ha")
if (rulang) p <- p + labs(x="",y="\nкг/га")
p <- p + guides(color = guide_legend(nrow = 2))
p

yr = dat1$Year[1]
# Caption
caption_text <- paste("Nitrogen fertilizers consumption in nutrients per ha of arable land, top ",nrow(dat_plot)," countries (",yr,")", sep = "")
if (rulang) caption_text <- paste("Использование азотных удобрений в переводе в питательное вещество, на гектар пашни, ",nrow(dat_plot)," стран с самыми высокими значениями (",yr," г.)", sep = "")

## ---- P1inputRIGHT ----
dat1 <- subset(temp, subset=Part %in% "P1input")
dat1 <- subset(dat1, subset=Position %in% "RIGHT")
dat1 <- subset(dat1, select = c(AreaName,Value,Year))

dat_plot <- dat1 %>% 
  arrange(-Value) %>% 
  slice(1:20) %>% 
  dplyr::mutate(color = "x")

dat_plot$AreaName <- fct_reorder(dat_plot$AreaName, dat_plot$Value) 

p <- ggplot(dat_plot, aes(x=AreaName,y=Value))
p <- p + geom_segment(aes(y = min(dat_plot1$Value), xend = AreaName, 
                          yend = Value, color=color), alpha=.5)
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75) + theme(panel.grid.major.y = element_blank())
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 1)[["Sub"]])
p <- p + theme(legend.position = "none") # hide legend as only one year plotted
p <- p + coord_flip()
p <- p + labs(x="",y="\nkg/ha")
if (rulang) p <- p + labs(x="",y="\nкг/га")
p <- p + guides(color = guide_legend(nrow = 2))
p

yr = dat1$Year[1]
# Caption
caption_text <- paste("Phosphate fertilizers consumption in nutrients per ha of arable land, top ",nrow(dat_plot)," countries (",yr,")", sep = "")
if (rulang) caption_text <- paste("Использование фосфорных удобрений в переводе в питательное вещество на гектар пашни, ",nrow(dat_plot)," стран с самыми высокими значениями (",yr," г.)", sep = "")

## ---- P1inputBOTTOM ----

dat1 <- subset(temp, subset=Part %in% "P1input")
dat1 <- subset(dat1, subset=Position %in% "BOTTOM")
dat1 <- subset(dat1, select = c(AreaName,Indicator,Value,Year))

dat1$fill[dat1$Indicator == "RF.FERT.NI.TN.SH"] <- "Nitrogen"
dat1$fill[dat1$Indicator == "RF.FERT.PH.TN.SH"] <- "Phosphate"
dat1$fill[dat1$Indicator == "RF.FERT.PO.TN.SH"] <- "Potash"

dat_plot <- dat1

dat_plot$AreaName <- factor(dat_plot$AreaName, levels=(dat_plot %>% dplyr::filter(fill == "Nitrogen") %>% arrange(-Value))$AreaName)

ncases <- nrow(dat_plot)

if (rulang){
  dat_plot$fill[dat_plot$fill == "Nitrogen"] <- "Азот"
  dat_plot$fill[dat_plot$fill == "Phosphate"] <- "Фосфат"
  dat_plot$fill[dat_plot$fill == "Potash"] <- "Поташ"
}

p <- ggplot(dat_plot, aes(x=AreaName, y=Value, fill=fill))
p <- p + geom_bar(stat="identity", position="stack")
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, 3)[["Sub"]])
p <- p + labs(x="",y="kg/ha\n")
if (rulang) p <- p + labs(x="",y="кг/га\n")
p <- p + theme(axis.text.x = element_text(angle=45))
p

yr = dat1$Year[1]
# Caption
caption_text <- paste("Fertilizer consumption in nutrients per ha of arable land (",yr,")", sep = "")
if (rulang) caption_text <- paste("Использование удобрений в переводе в питательные вещества на гектар пашни (",yr," г.)", sep = "")

## ---- P1inputMAP ----
dat1 <- subset(temp, subset=Part %in% "P1input")
dat1 <- subset(dat1, subset=Position %in% "MAP")
dat1 <- subset(dat1, select = c(AreaCode,Value,Year))

dat1$AreaCode <- as.integer(dat1$AreaCode)

map.plot <- left_join(map.df,dat1, by = c("FAOST_CODE" = "AreaCode")) # so that each country in the region will be filled (value/NA)

# Add region key and subset

map.plot <- map.plot[which(map.plot[[region_to_report]]),]

cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","Value")]
cat_data$value_cat <- categories(x=cat_data$Value, n=5,decimals = 2)

map.plot <- left_join(map.plot,cat_data[c("FAOST_CODE","value_cat")])

# define map unit
map_unit <- "kg/ha"
if (rulang) map_unit <- "кг/га"

p <- create_map_here()
p

minYr <- substr(dat1$Year[1], 1, 4)
maxYr <- substr(dat1$Year[1], 6, 9)
# Caption
caption_text <- paste("Pesticides per ha of arable land (kg/ha, ",minYr," to ",maxYr,"*)", sep = "")
if (rulang) caption_text <- paste("Использование пестицидов на гектар пашни (кг/га, с ",minYr," по ",maxYr," гг.*)", sep = "")

#   ___                                _                                _
#  |_ _|  _ __   __   __   ___   ___  | |_   _ __ ___     ___   _ __   | |_
#   | |  | '_ \  \ \ / /  / _ \ / __| | __| | '_ ` _ \   / _ \ | '_ \  | __|
#   | |  | | | |  \ V /  |  __/ \__ \ | |_  | | | | | | |  __/ | | | | | |_
#  |___| |_| |_|   \_/    \___| |___/  \__| |_| |_| |_|  \___| |_| |_|  \__|
#


## ---- P1investTEXT ----
spread_title <- "Investments"
if (region_to_report == "RAF") short_text <- "Investing in agriculture is one of the most effective strategies for reducing poverty and hunger, and promoting sustainability. The regions of the world where hunger and extreme poverty are most widespread today – South Asia and sub-Saharan Africa – have seen flat or declining rates of investment per worker in agriculture over the past thirty years. African governments' financial input in agriculture continues to be low. Côte d'Ivoire, Namibia, Kenya, Zambia and Swaziland are the only countries in the region that spent 5\\% or more of government expenditure in agriculture between 2008 and 2012." # Farmers tend to be the largest investors developing country agriculture, and therefore their investment decisions are paramount for any strategy aimed at improving agricultural investment."
if (region_to_report == "RAP") short_text <- "Investing in agriculture is one of the most effective strategies for reducing poverty and hunger, and promoting sustainability. The regions of the world where hunger and extreme poverty are most widespread today – South Asia and sub-Saharan Africa – have seen flat or declining rates of investment per worker in agriculture over the past thirty years. Farmers tend to be the largest investors in developing country agriculture, and therefore their investment decisions are paramount for any strategy aimed at improving agricultural investment."
if (region_to_report == "REU") short_text <- "Investing in agriculture is one of the most effective strategies for reducing poverty and hunger, and promoting sustainability. Farmers tend to be the largest investors in agriculture, and therefore their investment decisions are paramount for any strategy aimed at improving agricultural investment."
if (region_to_report == "RNE") short_text <- "Investing in agriculture is one of the most effective strategies for reducing poverty and hunger, and promoting sustainability. The region, however, has been showing a declining and fluctuating aid commitment flows to agriculture, forestry and fishing since 1995. In 2013, Morocco, Egypt and Sudan were the largest providers of aid to agriculture. Instead, in the period 2010-2012, countries like United Arab Emirates, Syria and Egypt have supported farmers with the biggest amount of agricultural credits."
if (region_to_report == "GLO") short_text <- "Investing in agriculture is one of the most effective strategies for reducing poverty and hunger, and promoting sustainability. The regions of the world where hunger and extreme poverty are most widespread today – South Asia and sub-Saharan Africa – have seen flat or declining rates of investment per worker in agriculture over the past thirty years. Farmers tend to be the largest investors in developing country agriculture, and therefore their investment decisions are paramount for any strategy aimed at improving agricultural investment."
if (rulang) spread_title <- "Инвестиции"
if (region_to_report == "REU" & rulang) short_text <- "Инвестирование в сельское хозяйство является одной из наиболее эффективных стратегий сокращения масштабов нищеты и голода и содействия устойчивому развитию. Фермеры, как правило, являются крупнейшими инвесторами в сельское хозяйство, и, следовательно, их инвестиционные решения имеют первостепенное значение для любой стратегии, направленной на повышение инвестиций в сельское хозяйство."

## ---- P1investData ----
dat1 <- subset(temp, subset=Part %in% "P1invest")
dat1 <- subset(dat1, subset=Position %in% "TOPRIGHT")
dat1 <- subset(dat1, select = c(Year,AreaName,Value))
dat1$Year <- as.integer(dat1$Year)

## ---- P1investTOPRIGHT ----
dat_plot <- dat1

p <- ggplot(data = dat_plot, aes(x = Year, y = Value,group=AreaName,color=AreaName))
p <- p + geom_line(size=1.1, alpha=.7)
p <- p + scale_color_manual(values = plot_colors(part = 1, length(unique(dat_plot$AreaName)))[["Sub"]])
p <- p + labs(y="percent\n", x="")
if (rulang) p <- p + labs(x="",y="проценты\n")
p <- p + guides(color = guide_legend(nrow = length(unique(dat_plot$AreaName))))
p

minYr <- min(dat1$Year)
maxYr <- max(dat1$Year)

# Caption
caption_text <- paste("Aid  commitment flows to agriculture, forestry and fishing, share of total aid in \\% (",minYr," to ",maxYr,")", sep = "")
if (rulang) caption_text <- paste("Потоки помощи в сельское хозяйство, лесное хозяйство и рыбное хозяйство, доля в общем объеме помощи в \\% (",minYr,"-",maxYr," гг.) ", sep = "")

## ---- P1investLEFT ----
# data
dat1 <- subset(temp, subset=Part %in% "P1invest")
dat1 <- subset(dat1, subset=Position %in% "LEFT")
dat1 <- subset(dat1, select = c(Year,AreaName,Value))
minYr <- min(dat1$Year)
maxYr <- max(dat1$Year)
dat1$Year <- as.integer(dat1$Year)


nro_latest_cases <- nrow(dat1[dat1$Year == max(dat1$Year),])
if (nro_latest_cases < 20) {ncases <- nro_latest_cases} else ncases <- 20
dat1 <- arrange(dat1, -Year, -Value)


# slice the data for both years
topY1 <- dat1 %>% slice(1:ncases) %>% dplyr::mutate(color = maxYr)
topY2 <- dat1 %>% dplyr::filter(AreaName %in% topY1$AreaName, Year == minYr) %>% dplyr::mutate(color = minYr)
dat_plot <- rbind(topY1,topY2)
# levels based on newest year
dat_plot$AreaName <- factor(dat_plot$AreaName, levels=arrange(topY1,Value)$AreaName)


# To make the latest point on top
dat_plot <- arrange(dat_plot, color)

p <- ggplot(data=dat_plot, aes(x=AreaName, y= Value, fill=color))
p <- p + geom_segment(data=dat_plot %>% select(Year,AreaName,Value) %>%
                        spread(key = Year, value = Value) %>% 
                        mutate(color=NA), 
                      aes_(y = as.name(minYr), xend = quote(AreaName),
                           yend = as.name(maxYr)), color="grey80")
p <- p + geom_point(aes(fill=color),size = 4, alpha = 0.75, pch=21, color="white") + theme(panel.grid.major.y = element_blank())
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="\nmillion US$")
if (rulang) p <- p + labs(x="",y="\nмлн долл. США")
p <- p + guides(fill = guide_legend(nrow = 1))
p


# Caption
caption_text <- paste("Total credit to agriculture, top ",ncases," countries in ",maxYr,")", sep = "")
if (rulang) caption_text <- paste("Общий объем кредитования сельского хозяйства, ",
                                  ncases,
                                  " стран с самыми высокими значениями в ",maxYr," г.", sep = "")

## ---- P1investRIGHT ----
dat1 <- subset(temp, subset=Part %in% "P1invest")
dat1 <- subset(dat1, subset=Position %in% "RIGHT")
dat1 <- subset(dat1, select = c(AreaName,Value,Year))

dat1 <- arrange(dat1, -Value)

top10 <- dat1 %>% slice(1:10) %>% dplyr::mutate(color = "Highest values")
if (rulang) top10 <- dat1 %>% slice(1:10) %>% dplyr::mutate(color = "Самые высокие значения")

bot10 <- dat1 %>% slice( (nrow(dat1)-9):nrow(dat1)) %>% dplyr::mutate(color = "Lowest values")
if (rulang) bot10 <- dat1 %>% slice( (nrow(dat1)-9):nrow(dat1)) %>% dplyr::mutate(color = "Самые низкие значения")

overlap <- top10$AreaName[top10$AreaName %in% bot10$AreaName]
if (length(overlap)!=0) dat_plot <- rbind(top10[!top10$AreaName %in% overlap,], bot10[!bot10$AreaName %in% overlap,]) else dat_plot <- rbind(top10,bot10)

dat_plot$AreaName <- fct_reorder(dat_plot$AreaName, dat_plot$Value) 

p <- ggplot(dat_plot, aes(x=AreaName,y=Value))
p <- p + geom_segment(aes(y = min(dat_plot$Value), xend = AreaName, 
                          yend = Value, color=color), alpha=.5)
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)  + theme(panel.grid.major.y = element_blank())
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="\nindex")
if (rulang) p <- p + labs(x="",y="\nиндекс")
p <- p + guides(color = guide_legend(nrow = 2))
p <- p + theme(legend.justification = "left")
p

yr = dat1$Year[1]
# Caption
caption_text <- paste("Development flows to agriculture, Agriculture Orientation Index, ",nrow(dat_plot)/2," countries with highest and lowest values, average (",yr,")", sep = "")
if (rulang) caption_text <- paste("Индекс ориентации на сельское хозяйство, ",
                                  nrow(dat_plot)/2," 
                                  стран с самыми высокими и самыми низкими значениями, среднее значение (",yr," гг.)", sep = "")

## ---- P1investBOTTOM ----
dat1 <- subset(temp, subset=Part %in% "P1invest")
dat1 <- subset(dat1, subset=Position %in% "BOTTOM")
dat1 <- subset(dat1, select = c(AreaName,Indicator,Value,Year))
dat1$Year <- as.integer(dat1$Year)

dat1$Indicator <- as.character(dat1$Indicator)
dat1$Indicator[dat1$Indicator == "bilat_don_agr"] <- "Bilateral"
dat1$Indicator[dat1$Indicator == "multilat_don_agr"] <- "Multilateral"
dat1$Indicator[dat1$Indicator == "privat_don_agr"] <- "Private"

dat1$Indicator <- factor(dat1$Indicator, levels= c("Multilateral","Bilateral","Private"))

dat_plot <- dat1 %>% group_by(AreaName) %>% 
  dplyr::mutate(value_sum = sum(Value, na.rm=TRUE)) %>% 
  select(AreaName,Indicator,Value,value_sum) %>% 
  ungroup() %>%
  arrange(-value_sum) %>% 
  mutate(r = dense_rank(-value_sum))

if (rulang){
  levels(dat_plot$Indicator)[levels(dat_plot$Indicator) == "Multilateral"] <- "Многосторонняя"
  levels(dat_plot$Indicator)[levels(dat_plot$Indicator) == "Bilateral"] <- "Двусторонняя"
  levels(dat_plot$Indicator)[levels(dat_plot$Indicator) == "Private"] <- "Частный"
}


p <- ggplot(data=arrange(dat_plot,Indicator), aes(x = reorder(AreaName,-value_sum), y = Value))
p <- p + geom_bar(aes(fill=Indicator), stat = "identity",position = "stack")
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, 3)[["Sub"]])
p <- p + labs(x="",y="million 2013 US$\n")
if (rulang) p <- p + labs(x="",y="млн долл. США (в постоянных ценах 2013 г.)\n")
# p <- p + geom_vline(aes(xintercept=2015), color="grey20", linetype="dashed")
# p <- p + scale_x_continuous(breaks=min(dat_plot$Year):max(dat_plot$Year))
p <- p + theme(axis.text.x = element_text(angle=90,vjust=.5))
p

yr = dat1$Year[1]
# Caption
caption_text <- paste("Aid commitment flows to Agriculture, Forestry and Fishing, top",length(unique(dat_plot$AreaName)),"countries in",yr,"(million 2013 US\\$)")
if (rulang) caption_text <- paste("Потоки помощи в сельское хозяйство, лесное хозяйство и рыбное хозяйство,",
                                  length(unique(dat_plot$AreaName)),
                                  "стран с самыми высокими значениями в",yr,"г. (млн долл. США, в постоянных ценах 2013 г.)")

## ---- p1investMAPdata ----
dat1 <- subset(temp, subset=Part %in% "P1invest")
dat1 <- subset(dat1, subset=Position %in% "MAP")
dat1 <- subset(dat1, select = c(AreaCode,Value,Year))
dat1$AreaCode <- as.integer(dat1$AreaCode)

dat1$AreaCode[dat1$AreaCode == 41] <- 351

map.plot <- left_join(map.df,dat1, by = c("FAOST_CODE" = "AreaCode")) # so that each country in the region will be filled (value/NA)

# Add region key and subset

map.plot <- map.plot[which(map.plot[[region_to_report]]),]

cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","Value")]
cat_data$value_cat <- categories(x=cat_data$Value, n=5, method="jenks",decimals=2)

map.plot <- left_join(map.plot,cat_data[c("FAOST_CODE","value_cat")])

## ---- p1investMAP ----

# define map unit
map_unit <- "percent"
if (rulang) map_unit <- "проценты"

p <- create_map_here()
p

minYr <- substr(dat1$Year[1], 1, 4)
maxYr <- substr(dat1$Year[1], 6, 9)

# # Caption
caption_text <- paste("Share of government expenditure on agriculture, share of total outlays (percent,",minYr,"to",maxYr,"*)")
if (rulang) caption_text <- paste("Доля государственных расходов на сельское хозяйство, доля в общем объеме расходов (в процентах, с",minYr,"по",maxYr,"гг.*)")


