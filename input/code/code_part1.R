## ---- part1_setup ----
source(paste0(root.dir,'/input/code/plot/plot_color.R'))

syb_part <- 1

## Part 1
colPart1 <- plot_colors(part = syb_part, 12)
col.main1 <- colPart1[["Main"]][1]
## color for the grid
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
library(FAOSTAT)
dat <- getFAOtoSYB(domainCode = "OA",
                   elementCode = 551,
                   itemCode = 3010)
dat1 <- dat$aggregates
dat <- getFAOtoSYB(domainCode = "OA",
                   elementCode = 561,
                   itemCode = 3010)
dat2 <- dat$aggregates
dat <- left_join(dat1,dat2)
dfX <- gather(dat, variable, value, 3:4)
datX <- dfX %>% select(FAOST_CODE,Year,variable,value)


## ---- P1overTOPRIGHT ----


# If you could aggrate the population by summing up the countries you would do it like this
# datX <- dfX %>% select(FAOST_CODE,Year,variable,value)
# datX <- left_join(datX,region_key)
# datX <- datX[which(datX[[region_to_report]]),]
# datX <- datX %>% group_by(Year,variable) %>%  summarise(value = sum(value, na.rm=TRUE)/1000000)

# But as you cant in the case of population at least, we need to use the specific aggregates from FAOSTAT

dat <- syb.df %>% select(FAOST_CODE,Year,OA.TPU.POP.PPL.NO,OA.TPR.POP.PPL.NO)
dat <- dat[!is.na(dat$OA.TPR.POP.PPL.NO),]
dat <- left_join(dat,region_key)

if (region_to_report == "RAF")  dat <- dat %>% filter(FAOST_CODE %in% 12000)
if (region_to_report == "RAP")  dat <- dat %>% filter(FAOST_CODE %in% 13000)
if (region_to_report == "RNE")  dat <- dat %>% filter(FAOST_CODE %in% 15000)
if (region_to_report == "GLO")  dat <- dat %>% filter(FAOST_CODE %in% 5000)

dat <- gather(dat, variable, value, 3:4)

dat$variable <- as.character(dat$variable)
dat$variable[dat$variable == "OA.TPR.POP.PPL.NO"] <- "Rural population"
dat$variable[dat$variable == "OA.TPU.POP.PPL.NO"] <- "Urban population"

if (region_to_report == "REU"){
  dat <- datX %>% filter(FAOST_CODE %in% c(5400,5301), Year <= 2050) %>% mutate(value = value * 1000) %>%
    group_by(variable,Year) %>%
    dplyr::summarise(value = sum(value, na.rm=TRUE))
  dat$variable <- as.character(dat$variable)
  if (!rulang){
    dat$variable[dat$variable == "OA_3010_551"] <- "Rural population"
    dat$variable[dat$variable == "OA_3010_561"] <- "Urban population"
    dat$variable <- factor(dat$variable, levels=c("Rural population",
                                                  "Urban population")) 
  } 
  if (rulang){
    
    dat$variable[dat$variable == "OA_3010_551"] <- "Сельское население"
    dat$variable[dat$variable == "OA_3010_561"] <- "Городское населениvе"
    dat$variable <- factor(dat$variable, levels=c("Сельское население",
                                                  "Городское населениvе"))
    } 
  
}



dat <- dat %>% group_by(Year,variable) %>%  dplyr::summarise(value = sum(value, na.rm=TRUE)/1000000000)

# print data for technical report
#datatable(dat)

# Draw the plot
dat_plot <- dat
p <- ggplot(dat_plot, aes(x = Year, y = value))
p <- p + geom_area(aes(fill=variable), stat = "identity",position = "stack")
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + labs(x="",y="billion people\n")
if (rulang) p <- p + labs(x="",y="млрд человек\n")
p <- p + geom_vline(aes(xintercept=2015), color="grey20", linetype="dashed")
p <- p + scale_x_continuous(breaks=c(1961,2000,2015,2050))
p <- p + guides(fill = guide_legend(nrow = 2))
p

if (table_type == "latex"){
  if (!rulang){
    cat("\\footnotesize{\\textit{Data after 2015 are projections}}")
  } else cat("\\footnotesize{\\textit{Данные на период после 2015 года являются прогнозом}}")
  cat("\\vspace{1mm}")
} else cat("<br><i>Data after 2015 are projections</i>")


# Caption
if (region_to_report != "REU")  caption_text <- "Rural and urban population (1961 to 2016)"
if (region_to_report == "REU" & !rulang)  caption_text <- "Rural and urban population (1961 to 2050)"
if (region_to_report == "REU" & rulang)  caption_text <- "Сельское и городское население  (с 1961 по 2050 гг.)"



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
top10 <- dat %>% slice(1:10) %>% dplyr::mutate(color = "With highest values")
if (rulang) top10 <- dat %>% slice(1:10) %>% dplyr::mutate(color = "Самые высокие значения")

bot10 <- dat %>% slice( (nrow(dat)-9):nrow(dat)) %>% dplyr::mutate(color = "With lowest values")
if (rulang) bot10 <- dat %>% slice( (nrow(dat)-9):nrow(dat)) %>% dplyr::mutate(color = "Самые низкие значения")

overlap <- top10$SHORT_NAME[top10$SHORT_NAME %in% bot10$SHORT_NAME]
if (length(overlap)!=0) dat_plot <- rbind(top10[!top10$SHORT_NAME %in% overlap,], bot10[!bot10$SHORT_NAME %in% overlap,]) else dat_plot <- rbind(top10,bot10)

if (rulang) dat_plot$SHORT_NAME <- countrycode.multilang::countrycode(dat_plot$FAOST_CODE, origin = "fao", destination = "country.name.russian.fao")

dat_plot$SHORT_NAME <- fct_reorder(dat_plot$SHORT_NAME, dat_plot$OA.TPBS.POP.PPL.GR10) 

p <- ggplot(dat_plot, aes(x=SHORT_NAME,y=OA.TPBS.POP.PPL.GR10))
p <- p + geom_segment(aes(y = min(dat_plot$OA.TPBS.POP.PPL.GR10), xend = SHORT_NAME, 
                          yend = OA.TPBS.POP.PPL.GR10, color=color), alpha=.5)
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="\npercent")
if (rulang) p <- p + labs(x="",y="\nпроценты")
p <- p + guides(color = guide_legend(nrow = 2))
p

# Caption
caption_text <- paste("Population, average annual growth, top and bottom",nrow(dat_plot)/2,"countries (2004-2014)")
if (rulang) caption_text <- paste("Население, среднегодовые темпы роста,",nrow(dat_plot)/2,"стран с самыми высокими и самыми низкими значениями (2004-2014 гг.)")



## ---- P1overRIGHT ----

# data
dat <- syb.df %>% filter(Year %in% 2011) %>% select(FAOST_CODE,SP.DYN.LE00.IN)
dat <- dat[!is.na(dat$SP.DYN.LE00.IN),]

# Add region key and subset
dat <- left_join(dat,region_key)
dat <- dat[which(dat[[region_to_report]]),]

dat <- arrange(dat, -SP.DYN.LE00.IN)
top10 <- dat %>% slice(1:10) %>% dplyr::mutate(color = "With highest values")
if (rulang) top10 <- dat %>% slice(1:10) %>% dplyr::mutate(color = "Самые высокие значения")

bot10 <- dat %>% slice( (nrow(dat)-9):nrow(dat)) %>% dplyr::mutate(color = "With lowest values")
if (rulang) bot10 <- dat %>% slice( (nrow(dat)-9):nrow(dat)) %>% dplyr::mutate(color = "Самые низкие значения")

overlap <- top10$SHORT_NAME[top10$SHORT_NAME %in% bot10$SHORT_NAME]
if (length(overlap)!=0) dat_plot <- rbind(top10[!top10$SHORT_NAME %in% overlap,], bot10[!bot10$SHORT_NAME %in% overlap,]) else dat_plot <- rbind(top10,bot10)

# translate country names
if (rulang) dat_plot$SHORT_NAME <- countrycode.multilang::countrycode(dat_plot$FAOST_CODE, origin = "fao", destination = "country.name.russian.fao")

dat_plot$SHORT_NAME <- fct_reorder(dat_plot$SHORT_NAME, dat_plot$SP.DYN.LE00.IN) 

p <- ggplot(dat_plot, aes(x=SHORT_NAME,y=SP.DYN.LE00.IN))
p <- p + geom_segment(aes(y = min(dat_plot$SP.DYN.LE00.IN), xend = SHORT_NAME, 
                     yend = SP.DYN.LE00.IN, color=color), alpha=.5)
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="\nyears")
if (rulang) p <- p + labs(x="",y="\nгоды")
p <- p + guides(color = guide_legend(nrow = 2))
p

# Caption
caption_text <- paste("Life expectancy at birth, top and bottom",nrow(dat_plot)/2,"countries (2013)")
if (rulang) caption_text <- paste("Ожидаемая продолжительность жизни при рождении, ",nrow(dat_plot)/2,"стран с самыми высокими самими низкими значениями (2013 г.)")


## ---- P1overBOTTOM ----
# data

if (region_to_report == "RAF")  dat <- syb.df %>% filter(FAOST_CODE %in% 12001:12005) %>% select(FAOST_CODE,Year,SHORT_NAME,OA.TEAPT.POP.PPL.NO)
if (region_to_report == "RAP")  dat <- syb.df %>% filter(FAOST_CODE %in% 13001:13014) %>% select(FAOST_CODE,Year,SHORT_NAME,OA.TEAPT.POP.PPL.NO)
if (region_to_report == "REU")  dat <- syb.df %>% filter(FAOST_CODE %in% 14001:14007) %>% select(FAOST_CODE,Year,SHORT_NAME,OA.TEAPT.POP.PPL.NO)
if (region_to_report == "RNE")  dat <- syb.df %>% filter(FAOST_CODE %in% 15001:15003) %>% select(FAOST_CODE,Year,SHORT_NAME,OA.TEAPT.POP.PPL.NO)
if (region_to_report == "GLO")  dat <- syb.df %>% filter(FAOST_CODE %in% c(5100,5200,5300,5400,5500)) %>% select(FAOST_CODE,Year,SHORT_NAME,OA.TEAPT.POP.PPL.NO)

dat_plot <- dat[!is.na(dat$OA.TEAPT.POP.PPL.NO),]

dat_plot$OA.TEAPT.POP.PPL.NO <- dat_plot$OA.TEAPT.POP.PPL.NO / 1000000

if (rulang) dat_plot$SHORT_NAME <- translate_subgroups(dat_plot$SHORT_NAME)

p <- ggplot(dat_plot, aes(x=Year,y=OA.TEAPT.POP.PPL.NO,color=SHORT_NAME))
p <- p + geom_line(size=1.1, alpha=.7)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, length(unique(dat_plot$SHORT_NAME)))[["Sub"]])
p <- p + labs(x="",y="million people\n")
if (rulang) p <- p + labs(x="",y="млн человек\n")
p <- p + guides(color = guide_legend(nrow = 3))
p

# Caption
caption_text <- "Total economically active population (2000 to 2014)"
if (rulang) caption_text <- "Общая численность экономически активного населения (с 2000 по 2014 гг.)"



## ---- P1overMAP ----
dat <- syb.df %>% filter(Year %in% 2014, FAOST_CODE < 5000) %>% select(FAOST_CODE,SHORT_NAME,OA.TPR.POP.PPL.SHP) %>%
  mutate(OA.TPR.POP.PPL.SHP = OA.TPR.POP.PPL.SHP * 100)

map.plot <- left_join(map.df,dat) # so that each country in the region will be filled (value/NA)

# Add region key and subset

map.plot <- map.plot[which(map.plot[[region_to_report]]),]

cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","OA.TPR.POP.PPL.SHP")]
cat_data$value_cat <- categories(x=cat_data$OA.TPR.POP.PPL.SHP, n=5, method="jenks")

map.plot <- left_join(map.plot,cat_data[c("FAOST_CODE","value_cat")])

# define map unit
map_unit <- "percent"
if (rulang) map_unit <- "проценты"
  


p <- create_map_here()
p

# Caption
caption_text <- "Rural population, share of total population (2014)"
if (rulang) caption_text <- "Сельское население, доля сельского населения (2014 г.)"



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

if (region_to_report == "RAF")  dat <- syb.df %>% filter(FAOST_CODE %in% 12001:12005) %>% select(FAOST_CODE,Year,SHORT_NAME,NV.AGR.TOTL.ZS,NV.IND.TOTL.ZS,NV.SRV.TETC.ZS)
if (region_to_report == "RAP")  dat <- syb.df %>% filter(FAOST_CODE %in% 13001:13014) %>% select(FAOST_CODE,Year,SHORT_NAME,NV.AGR.TOTL.ZS,NV.IND.TOTL.ZS,NV.SRV.TETC.ZS)
if (region_to_report == "REU")  dat <- syb.df %>% filter(FAOST_CODE %in% 14001:14007) %>% select(FAOST_CODE,Year,SHORT_NAME,NV.AGR.TOTL.ZS,NV.IND.TOTL.ZS,NV.SRV.TETC.ZS)
if (region_to_report == "RNE")  dat <- syb.df %>% filter(FAOST_CODE %in% 15001:15003) %>% select(FAOST_CODE,Year,SHORT_NAME,NV.AGR.TOTL.ZS,NV.IND.TOTL.ZS,NV.SRV.TETC.ZS)
if (region_to_report == "GLO")  dat <- syb.df %>% filter(FAOST_CODE %in% c(5100,5200,5300,5400,5500)) %>% select(FAOST_CODE,Year,SHORT_NAME,NV.AGR.TOTL.ZS,NV.IND.TOTL.ZS,NV.SRV.TETC.ZS)

dat <- filter(dat, Year %in% 2013) %>% select(SHORT_NAME,NV.AGR.TOTL.ZS,NV.IND.TOTL.ZS,NV.SRV.TETC.ZS)

# Add region key and subset

dat_plot <- gather(dat, variable, value, 2:4)
dat_plot$fill[dat_plot$variable == "NV.AGR.TOTL.ZS"] <- "Agriculture"
dat_plot$fill[dat_plot$variable == "NV.IND.TOTL.ZS"] <- "Industry"
dat_plot$fill[dat_plot$variable == "NV.SRV.TETC.ZS"] <- "Services"

dat_plot$SHORT_NAME <- factor(dat_plot$SHORT_NAME, levels=(dat_plot %>% 
                                                             filter(fill == "Agriculture") %>% 
                                                             arrange(-value))$SHORT_NAME)

if (rulang) dat_plot$fill[dat_plot$fill == "Agriculture"] <- "в сельском хозяйстве"
if (rulang) dat_plot$fill[dat_plot$fill == "Industry"] <- "в промышленности"
if (rulang) dat_plot$fill[dat_plot$fill == "Services"] <- "в сфере услуг"

if (rulang) dat_plot$SHORT_NAME <- translate_subgroups(dat_plot$SHORT_NAME,isfactor = TRUE)

p <- ggplot(dat_plot, aes(x=SHORT_NAME, y=value, fill=fill))
p <- p + geom_bar(stat="identity", position="stack")
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, 3)[["Sub"]])
p <- p + labs(x="",y="percent\n")
if (rulang) p <- p + labs(x="",y="проценты\n")
if (rulang) p <- p + guides(fill = guide_legend(nrow = 3))
p <- p + theme(axis.text.x = element_text(angle=45))
p

# Caption
caption_text <- "Value added in agriculture, industry and services, share of GDP (2013)"
if (rulang) caption_text <- "Добавленная стоимость в сельском хозяйстве, промышленности и сфере услуг, доля от ВВП (2013 г.)"

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
dat_plot <- dat %>% group_by(SHORT_NAME) %>% dplyr::filter(Year == max(Year)) %>% ungroup() %>% arrange(-EA.PRD.AGRI.KD) %>% slice(1:20) %>% dplyr::mutate(color = "2013",
                                                                                                                                                           EA.PRD.AGRI.KD = EA.PRD.AGRI.KD / 1000)

# translate country names
if (rulang) dat_plot$SHORT_NAME <- countrycode.multilang::countrycode(dat_plot$FAOST_CODE, origin = "fao", destination = "country.name.russian.fao")

dat_plot$SHORT_NAME <- fct_reorder(dat_plot$SHORT_NAME, dat_plot$EA.PRD.AGRI.KD) 

p <- ggplot(dat_plot, aes(x=SHORT_NAME,y=EA.PRD.AGRI.KD))
p <- p + geom_segment(aes(y = min(dat_plot$EA.PRD.AGRI.KD), xend = SHORT_NAME, 
                          yend = EA.PRD.AGRI.KD, color=color), alpha=.5)
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 1)[["Sub"]])
p <- p + theme(legend.position = "none") # hide legend as only one year plotted
p <- p + coord_flip()
p <- p + labs(x="",y="\nthousand US$ (constant 2000)")
if (rulang) p <- p + labs(x="",y="\nтыс. долл. США \n(в пост. ценах 2000 г.)")
p <- p + guides(color = guide_legend(nrow = 2))
p

# Caption
caption_text <- paste("Agriculture value added per worker, top",nrow(dat_plot),"countries with the highest values (2003-2013*)")
if (rulang) caption_text <- paste("Добавленная стоимость в сельском хозяйстве на одного работника,",nrow(dat_plot),"стран с самыми высокими значениями (2003-2013 гг.*)")

 

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
  dplyr::mutate(color = "With highest values")

bot10 <- dat %>% arrange(FAOST_CODE,Year) %>%
  group_by(FAOST_CODE) %>% dplyr::mutate(Growth=c(NA,exp(diff(log(NV.AGR.TOTL.KD)))-1)) %>%
  group_by(SHORT_NAME) %>%
  dplyr::summarise(growth_NV.AGR.TOTL.KD = mean(Growth, na.rm = TRUE)*100) %>%
  arrange(growth_NV.AGR.TOTL.KD) %>%
  slice(1:10) %>%
  dplyr::mutate(color = "With lowest values")

overlap <- top10$SHORT_NAME[top10$SHORT_NAME %in% bot10$SHORT_NAME]
if (length(overlap)!=0) dat_plot <- rbind(top10[!top10$SHORT_NAME %in% overlap,], bot10[!bot10$SHORT_NAME %in% overlap,]) else dat_plot <- rbind(top10,bot10)

# translate
if (rulang){
  dat_plot$color[dat_plot$color == "With highest values"] <- "Самые высокие значения"
  dat_plot$color[dat_plot$color == "With lowest values"] <- "Самые низкие значения"
  dat_plot$SHORT_NAME <- countrycode.multilang::countrycode(dat_plot$SHORT_NAME, origin = "country.name", destination = "country.name.russian.fao")
} 

dat_plot$SHORT_NAME <- fct_reorder(dat_plot$SHORT_NAME, dat_plot$growth_NV.AGR.TOTL.KD) 

p <- ggplot(dat_plot, aes(x=SHORT_NAME,y=growth_NV.AGR.TOTL.KD))
p <- p + geom_segment(aes(y = min(dat_plot$growth_NV.AGR.TOTL.KD), xend = SHORT_NAME, 
                          yend = growth_NV.AGR.TOTL.KD, color=color), alpha=.5)
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="\npercent")
if (rulang) p <- p + labs(x="",y="\nпроценты")
p <- p + guides(color = guide_legend(nrow = 2))
p

# Caption
caption_text <- paste("Value added in agriculture, average annual growth,",nrow(dat_plot)/2,"countries with highest and lowest values (2003-2013)")
if (rulang) caption_text <- paste("Добавленная стоимость в сельском хозяйстве, среднегодовой прирост,",nrow(dat_plot)/2,"стран с самыми высокими и самыми низкими значениями (2003-2013 гг.)")


## ---- P1econBOTTOM_data ----
# data

nomin <- syb.df[c("FAOST_CODE","SHORT_NAME","Year","NV.AGR.TOTL.KD")]




## ---- P1econBOTTOM ----

if (region_to_report == "RAF")  dat <- syb.df %>% filter(FAOST_CODE %in% 12001:12005, Year %in% 2000:2014) %>% select(SHORT_NAME,Year,NY.GDP.MKTP.KD,NV.AGR.TOTL.KD)
if (region_to_report == "RAP")  dat <- syb.df %>% filter(FAOST_CODE %in% 13001:13014, Year %in% 2000:2014) %>% select(SHORT_NAME,Year,NY.GDP.MKTP.KD,NV.AGR.TOTL.KD)
if (region_to_report == "REU")  dat <- syb.df %>% filter(FAOST_CODE %in% 14001:14007, Year %in% 2000:2014) %>% select(SHORT_NAME,Year,NY.GDP.MKTP.KD,NV.AGR.TOTL.KD)
if (region_to_report == "RNE")  dat <- syb.df %>% filter(FAOST_CODE %in% 15001:15003, Year %in% 2000:2014) %>% select(SHORT_NAME,Year,NY.GDP.MKTP.KD,NV.AGR.TOTL.KD)
if (region_to_report == "GLO")  dat <- syb.df %>% filter(FAOST_CODE %in% c(5100,5200,5300,5400,5500), Year %in% 2000:2014) %>% select(SHORT_NAME,Year,NY.GDP.MKTP.KD,NV.AGR.TOTL.KD)


dat_plot <- dat %>%  group_by(Year) %>%
  dplyr::mutate(share = NV.AGR.TOTL.KD/NY.GDP.MKTP.KD*100) %>%
  ungroup() %>% 
  # group needs to have at least 2 data points to show in this line plots
  na.omit() %>% group_by(SHORT_NAME) %>% mutate(n = n()) %>% filter(n >= 2) %>% 
  ungroup() %>% arrange(-share) 

if (rulang) dat_plot$SHORT_NAME <- translate_subgroups(dat_plot$SHORT_NAME,isfactor = FALSE,add_row_breaks=FALSE)

p <- ggplot(data = dat_plot, aes(x = Year, y = share,group=SHORT_NAME,color=SHORT_NAME))
p <- p + geom_line(size=1.1, alpha=.7)
p <- p + scale_color_manual(values = plot_colors(part = 1, length(unique(dat_plot$SHORT_NAME)))[["Sub"]])
p <- p + labs(y="percent\n", x="")
if (rulang) p <- p + labs(x="",y="проценты\n")
p <- p + guides(color = guide_legend(nrow = 3))
p

# Caption
caption_text <- "Value added in agriculture as share of GDP"
if (rulang) caption_text <- "Добавленная стоимость в сельском хозяйстве в процентах от ВВП"

## ---- P1econMAP ----
dat <- syb.df %>% filter(Year %in% c(2010:2013), FAOST_CODE < 5000) %>%
  select(FAOST_CODE,SHORT_NAME,NV.AGR.TOTL.ZS) %>%
  group_by(FAOST_CODE) %>% dplyr::summarise(NV.AGR.TOTL.ZS = max(NV.AGR.TOTL.ZS)) %>%
  ungroup()

map.plot <- left_join(map.df,dat) # so that each country in the region will be filled (value/NA)

# Add region key and subset

map.plot <- map.plot[which(map.plot[[region_to_report]]),]

cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","NV.AGR.TOTL.ZS")]
cat_data$value_cat <- categories(x=cat_data$NV.AGR.TOTL.ZS, n=5, method="jenks")

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

# Caption
caption_text <- "Value added in agriculture, share of GDP (percent, 2010 to 2013*)"
if (rulang) caption_text <- "Добавленная стоимость в сельском хозяйстве, доля в ВВП (в процентах, 2010-2013 гг.*)"



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


## ---- P1laboTOPRIGHT, eval=P1labo, top_right_plot=P1labo, fig.height=top_right_plot_height, fig.width=top_right_plot_width ----
if (region_to_report == "RAF")  dat <- syb.df %>% filter(FAOST_CODE %in% 12001:12005, Year %in% 2013) %>% select(SHORT_NAME,SL.TLF.CACT.MA.ZS,SL.TLF.CACT.FE.ZS,OA.TPBS.POP.PPL.NO)
if (region_to_report == "RAP")  dat <- syb.df %>% filter(FAOST_CODE %in% 13001:13014, Year %in% 2013) %>% select(SHORT_NAME,SL.TLF.CACT.MA.ZS,SL.TLF.CACT.FE.ZS,OA.TPBS.POP.PPL.NO)
if (region_to_report == "REU")  dat <- syb.df %>% filter(FAOST_CODE %in% 14001:14007, Year %in% 2013) %>% select(SHORT_NAME,SL.TLF.CACT.MA.ZS,SL.TLF.CACT.FE.ZS,OA.TPBS.POP.PPL.NO)
if (region_to_report == "RNE")  dat <- syb.df %>% filter(FAOST_CODE %in% 15001:15003, Year %in% 2013) %>% select(SHORT_NAME,SL.TLF.CACT.MA.ZS,SL.TLF.CACT.FE.ZS,OA.TPBS.POP.PPL.NO)
if (region_to_report == "GLO")  dat <- syb.df %>% filter(FAOST_CODE %in% c(5100,5200,5300,5400,5500), Year %in% 2013) %>% select(SHORT_NAME,SL.TLF.CACT.MA.ZS,SL.TLF.CACT.FE.ZS,OA.TPBS.POP.PPL.NO)


dat <- gather(dat, variable, value, 2:3)
dat$fill[dat$variable == "SL.TLF.CACT.MA.ZS"] <- "Male"
dat$fill[dat$variable == "SL.TLF.CACT.FE.ZS"] <- "Female"
dat$fill <- factor(dat$fill, levels=c("Male","Female"))

dat_plot <- dat
# reorder
dat_plot$SHORT_NAME <- factor(dat_plot$SHORT_NAME, levels=(dat_plot %>% filter(fill == "Male") %>% arrange(-value))$SHORT_NAME)



if (rulang){
  levels(dat_plot$fill)[levels(dat_plot$fill) == "Male"] <- "Мужчины"
  levels(dat_plot$fill)[levels(dat_plot$fill) == "Female"] <- "Женщины"
  dat_plot$SHORT_NAME <- translate_subgroups(dat_plot$SHORT_NAME, add_row_breaks = T, isfactor = T)
}

p <- ggplot(dat_plot, aes(x=SHORT_NAME, y=value, fill=fill))
p <- p + geom_bar(stat="identity", position="dodge")
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + labs(x="",y="percent\n")
if (rulang) p <- p + labs(x="",y="проценты\n")
p <- p + theme(axis.text.x = element_text(angle=45))
p

# Caption
caption_text <- "Labour force participation rate by gender, ages 15+ (2013)"
if (rulang) caption_text <- "Показатель экономической активности населения, с разбивкой по полу, в возрасте 15+ (2013 г.)"

## ---- P1laboLEFT ----
dat <- syb.df[syb.df$Year %in%  2003:2013 ,c("FAOST_CODE","Year","SHORT_NAME","SL.AGR.EMPL.FE.ZS")]

dat <- dat[!is.na(dat$SL.AGR.EMPL.FE.ZS),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

#and subset
dat <- dat[which(dat[[region_to_report]]),]

dat_plot <- dat %>% group_by(SHORT_NAME) %>% dplyr::filter(Year == max(Year)) %>% ungroup() %>% arrange(-SL.AGR.EMPL.FE.ZS) %>% slice(1:20) %>% dplyr::mutate(color = "2013")

ncases <- nrow(dat_plot)

if (rulang) dat_plot$SHORT_NAME <- countrycode.multilang::countrycode(dat_plot$FAOST_CODE, origin = "fao", destination = "country.name.russian.fao")

dat_plot$SHORT_NAME <- fct_reorder(dat_plot$SHORT_NAME, dat_plot$SL.AGR.EMPL.FE.ZS) 

p <- ggplot(dat_plot, aes(x=SHORT_NAME,y=SL.AGR.EMPL.FE.ZS))
p <- p + geom_segment(aes(y = min(dat_plot$SL.AGR.EMPL.FE.ZS), xend = SHORT_NAME, 
                          yend = SL.AGR.EMPL.FE.ZS, color=color), alpha=.5)
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 1)[["Sub"]])
p <- p + theme(legend.position = "none") # hide legend as only one year plotted
p <- p + coord_flip()
p <- p + labs(x="",y="\npercent")
if (rulang) p <- p + labs(x="",y="\nпроценты")
p <- p + guides(color = guide_legend(nrow = 2))
p

# Caption
caption_text <- paste("Female employment in agriculture in top",ncases,"countries, share of female employment (percent 2003-2013*)")
if (rulang) caption_text <- paste("Доля женского населения, занятого в сельском хозяйстве среди экономически активного женского населения в",ncases,"странах с самыми высокими значениями, (в процентах, 2003-2013 гг.*)")

## ---- P1laboRIGHT ----
dat <- syb.df[syb.df$Year %in%  2003:2013 ,c("FAOST_CODE","Year","SHORT_NAME","SL.AGR.EMPL.MA.ZS")]

dat <- dat[!is.na(dat$SL.AGR.EMPL.MA.ZS),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

#and subset
dat <- dat[which(dat[[region_to_report]]),]

dat_plot <- dat %>% group_by(SHORT_NAME) %>% dplyr::filter(Year == max(Year)) %>% ungroup() %>% arrange(-SL.AGR.EMPL.MA.ZS) %>% slice(1:20) %>% dplyr::mutate(color = "2013")
ncases <- nrow(dat_plot)

if (rulang) dat_plot$SHORT_NAME <- countrycode.multilang::countrycode(dat_plot$FAOST_CODE, origin = "fao", destination = "country.name.russian.fao")

dat_plot$SHORT_NAME <- fct_reorder(dat_plot$SHORT_NAME, dat_plot$SL.AGR.EMPL.MA.ZS) 

p <- ggplot(dat_plot, aes(x=SHORT_NAME,y=SL.AGR.EMPL.MA.ZS))
p <- p + geom_segment(aes(y = min(dat_plot$SL.AGR.EMPL.MA.ZS), xend = SHORT_NAME, 
                          yend = SL.AGR.EMPL.MA.ZS, color=color), alpha=.5)
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 1)[["Sub"]])
p <- p + theme(legend.position = "none") # hide legend as only one year plotted
p <- p + coord_flip()
p <- p + labs(x="",y="\npercent")
if (rulang) p <- p + labs(x="",y="\nпроценты")
p <- p + guides(color = guide_legend(nrow = 2))
p

# Caption
caption_text <- paste("Male employment in agriculture in top",ncases,"countries, share of male employment (percent 2003 - 2013*)")
if (rulang) caption_text <- paste("Доля мужского населения, занятого в сельском хозяйстве среди экономически активного мужского населения в",ncases,"странах с самыми высокими значениями, (в процентах, 2003-2013 гг.*)")

## ---- P1laboBOTTOM_data ----



## ---- P1laboBOTTOM ----
if (region_to_report == "RAF")  dat <- syb.df %>% filter(FAOST_CODE %in% 12001:12005, Year %in% 2000:2014) %>% select(SHORT_NAME,Year,OA.TEAPFA.POP.PPL.NO,OA.TEAPF.POP.PPL.NO)
if (region_to_report == "RAP")  dat <- syb.df %>% filter(FAOST_CODE %in% 13001:13014, Year %in% 2000:2014) %>% select(SHORT_NAME,Year,OA.TEAPFA.POP.PPL.NO,OA.TEAPF.POP.PPL.NO)
if (region_to_report == "REU")  dat <- syb.df %>% filter(FAOST_CODE %in% 14001:14007, Year %in% 2000:2014) %>% select(SHORT_NAME,Year,OA.TEAPFA.POP.PPL.NO,OA.TEAPF.POP.PPL.NO)
if (region_to_report == "RNE")  dat <- syb.df %>% filter(FAOST_CODE %in% 15001:15003, Year %in% 2000:2014) %>% select(SHORT_NAME,Year,OA.TEAPFA.POP.PPL.NO,OA.TEAPF.POP.PPL.NO)
if (region_to_report == "GLO")  dat <- syb.df %>% filter(FAOST_CODE %in% c(5100,5200,5300,5400,5500), Year %in% 2000:2014) %>% select(SHORT_NAME,Year,OA.TEAPFA.POP.PPL.NO,OA.TEAPF.POP.PPL.NO)


dat$share <- dat$OA.TEAPFA.POP.PPL.NO / dat$OA.TEAPF.POP.PPL.NO * 100

dat_plot <- dat

dat_plot$SHORT_NAME <- translate_subgroups(dat_plot$SHORT_NAME, isfactor = TRUE, add_row_breaks = FALSE)

if (rulang) dat_plot$SHORT_NAME <- translate_subgroups(dat_plot$SHORT_NAME, isfactor = FALSE, add_row_breaks = FALSE)

p <- ggplot(data = dat_plot, aes(x = Year, y = share,group=SHORT_NAME,color=SHORT_NAME))
p <- p + geom_line(size=1.1, alpha=.7)
p <- p + scale_color_manual(values = plot_colors(part = 1, length(unique(dat_plot$SHORT_NAME)))[["Sub"]])
p <- p + labs(y="percent\n", x="")
if (rulang) p <- p + labs(x="",y="проценты\n")
p <- p + guides(color = guide_legend(nrow = 3))
p

# Caption
caption_text <- "Female employment in agriculture, share of female employment (2000-2014)"
if (rulang) caption_text <- "Доля женского населения, занятого в сельском хозяйстве, среди экономически активного женского населения (2000-2014 гг.)"

## ---- P1laboMAP ----
dat <- syb.df %>% filter(Year %in% c(2007:2012)) %>%
  select(FAOST_CODE,SHORT_NAME,SL.AGR.EMPL.ZS) %>%
  group_by(FAOST_CODE) %>% dplyr::summarise(SL.AGR.EMPL.ZS = max(SL.AGR.EMPL.ZS, na.rm = TRUE)) %>%
  #filter(!is.na(SL.AGR.EMPL.ZS)) %>%
  ungroup()

map.plot <- left_join(map.df,dat) # so that each country in the region will be filled (value/NA)

# Add region key and subset

map.plot <- map.plot[which(map.plot[[region_to_report]]),]

cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","SL.AGR.EMPL.ZS")]
cat_data$value_cat <- categories(x=cat_data$SL.AGR.EMPL.ZS, n=5, method="jenks")

map.plot <- left_join(map.plot,cat_data[c("FAOST_CODE","value_cat")])

# define map unit
map_unit <- "percent"
if (rulang) map_unit <- "проценты"

p <- create_map_here()
p

# Caption
caption_text <- "Employment in agriculture, share of total employment (percent, 2007 to 2012*)"
if (rulang) caption_text <- "Занятость в сельском хозяйстве, доля в общей занятости (в процентах, с 2007 по 2012 гг.*)"


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
if (region_to_report == "REU" & rulang) short_text <- "Адекватный доступ к факторам производства, таким как земля, пестициды и удобрения, имеет первостепенное значение для сельскохозяйственного производства и роста. Что касается применения удобрений и пестицидов, ситуация в регионе неоднородна. В субрегионе «Другие страны ЕС и ЕАСТ», благодаря усилению тенденций перехода к ресурсосберегающему сельскому хозяйству и к органическому сельскому хозяйству, использование минеральных удобрений и пестицидов на гектар пашни на протяжении последнего десятилетия снижается, в то время как в остальной части региона применение удобрений и пестицидов увеличивается. Тем не менее, с точки зрения общего уровня применения удобрений, субрегион «Другие страны ЕС и ЕАСТ» по-прежнему лидирует, вместе со субрегионами «Юго-Восточноя Европа» и «Центральная и Восточная часть ЕС». "

## ---- P1inputData ----


## ---- P1inputTOPRIGHT ----
if (region_to_report == "RAF")  dat <- syb.df %>% filter(FAOST_CODE %in% 12000, Year %in% 2002:2012) %>% select(SHORT_NAME,Year,RF.FERT.NI.TN.SH,RF.FERT.PH.TN.SH,RF.FERT.PO.TN.SH)
if (region_to_report == "RAP")  dat <- syb.df %>% filter(FAOST_CODE %in% 13000, Year %in% 2002:2012) %>% select(SHORT_NAME,Year,RF.FERT.NI.TN.SH,RF.FERT.PH.TN.SH,RF.FERT.PO.TN.SH)
if (region_to_report == "REU")  dat <- syb.df %>% filter(FAOST_CODE %in% 14000, Year %in% 2002:2012) %>% select(SHORT_NAME,Year,RF.FERT.NI.TN.SH,RF.FERT.PH.TN.SH,RF.FERT.PO.TN.SH)
if (region_to_report == "RNE")  dat <- syb.df %>% filter(FAOST_CODE %in% 15000, Year %in% 2002:2012) %>% select(SHORT_NAME,Year,RF.FERT.NI.TN.SH,RF.FERT.PH.TN.SH,RF.FERT.PO.TN.SH)
if (region_to_report == "GLO")  dat <- syb.df %>% filter(FAOST_CODE %in% 5000, Year %in% 2002:2012) %>% select(SHORT_NAME,Year,RF.FERT.NI.TN.SH,RF.FERT.PH.TN.SH,RF.FERT.PO.TN.SH)

dat <- gather(dat, variable, value, 3:5)
dat$fill[dat$variable == "RF.FERT.NI.TN.SH"] <- "Nitrogen"
dat$fill[dat$variable == "RF.FERT.PH.TN.SH"] <- "Phosphate"
dat$fill[dat$variable == "RF.FERT.PO.TN.SH"] <- "Potash"

dat$value <- dat$value * 1000 # As we want kg per ha

dat_plot <- dat

if (rulang){
  dat_plot$fill[dat_plot$fill == "Nitrogen"] <- "Азот"
  dat_plot$fill[dat_plot$fill == "Phosphate"] <- "Фосфат"
  dat_plot$fill[dat_plot$fill == "Potash"] <- "Поташ"
}


p <- ggplot(dat_plot, aes(x=Year, y=value, fill=fill))
p <- p + geom_area(stat="identity", position="stack")
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, 3)[["Sub"]])
p <- p + labs(x="",y="kg/ha\n")
if (rulang) p <- p + labs(x="",y="кг/га\n")
p <- p + scale_x_continuous(breaks=c(2002,2004,2006,2008,2010,2012))
p

# Caption
caption_text <- "Fertilizer consumption in nutrients per ha of arable land (2002 to 2012)"
if (rulang) caption_text <- "Использование удобрений в переводе в питательные вещества, на гектар пашни, 20 стран с самыми высокими значениями (с 2002 по 2012 гг.)"

## ---- P1inputLEFT ----
dat <- syb.df %>% filter(Year %in% 2012, FAOST_CODE < 5000) %>%
  select(FAOST_CODE,Year,RF.FERT.NI.TN.SH) %>%
  mutate(RF.FERT.NI.TN.SH = RF.FERT.NI.TN.SH * 1000) # As we want kg per ha

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

if (rulang) dat_plot$SHORT_NAME <- countrycode.multilang::countrycode(dat_plot$FAOST_CODE, origin = "fao", destination = "country.name.russian.fao")

dat_plot$SHORT_NAME <- fct_reorder(dat_plot$SHORT_NAME, dat_plot$RF.FERT.NI.TN.SH) 

p <- ggplot(dat_plot, aes(x=SHORT_NAME,y=RF.FERT.NI.TN.SH))
p <- p + geom_segment(aes(y = min(dat_plot$RF.FERT.NI.TN.SH), xend = SHORT_NAME, 
                          yend = RF.FERT.NI.TN.SH, color=color), alpha=.5)
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 1)[["Sub"]])
p <- p + theme(legend.position = "none") # hide legend as only one year plotted
p <- p + coord_flip()
p <- p + labs(x="",y="\nkg/ha")
if (rulang) p <- p + labs(x="",y="\nкг/га")
p <- p + guides(color = guide_legend(nrow = 2))
p <- p + scale_y_continuous(labels=space)
p

# Caption
caption_text <- paste("Nitrogen fertilizers consumption in nutrients per ha of arable land, top",nrow(dat_plot),"countries (2012)")
if (rulang) caption_text <- paste("Использование азотных удобрений в переводе в питательное вещество, на гектар пашни,",nrow(dat_plot),"стран с самыми высокими значениями (2012 г.)")

## ---- P1inputRIGHT ----
dat <- syb.df %>% filter(Year %in% 2012, FAOST_CODE < 5000) %>%
  select(FAOST_CODE,Year,RF.FERT.PH.TN.SH) %>%
  mutate(RF.FERT.PH.TN.SH = RF.FERT.PH.TN.SH * 1000) # As we want kg per ha

dat <- dat[!is.na(dat$RF.FERT.PH.TN.SH),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

dat <- dat[which(dat[[region_to_report]]),]

# top for this plot
dat <- arrange(dat, -RF.FERT.PH.TN.SH)
dat_plot <- dat %>% slice(1:20) %>% dplyr::mutate(color = "2012")

if (rulang) dat_plot$SHORT_NAME <- countrycode.multilang::countrycode(dat_plot$FAOST_CODE, origin = "fao", destination = "country.name.russian.fao")

dat_plot$SHORT_NAME <- fct_reorder(dat_plot$SHORT_NAME, dat_plot$RF.FERT.PH.TN.SH) 

p <- ggplot(dat_plot, aes(x=SHORT_NAME,y=RF.FERT.PH.TN.SH))
p <- p + geom_segment(aes(y = min(dat_plot$RF.FERT.PH.TN.SH), xend = SHORT_NAME, 
                          yend = RF.FERT.PH.TN.SH, color=color), alpha=.5)
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 1)[["Sub"]])
p <- p + theme(legend.position = "none") # hide legend as only one year plotted
p <- p + coord_flip()
p <- p + labs(x="",y="\nkg/ha")
if (rulang) p <- p + labs(x="",y="\nкг/га")
p <- p + guides(color = guide_legend(nrow = 2))
p

# Caption
caption_text <- paste("Phosphate fertilizers consumption in nutrients per ha of arable land, top",nrow(dat_plot)," countries (2012)")
if (rulang) caption_text <- paste("Использование фосфорных удобрений в переводе в питательное вещество на гектар пашни,",nrow(dat_plot)," стран с самыми высокими значениями (2012 г.)")

## ---- P1inputBOTTOM ----

# if (region_to_report == "RAF")  dat <- syb.df %>% filter(FAOST_CODE %in% 12001:12005, Year %in% 2012) %>% select(SHORT_NAME,RF.FERT.NI.TN.NO,RF.FERT.PH.TN.NO,RF.FERT.PO.TN.NO,RL.AREA.ARBLPRMN.HA.NO)
# if (region_to_report == "RAP")  dat <- syb.df %>% filter(FAOST_CODE %in% 13001:13014, Year %in% 2012) %>% select(SHORT_NAME,RF.FERT.NI.TN.NO,RF.FERT.PH.TN.NO,RF.FERT.PO.TN.NO,RL.AREA.ARBLPRMN.HA.NO)
# if (region_to_report == "REU")  dat <- syb.df %>% filter(FAOST_CODE %in% 14001:14007, Year %in% 2012) %>% select(SHORT_NAME,RF.FERT.NI.TN.NO,RF.FERT.PH.TN.NO,RF.FERT.PO.TN.NO,RL.AREA.ARBLPRMN.HA.NO)
# if (region_to_report == "RNE")  dat <- syb.df %>% filter(FAOST_CODE %in% 15001:15003, Year %in% 2012) %>% select(SHORT_NAME,RF.FERT.NI.TN.NO,RF.FERT.PH.TN.NO,RF.FERT.PO.TN.NO,RL.AREA.ARBLPRMN.HA.NO)
# if (region_to_report == "GLO")  dat <- syb.df %>% filter(FAOST_CODE %in% c(5100,5200,5300,5400,5500), Year %in% 2012) %>% select(SHORT_NAME,RF.FERT.NI.TN.NO,RF.FERT.PH.TN.NO,RF.FERT.PO.TN.NO,RL.AREA.ARBLPRMN.HA.NO)
#
#
#
# dat <- gather(dat, variable, value, 2:4)
# dat$fill[dat$variable == "RF.FERT.NI.TN.NO"] <- "Nitrogen"
# dat$fill[dat$variable == "RF.FERT.PH.TN.NO"] <- "Phosphate"
# dat$fill[dat$variable == "RF.FERT.PO.TN.NO"] <- "Potash"
#
# dat$share <- (dat$value * 1000) / dat$RL.AREA.ARBLPRMN.HA.NO

if (region_to_report == "RAF")  dat <- syb.df %>% filter(FAOST_CODE %in% 12001:12005, Year %in% 2012) %>% select(SHORT_NAME,RF.FERT.NI.TN.SH,RF.FERT.PH.TN.SH,RF.FERT.PO.TN.SH)
if (region_to_report == "RAP")  dat <- syb.df %>% filter(FAOST_CODE %in% 13001:13014, Year %in% 2012) %>% select(SHORT_NAME,RF.FERT.NI.TN.SH,RF.FERT.PH.TN.SH,RF.FERT.PO.TN.SH)
if (region_to_report == "REU")  dat <- syb.df %>% filter(FAOST_CODE %in% 14001:14007, Year %in% 2012) %>% select(SHORT_NAME,RF.FERT.NI.TN.SH,RF.FERT.PH.TN.SH,RF.FERT.PO.TN.SH)
if (region_to_report == "RNE")  dat <- syb.df %>% filter(FAOST_CODE %in% 15001:15003, Year %in% 2012) %>% select(SHORT_NAME,RF.FERT.NI.TN.SH,RF.FERT.PH.TN.SH,RF.FERT.PO.TN.SH)
if (region_to_report == "GLO")  dat <- syb.df %>% filter(FAOST_CODE %in% c(5100,5200,5300,5400,5500), Year %in% 2012) %>% select(SHORT_NAME,RF.FERT.NI.TN.SH,RF.FERT.PH.TN.SH,RF.FERT.PO.TN.SH)

dat <- gather(dat, variable, value, 2:4)
dat$fill[dat$variable == "RF.FERT.NI.TN.SH"] <- "Nitrogen"
dat$fill[dat$variable == "RF.FERT.PH.TN.SH"] <- "Phosphate"
dat$fill[dat$variable == "RF.FERT.PO.TN.SH"] <- "Potash"

dat$value <- dat$value * 1000 # As we want kg per ha

dat_plot <- dat

dat_plot$SHORT_NAME <- factor(dat_plot$SHORT_NAME, levels=(dat_plot %>% filter(fill == "Nitrogen") %>% arrange(-value))$SHORT_NAME)

ncases <- nrow(dat_plot)

if (rulang){
  dat_plot$fill[dat_plot$fill == "Nitrogen"] <- "Азот"
  dat_plot$fill[dat_plot$fill == "Phosphate"] <- "Фосфат"
  dat_plot$fill[dat_plot$fill == "Potash"] <- "Поташ"
  dat_plot$SHORT_NAME <- translate_subgroups(dat_plot$SHORT_NAME, isfactor = TRUE, add_row_breaks = TRUE)
}

p <- ggplot(dat_plot, aes(x=SHORT_NAME, y=value, fill=fill))
p <- p + geom_bar(stat="identity", position="stack")
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, 3)[["Sub"]])
p <- p + labs(x="",y="kg/ha\n")
if (rulang) p <- p + labs(x="",y="кг/га\n")
p <- p + theme(axis.text.x = element_text(angle=45))
p

# Caption
caption_text <- paste("Fertilizer consumption in nutrients per ha of arable land (2012)")
if (rulang) caption_text <- "Использование удобрений в переводе в питательные вещества на гектар пашни (2012 г.)"

## ---- P1inputMAP ----
# TRY RP.PEST.TOT.TN.SH.EXP
dat <- syb.df %>% filter(Year %in% 2007:2012) %>%
  select(FAOST_CODE, Year, RP.PEST.TOT.TN.SH) %>%
  dplyr::mutate(RP.PEST.TOT.TN.SH = RP.PEST.TOT.TN.SH * 1000) # we want kg per ha

dat <- dat[!is.na(dat$RP.PEST.TOT.TN.SH),]

dat <- dat %>% group_by(FAOST_CODE) %>% dplyr::filter(Year == max(Year)) %>% ungroup()

# dat <- dat[dat$FAOST_CODE != 41,]
dat$FAOST_CODE[dat$FAOST_CODE == 41] <- 351

# set Robinson projection
map.plot <- left_join(map.df,dat) # so that each country in the region will be filled (value/NA)

# Subset
map.plot <- map.plot[which(map.plot[[region_to_report]]),]

cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","RP.PEST.TOT.TN.SH")]
cat_data$value_cat <- categories(x=cat_data$RP.PEST.TOT.TN.SH, n=5,decimals = 2)

map.plot <- left_join(map.plot,cat_data[c("FAOST_CODE","value_cat")])

# define map unit
map_unit <- "kg/ha"
if (rulang) map_unit <- "кг/га"

p <- create_map_here()
p

# Caption
caption_text <- "Pesticides per ha of arable land (kg/ha, 2007 to 2012*)"
if (rulang) caption_text <- "Использование пестицидов на гектар пашни (кг/га, с 2007 по 2012 гг.*)"

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


## ---- P1investTOPRIGHT ----

if (region_to_report == "RAF")  dat <- syb.df %>% filter(FAOST_CODE %in% 12001:12005, Year %in% 1995:2013) %>% select(SHORT_NAME,Year,dfa_share_commit_tot)
if (region_to_report == "RAP")  dat <- syb.df %>% filter(FAOST_CODE %in% 13001:13014, Year %in% 1995:2013) %>% select(SHORT_NAME,Year,dfa_share_commit_tot)
if (region_to_report == "REU")  dat <- syb.df %>% filter(FAOST_CODE %in% 14001:14007, Year %in% 1995:2013) %>% select(SHORT_NAME,Year,dfa_share_commit_tot)
if (region_to_report == "RNE")  dat <- syb.df %>% filter(FAOST_CODE %in% 15001:15003, Year %in% 1995:2013) %>% select(SHORT_NAME,Year,dfa_share_commit_tot)
if (region_to_report == "GLO")  dat <- syb.df %>% filter(FAOST_CODE %in% c(5100,5200,5300,5400,5500), Year %in% 1995:2013) %>% select(SHORT_NAME,Year,dfa_share_commit_tot)



dat_plot <- dat

if (rulang) dat_plot$SHORT_NAME <- translate_subgroups(dat_plot$SHORT_NAME, isfactor = FALSE, add_row_breaks = FALSE)

p <- ggplot(data = dat_plot, aes(x = Year, y = dfa_share_commit_tot,group=SHORT_NAME,color=SHORT_NAME))
p <- p + geom_line(size=1.1, alpha=.7)
p <- p + scale_color_manual(values = plot_colors(part = 1, length(unique(dat_plot$SHORT_NAME)))[["Sub"]])
p <- p + labs(y="percent\n", x="")
if (rulang) p <- p + labs(x="",y="проценты\n")
p <- p + guides(color = guide_legend(nrow = 6))
p


# Caption
caption_text <- "Aid  commitment flows to agriculture, forestry and fishing, share of total aid in \\% (1995-2013)"
if (rulang) caption_text <- "Потоки помощи в сельское хозяйство, лесное хозяйство и рыбное хозяйство, доля в общем объеме помощи в \\% (1995-2013 гг.) "

## ---- P1investLEFT ----
# data
gg <- read.csv(paste0(data.dir, "/credit_to_agriculture.csv"))
gg <- gg[gg$ElementName == "Value US$",]
gg <- gg[gg$ItemName == "Total Credit",]
# into millions
names(gg)[names(gg)=="AreaCode"] <- "FAOST_CODE"

dat1 <- gg %>%  filter(Year %in% c(1999:2001)) %>% group_by(FAOST_CODE) %>% dplyr::summarise(value = mean(Value, na.rm=TRUE)/1000000) %>%
  dplyr::mutate(Year = 2000)
dat2 <- gg %>%  filter(Year %in% c(2010:2012)) %>% group_by(FAOST_CODE) %>% dplyr::summarise(value = mean(Value, na.rm=TRUE)/1000000) %>%
  dplyr::mutate(Year = 2011)

dat <- rbind(dat1,dat2)

dat <- left_join(dat,region_key)
dat <- dat[which(dat[[region_to_report]]),]

# give name Value for value-col
names(dat)[names(dat)=="value"] <- "Value"
# Plot only as many countries as there are for particular region, max 20
nro_latest_cases <- nrow(dat[dat$Year == max(dat$Year),])
if (nro_latest_cases < 20) {ncases <- nro_latest_cases} else ncases <- 20
dat <- arrange(dat, -Year, -Value)
# slice the data for both years
top2015 <- dat %>% slice(1:ncases) %>% dplyr::mutate(color = "2010-2012")
top2000 <- dat %>% filter(FAOST_CODE %in% top2015$FAOST_CODE, Year == 2000) %>% dplyr::mutate(color = "1999-2001")
dat_plot <- rbind(top2015,top2000)
# levels based on newest year
dat_plot$SHORT_NAME <- factor(dat_plot$SHORT_NAME, levels=arrange(top2015,Value)$SHORT_NAME)

if (rulang){
  dat_plot$color[dat_plot$color == "1999-2001"] <- "1999−2001 гг."
  dat_plot$color[dat_plot$color == "2010-2012"] <- "2010−2012 гг."
  dat_plot$SHORT_NAME <- countrycode.multilang::countrycode(dat_plot$FAOST_CODE, origin = "fao", destination = "country.name.russian.fao")
}

dat_plot$SHORT_NAME <- fct_reorder(dat_plot$SHORT_NAME, dat_plot$Value) 

p <- ggplot(dat_plot, aes(x=SHORT_NAME,y=Value))
p <- p + geom_segment(aes(y = min(dat_plot$Value), xend = SHORT_NAME, 
                          yend = Value, color=color), alpha=.5)
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="\nmillion US$")
if (rulang) p <- p + labs(x="",y="\nмлн долл. США")
p <- p + guides(color = guide_legend(nrow = 1))
p

# Caption
caption_text <- paste("Total credit to agriculture, top",ncases,"countries in 2010-12")
if (rulang) caption_text <- paste("Общий объем кредитования сельского хозяйства,",
                                  ncases,
                                  "стран с самыми высокими значениями в 2010-12 гг.")

## ---- P1investRIGHT ----

dat <- syb.df %>% filter(Year %in% c(2009:2013)) %>% select(FAOST_CODE,SHORT_NAME,Year,dfa_AOI_commit)
dat <- dat[!is.na(dat$dfa_AOI_commit),]
dat <- dat[!is.na(dat$SHORT_NAME),]
# Add region key and subset
dat <- left_join(dat,region_key)
dat <- dat[which(dat[[region_to_report]]),]

top10 <- dat %>%  group_by(SHORT_NAME) %>% dplyr::summarise(dfa_AOI_commit = mean(dfa_AOI_commit, na.rm=TRUE)) %>%
  arrange(-dfa_AOI_commit) %>%
  slice(1:10) %>%
  dplyr::mutate(color = "With highest values")

bot10 <- dat %>%  group_by(SHORT_NAME) %>% dplyr::summarise(dfa_AOI_commit = mean(dfa_AOI_commit, na.rm=TRUE)) %>%
  arrange(dfa_AOI_commit) %>%
  slice(1:10) %>%
  dplyr::mutate(color = "With lowest values")

overlap <- top10$SHORT_NAME[top10$SHORT_NAME %in% bot10$SHORT_NAME]
if (length(overlap)!=0) dat_plot <- rbind(top10[!top10$SHORT_NAME %in% overlap,], bot10[!bot10$SHORT_NAME %in% overlap,]) else dat_plot <- rbind(top10,bot10)

if (rulang){
  dat_plot$color[dat_plot$color == "With highest values"] <- "Самые высокие значения"
  dat_plot$color[dat_plot$color == "With lowest values"] <- "Самые низкие значения"
  dat_plot$SHORT_NAME <- countrycode.multilang::countrycode(dat_plot$SHORT_NAME, origin = "country.name", destination = "country.name.russian.fao")
}

dat_plot$SHORT_NAME <- fct_reorder(dat_plot$SHORT_NAME, dat_plot$dfa_AOI_commit) 

p <- ggplot(dat_plot, aes(x=SHORT_NAME,y=dfa_AOI_commit))
p <- p + geom_segment(aes(y = min(dat_plot$dfa_AOI_commit), xend = SHORT_NAME, 
                          yend = dfa_AOI_commit, color=color), alpha=.5)
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="\nindex")
if (rulang) p <- p + labs(x="",y="\nиндекс")
p <- p + guides(color = guide_legend(nrow = 2))
p


# Caption
caption_text <- paste("DFA Agriculture Orientation Index,",nrow(dat_plot)/2,"countries with highest and lowest values, average (2009-2013)")
if (rulang) caption_text <- paste("Индекс ориентации на сельское хозяйство,",
                                  nrow(dat_plot)/2,"
                                  стран с самыми высокими и самыми низкими значениями, среднее значение (2009-2013 гг.)")

## ---- P1investBOTTOM ----


if (region_to_report == "RAF")  dat <- syb.df %>% select(FAOST_CODE,Year,bilat_don_agr,multilat_don_agr,privat_don_agr) %>% filter(FAOST_CODE %in% 12000)
if (region_to_report == "RAP")  dat <- syb.df %>% select(FAOST_CODE,Year,bilat_don_agr,multilat_don_agr,privat_don_agr) %>% filter(FAOST_CODE %in% 13000)
if (region_to_report == "REU")  dat <- syb.df %>% select(FAOST_CODE,Year,bilat_don_agr,multilat_don_agr,privat_don_agr) %>% filter(FAOST_CODE %in% 14000)
if (region_to_report == "RNE")  dat <- syb.df %>% select(FAOST_CODE,Year,bilat_don_agr,multilat_don_agr,privat_don_agr) %>% filter(FAOST_CODE %in% 15000)
if (region_to_report == "GLO")  dat <- syb.df %>% select(FAOST_CODE,Year,bilat_don_agr,multilat_don_agr,privat_don_agr) %>% filter(FAOST_CODE %in% 5000)

dat <- syb.df %>% select(FAOST_CODE,Year,bilat_don_agr,multilat_don_agr,privat_don_agr)

# Add region key and subset
dat <- left_join(dat,region_key)
dat <- dat[which(dat[[region_to_report]]),]

dat <- gather(dat, variable, value, 3:5)
dat <- dat[!is.na(dat$value),]

dat$variable <- as.character(dat$variable)
dat$variable[dat$variable == "bilat_don_agr"] <- "Bilateral"
dat$variable[dat$variable == "multilat_don_agr"] <- "Multilateral"
dat$variable[dat$variable == "privat_don_agr"] <- "Private"

dat$variable <- factor(dat$variable, levels= c("Multilateral","Bilateral","Private"))

dat_plot <- dat %>% group_by(FAOST_CODE) %>% 
              filter(Year == 2013) %>% 
              dplyr::mutate(value_sum = sum(value, na.rm=TRUE)) %>% 
              select(FAOST_CODE,Year,SHORT_NAME,variable,value,value_sum) %>% 
              ungroup() %>%
              arrange(-value_sum) %>% 
              mutate(r = dense_rank(-value_sum)) %>% 
              filter(r %in% 1:10)

# dat$value <- dat$value / 1000 # into bilion dollars

# dat_plot <- dat

# Draw the plot
# p <- ggplot(dat_plot, aes(x = Year, y = value))
# p <- p + geom_area(aes(fill=variable), stat = "identity",position = "stack")
# p <- p + scale_fill_manual(values=plot_colors(part = syb_part, 3)[["Sub"]])
# p <- p + labs(x="",y="billion 2013 US$\n")

# if (rulang) p <- p + labs(x="",y="\n")
# # p <- p + geom_vline(aes(xintercept=2015), color="grey20", linetype="dashed")
# # p <- p + scale_x_continuous(breaks=c(1961,2000,2015,2050))
# p

if (rulang){
  levels(dat_plot$variable)[levels(dat_plot$variable) == "Multilateral"] <- "Многосторонняя"
  levels(dat_plot$variable)[levels(dat_plot$variable) == "Bilateral"] <- "Двусторонняя"
  levels(dat_plot$variable)[levels(dat_plot$variable) == "Private"] <- "Частный"
  dat_plot$SHORT_NAME <- countrycode.multilang::countrycode(dat_plot$FAOST_CODE, origin = "fao", destination = "country.name.russian.fao")
}


p <- ggplot(data=arrange(dat_plot,variable), aes(x = reorder(SHORT_NAME,-value_sum), y = value))
p <- p + geom_bar(aes(fill=variable), stat = "identity",position = "stack")
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, 3)[["Sub"]])
p <- p + labs(x="",y="million 2013 US$\n")
if (rulang) p <- p + labs(x="",y="млн долл. США (в постоянных ценах 2013 г.)\n")
# p <- p + geom_vline(aes(xintercept=2015), color="grey20", linetype="dashed")
# p <- p + scale_x_continuous(breaks=min(dat_plot$Year):max(dat_plot$Year))
p <- p + theme(axis.text.x = element_text(angle=90,vjust=.5))
p



# Caption
caption_text <- paste("Aid commitment flows to Agriculture, Forestry and Fishing, top",length(unique(dat_plot$SHORT_NAME)),"countries in 2013 (million 2013 US\\$)")
if (rulang) caption_text <- paste("Потоки помощи в сельское хозяйство, лесное хозяйство и рыбное хозяйство,",
                                  length(unique(dat_plot$SHORT_NAME)),
                                  "стран с самыми высокими значениями в 2013 г. (млн долл. США, в постоянных ценах 2013 г.)")

## ---- p1investMAPdata ----
# dat <- getFAOtoSYB(domainCode = "IG",
#                    elementCode = 6111,
#                    itemCode = 23101)
# dat <- dat[["aggregates"]]
# dat <- dat[!is.na(dat$IG_23101_6111),]
# dat <- dat %>% filter(Year %in% 2008:2012) %>% group_by(FAOST_CODE) %>% dplyr::mutate(maxyear = max(Year)) %>% ungroup () %>% filter(Year == maxyear)




## ---- P1investMAP ----
# map.plot <- left_join(map.df,dat) # so that each country in the region will be filled (value/NA)
# 
# # Add region key and subset
# 
# map.plot <- map.plot[which(map.plot[[region_to_report]]),]
# 
# cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","IG_23101_6111")]
# cat_data$value_cat <- categories(x=cat_data$IG_23101_6111, n=5, method="jenks",decimals=2)
# 
# map.plot <- left_join(map.plot,cat_data[c("FAOST_CODE","value_cat")])
# 
# # define map unit
# map_unit <- "percent"
# if (rulang) map_unit <- ""
# #
# p <- create_map_here()
# p
# 
dat <- as.data.frame(unique(syb.df$FAOST_CODE))
names(dat) <- "FAOST_CODE"
dat$value <- dat$FAOST_CODE
map.plot <- left_join(map.df,dat)
map.plot <- map.plot[which(map.plot[[region_to_report]]),]
cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","value")]
cat_data$value_cat <- categories(x=cat_data$value, n=5, method="jenks",decimals=2)

map.plot <- left_join(map.plot,cat_data[c("FAOST_CODE","value_cat")])

map_unit <- "nonsense"
if (rulang) map_unit <- "бред какой то"
#
p <- create_map_here()
p

# 
# map.plot <- left_join(map.plot,cat_data[c("FAOST_CODE","value_cat")])
# 
# # define map unit
# map_unit <- "percent"

# # Caption
caption_text <- "Share of government expenditure on agriculture, share of total outlays (percent, 2008 to 2012*)"
if (rulang) caption_text <- "Доля государственных расходов на сельское хозяйство, доля в общем объеме расходов (в процентах, с 2008 по 2012 гг.*)"


