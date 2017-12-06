
## ---- part3_setup ----
## new data source
## can't get it to work with csv
temp <- readxl::read_excel(paste0(root.dir,"/input_data/",region_to_report,"_Charts_data_final.xlsx"), 
                           col_types = c("text", "text", "numeric", "text",
                                         "text", "text", "numeric", "text",
                                         "text", "text", "text", "text"))


## if RU, then remove EN names and rename RU columns
if (rulang) {
  temp <- subset(temp, select = -c(AreaName,ItemName))
  names(temp)[names(temp) == 'AreaNameRU'] <- 'AreaName'
  names(temp)[names(temp) == 'ItemNameRU'] <- 'ItemName'
}

# Fix linebreaks in area names
temp$AreaName <- gsub("\\\\n", "\n", temp$AreaName)

source(paste0(root.dir,'/input/code/plot/plot_color.R'))
library(dplyr)

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
if (region_to_report == "RAF") short_text <- "The dietary energy supply (DES) is the food available for human consumption, expressed in kilocalories per person per day. At the country level, it is calculated as a measure of food available for human use after taking out all non-food utilization, including exports, industrial use, animal feed, seed, wastage and changes in stocks. In 1961 the average global calorie availability was as low as 1163 kcal/cap/day; by 2014, Africa's had reached 2411 kcal/cap/day, and was centred more on a narrow base of staple grains as well as starchy roots and dairy products."
if (region_to_report == "RAP") short_text <- "The dietary energy supply (DES) is the food available for human consumption, expressed in kilocalories per person per day. At the country level, it is calculated as a measure of food available for human use after taking out all non-food utilization, including exports, industrial use, animal feed, seed, wastage and changes in stocks. In 1961 the average global calorie availability was as low as 2 196 kcal/cap/day; by 2011, it had reached 2 870 kcal/cap/day, and was centered more around a narrow base of staple grains as well as meat and dairy products."
if (region_to_report == "REU") short_text <- "The dietary energy supply (DES) is the food available for human consumption, expressed in kilocalories per person per day. At the country level, it is calculated as a measure of food available for human use after taking out all non-food utilization, including exports, industrial use, animal feed, seed, wastage and changes in stocks. The average dietary energy availability in the region is quite high. In 2000 it was 3067 kcal/cap/day and in 2014, it had reached 3271 kcal/cap/day. Average dietary energy supply adequacy is around 140\\%."
if (region_to_report == "RNE") short_text <- "The dietary energy supply (DES) is the food available for human consumption, expressed in kilocalories per person per day. At the country level, it is calculated as a measure of food available for human use after taking out all non-food utilization, including exports, industrial use, animal feed, seed, wastage and changes in stocks. The average dietary energy availability in the region is quite high as it reached 3 183 kcal/cap/day in 2014 that leads to an average dietary energy supply adequacy equal to 136\\%."
if (region_to_report == "GLO") short_text <- "The dietary energy supply (DES) is the food available for human consumption, expressed in kilocalories per person per day. At the country level, it is calculated as a measure of food available for human use after taking out all non-food utilization, including exports, industrial use, animal feed, seed, wastage and changes in stocks. In 1961 the average global calorie availability was as low as 2 196 kcal/cap/day; by 2011, it had reached 2 870 kcal/cap/day, and was centered more around a narrow base of staple grains as well as meat and dairy products."
if (rulang) spread_title <- "Предложение пищевой энергии"
if (region_to_report == "REU" & rulang) short_text <- "Предложение пищевой энергии (DES) – это объем продовольствия, доступного для потребления людьми, выраженный в килокалориях на человека в день (ккал/чел/день). На страновом уровне рассчитывается как объем продовольствия, доступного для потребления людьми, за вычетом непродовольственного использования (экспорт, промышленное использование, фураж, семена, порча, изменения в запасах). Среднее предложение пищевой энергии в регионе достаточно высоко. В 2000 году оно составляло 3067 ккал/чел/день, а в 2014 году достигло 3271 ккал/чел/день. Адекватность средней энергетической ценности пищевого рациона составляет около 140 процентов."


## ---- P3desData ----
# Retrieve data
dat1 <- subset(temp, subset=Part %in% "P3des")
dat1 <- subset(dat1, subset=Position %in% "TOPRIGHT")
dat1 <- subset(dat1, select = c(AreaName,Year,Indicator,Value,ItemName))


## ---- P3desTOPRIGHT ----
if (rulang){

  dat1$ItemName[dat1$Indicator == "FBS.SDES.CRLS.PCT3D"] <- "Злаки \n(за искл. пива)"
  dat1$ItemName[dat1$Indicator == "FBS.SDES.SR.PCT3D"] <- "Крахмалистые \nкорнеплоды"
  dat1$ItemName[dat1$Indicator == "FBS.SDES.SS.PCT3D"] <- "Сахар и \nсахарозаменители"
  dat1$ItemName[dat1$Indicator == "FBS.SDES.MO.PCT3D"] <- "Мясо и \nсубпродукты"
  dat1$ItemName[dat1$Indicator == "FBS.SDES.VOAF.PCT3D"] <- "Молоко (за искл. \nсливочного масла)"
  dat1$ItemName[dat1$Indicator == "FBS.SDES.MEB.PCT3D"] <- "Растительные масла и \nживотные жиры"
  
} else {
  dat1$ItemName[dat1$Indicator == "FBS.SDES.CRLS.PCT3D"] <- "Cereals\n(excl. beer)"
  dat1$ItemName[dat1$Indicator == "FBS.SDES.SR.PCT3D"] <- "Starchy roots"
  dat1$ItemName[dat1$Indicator == "FBS.SDES.SS.PCT3D"] <- "Sugar and\nsweeteners"
  dat1$ItemName[dat1$Indicator == "FBS.SDES.MO.PCT3D"] <- "Meat and offals"
  dat1$ItemName[dat1$Indicator == "FBS.SDES.VOAF.PCT3D"] <- "Milk\n(excl. butter)"
  dat1$ItemName[dat1$Indicator == "FBS.SDES.MEB.PCT3D"] <- "Veg. oils and\nanimal fats"
  
}
sum <- 100
dat_plot <- dat1
dat_plot <- dat_plot %>% dplyr::mutate(mean = Value/sum(Value)*100)

p <- ggplot(dat_plot, aes(x=sum/2, y = mean, fill = var, width = sum, ymax=1))
p <- p + geom_bar(position="fill", stat="identity")
p <- p + geom_label(aes(x=sum*2.0/2,y=mean+2,label=paste0(round(wmean,1),"%")),
                    label.padding = unit(0.10, "lines"),
                    position="fill",
                    color="white",lineheight=.7,
                    stat="identity",alpha=.9,
                    size=3,family="PT Sans",fontface="bold",show.legend=FALSE)


p <- ggplot(dat_plot, aes(x=sum/2, y = mean, fill = ItemName, width = sum, ymax=1))
p <- p + geom_bar(position="fill", stat="identity")
#p <- p + geom_label(aes(x=sum*2.0/2,y=mean+2,label=paste0(round(Value,1),"%")),
p <- p + geom_label(aes(x=sum,y=mean,label=paste0(round(Value,1),"%")),
                    label.padding = unit(0.10, "lines"),
                    position="fill",
                    color="white",lineheight=.7,
                    stat="identity",alpha=.9,
                    size=3,family="PT Sans",fontface="bold",show.legend=FALSE)
p <- p + coord_polar("y")
p <- p + theme_minimal()
p <- p + theme(legend.position = "right")
# p <- p + theme(legend.position = "none")
p <- p + theme(axis.text = element_blank())
p <- p + theme(axis.title = element_blank())
p <- p + theme(axis.ticks = element_blank())
p <- p + theme(panel.grid.minor = element_blank())
p <- p + theme(panel.grid.major.x = element_blank())
p <- p + theme(panel.grid.major.y = element_blank())
# p <- p + scale_fill_manual(values=rev(colPart2$Sub))
# p <- p + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73","#984ea3", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#4daf4a"))
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, length(unique(dat_plot$ItemName)))[["Sub"]])
p <- p + theme(legend.title = element_blank())
if (table_type == "latex"){
  p <- p + theme(text = element_text(size=11, family="PT Sans"))
  p <- p + theme(legend.key.height = unit(7, "mm"))
  p <- p + theme(legend.key.width = unit(3, "mm"))
} else {
  p <- p + theme(text = element_text(size=14, family="PT Sans"))
  p <- p + theme(legend.key.height = unit(9, "mm"))
  p <- p + theme(legend.key.width = unit(6, "mm"))
}
p <- p + theme(panel.grid=element_blank(), panel.border=element_blank())
p <- p + labs(x=NULL, y=NULL)
if (rulang) p <- p + labs(x="",y="\n")
p <- p + theme(plot.margin=unit(c(0,0,0,0),"mm"))
p



# Caption
caption_text <- paste("Share of dietary energy supply, kcal/capita/day (",dat1$Year[1],")", sep = "")
if (rulang) caption_text <- paste("Доля энергетической ценности пищевого рациона, ккал/чел/сутки (",dat1$Year[1]," г.)", sep = "")

## ---- P3desLEFT ----
# data
dat1 <- temp %>%
  dplyr::filter(Part == "P3des",
         Position == "LEFT") %>%
  dplyr::select(AreaName,Year,Indicator,Value) %>%
  dplyr::mutate(Yr = as.integer(substr(Year,1,4)))


# dat1 <- subset(temp, subset=Part %in% "P3des")
# dat1 <- subset(dat1, subset=Position %in% "LEFT")
# dat1 <- subset(dat1, select = c(AreaName,Year,Indicator,Value))
# dat1 <- dat1 %>% 
#   dplyr::mutate(Yr = substr(dat1$Year,1,4))
# dat1$Yr <- as.integer((dat1$Yr))

minYr <- min(dat1$Year)
maxYr <- max(dat1$Year)
# 
# # Plot only as many countries as there are for particular region, max 20
nro_latest_cases <- nrow(dat1[dat1$Year == max(dat1$Year),])
if (nro_latest_cases < 20) {ncases <- nro_latest_cases} else ncases <- 20
dat1 <- arrange(dat1, -Yr, -Value)
# slice the data for both years
top2015 <- dat1 %>% slice(1:ncases) %>% dplyr::mutate(color = maxYr)
top2000 <- dat1 %>% dplyr::filter(AreaName %in% top2015$AreaName, Year == minYr) %>% dplyr::mutate(color = minYr)
dat_plot <- rbind(top2015,top2000)
# levels based on newest year
dat_plot$AreaName <- factor(dat_plot$AreaName, levels=arrange(top2015,Value)$AreaName)
# ###############
# 
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
p <- p + labs(x="",y="\nkcal/capita/day")
if (rulang) p <- p + labs(x="",y="\nккал/чел/день")
p <- p + guides(color = guide_legend(nrow = 1))
p

# Caption
caption_text <- paste("Dietary energy supply, top ",ncases," countries in ",maxYr,"", sep = "")
if (rulang) caption_text <- paste("Предложение пищевой энергии, ",ncases," стран с самыми высокими показателями в ",maxYr," гг.", sep = "")


## ---- P3desRIGHT ----
dat1 <- subset(temp, subset=Part %in% "P3des")
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
top91 <- dat1 %>% dplyr::filter(AreaName %in% top15$AreaName, Year == minYr) %>% dplyr::mutate(color = minYr)
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
                      aes_(y = as.name(minYr), xend = quote(AreaName),
                           yend = as.name(maxYr)), color="grey80")
p <- p + geom_point(aes(fill=color),size = 4, alpha = 0.75, pch=21, color="white") + theme(panel.grid.major.y = element_blank())
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="\npercent")
if (rulang) p <- p + labs(x="",y="\nпроценты")
p <- p + guides(color = guide_legend(nrow = 2))
p



# Caption
caption_text <- paste("Average dietary energy supply adequacy, percent, top ",ncases," countries (",maxYr,")", sep = "")
if (rulang) caption_text <- paste("Адекватность средней энергетической ценности пищевого рациона, в процентах," ,
                                  ncases," стран с самыми высокими значениями (",maxYr," гг.)", sep = "")


## ---- P3desBOTTOM ----
dat1 <- subset(temp, subset=Part %in% "P3des")
dat1 <- subset(dat1, subset=Position %in% "BOTTOM")
dat1 <- subset(dat1, select = c(AreaName,Year,Value))
dat1$Year <- as.integer(dat1$Year)

dat_plot <- dat1[!is.na(dat1$Value),]

p <- ggplot(dat_plot, aes(x=Year,y=Value,color=AreaName))
p <- p + geom_line(size=1.1, alpha=.7)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 5)[["Sub"]])
p <- p + labs(x="",y="kcal/cap/day\n")
if (rulang) p <- p + labs(x="",y="ккал/чел/день\n")
p <- p + guides(color = guide_legend(nrow = 3))
p <- p + scale_x_continuous(breaks=c(2000,2005,2010,2013))
p


# Caption
caption_text <- "Dietary energy supply in top 5 countries"
if (rulang) caption_text <- "Предложение пищевой энергии в 5 странах с самыми высокими значениями"

## ---- P3desMAP ----
dat1 <- subset(temp, subset=Part %in% "P3des")
dat1 <- subset(dat1, subset=Position %in% "MAP")
dat1 <- subset(dat1, select = c(AreaCode,Value,Year))
dat1$AreaCode <- as.integer(dat1$AreaCode)

map.plot <- left_join(map.df,dat1, by = c("FAOST_CODE" = "AreaCode")) # so that each country in the region will be filled (value/NA)

# Add region key and subset

map.plot <- map.plot[which(map.plot[[region_to_report]]),]

cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","Value")]
cat_data$value_cat <- categories(x=cat_data$Value, n=5, manual=FALSE, method="jenks") # manualBreaks = c(0, 5, 15, 25, 35, 100),

map.plot <- left_join(map.plot,cat_data[c("FAOST_CODE","value_cat")])

# define map unit
map_unit <- "percent"
if (rulang) map_unit <- "проценты"

p <- create_map_here()
p

yr = dat1$Year[1]
# Caption
caption_text <- paste("Average dietary energy supply adequacy, percent (",yr,")", sep = "")
if (rulang) caption_text <- paste("Адекватность средней энергетической ценности пищевого рациона, в процентах (",yr," гг.)", sep = "")

#    ____                                             _               _    _
#   / ___| _ __  ___   _ __    _ __   _ __  ___    __| | _   _   ___ | |_ (_)  ___   _ __
#  | |    | '__|/ _ \ | '_ \  | '_ \ | '__|/ _ \  / _` || | | | / __|| __|| | / _ \ | '_ \
#  | |___ | |  | (_) || |_) | | |_) || |  | (_) || (_| || |_| || (__ | |_ | || (_) || | | |
#   \____||_|   \___/ | .__/  | .__/ |_|   \___/  \__,_| \__,_| \___| \__||_| \___/ |_| |_|
#                     |_|     |_|


## ---- P3cropproTEXT ----
spread_title <- "Crop production"
if (region_to_report == "RAF") short_text <- "The majority of people in developing countries live in rural areas, and most of them depend on agriculture for their livelihoods. Over the past 50 years, growth in crop production has been driven largely by higher yields per unit of land, and crop intensification. Trends are not uniform across regions, however. Most of the growth in wheat and rice production in Asia and Northern Africa has been from gains in yield, while expansion of harvested land has led to production growth of maize in Latin America and in sub-Saharan Africa."
if (region_to_report == "RAP") short_text <- "The majority of people in developing countries live in rural areas, and most of them depend on agriculture for their livelihoods. Over the past 50 years, growth in crop production has been driven largely by higher yields per unit of land, and crop intensification. Trends are not uniform across regions, however. Most of the growth in wheat and rice production in Asia and Northern Africa has been from gains in yield, while expansion of harvested land has led to production growth of maize in Latin America and in sub-Saharan Africa."
if (region_to_report == "REU") short_text <- "Most of people living in rural areas depend on agriculture for their livelihoods. Over the last decade, crop production has grown in the region as a whole and in all its sub-regions except EU other and EFTA, where it has declined mostly due to decrease of the planted areas. Crop production growth is largely due to higher yields and crop intensification. The average cropping intensity ratio is 2 percent in the region and is as high as 4.1 percent for the EU other and EFTA countries. "
if (region_to_report == "RNE") short_text <- "The majority of people in developing countries live in rural areas, and most of them depend on agriculture for their livelihoods. Over the past 50 years, growth in crop production has been driven largely by higher yields per unit of land, and crop intensification, as we can see for the average annual growth in cereal production where the harvested area has been declining while production and yield have been increasing since 2000 until 2013. In the region, Tunisia, Iran, Egypt and Syria were the top crop production countries in 2012."
if (region_to_report == "GLO") short_text <- "The majority of people in developing countries live in rural areas, and most of them depend on agriculture for their livelihoods. Over the past 50 years, growth in crop production has been driven largely by higher yields per unit of land, and crop intensification. Trends are not uniform across regions, however. Most of the growth in wheat and rice production in Asia and Northern Africa has been from gains in yield, while expansion of harvested land has led to production growth of maize in Latin America and in sub-Saharan Africa."
if (rulang) spread_title <- "Производство сельскохозяйственных культур"
if (region_to_report == "REU" & rulang) short_text <- "Большинство людей, живущих в сельских районах, зависят от сельского хозяйства для обеспечения средств к существованию. За последнее десятилетие производство сельскохозяйственных культур выросло как в регионе в целом, так и во всех субрегионах, за исключением субрегиона «Другие страны ЕС и ЕАСТ», где оно снизилась в основном за счет уменьшения посевных площадей. Рост производства продукции растениеводства происходит во многом благодаря росту урожайности и интенсификации растениеводства. Средний коэффициент интенсивности земледелия составляет 2 процента в регионе и 4,1 процента в субрегионе «Другие страны ЕС и ЕАСТ»."

## ---- P3cropproData ----
dat1 <- subset(temp, subset=Part %in% "P3croppro")
dat1 <- subset(dat1, subset=Position %in% "TOPRIGHT")
dat1 <- subset(dat1, select = c(ItemName,Value,Year))
yr = dat1$Year[1]
dat1 <- subset(dat1, select = c(ItemName,Value))

rc <- dat1 %>% arrange(-Value) %>% slice(1:5)

names(rc) <- c("","%")

tbl_data <- rc
if (table_type == "latex") cap <- paste("\\large{Fastest growing products based on quantities (average annual growth rate, 2000 to ",yr,")}", sep = "")
if (table_type == "html")  cap <- paste("<b>Table: Fastest growing products based on quantities (average annual growth rate, 2000 to ",yr,")</b>", sep = "")
caption_text <- cap
if (rulang){
  caption_text <- paste("\\large{Продукты, производство которых растет самыми быстрыми темпами, исходя из количества (среднегодовые темпы роста, с 2000 по ",yr," гг.)}", sep = "")
} 

print.xtable(xtable(tbl_data, caption = caption_text, digits = c(0,0,0),
                    align= "l{\raggedright\arraybackslash}p{2.2cm}r"),
             type = table_type, table.placement = NULL, booktabs = TRUE,
             comment = FALSE,
             include.rownames = FALSE, size = "footnotesize", caption.placement = "top",
             html.table.attributes = 'class="table table-striped table-hover"')




## ---- P3cropproLEFT ----
# data
dat1 <- subset(temp, subset=Part %in% "P3croppro")
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
top2000 <- dat1 %>% dplyr::filter(AreaName %in% top2015$AreaName, Year == minYr) %>% dplyr::mutate(color = minYr)
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
p <- p + labs(x="",y="\nconstant 2004 - 2006 Int$")
if (rulang) p <- p + labs(x="",y="\nпост. межд. долл.  \n2004 − 2006 гг.")
p <- p + guides(color = guide_legend(nrow = 1))
p <- p + scale_y_continuous(labels=space)
p


# Caption
caption_text <- paste("Top ",ncases," crop producing countries in ",maxYr," based on net per capita crop production value (constant 2004 - 2006 Int\\$)", sep = "")
if (rulang) caption_text <- paste(ncases," стран с наиболее высокими показателями производства сельскохозяйственных культур в ",maxYr," году на основе показателей чистого объема производства сельскохозяйственных культур на душу населения (в межд. постоянных долларах  2004 – 2006 гг.)", sep = "")


## ---- P3cropproRIGHT ----

# data
dat1 <- subset(temp, subset=Part %in% "P3croppro")
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
top2000 <- dat1 %>% dplyr::filter(AreaName %in% top2015$AreaName, Year == minYr) %>% dplyr::mutate(color = minYr)
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
p <- p + labs(x="",y="\nconstant 2004 - 2006 Int$")
if (rulang) p <- p + labs(x="",y="\nпост. межд. долл.  \n2004 − 2006 гг.")
p <- p + guides(color = guide_legend(nrow = 1))
p <- p + scale_y_continuous(labels=space)
p



# Caption
caption_text <- paste("Top ",ncases," food producing countries in ",maxYr," based on net per capita food production value (constant 2004 - 2006 Int\\$)", sep = "")
if (rulang) caption_text <- paste(ncases," стран с наиболее высокими показателями производства продовольствия в ",maxYr," году на основе показателей чистого объема производства продовольствия на душу населения (в межд. постоянных долларах  2004 – 2006 гг.)", sep = "")


## ---- P3cropproBOTTOM ----
dat1 <- subset(temp, subset=Part %in% "P3croppro")
dat1 <- subset(dat1, subset=Position %in% "BOTTOM")
dat1 <- subset(dat1, select = c(AreaName,Year,Indicator,Value))
minYr <- min(dat1$Year)
maxYr <- max(dat1$Year)

dat_plot <- dat1

if (!rulang){
  dat_plot$Indicator[dat_plot$Indicator == "QC.PRD.CRLS.TN.GR"] <- "Production"
  dat_plot$Indicator[dat_plot$Indicator == "QC.RHRV.CRLS.HA.GR"] <- "Harvested area"
  dat_plot$Indicator[dat_plot$Indicator == "QC.YIELD.CRLS.HG.GR"] <- "Yield"
  dat_plot$Indicator <- factor(dat_plot$Indicator, levels=c("Harvested area",
                                                          "Production",
                                                          "Yield"
  ))
} else{
  dat_plot$Indicator[dat_plot$Indicator == "QC.PRD.CRLS.TN.GR"] <- "Производство"
  dat_plot$Indicator[dat_plot$Indicator == "QC.RHRV.CRLS.HA.GR"] <- "Уборочная площадь"
  dat_plot$Indicator[dat_plot$Indicator == "QC.YIELD.CRLS.HG.GR"] <- "Урожайность"
  dat_plot$Indicator <- factor(dat_plot$Indicator, levels=c("Уборочная площадь",
                                                          "Производство",
                                                          "Урожайность"
  ))
}

p <- ggplot(dat_plot, aes(x=Indicator,y=Value,fill=Indicator))
p <- p + geom_bar(stat="identity",position="dodge")
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, length(unique(dat_plot$Indicator)))[["Sub"]])
p <- p + labs(x="",y="percent\n")
if (rulang) p <- p + labs(x="",y="проценты\n")
p <- p + theme(legend.position = "none")
p


# Caption
caption_text <- paste("Average annual growth in cereals production (",minYr,"-",maxYr,")", sep = "")
if (rulang) caption_text <- paste("Среднегодовые темпы роста производства зерновых (",minYr,"-",maxYr," гг.)", sep = "")


## ---- P3cropproMAP ----
dat1 <- subset(temp, subset=Part %in% "P3croppro")
dat1 <- subset(dat1, subset=Position %in% "MAP")
dat1 <- subset(dat1, select = c(AreaCode,Value,Year))
dat1$AreaCode <- as.integer(dat1$AreaCode)

map.plot <- left_join(map.df,dat1, by = c("FAOST_CODE" = "AreaCode")) # so that each country in the region will be filled (value/NA)

# Add region key and subset

map.plot <- map.plot[which(map.plot[[region_to_report]]),]

cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","Value")]
cat_data$value_cat <- categories(x=cat_data$Value, n=5, method="jenks",decimals=0)

map.plot <- left_join(map.plot,cat_data[c("FAOST_CODE","value_cat")])

# define map unit
map_unit <- "index"
if (rulang) map_unit <- "индекс"
p <- create_map_here()
p

yr = dat1$Year[1]
# Caption
caption_text <- paste("Crops, net per capita production index (2004-06 = 100, ",yr,")", sep = "")
if (rulang) caption_text <- paste("Сельскохозяйственные культуры, чистого индекс производства на душу населения (2004-06 гг.= 100, ",yr," г.)", sep = "")


#
#    ____
#   / ___| _ __  ___   _ __
#  | |    | '__|/ _ \ | '_ \
#  | |___ | |  | (_) || |_) |
#   \____||_|   \___/ | .__/
#                     |_|



## ---- P3cropTEXT ----
spread_title <- "Crops"
if (region_to_report == "RAF") short_text <- "In Africa, maize and starchy roots (cassava, yam and potatoes) make up the majority of the production of the crop sector. They continue to be the most important food source for human consumption. Yet external factors, such as rising incomes and urbanization, are causing diets to shift towards diets that are higher in protein, fats and sugar. In addition, livestock and biofuel production have and will most likely grow at a faster rate than crop production. This is causing a shift away from crops, like wheat and rice, towards coarse grains and oilseeds to meet demands for food, feed and biofuel."
if (region_to_report == "RAP") short_text <- "Cereals, which include wheat, rice, barley, maize, rye, oats and millet, make up the majority of the production of the crop sector. They continue to be the most important food source for human consumption. Yet external factors, such as rising incomes and urbanization, are causing diets to shift towards diets that are higher in protein, fats and sugar. In addition, livestock and biofuel production have and will most likely grow at a faster rate than crop production. This is causing a shift away from crops, like wheat and rice, towards coarse grains and oilseeds to meet demands for food, feed and biofuel."
if (region_to_report == "REU") short_text <- "Cereals, specifically wheat, maize and barley, as well as sugar beet and  potatoes make up the top five of the production of the crop sector in the region. They continue to be an important food source for human consumption. In addition, livestock and biofuel production have grown and will most likely grow at a faster rate than crop production. This is causing a shift towards the crops allowing to meet demands for food, feed and biofuel."
if (region_to_report == "RNE") short_text <- "Wheat, sugar cane, tomatoes, potatoes and sugar beet, make up the majority of the production of the crop sector. They continue to be the most important food source for human consumption. Within the region, Morocco, Iran and Syria were the top per capita wheat producing countries, while Egypt, Iran and Morocco were the top per capita sugar cane producing countries in 2013. Since 2010, Other Near East countries have been showing the higher yield of cereals after Gulf States had been the best in the region since 2000, probably at higher costs of production."
if (region_to_report == "GLO") short_text <- "Cereals, which include wheat, rice, barley, maize, rye, oats and millet, make up the majority of the production of the crop sector. They continue to be the most important food source for human consumption. Yet external factors, such as rising incomes and urbanization, are causing diets to shift towards diets that are higher in protein, fats and sugar. In addition, livestock and biofuel production have and will most likely grow at a faster rate than crop production. This is causing a shift away from crops, like wheat and rice, towards coarse grains and oilseeds to meet demands for food, feed and biofuel."
if (rulang) spread_title <- "Сельскохозяйственные культуры"
if (region_to_report == "REU" & rulang) short_text <- "Злаки, в частности, пшеница, кукуруза и ячмень, а также сахарная свекла и картофель составляют пятерку самых выращиваемых сельскохозяйственных культур в регионе. Они продолжают быть важным источником пищи для потребления человеком. Кроме того, растет уровень производства продукции животноводства и биотоплива и скорее всего будет расти более высокими темпами, чем растениеводство. Это вызывает сдвиг в сторону производства сельскохозяйственных культур, позволяющих удовлетворить спрос на продовольствие, фураж и биотопливо."

## ---- P3cropData ----
dat1 <- subset(temp, subset=Part %in% "P3crop")
dat1 <- subset(dat1, subset=Position %in% "TOPRIGHT")
dat1 <- subset(dat1, select = c(AreaName,Year,Indicator,Value,ItemName))
minYr <- min(dat1$Year)
maxYr <- max(dat1$Year)

first <- dat1$ItemName[1]
second <- dat1$ItemName[2]

d13 <- dat1 %>%  dplyr::filter(Year == maxYr)
d00 <- dat1 %>% dplyr::filter(Year == minYr )
gg <- merge(d00,d13,by="ItemName")
gg <- subset(gg, select = c(ItemName,Value.x,Value.y))
gg <- arrange(gg, -gg$Value.y)
gg$Value.x<- prettyNum(gg$Value.x, big.mark=" ")
gg$Value.y<- prettyNum(gg$Value.y, big.mark=" ")


tbl_data <- gg
if (table_type == "latex") cap <- paste("\\large{Top five items produced in ",maxYr,", thousand tonnes}", sep = "")
if (table_type == "html")  cap <- paste("<b>Table: Top five items produced in ",maxYr,", thousand tonnes</b>", sep = "")
caption_text <- cap
if (rulang){
  caption_text <- paste("\\large{Пять самых распространенных продуктов, произведенных в ",maxYr," году, тыс. тонн}", sep = "")
} 


print(xtable(tbl_data, caption = caption_text, digits = c(0,0,0,0),
             align= "l{\raggedright\arraybackslash}p{1.0cm}rr"),
      type = table_type, table.placement = NULL,
      booktabs = TRUE, include.rownames = FALSE,
      comment = FALSE,
      size = "footnotesize", caption.placement = "top",
      html.table.attributes = 'class="table table-striped table-hover"')


## ---- P3cropLEFT ----

# data
dat1 <- subset(temp, subset=Part %in% "P3crop")
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
top2000 <- dat1 %>% dplyr::filter(AreaName %in% top2015$AreaName, Year == minYr) %>% dplyr::mutate(color = minYr)
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
p <- p + labs(x="",y="\nkg per capita")
if (rulang) p <- p + labs(x="",y="\nкг на душу населения")
p <- p + guides(color = guide_legend(nrow = 1))
p


# Caption
caption_text <- paste("Top",ncases,tolower(first),"producing countries, kg per capita")
if (rulang) caption_text <- paste(ncases,
                                  #tolower(second),
                                  "стран с самыми высокими показателями производства пшеницы, в кг на душу населения")


## ---- P3cropRIGHT ----

dat1 <- subset(temp, subset=Part %in% "P3crop")
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
top2000 <- dat1 %>% dplyr::filter(AreaName %in% top2015$AreaName, Year == minYr) %>% dplyr::mutate(color = minYr)
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
p <- p + labs(x="",y="\nkg per capita")
if (rulang) p <- p + labs(x="",y="\nкг на душу населения")
p <- p + guides(color = guide_legend(nrow = 1))
p

# Caption
caption_text <- paste("Top",ncases,tolower(second),"producing countries, kg per capita")
if (rulang) caption_text <- paste(ncases,
                                  #tolower(second),
                                  "стран с самыми высокими показателями производства сахарной свеклы, в кг на душу населения")


## ---- P3cropBOTTOM ----
dat1 <- subset(temp, subset=Part %in% "P3crop")
dat1 <- subset(dat1, subset=Position %in% "BOTTOM")
dat1 <- subset(dat1, select = c(AreaName,Year,Value))
dat1$Year <- as.integer(dat1$Year)

minYr <- min(dat1$Year)
maxYr <- max(dat1$Year)

dat_plot <- dat1[!is.na(dat1$Value),]

p <- ggplot(dat_plot, aes(x=Year,y=Value,color=AreaName))
p <- p + geom_line(size=1.1, alpha=.7)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, length(unique(dat_plot$AreaName)))[["Sub"]])
p <- p + labs(y="hg/capita\n", x="")
if (rulang) p <- p + labs(x="",y="Гг на душу населения\n")
p <- p + guides(color = guide_legend(nrow = 3))
p <- p + scale_x_continuous(breaks=c(minYr,2003,2006,2009,2012,maxYr))
p

# Caption
caption_text <- "Cereals, yield, hg per capita"
if (rulang) caption_text <- "Зерновые, урожайность, в гектограммах на душу населения "


## ---- P3cropMAP ----

dat1 <- subset(temp, subset=Part %in% "P3crop")
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
map_unit <- "kg/cap"
if (rulang) map_unit <- "кг/чел"

p <- create_map_here()
p


# Caption
caption_text <- paste("Cereal production, kg/cap (",dat1$Year[1],")", sep = "")
if (rulang) caption_text <- paste("Производство зерновых, кг/чел (",dat1$Year[1]," г.)", sep = "")



#   _      _                   _                _
#  | |    (_)__   __ ___  ___ | |_  ___    ___ | | __
#  | |    | |\ \ / // _ \/ __|| __|/ _ \  / __|| |/ /
#  | |___ | | \ V /|  __/\__ \| |_| (_) || (__ |   <
#  |_____||_|  \_/  \___||___/ \__|\___/  \___||_|\_\
#

## ---- P3livestockTEXT ----
spread_title <- "Livestock"
if (region_to_report == "RAF") short_text <- "The world food economy is being increasingly driven by the shift of diets towards animal-based products such as meat, milk and dairy. As a result, agriculture is being affected, not only through growth of livestock production, but also through linkages to other sectors that supply feeding stuffs, such as crops and fisheries. In the African region, livestock production remains the largest user of agricultural land and therefore also leaves a significant imprint on the environment. Northern and Western Africa continue to be the largest producers of chickens, which is the top reared animal in 2013."
if (region_to_report == "RAP") short_text <- "The world food economy is being increasingly driven by the shift of diets towards animal-based products such as meat, milk and dairy. As a result, agriculture is being affected, not only through growth of livestock production, but also through linkages to other sectors that supply feeding stuffs, such as crops and fisheries. Globally livestock production is the largest user of agricultural land and therefore also leaves a significant imprint on the environment."
if (region_to_report == "REU") short_text <- "The food economy of the region is being increasingly driven by the shift of diets towards animal-based products such as meat, milk and dairy. As a result, agriculture is being affected, not only through growth of livestock production, but also through linkages to other sectors that supply feeding stuffs, such as crops and fisheries. Livestock production is the largest user of agricultural land and therefore also leaves a significant imprint on the environment."
if (region_to_report == "RNE") short_text <- "The world food economy is being increasingly driven by the shift of diets towards animal-based products such as meat, milk and dairy. As a result, agriculture is being affected, not only through growth of livestock production, but also through linkages to other sectors that supply feeding stuffs, such as crops and fisheries. Globally livestock production is the largest user of agricultural land and therefore also leaves a significant imprint on the environment. Chickens, sheep, goats, cattle and birds are the top live animal produced in the region. Near East counties continue to be the largest producers of chickens up to 2013."
if (region_to_report == "GLO") short_text <- "The world food economy is being increasingly driven by the shift of diets towards animal-based products such as meat, milk and dairy. As a result, agriculture is being affected, not only through growth of livestock production, but also through linkages to other sectors that supply feeding stuffs, such as crops and fisheries. Globally livestock production is the largest user of agricultural land and therefore also leaves a significant imprint on the environment."
if (rulang) spread_title <- "Животноводство"
if (region_to_report == "REU" & rulang) short_text <- "Продовольственной экономикой региона все больше движет сдвиг рациона питания в сторону продуктов животного происхождения, таких как мясо, молоко и молочные продукты. В результате на сельское хозяйство воздействует не только рост производства животноводческой продукции, но также взаимодействие с другими секторами, которые поставляют корм для животных, такими как растениеводство и рыболовство. Животноводство является крупнейшим пользователем сельскохозяйственных земель и, следовательно, накладывает значительный отпечаток на окружающую среду."

## ---- P3livestockData ----
dat1 <- subset(temp, subset=Part %in% "P3livestock")
dat1 <- subset(dat1, subset=Position %in% "TOPRIGHT")
dat1 <- subset(dat1, select = c(AreaName,Year,Indicator,Value,ItemName))
minYr <- min(dat1$Year)
maxYr <- max(dat1$Year)


d13 <- dat1 %>%  dplyr::filter(Year == maxYr)
d00 <- dat1 %>% dplyr::filter(Year == minYr )
gg <- merge(d00,d13,by="ItemName")
gg <- subset(gg, select = c(ItemName,Value.x,Value.y))
gg <- arrange(gg, -gg$Value.y)
gg$Value.x<- prettyNum(gg$Value.x, big.mark=" ")
gg$Value.y<- prettyNum(gg$Value.y, big.mark=" ")


tbl_data <- gg
if (table_type == "latex") cap <- paste("\\large{Live animal number, top 5 in ",maxYr," (thousand heads)}", sep = "")
if (table_type == "html")  cap <- paste("<b>Table: Live animal number, top 5 in ",maxYr," (thousand heads)</b>", sep = "")
caption_text <- cap
if (rulang){
  caption_text <- paste("\\large{Число самых распространенных животных в ",maxYr," году (тыс. голов)}", sep = "")
} 


print(xtable(tbl_data, caption = caption_text, digits = c(0,0,0,0),
             align= "l{\raggedright\arraybackslash}p{1.0cm}rr"),
      type = table_type, table.placement = NULL,
      booktabs = TRUE, include.rownames = FALSE,
      comment = FALSE,
      size = "footnotesize", caption.placement = "top",
      html.table.attributes = 'class="table table-striped table-hover"')


## ---- P3livestockLEFT ----
# data
dat1 <- subset(temp, subset=Part %in% "P3livestock")
dat1 <- subset(dat1, subset=Position %in% "LEFT")
dat1 <- subset(dat1, select = c(AreaName,Year,Indicator,Value))
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
                          yend = Value, color=color), alpha=.5, show.legend = FALSE)
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75) + theme(panel.grid.major.y = element_blank())
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="\nmillion tonnes")
if (rulang) p <- p + labs(x="",y="\nмлн тонн")
p <- p + guides(color = guide_legend(nrow = 2))
p


# Caption
caption_text <- paste("Total milk production, top and bottom ",nrow(dat_plot)/2," countries (",dat1$Year[1],")", sep = "")
if (rulang) caption_text <- paste("Общий объем производства молока, ",nrow(dat_plot)/2," стран с самыми высокими и самими низкими показателями (",dat1$Year[1]," г.)", sep = "")


## ---- P3livestockRIGHT ----
dat1 <- subset(temp, subset=Part %in% "P3livestock")
dat1 <- subset(dat1, subset=Position %in% "RIGHT")
dat1 <- subset(dat1, select = c(AreaName,Year,Indicator,Value))
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
                          yend = Value, color=color), alpha=.5, show.legend = FALSE)
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75) + theme(panel.grid.major.y = element_blank())
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="\nmillion tonnes")
if (rulang) p <- p + labs(x="",y="\nмлн тонн")
p <- p + guides(color = guide_legend(nrow = 2))
p

# Caption
caption_text <- paste("Total egg production, top and bottom ",nrow(dat_plot)/2," countries (",dat1$Year[1],")", sep = "")
if (rulang) caption_text <- paste("Общий объем производства яиц, ",nrow(dat_plot)/2," стран с самыми высокими и самими низкими показателями (",dat1$Year[1]," г.)")


## ---- P3livestockBOTTOM ----
dat1 <- subset(temp, subset=Part %in% "P3livestock")
dat1 <- subset(dat1, subset=Position %in% "BOTTOM")
dat1 <- subset(dat1, select = c(AreaName,Year,Indicator,Value,ItemName))
minYr <- min(dat1$Year)
maxYr <- max(dat1$Year)

dat_plot <- dat1
dat_plot <- dat1 %>% 
  group_by(Year) %>%
  dplyr::mutate(sum = sum(Value,na.rm=TRUE)) %>%
  dplyr::mutate(share = round(Value/sum*100,1)) %>%
  dplyr::mutate(pos = cumsum(share)- share/2) 

library(ggrepel)
p <- ggplot(dat_plot, aes(x=sum/2, y = Value, fill = AreaName, width = sum, label=Value, ymax=1))
p <- p + geom_bar(position="fill", stat="identity")
p <- p + geom_label(aes(x=sum,y=share),
                    label.padding = unit(0.10, "lines"),
                    position="fill",
                    color="white",
                    stat="identity",alpha=.8,
                    size=3,family="PT Sans",fontface="bold",show.legend=FALSE)
p <- p + facet_wrap(~Year)
p <- p + coord_polar("y")
p <- p + theme_minimal()
p <- p + theme(legend.position = "top")
p <- p + theme(axis.text = element_blank())
p <- p + theme(axis.title = element_blank())
p <- p + theme(axis.ticks = element_blank())
p <- p + theme(panel.grid.minor = element_blank())
p <- p + theme(panel.grid.major.x = element_blank())
p <- p + theme(panel.grid.major.y = element_blank())
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, length(unique(dat_plot$AreaName)))[["Sub"]])
p <- p + scale_color_manual(values=plot_colors(part = syb_part, length(unique(dat_plot$AreaName)))[["Sub"]])
p <- p + theme(legend.title = element_blank())
if (table_type == "latex"){
  p <- p + theme(text = element_text(size=11, family="PT Sans"))
  p <- p + theme(legend.key.height = unit(6, "mm"))
  p <- p + theme(legend.key.width = unit(4, "mm"))
} else {
  p <- p + theme(text = element_text(size=14, family="PT Sans"))
  p <- p + theme(legend.key.height = unit(9, "mm"))
  p <- p + theme(legend.key.width = unit(6, "mm"))
}
p <- p + labs(x=NULL, y=NULL)
if (rulang) p <- p + labs(x="",y="\n")
p <- p + theme(plot.margin=unit(c(0,0,0,0),"mm"))
if (!rulang) p <- p + guides(fill = guide_legend(nrow = 2))
if (rulang) p <- p + guides(fill = guide_legend(nrow = 3))
p




# Caption
caption_text <- paste0("Production of ",tolower(dat1$ItemName[1])," (regions most produced animal) in ",minYr," and ",maxYr," (million heads)", sep = "")
if (rulang) caption_text <- paste("Производство ",dat1$ItemName[1]," (самое распространенное домашнее животное в регионе) в ",minYr," и ",maxYr," гг. (в млн голов)", sep = "")



## ---- P3livestockMAP ----

dat1 <- subset(temp, subset=Part %in% "P3livestock")
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
map_unit <- "head/ha"
if (rulang) map_unit <- "голов/га"

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
caption_text <- paste("Cattle and buffaloes per ha of agricultural area, heads per ha (",dat1$Year[1],")", sep = "")
if (rulang) caption_text <- paste("Крупный рогатый скот и буйволы на один гектар сельскохозяйственных угодий, голов/га (",dat1$Year[1]," г.)", sep = "")


#   _____  _       _                  _
#  |  ___|(_) ___ | |__    ___  _ __ (_)  ___  ___
#  | |_   | |/ __|| '_ \  / _ \| '__|| | / _ \/ __|
#  |  _|  | |\__ \| | | ||  __/| |   | ||  __/\__ \
#  |_|    |_||___/|_| |_| \___||_|   |_| \___||___/
#

## ---- P3fisheriesTEXT ----
spread_title <- "Fisheries"
if (region_to_report == "RAF") short_text <- "Fish is an important component in people’s diets, providing about 3.1 billion people with almost 20 percent of their average intake of animal protein. Capture fisheries continue to dominate Africa’s output, but aquaculture also accounts for a steady growing percentage of total fish supply since 1990. Fishery sectors are particularly important in developing countries, providing both food and livelihoods."
if (region_to_report == "RAP") short_text <- "Fish is an important component in people’s diets, providing about 3.1 billion people with almost 20 percent of their average intake of animal protein. Capture fisheries continue to dominate world output, but aquaculture accounts for a growing percentage of total fish supply. Fishery sectors are particularly important in developing countries, providing both food and livelihoods."
if (region_to_report == "REU") short_text <- "Fish is an important component in people’s diets in the region. Capture fisheries continue to dominate the output of the region, but aquaculture accounts for a growing percentage of total fish supply."
if (region_to_report == "RNE") short_text <- "Fish is an important component in people’s diets, providing about 3.1 billion people with almost 20 percent of their average intake of animal protein. Capture fisheries continue to dominate world output, but aquaculture accounts for small but a growing percentage of total fish supply in the region. Morocco, Iran and Egypt had the highest capture production in 2013, whereas Egypt, Iran and Iraq had the highest aquaculture production at the same year."
if (region_to_report == "GLO") short_text <- "Fish is an important component in people’s diets, providing about 3.1 billion people with almost 20 percent of their average intake of animal protein. Capture fisheries continue to dominate world output, but aquaculture accounts for a growing percentage of total fish supply. Fishery sectors are particularly important in developing countries, providing both food and livelihoods."
if (rulang) spread_title <- "Рыбное хозяйство"
if (region_to_report == "REU" & rulang) short_text <- "Рыба является важным компонентом в рационе питания населения региона. Рыболовство продолжает доминировать в рыбном хозяйстве в регионе, однако доля продукция аквакультуры в общем объеме производства рыбы растет."

## ---- P3fisheriesData ----

dat1 <- subset(temp, subset=Part %in% "P3fisheries")
dat1 <- subset(dat1, subset=Position %in% "TOPRIGHT")
dat1 <- subset(dat1, select = c(AreaName,Year,Indicator,Value))
dat1$Year <- as.integer(dat1$Year)
minYr <- min(dat1$Year)
maxYr <- max(dat1$Year)

## ---- P3fisheriesTOPRIGHT ----
if (rulang){
  dat1$fill[dat1$Indicator == "capture_fish_production"] <- "Рыболовство"
  dat1$fill[dat1$Indicator == "aquaculture_fish_production"] <- "Аквакультура"
} else {
  dat1$fill[dat1$Indicator == "capture_fish_production"] <- "From capture fishing"
  dat1$fill[dat1$Indicator == "aquaculture_fish_production"] <- "From aquaculture"
}

dat1 <- na.omit(dat1)
p <- ggplot(dat1, aes(x=Year, y=Value, color=fill))
p <- p + geom_line(size=1.1, alpha=.7)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + labs(x="",y="million tonnes\n")
if (rulang) p <- p + labs(x="",y="млн тонн\n")
p <- p + guides(color = guide_legend(nrow = 2))
p <- p + theme(axis.text.x = element_text(angle=45))
p  <-p +  scale_x_continuous(breaks=c(minYr,1995,2000,2005,2010,maxYr))
p



# Caption
caption_text <- "Fish production from aquaculture and capture fishing"
if (rulang) caption_text <- "Рыбная продукция аквакультуры и рыболовства"


## ---- P3fisheriesLEFT ----
dat1 <- subset(temp, subset=Part %in% "P3fisheries")
dat1 <- subset(dat1, subset=Position %in% "LEFT")
dat1 <- subset(dat1, select = c(AreaName,Year,Indicator,Value))

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
                          yend = Value, color=color), alpha=.5, show.legend = FALSE)
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75) + theme(panel.grid.major.y = element_blank())
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="\nmillion tonnes")
if (rulang) p <- p + labs(x="",y="\n млн тонн")
p <- p + guides(color = guide_legend(nrow = 2))
p


# Caption
caption_text <- paste(nrow(dat_plot)/2," countries with highest and lowest value of capture production (",dat1$Year[1],")", sep = "")
if (rulang) caption_text <- paste(nrow(dat_plot)/2," стран с самыми высокими и самыми низкими значениями производства аквакультуры (",dat1$Year[1]," г.)", sep = "")


## ---- P3fisheriesRIGHT ----
dat1 <- subset(temp, subset=Part %in% "P3fisheries")
dat1 <- subset(dat1, subset=Position %in% "RIGHT")
dat1 <- subset(dat1, select = c(AreaName,Year,Indicator,Value))

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
                          yend = Value, color=color), alpha=.5, show.legend = FALSE)
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75) + theme(panel.grid.major.y = element_blank())
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="\nmillion tonnes")
if (rulang) p <- p + labs(x="",y="\n млн тонн")
p <- p + guides(color = guide_legend(nrow = 2))
p



# Caption

caption_text <- paste(nrow(dat_plot)/2," countries with highest and lowest value of aquaculture production (",dat1$Year[1],")", sep = "")
if (rulang) caption_text <- paste(nrow(dat_plot)/2," стран с самыми высокими и самыми низкими значениями производства аквакультуры (",dat1$Year[1]," г.)", sep = "")


## ---- P3fisheriesBOTTOM ----
dat1 <- subset(temp, subset=Part %in% "P3fisheries")
dat1 <- subset(dat1, subset=Position %in% "BOTTOM")
dat1 <- subset(dat1, select = c(AreaName,Year,Value))
dat1$Year <- as.integer(dat1$Year)

minYr <- min(dat1$Year)
maxYr <- max(dat1$Year)

dat_plot <- dat1[!is.na(dat1$Value),]

p <- ggplot(dat_plot, aes(x=Year,y=Value,color=AreaName))
p <- p + geom_line(size=1.1, alpha=.7)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, length(unique(dat_plot$AreaName)))[["Sub"]])
p <- p + labs(y="index\n", x="")
if (rulang) p <- p + labs(x="",y="индекс\n")
p <- p + guides(color = guide_legend(nrow = 3))
p <- p + scale_x_continuous(breaks=c(minYr,2003,2006,2009,2012,maxYr))
p


# Caption
caption_text <- "Fish production indices (2004-06=100)"
if (rulang) caption_text <- "Индексы производства рыбы (2004-06 гг.=100)"



## ---- P3fisheriesMAP ----
dat1 <- subset(temp, subset=Part %in% "P3fisheries")
dat1 <- subset(dat1, subset=Position %in% "MAP")
dat1 <- subset(dat1, select = c(AreaCode,Value,Year))
dat1$AreaCode <- as.integer(dat1$AreaCode)

map.plot <- left_join(map.df,dat1, by = c("FAOST_CODE" = "AreaCode")) # so that each country in the region will be filled (value/NA)

# Add region key and subset

map.plot <- map.plot[which(map.plot[[region_to_report]]),]

cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","Value")]
cat_data$value_cat <- categories(x=cat_data$Value, n=5, method="jenks",decimals=0)

map.plot <- left_join(map.plot,cat_data[c("FAOST_CODE","value_cat")])


# define map unit
map_unit <- "million US$"
if (rulang) map_unit <- "миллиона долл. США"

p <- create_map_here()
p

# Caption
caption_text <- paste("Net trade of fish in ",dat1$Year[1], sep = "")
if (rulang) caption_text <- paste("Чистый объем торговли рыбой в ",dat1$Year[1]," году", sep = "")


#      _                 _               _  _                       _   _                    _
#     / \    __ _  _ __ (_)  ___  _   _ | || |_  _   _  _ __  __ _ | | | |_  _ __  __ _   __| |  ___
#    / _ \  / _` || '__|| | / __|| | | || || __|| | | || '__|/ _` || | | __|| '__|/ _` | / _` | / _ \
#   / ___ \| (_| || |   | || (__ | |_| || || |_ | |_| || |  | (_| || | | |_ | |  | (_| || (_| ||  __/
#  /_/   \_\\__, ||_|   |_| \___| \__,_||_| \__| \__,_||_|   \__,_||_|  \__||_|   \__,_| \__,_| \___|
#           |___/
#

## ---- P3tradeTEXT ----
spread_title <- "Agricultural trade"
if (region_to_report == "RAF") short_text <- "Most of the food consumed worldwide and especially in Africa is grown locally. Where there is not enough local production to meet demand, trade has been instrumental in filling the gap. The scale of food and agricultural trade today is unprecedented. In real terms, the value of food imports and exports in Africa has increased around fivefold over the past 15 years, reflecting global trends in the overall volume of trade. However, this expansion has been unevenly distributed across region-Western Africa is so far the largest exporter of food whereas Central Africa  records low levels of food exports. On the import side, Northern Africa brings in about 4 times the value of the food it exports.  High-income countries have generally outpaced developing regions, although several of the latter have comparative advantages in food and agricultural production."
if (region_to_report == "RAP") short_text <- "Most of the food consumed worldwide is grown locally. Where there is not enough local production to meet demand, trade has been instrumental in filling the gap. The scale of food and agricultural trade today is unprecedented. In real terms, the value of international flows has increased around fivefold over the past 50 years, reflecting global trends in the overall volume of trade. However, this expansion has been unevenly distributed across regions. High-income countries have generally outpaced developing regions, although several of the latter have comparative advantages in food and agricultural production."
if (region_to_report == "REU") short_text <- "Most of the food trade in the region happens between developed countries. The scale of food and agricultural trade today is unprecedented. The region is one of the important cereal exporters, and EU Other and EFTA leads in that area, followed by CIS Europe and EU Central Eastern."
if (region_to_report == "RNE") short_text <- "Most of the food consumed worldwide is grown locally. Where there is not enough local production to meet demand, trade has been instrumental in filling the gap. The scale of food and agricultural trade today is unprecedented. In real terms, the value of international flows has increased around fivefold over the past 50 years, reflecting global trends in the overall volume of trade. However, this expansion has been unevenly distributed across regions. Within the region, in general, imports are 3 to 5 times the value of its exports."
if (region_to_report == "GLO") short_text <- "Most of the food consumed worldwide is grown locally. Where there is not enough local production to meet demand, trade has been instrumental in filling the gap. The scale of food and agricultural trade today is unprecedented. In real terms, the value of international flows has increased around fivefold over the past 50 years, reflecting global trends in the overall volume of trade. However, this expansion has been unevenly distributed across regions. High-income countries have generally outpaced developing regions, although several of the latter have comparative advantages in food and agricultural production."
if (rulang) spread_title <- "Торговля сельскохозяйственной продукцией"
if (region_to_report == "REU" & rulang) short_text <- "Большая часть торговли продовольствием в регионе происходит между развитыми странами. Масштабы торговли продовольственной и сельскохозяйственной продукцией на сегодняшний день являются беспрецедентными. Регион является одним из важных экспортеров зерновых, при этом субрегион «Другие страны ЕС и ЕАСТ» лидирует в этой сфере, далее следуют субрегионы «СНГ Европа» и «Центральная и Восточная часть ЕС»."

## ---- P3tradeData ----
dat1 <- subset(temp, subset=Part %in% "P3trade")
dat1 <- subset(dat1, subset=Position %in% "TOPRIGHT")
dat1 <- subset(dat1, select = c(AreaName,Year,Indicator,Value))

## ---- P3tradeTOPRIGHT ----

dw1 <- dat1 %>%
  select(AreaName,Indicator,Value) %>%
  spread(key = Indicator,value = Value)

names(dw1)[names(dw1) == 'TP.EXVAL.FOOD.USD.NO'] <- 'Export'
names(dw1)[names(dw1) == 'TP.IMVAL.FOOD.USD.NO'] <- 'Import'
dw1 <- arrange(dw1,-Import)
names(dw1) <- c("","Export", "Import")

dw1[[2]] <- round(dw1[[2]],0)
dw1[[3]] <- round(dw1[[3]],0)
dw1[[2]]<- prettyNum(dw1[[2]], big.mark=" ")
dw1[[3]]<- prettyNum(dw1[[3]], big.mark=" ")

tbl_data <- dw1
if (table_type == "latex") cap <- paste("\\large{Export and Import values of food, billion US\\$ (",dat1$Year[1],")}", sep = "")
if (table_type == "html")  cap <- paste("<b>Table: Export and Import values of food, billion US$ (",dat1$Year[1],") </b>", sep = "")
caption_text <- cap

if (rulang){
  caption_text <- paste("\\large{Экспорт и импорт продовольствия, в миллиард долл. США (в постоянных ценах ",dat1$Year[1]," г.)}", sep = "")
  # names(tbl_data) <- c("","Стоимость экспорта", "Стоимость импорта")
  names(tbl_data) <- c("","экспорт", "импорт")
} 

tbl_data[[1]] <- ifelse(grepl("Gulf Cooperation",tbl_data[[1]]), "GCCSY*", tbl_data[[1]])


print.xtable(xtable(tbl_data, caption = caption_text, digits = c(0,0,0,0),
                    # align= "l{\raggedright\arraybackslash}rrr"),
                    align= "lrrr"),
             type = table_type, table.placement = NULL, booktabs = TRUE, include.rownames = FALSE,
             comment = FALSE,
             size = "footnotesize", caption.placement = "top",
             html.table.attributes = 'class="table table-striped table-hover"')

if (table_type == "latex" & region_to_report == "RNE"){
  cat("\\footnotesize{\\textit{* Gulf Cooperation Council States and Yemen}}")
  cat("\\vspace{1mm}")
} 


## ---- P3tradeLEFT ----
# data
dat1 <- subset(temp, subset=Part %in% "P3trade")
dat1 <- subset(dat1, subset=Position %in% "LEFT")
dat1 <- subset(dat1, select = c(AreaName,Year,Indicator,Value))

minYr <- min(dat1$Year)
maxYr <- max(dat1$Year)
dat1$Yr <- as.integer((dat1$Year))

# Plot only as many countries as there are for particular region, max 20
nro_latest_cases <- nrow(dat1[dat1$Year == max(dat1$Year),])
if (nro_latest_cases < 20) {ncases <- nro_latest_cases} else ncases <- 20
dat1 <- arrange(dat1, -Yr, -Value)
# slice the data for both years
top2015 <- dat1 %>% slice(1:ncases) %>% dplyr::mutate(color = maxYr)
top2000 <- dat1 %>% dplyr::filter(AreaName %in% top2015$AreaName, Year == minYr) %>% dplyr::mutate(color = minYr)
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
p <- p + labs(x="",y="\nbillion US$")
if (rulang) p <- p + labs(x="",y="\nмлрд долл. США")
p <- p + guides(color = guide_legend(nrow = 1))
p




# Caption
caption_text <- paste("Top ",ncases," food importing countries in ",maxYr, sep = "")
if (rulang) caption_text <- paste(ncases," стран с самыми высокими показателями импорта продовольствия в ",maxYr," году", sep = "")



## ---- P3tradeRIGHT ----

# data
dat1 <- subset(temp, subset=Part %in% "P3trade")
dat1 <- subset(dat1, subset=Position %in% "RIGHT")
dat1 <- subset(dat1, select = c(AreaName,Year,Indicator,Value))

minYr <- min(dat1$Year)
maxYr <- max(dat1$Year)
dat1$Yr <- as.integer((dat1$Year))

# Plot only as many countries as there are for particular region, max 20
nro_latest_cases <- nrow(dat1[dat1$Year == max(dat1$Year),])
if (nro_latest_cases < 20) {ncases <- nro_latest_cases} else ncases <- 20
dat1 <- arrange(dat1, -Yr, -Value)
# slice the data for both years
top2015 <- dat1 %>% slice(1:ncases) %>% dplyr::mutate(color = maxYr)
top2000 <- dat1 %>% dplyr::filter(AreaName %in% top2015$AreaName, Year == minYr) %>% dplyr::mutate(color = minYr)
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
p <- p + labs(x="",y="\nbillion US$")
if (rulang) p <- p + labs(x="",y="\nмлрд долл. США")
p <- p + guides(color = guide_legend(nrow = 1))
p


# Caption
caption_text <- paste("Top ",ncases," food exporting countries in ",maxYr, sep = "")
if (rulang) caption_text <- paste(ncases," стран с самыми высокими показателями экспорта продовольствия в ",maxYr," году", sep = "")


## ---- P3tradeBOTTOM ----
dat1 <- subset(temp, subset=Part %in% "P3trade")
dat1 <- subset(dat1, subset=Position %in% "BOTTOM")
dat1 <- subset(dat1, select = c(AreaName,Year,Value))
dat1$Year <- as.integer(dat1$Year)

minYr <- min(dat1$Year)
maxYr <- max(dat1$Year)

dat_plot <- dat1[!is.na(dat1$Value),]

p <- ggplot(dat_plot, aes(x=Year,y=Value,color=AreaName))
p <- p + geom_line(size=1.1, alpha=.7)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, length(unique(dat_plot$AreaName)))[["Sub"]])
p <- p + labs(y="billion constant 2005 US$\n", x="")
if (rulang) p <- p + labs(x="",y="млрд долл. США в постоянных ценах 2005 г.\n")
p <- p + guides(color = guide_legend(nrow = 3))
p <- p +  scale_x_continuous(breaks=c(minYr,2002,2004,2006,2008,2010,minYr))
p


# Caption
caption_text <- "Cereal exports"
if (rulang) caption_text <- "Экспорт зерновых"

## ---- P3tradeMAP ----
dat1 <- subset(temp, subset=Part %in% "P3trade")
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
map_unit <- "index"
if (rulang) map_unit <- "индекс"

p <- create_map_here()
p

# Caption
caption_text <- paste("Import value index (2004-2006 = 100, ",dat1$Year[1],")", sep = "")
if (rulang) caption_text <- paste("Индекс стоимости импорта (2004-2006 гг.= 100, ",dat1$Year[1]," г.)", sep = "")
