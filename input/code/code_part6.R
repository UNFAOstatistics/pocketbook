## ---- part6_setup ------

source(paste0(root.dir,'/input/code/plot/plot_color.R'))

syb_part <- 6

## Part 4
colPart6 <- plot_colors(part = syb_part, 8)
col.main1 <- colPart6[["Main"]][1]
## color for the grid
col.main2 <- colPart6[["Main"]][2]

source(paste0(root.dir,"/input/code/plot/theme.R"))

# map functions
source(paste0(root.dir,'/input/code/plot/map_categories.R'))


#
#    ____            __    __                                              _                  _     _
#   / ___|   ___    / _|  / _|   ___    ___     _ __    _ __    ___     __| |  _   _    ___  | |_  (_)   ___    _ __
#  | |      / _ \  | |_  | |_   / _ \  / _ \   | '_ \  | '__|  / _ \   / _` | | | | |  / __| | __| | |  / _ \  | '_ \
#  | |___  | (_) | |  _| |  _| |  __/ |  __/   | |_) | | |    | (_) | | (_| | | |_| | | (__  | |_  | | | (_) | | | | |
#   \____|  \___/  |_|   |_|    \___|  \___|   | .__/  |_|     \___/   \__,_|  \__,_|  \___|  \__| |_|  \___/  |_| |_|
#                                              |_|
#




## ---- P6coffeeprodTEXT ----
spread_title <- "Coffee production"
short_text <- "Over the past 50 years, both production and consumption of coffee have risen considerably. Consumers have reaped some of the benefits through a greater variety of coffee products, improved quality and lower real prices. Now over 70 countries produce coffee, and over 50 percent comes from just three countries. Some coffee producing countries have seen considerable benefits through higher yields and growing volumes of sales. But many, especially smallholders, who produce the majority of the world’s coffee, are also facing growing challenges from climate change and more difficult natural growing conditions."



## ---- P6coffeeprodData ----

# Area harvested, coffee (ha)
dat <- getFAOtoSYB(domainCode = "QC",
                   elementCode = 5312,
                   itemCode = 656)
QC_656_5312 <- dat$aggregates

# Yield, coffee (hg/ha)
dat <- getFAOtoSYB(domainCode = "QC",
                   elementCode = 5419,
                   itemCode = 656)
QC_656_5419 <- dat$aggregates

# Production quantity, coffee (mln tonnes)
dat <- getFAOtoSYB(domainCode = "QC",
                   elementCode = 5510,
                   itemCode = 656)
QC_656_5510 <- dat$aggregates





## ---- P6coffeeprodTOPRIGHT ----
# dat <- filter(syb.df, Year %in%
#                 c(1999:2014)) %>%
#   group_by(FAOST_CODE,SHORT_NAME) %>%
#   select(FAOST_CODE,Year,SI.POV.DDAY,OA.TPBS.POP.PPL.NO) %>%
#   dplyr::mutate(no_of_poor = OA.TPBS.POP.PPL.NO * (SI.POV.DDAY/100))
# 
# dat <- dat[!is.na(dat$no_of_poor),]
# Add region key and subset

# DEFAULT GROUPING
# df <- subgrouping(region_to_report = region_to_report)
# 
# # merge data with the region info
# dat <- merge(dat,df[c("FAOST_CODE","subgroup")],by="FAOST_CODE")
#
# dat_2000 <- dat %>% group_by(subgroup) %>%
#   filter(Year %in% 1999:2001) %>%
#   dplyr::summarise(no_of_poor = sum(no_of_poor, na.rm=TRUE)/1000000) %>%
#   dplyr::mutate(no_of_poor = round(no_of_poor,0)) %>%
#   ungroup()
#
# dat_2010 <- dat %>% group_by(subgroup) %>%
#   filter(Year %in% 2009:2011) %>%
#   dplyr::summarise(no_of_poor = sum(no_of_poor, na.rm=TRUE)/1000000) %>%
#   dplyr::mutate(no_of_poor = round(no_of_poor,0)) %>%
#   ungroup()
#
# dw <- merge(dat_2000,dat_2010,by="subgroup")
#
# names(dw) <- c("","1999-2001","2009-2011")



growth <- data.frame()

gr_dat <- QC_656_5510 %>% filter(Year %in% 2000:2013,FAOST_CODE < 5000)
gr_dat <- gr_dat[!is.na(gr_dat$QC_656_5510),]
for (fs in unique(gr_dat$FAOST_CODE)){
  d <- gr_dat[gr_dat$FAOST_CODE %in% fs,]
  if (sum(d$QC_656_5510) == 0) next
  d <- d[d$QC_656_5510 > 0,]
  grate <- as.numeric((exp(coef(lm(log(d$QC_656_5510) ~ Year, d))[2]) - 1) * 100)
  row <- data.frame(FAOST_CODE = fs,
                    growth_rate = grate)
  growth <- rbind(growth,row)
}
rc <- growth %>% arrange(-growth_rate) %>% slice(1:5) 

rcc <- left_join(rc,region_key[c("FAOST_CODE","SHORT_NAME")])
rcc$SHORT_NAME[rcc$FAOST_CODE == 41] <- "China"
rcc <- rcc[c(3,2)]

# rc <- QC_656_5510 %>%  filter(Year %in% 2000:2013) %>% group_by(FAOST_CODE) %>% 
#   dplyr::mutate(Growth=c(NA,exp(diff(log(QC_656_5510)))-1)) %>%
#   dplyr::summarise(mean_growth = mean(Growth, na.rm = TRUE)*100) %>%  filter(!is.infinite(mean_growth)) %>%
#   arrange(-mean_growth) %>% slice(1:5) 
# rcc <- left_join(rc,region_key[c("FAOST_CODE","SHORT_NAME")])
# rcc <- rcc[c(3,2)]


names(rcc) <- c("","%")



print.xtable(xtable(rcc, caption = "\\large{Fastest growing coffee producers based on quantities (average annual growth rate, 2000 to 2013)}", digits = c(0,0,0),
                    align= "l{\raggedright\arraybackslash}p{2.2cm}r"),
             type = "latex", table.placement = NULL,
             booktabs = TRUE, include.rownames = FALSE, size = "footnotesize", caption.placement = "top")



## ---- P6coffeeprodLEFT ----
dat <- QC_656_5510[!is.na(QC_656_5510$QC_656_5510),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

dat <- dat[which(dat[[region_to_report]]),]

dat <- arrange(dat, -Year, -QC_656_5510) %>% dplyr::mutate(QC_656_5510 = QC_656_5510/1000)
top15 <- dat %>% slice(1:20) %>% dplyr::mutate(color = "2013")
top91 <- dat %>% filter(FAOST_CODE %in% top15$FAOST_CODE, Year == 2000) %>% dplyr::mutate(color = "2000")
dat_plot <- rbind(top15,top91)

dat_plot$SHORT_NAME  <- factor(dat_plot$SHORT_NAME, levels=top15[order(top15$QC_656_5510),]$SHORT_NAME)

#
p <- ggplot(dat_plot, aes(x=SHORT_NAME,y=QC_656_5510))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="mln tonnes")
p <- p + scale_y_continuous(labels = space)
p

# Caption
caption_text <- "Countries with the highest coffee production quantities in 2013"



## ---- P6coffeeprodRIGHT ----

dat <- QC_656_5312[!is.na(QC_656_5312$QC_656_5312),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

dat <- dat[which(dat[[region_to_report]]),]

dat <- arrange(dat, -Year, -QC_656_5312) %>% dplyr::mutate(QC_656_5312 = QC_656_5312 / 1000)
top15 <- dat %>% slice(1:20) %>% dplyr::mutate(color = "2013")
top91 <- dat %>% filter(FAOST_CODE %in% top15$FAOST_CODE, Year == 2000) %>% dplyr::mutate(color = "2000")
dat_plot <- rbind(top15,top91)

dat_plot$SHORT_NAME  <- factor(dat_plot$SHORT_NAME, levels=top15[order(top15$QC_656_5312),]$SHORT_NAME)

#
p <- ggplot(dat_plot, aes(x=SHORT_NAME,y=QC_656_5312))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="1 000 ha")
p <- p + scale_y_continuous(labels = space)
p
# Caption
caption_text <- "Countries with the highest harvested area of coffee in 2013"




## ---- P6coffeeprodBOTTOM ----

top5_codes <- QC_656_5510 %>% filter(Year == 2013, FAOST_CODE < 5000) %>% arrange(-QC_656_5510) %>% slice(1:5)

rc <- QC_656_5510 %>%  filter(Year %in% 2000:2013, FAOST_CODE %in% top5_codes$FAOST_CODE) %>%  dplyr::mutate(value = QC_656_5510/1000)

dat_plot <- left_join(rc,region_key[c("FAOST_CODE","SHORT_NAME")])

p <- ggplot(dat_plot, aes(x=Year, y=value, color=SHORT_NAME))
p <- p + geom_line(size = 1.1, alpha=.7)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 5)[["Sub"]])
p <- p + labs(x="",y="1 000 mln tonnes")
p <- p + guides(color = guide_legend(nrow = 2))
p <- p + scale_y_continuous(labels = space)
p <- p + scale_x_continuous(breaks = c(2000,2005,2010,2013))
p


caption_text <- "Production quantities of top 5 coffee producers 2000 - 2013"


## ---- P6coffeeprodMAP ----
dat <- QC_656_5419 %>% filter(Year %in% 2013, FAOST_CODE < 5000)

map.plot <- full_join(dat,map.df)

# Add region key and subset

# map.plot <- map.plot[which(map.plot[[region_to_report]]),]

cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","QC_656_5419")]
cat_data$value_cat <- categories(x=cat_data$QC_656_5419, n=5, method="jenks")

map.plot <- left_join(map.plot,cat_data[c("FAOST_CODE","value_cat")])

# define map unit
map_unit <- "Hg/Ha"

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
caption_text <- "Coffee yield (2013)"






#    ____            __    __                   _                        _
#   / ___|   ___    / _|  / _|   ___    ___    | |_   _ __    __ _    __| |   ___
#  | |      / _ \  | |_  | |_   / _ \  / _ \   | __| | '__|  / _` |  / _` |  / _ \
#  | |___  | (_) | |  _| |  _| |  __/ |  __/   | |_  | |    | (_| | | (_| | |  __/
#   \____|  \___/  |_|   |_|    \___|  \___|    \__| |_|     \__,_|  \__,_|  \___|
#


## ---- P6coffeetradeTEXT ----
spread_title <- "Coffee trade"
short_text <- "Coffee is not only traded in large quantities, it also comes in a multitude of different qualities, varieties and forms. These include arabica and robusta, roasted or green, as well as instant and solubles. Coffee exports are also a key source of foreign exchange and national income for many developing countries. In 2012, total exports of 7 million tonnes accounted for a value of US\\$24 billion. Just 10 years ago, the value was only US\\$5.1 billion for a total of 5.5 million tonnes."



## ---- P6coffeetradeData ----

# Exports, coffee:
## VALUE
## Green
dat <- getFAOtoSYB(domainCode = "TP",
                   elementCode = 5922,
                   itemCode = 656)
TP_656_5922 <- dat$aggregates

## Roasted
dat <- getFAOtoSYB(domainCode = "TP",
                   elementCode = 5922,
                   itemCode = 657)
TP_657_5922 <- dat$aggregates

## Extracts
dat <- getFAOtoSYB(domainCode = "TP",
                   elementCode = 5922,
                   itemCode = 659)
TP_659_5922 <- dat$aggregates
## QUENTITY
## Green
dat <- getFAOtoSYB(domainCode = "TP",
                   elementCode = 5910,
                   itemCode = 656)
TP_656_5910 <- dat$aggregates

## Roasted
dat <- getFAOtoSYB(domainCode = "TP",
                   elementCode = 5910,
                   itemCode = 657)
TP_657_5910 <- dat$aggregates

## Extracts
dat <- getFAOtoSYB(domainCode = "TP",
                   elementCode = 5910,
                   itemCode = 659)
TP_659_5910 <- dat$aggregates



# Imports, coffee:
## VALUE
## Green
dat <- getFAOtoSYB(domainCode = "TP",
                   elementCode = 5622,
                   itemCode = 656)
TP_656_5622 <- dat$aggregates


## Roasted
dat <- getFAOtoSYB(domainCode = "TP",
                   elementCode = 5622,
                   itemCode = 657)
TP_657_5622 <- dat$aggregates

## Extracts
dat <- getFAOtoSYB(domainCode = "TP",
                   elementCode = 5622,
                   itemCode = 659)
TP_659_5622 <- dat$aggregates

## QUANTITY
## Green
dat <- getFAOtoSYB(domainCode = "TP",
                   elementCode = 5610,
                   itemCode = 656)
TP_656_5610 <- dat$aggregates


## Roasted
dat <- getFAOtoSYB(domainCode = "TP",
                   elementCode = 5610,
                   itemCode = 657)
TP_657_5610 <- dat$aggregates

## Extracts
dat <- getFAOtoSYB(domainCode = "TP",
                   elementCode = 5610,
                   itemCode = 659)
TP_659_5610 <- dat$aggregates





## ---- P6coffeetradeTOPRIGHT ----

#(exp(coef(lm(log(x[(i - n):(i)]) ~ t))[2]) - 1) * 100

growth <- data.frame()

gr_dat <- TP_656_5922 %>% filter(Year %in% 2000:2012,FAOST_CODE < 5000)
gr_dat <- gr_dat[!is.na(gr_dat$TP_656_5922),]
for (fs in unique(gr_dat$FAOST_CODE)){
  d <- gr_dat[gr_dat$FAOST_CODE %in% fs,]
  if (sum(d$TP_656_5922) == 0) next
  d <- d[d$TP_656_5922 > 0,]
  grate <- as.numeric((exp(coef(lm(log(d$TP_656_5922) ~ Year, d))[2]) - 1) * 100)
  row <- data.frame(FAOST_CODE = fs,
                    growth_rate = grate)
  growth <- rbind(growth,row)
}
rc <- growth %>% arrange(-growth_rate) %>% slice(1:5) 

rcc <- left_join(rc,region_key[c("FAOST_CODE","SHORT_NAME")])
rcc <- rcc[c(3,2)]

# rc <- TP_656_5922 %>%  filter(Year %in% 2000:2012, FAOST_CODE < 5000) %>% group_by(FAOST_CODE) %>% 
#   dplyr::mutate(Growth=c(NA,exp(diff(log(TP_656_5922)))-1)) %>%
#   dplyr::summarise(mean_growth = mean(Growth, na.rm = TRUE)*100) %>%  filter(!is.infinite(mean_growth)) %>%
#   arrange(-mean_growth) %>% slice(1:5) 
# rcc <- left_join(rc,region_key[c("FAOST_CODE","SHORT_NAME")])
# rcc$SHORT_NAME[rcc$FAOST_CODE == 128] <- "China, Macao SAR"
# rcc <- rcc[c(3,2)]

names(rcc) <- c("","%")

print.xtable(xtable(rcc, caption = "\\large{Fastest growing coffee exporters based on export value (average annual growth rate, 2000 to 2013)}", digits = c(0,0,0),
                    align= "l{\raggedright\arraybackslash}p{2.2cm}r"),
             type = "latex", table.placement = NULL,
             booktabs = TRUE, include.rownames = FALSE, size = "footnotesize", caption.placement = "top")




## ---- P6coffeetradeLEFT ----

dat <- TP_656_5922[!is.na(TP_656_5922$TP_656_5922),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

dat <- dat[which(dat[[region_to_report]]),]

dat$TP_656_5922 <- dat$TP_656_5922 / 1000 # into millions dollars
dat <- arrange(dat, -Year, -TP_656_5922)
top15 <- dat %>% slice(1:20) %>% dplyr::mutate(color = "2012")
top91 <- dat %>% filter(FAOST_CODE %in% top15$FAOST_CODE, Year == 2000) %>% dplyr::mutate(color = "2000")
dat_plot <- rbind(top15,top91)

dat_plot$SHORT_NAME  <- factor(dat_plot$SHORT_NAME, levels=top15[order(top15$TP_656_5922),]$SHORT_NAME)

#
p <- ggplot(dat_plot, aes(x=SHORT_NAME,y=TP_656_5922))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="million US$")
p <- p + scale_y_continuous(labels = space)
p

# Caption
caption_text <- "Top 20 coffee exporters in 2012"



## ---- P6coffeetradeRIGHT ----

dat <- TP_656_5622[!is.na(TP_656_5622$TP_656_5622),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

dat <- dat[which(dat[[region_to_report]]),]

dat$TP_656_5622 <- dat$TP_656_5622 / 1000 # into millions dollars
dat <- arrange(dat, -Year, -TP_656_5622)
top15 <- dat %>% slice(1:20) %>% dplyr::mutate(color = "2012")
top91 <- dat %>% filter(FAOST_CODE %in% top15$FAOST_CODE, Year == 2000) %>% dplyr::mutate(color = "2000")
dat_plot <- rbind(top15,top91)


dat_plot$SHORT_NAME  <- factor(dat_plot$SHORT_NAME, levels=top15[order(top15$TP_656_5622),]$SHORT_NAME)
#
p <- ggplot(dat_plot, aes(x=SHORT_NAME,y=TP_656_5622))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="million US$")
p <- p + scale_y_continuous(labels = space)
p

# Caption
caption_text <- "Top 20 coffee importers in 2012"





## ---- P6coffeetradeBOTTOM ----


dat <- left_join(TP_656_5622,TP_657_5622)
dat <- left_join(dat,TP_659_5622)

# Add region key and subset
dat <- left_join(dat,region_key)
dat <- dat[which(dat[[region_to_report]]),]

dat <- gather(dat, variable, value, 3:5)
dat$fill[dat$variable == "TP_656_5622"] <- "Coffee, green"
dat$fill[dat$variable == "TP_657_5622"] <- "Coffee, roasted"
dat$fill[dat$variable == "TP_659_5622"] <- "Coffee, extracts"

dat$value <- dat$value / 1000 # reporting million US$

dat <- dat[!is.na(dat$value),]

# DEFAULT GROUPING
df <- subgrouping(region_to_report = region_to_report)

# merge data with the region info
dat_plot <- merge(dat,df[c("FAOST_CODE","subgroup")],by="FAOST_CODE")

# AGREGATE
dat_plot <- dat_plot %>% group_by(Year,fill) %>%
          dplyr::summarise(value  = sum(value, na.rm=TRUE)) %>%  ungroup()

p <- ggplot(dat_plot, aes(x=Year, y=value, color=fill))
p <- p + geom_line(size = 1.1, alpha=.7)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 3)[["Sub"]])
p <- p + labs(x="",y="mln US$")
p <- p + guides(color = guide_legend(nrow = 2))
p <- p + scale_y_continuous(labels = space)
p

# Caption
caption_text <- "Value of coffee imports"




## ---- P6coffeetradeMAP ----

# old per capita
# dat <- TP_656_5622 %>% filter(Year %in% 2012, FAOST_CODE < 5000)
# pop <- syb.df %>% filter(Year %in% 2012, FAOST_CODE < 5000) %>% select(FAOST_CODE,OA.TPBS.POP.PPL.NO)
# 
# dat <- left_join(dat,pop)
# 
# dat <- dat %>% dplyr::mutate(import_per_capita = (TP_656_5622 * 1000) / OA.TPBS.POP.PPL.NO)

# new net trade
dat1 <- TP_656_5622 %>% filter(Year %in% 2012, FAOST_CODE < 5000)
dat2 <- TP_656_5922 %>% filter(Year %in% 2012, FAOST_CODE < 5000)
dat <- left_join(dat1,dat2)
dat$net_trade <- dat$TP_656_5922 - dat$TP_656_5622
dat$net_trade <- dat$net_trade / 1000 # into million USD

map.plot <- full_join(dat,map.df)

# Add region key and subset

# map.plot <- map.plot[which(map.plot[[region_to_report]]),]

cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","net_trade")]
# cat_data$value_cat <- categories(x=cat_data$net_trade, n=5, manual=TRUE, manual_breaks = c(-10000,-2000,-50,50,5000,10000))
cat_data$value_cat <- categories(x=cat_data$net_trade, n=5, manual=FALSE)

map.plot <- left_join(map.plot,cat_data[c("FAOST_CODE","value_cat")])

# define map unit
map_unit <- "million US$"

# graticule
grat_robin <- spTransform(graticule, CRS("+proj=robin"))  # reproject graticule
gr_rob <- fortify(grat_robin)
# crop the grid
if (!(region_to_report %in% c("GLO","COF"))) {
  gr_rob <- gr_rob[gr_rob$lat >= min(map.plot$lat) & gr_rob$lat <= max(map.plot$lat),]
  gr_rob <- gr_rob[gr_rob$long >= min(map.plot$long) & gr_rob$long <= max(map.plot$long),]
} else gr_rob <- gr_rob

# create_map_here(manualPalette=TRUE,manual_palette=c("grey70","#d7191c","#fdae61","#ffffbf","#a6d96a","#1a9641"))
create_map_here()

# Caption
caption_text <- "Net trade of coffee in 2012"







#    ____         __   __                            _
#   / ___| ___   / _| / _|  ___   ___   _ __   _ __ (_)  ___  ___  ___
#  | |    / _ \ | |_ | |_  / _ \ / _ \ | '_ \ | '__|| | / __|/ _ \/ __|
#  | |___| (_) ||  _||  _||  __/|  __/ | |_) || |   | || (__|  __/\__ \
#   \____|\___/ |_|  |_|   \___| \___| | .__/ |_|   |_| \___|\___||___/
#                                      |_|





## ---- P6coffeepricesTEXT ----
spread_title <- "Coffee prices"
short_text <- "Approximately 60 percent of the world’s coffee production is arabica, while the other 40 percent is robusta. The former is generally considered of superior quality and fetches a higher price. The latter tends to be hardier and has a bitter taste. In developing countries, including China and Indonesia, robusta is now the bean of choice. Up until 2013, prices of arabica were declining, primarily due to the growing availability of the cheaper robusta beans on world markets. Between 2013 and 2014, however, prices of both types rose, arabica from US\\$2.90 to US\\$4.18 and robusta from US\\$1.96 to US\\$2.09."



## ---- P6coffeepricesData ----

# Producer prices, green coffee (US$ per tonne)
dat <- getFAOtoSYB(domainCode = "PP",
                   elementCode = 5532,
                   itemCode = 656)
PP_656_5532 <- dat$aggregates

# Producer price index (2004-06=100)
dat <- getFAOtoSYB(domainCode = "PI",
                   elementCode = 5539,
                   itemCode = 656)
PI_656_5539 <- dat$aggregates





## ---- P6coffeepricesTOPRIGHT ----

if (file.exists(paste0(root.dir,"/input/data/tmp/pink_data_a.xlsx"))) {
  dd <- read_excel(paste0(root.dir,"/input/data/tmp/pink_data_a.xlsx"), sheet = 4, skip = 8)
  
} else {
  download.file("http://siteresources.worldbank.org/INTPROSPECTS/Resources/334934-1304428586133/pink_data_a.xlsx", destfile = paste0(root.dir,"/input/data/tmp/pink_data_a.xlsx"))
  dd <- read_excel(paste0(root.dir,"/input/data/tmp/pink_data_a.xlsx"), sheet = 4, skip = 8)
}
names(dd)[1] <- "Year"

ddd <- dd %>%  filter(Year > 1960) %>% select(Year,KCOFFEE_ARABIC,KCOFFEE_ROBUS)
dat_plot <- gather(ddd, type, value, 2:3)
dat_plot$variety[dat_plot$type == "KCOFFEE_ARABIC"] <- "Coffee, arabica"
dat_plot$variety[dat_plot$type == "KCOFFEE_ROBUS"] <- "Coffee, robusta"

p <- ggplot(dat_plot, aes(x=Year, y=value, color=variety))
p <- p + geom_line(size = 1.1, alpha=.7)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + labs(x="",y="$/kg")
p <- p + guides(color = guide_legend(nrow = 1))
p <- p + coord_cartesian(ylim=c(0,12))
p


# Caption
caption_text <- "Annual coffee prices, 1960 to 2014, real 2010 US\\$"



## ---- P6coffeepricesLEFT ----

dat <- PP_656_5532[!is.na(PP_656_5532$PP_656_5532),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

dat <- dat[which(dat[[region_to_report]]),]

dat <- dat %>%  filter(Year == 2000) %>%  arrange( -PP_656_5532)
dat_plot <- dat %>% slice(1:20) %>% dplyr::mutate(color = "2000")

#
p <- ggplot(dat_plot, aes(x=reorder(SHORT_NAME, PP_656_5532) ,y=PP_656_5532))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 1)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="US$ per tonne")
p <- p + scale_y_continuous(labels=space)
p <- p + theme(legend.position="none")
p

# Caption
caption_text <- "Producer prices, green coffee (US\\$ per tonne) in 2000, top 20 countries"



## ---- P6coffeepricesRIGHT ----

dat <- PP_656_5532[!is.na(PP_656_5532$PP_656_5532),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

dat <- dat[which(dat[[region_to_report]]),]

dat <- dat %>%  filter(Year == 2013) %>%  arrange( -PP_656_5532)
dat_plot <- dat %>% slice(1:20) %>% dplyr::mutate(color = "2013")

#
p <- ggplot(dat_plot, aes(x=reorder(SHORT_NAME, PP_656_5532) ,y=PP_656_5532))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 1)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="US$ per tonne")
p <- p + scale_y_continuous(labels=space)
p <- p + theme(legend.position="none")
p

# Caption
caption_text <- "Producer prices, green coffee (US\\$ per tonne) in 2013, top 20 countries"




## ---- P6coffeepricesBOTTOM ----

top5 <- QC_656_5510 %>% filter(Year == 2013, FAOST_CODE < 5000 ) %>%  arrange(-QC_656_5510) %>%  slice(1:5)

dat_plot <- PP_656_5532 %>% filter(Year > 2000, FAOST_CODE %in% top5$FAOST_CODE) 

dat_plot <- left_join(dat_plot,region_key[c("FAOST_CODE","SHORT_NAME")])

p <- ggplot(dat_plot, aes(x=Year, y=PP_656_5532, color=SHORT_NAME))
p <- p + geom_line(size = 1.1, alpha=.7)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 5)[["Sub"]])
p <- p + labs(x="",y="US$ per tonne")
p <- p + guides(color = guide_legend(nrow = 2))
p <- p + scale_y_continuous(labels = space)
p <- p + scale_x_continuous(breaks=c(2001,2005,2010,2013))
p

# Caption
caption_text <- "Producer prices, green coffee, top 5 coffee producers"




## ---- P6coffeepricesMAP ----

dat <- PI_656_5539 %>% filter(Year %in% 2012, FAOST_CODE < 5000)

map.plot <- full_join(dat,map.df)

# Add region key and subset

# map.plot <- map.plot[which(map.plot[[region_to_report]]),]

cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","PI_656_5539")]
cat_data$value_cat <- categories(x=cat_data$PI_656_5539, n=5, method="jenks")

map.plot <- left_join(map.plot,cat_data[c("FAOST_CODE","value_cat")])

# define map unit
map_unit <- "index"

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
caption_text <- "Producer Price Index (2004-2006 = 100) (2012)"

