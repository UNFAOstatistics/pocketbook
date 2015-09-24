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
short_text <- "Undernourishment is a state, lasting for at least one year, of inability to acquire enough food, defined as a level of food intake insufficient to meet dietary energy requirements. About 795 million people – just over one in every nine people – in the world still lack sufficient food for conducting an active and healthy life. Yet progress has been made, even in the presence of significant population growth. Two hundred and sixteen million million fewer people suffer from undernourishment than 25 years ago and 167 million fewer than a decade ago."


## ---- P2undernuData ----
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



## ---- P2undernuTOPRIGHT ----

# This should be thought twice how to produce it for regional books!

tbl <- df[df$FAOST_CODE %in% c(5000,5852,5851,5100,5300,5205,5500),] # World
tbl <- tbl[tbl$Year %in% c(1991,2015),]
library(tidyr)
tbl$Year <- paste0("X",tbl$Year)
tbl <- tbl[c("Year","FAO_TABLE_NAME","FS.OA.POU.PCT3D1")]
dw <- spread(tbl,
             Year,
             FS.OA.POU.PCT3D1)
dw$FAO_TABLE_NAME[dw$FAO_TABLE_NAME == "Latin America and the Caribbean"] <- "Latin America and \n the Caribbean"
dw$X2015[dw$X2015 == "20"] <- "20.0"
names(dw) <- c("","1990-92","2014-16")

#dw <- dw[c(7,3,4,1,2,5,6),]
# Chiaras comments
print.xtable(xtable(dw, caption = " Prevalence of undernourishment (percent)", digits = c(0,0,0,0),
                    align= "l{\raggedright\arraybackslash}p{1.7cm}rr"),
             type = "latex", table.placement = NULL,
             booktabs = TRUE, include.rownames = FALSE, size = "footnotesize", caption.placement = "top")



## ---- P2undernuLEFT ----
# data

dat <- df[df$Year %in%  c(1991,2015) & df$FAOST_CODE < 5000,c("FAOST_CODE","Year","FAO_TABLE_NAME","FS.OA.NOU.P3D1")]

dat <- dat[!is.na(dat$FS.OA.NOU.P3D1),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

#dat <- dat[which(dat[[region_to_report]]),]

dat <- arrange(dat, -Year, -FS.OA.NOU.P3D1)
top15 <- dat %>% slice(1:20) %>% dplyr::mutate(color = "2014-2016")
top91 <- dat %>% filter(FAOST_CODE %in% top15$FAOST_CODE, Year == 1991) %>% dplyr::mutate(color = "1990-1992")
dat_plot <- rbind(top15,top91)

p <- ggplot(dat_plot, aes(x=reorder(SHORT_NAME, FS.OA.NOU.P3D1),y=FS.OA.NOU.P3D1))
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

# limit the nro of printed for REU/RNE countries
if (region_to_report %in% c("REU","RNE")){
  max_nro_countries <- 8
} else max_nro_countries <- 20 


top15 <- dat %>% slice(1:max_nro_countries) %>% dplyr::mutate(color = "2014-2016")
top91 <- dat %>% filter(FAOST_CODE %in% top15$FAOST_CODE, Year == 1991) %>% dplyr::mutate(color = "1990-1992")
dat_plot <- rbind(top15,top91)

p <- ggplot(dat_plot, aes(x=reorder(SHORT_NAME, FS.OA.NOU.P3D1),y=FS.OA.NOU.P3D1))
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
p <- p + geom_point() + geom_line()
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

dat <- dat[dat$FAOST_CODE != 41,]
dat$FAOST_CODE[dat$FAOST_CODE == 351] <- 41

#dat <- dat[!is.na(dat$FS.OA.POU.PCT3D1),]

map.plot <- left_join(map.df,dat)

# Add region key and subset
map.plot <- map.plot[which(map.plot[[region_to_report]]),]

cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","FS.OA.POU.PCT3D1")]
cat_data$value_cat <- categories(x=cat_data$FS.OA.POU.PCT3D1, n=5, manual = TRUE, manual_breaks = c(0, 5, 15, 25, 35, 100), method="sd") # manualBreaks = c(0, 5, 15, 25, 35, 100),

map.plot <- left_join(map.plot,cat_data[c("FAOST_CODE","value_cat")])

# define map unit
map_unit <- "Percent"


create_map_here()

# Caption
caption_text <- "Prevalence of undernourishment (percent, 2014-16)"


#   _____                       _                             _   _           _       _   _   _   _
#  |  ___|   ___     ___     __| |     __ _  __   __   __ _  (_) | |   __ _  | |__   (_) | | (_) | |_   _   _
#  |  _|   | (_) | | (_) | | (_| |   | (_| |  \ V /  | (_| | | | | | | (_| | | |_) | | | | | | | | |_  | |_| |
#  | |_     / _ \   / _ \   / _` |    / _` | \ \ / /  / _` | | | | |  / _` | | '_ \  | | | | | | | __| | | | |
#  |_|      \___/   \___/   \__,_|    \__,_|   \_/    \__,_| |_| |_|  \__,_| |_.__/  |_| |_| |_|  \__|  \__, |
#                                                                                                       |___/

## ---- P2availabTEXT ----
spread_title <- "Food availability"
short_text <- "Availability is an important dimension of food security. Supplying enough food to the reference population is a necessary, but insufficient, condition for ensuring adequate access for individuals. Over recent decades, trends in food production per capita have been generally positive across most regions. However, growth rates in Africa have been lower for the last 20 years, despite notable exceptions. In most countries and regions, high food availability is associated with relatively low prevalence of undernourishment. However, outcome indicators show that high food availability does not always guarantee high food security."



## ---- P2availabData ----



## ---- P2availabTOPRIGHT ----
dat_plot <- df %>% filter(FAOST_CODE %in% c(5100,5300,5500,5205,5000)) %>%  select(FAOST_CODE,Year,FAO_TABLE_NAME,FS.DA.ADESA.PCT3D)

dat_plot$FAO_TABLE_NAME[dat_plot$FAO_TABLE_NAME == "Latin America and the Caribbean"] <- "Latin Am. and the Carib."

p <- ggplot(data = dat_plot, aes(x = Year, y = FS.DA.ADESA.PCT3D,group=FAO_TABLE_NAME,color=FAO_TABLE_NAME))
p <- p + geom_line()
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

dat <- arrange(dat, -Year, -FBS.PCSS.CSR.PCT3D)

# limit the nro of printed for REU/RNE countries
if (region_to_report %in% c("REU","RNE")){
  max_nro_countries <- 8
} else max_nro_countries <- 20 


top15 <- dat %>% slice(1:max_nro_countries) %>% dplyr::mutate(color = "2009-2011")
top91 <- dat %>% filter(FAOST_CODE %in% top15$FAOST_CODE, Year == 2000) %>% dplyr::mutate(color = "1999-2001")
dat_plot <- rbind(top15,top91)

dat_plot$SHORT_NAME <- factor(dat_plot$SHORT_NAME, levels=arrange(top15, FBS.PCSS.CSR.PCT3D)$SHORT_NAME)

p <- ggplot(dat_plot, aes(x=SHORT_NAME,y=FBS.PCSS.CSR.PCT3D))
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


## ---- P2availabRIGHT, eval=P2availab ,right_plot=P2availab, fig.height=right_plot_height, fig.width=right_plot_width ----


dat <- df[df$Year %in%  c(2010) & df$FAOST_CODE < 5000,c("FAOST_CODE","Year","FAO_TABLE_NAME","FBS.PPCS.GT.GCD3D")]

dat <- dat[!is.na(dat$FBS.PPCS.GT.GCD3D),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

dat <- dat[which(dat[[region_to_report]]),]

dat <- arrange(dat, -Year, -FBS.PPCS.GT.GCD3D)

# limit the nro of printed for REU/RNE countries
if (region_to_report %in% c("REU","RNE")){
  max_nro_countries <- 8
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
p <- p + guides(color = guide_legend(nrow = 2))
p

# Caption
caption_text <- "Average protein supply, top 20 countries in 2009-2011"
if (region_to_report == "RAF") caption_text <- "Average protein supply, top 20 African countries in 2009-2011"
if (region_to_report == "RAP") caption_text <- "Average protein supply, top 20 Asian countries in 2009-2011"
if (region_to_report == "REU") caption_text <- "Average protein supply, top 6 European countries in 2009-2011"
if (region_to_report == "RNE") caption_text <- "Average protein supply, top 6 North African countries in 2009-2011"
if (region_to_report == "GLO") caption_text <- "Average protein supply, top 20 countries in 2009-2011"



## ---- P2availabBOTTOM, eval=P2availab, bottom_plot=P2availab, fig.height=bottom_plot_height, fig.width=bottom_plot_width ----

dat_plot <- df %>% filter(FAOST_CODE %in% c(5100,5300,5500,5205,5000)) %>%  select(FAOST_CODE,Year,FAO_TABLE_NAME,FBS.PPCS.AO.GCD3D)

dat_plot$FAO_TABLE_NAME[dat_plot$FAO_TABLE_NAME == "Latin America and the Caribbean"] <- "Latin Am. and the Carib."

p <- ggplot(data = dat_plot, aes(x = Year, y = FBS.PPCS.AO.GCD3D,group=FAO_TABLE_NAME,color=FAO_TABLE_NAME))
p <- p + geom_line()
p <- p + scale_color_manual(values = plot_colors(part = 1, length(unique(dat_plot$FAO_TABLE_NAME)))[["Sub"]])
p <- p + labs(y="percent", x="")
p <- p + guides(color = guide_legend(nrow = 3))
p <- p + scale_x_continuous(breaks = c(1991, 2001, 2006, 2010),
                            labels = c("1990-92", "2000-02", "2005-07", "2009-11"))
p <- p + theme(axis.text.x = element_text(angle = 45))
p

# Caption
caption_text <- "Average supply of protein of animal origin"


## ---- P2availabMAP, eval=P2availab, map_plot=P2availab, fig.width=map.fig.width, fig.height= map.fig.height ,out.width=map.out.width, out.height=map.out.height, out.extra=map.out.extra ----

dat <- df[df$Year %in%  2012 & df$FAOST_CODE < 5000,c("Year","FAOST_CODE","QV.PCNPV.FOOD.ID3D")]

dat <- dat[dat$FAOST_CODE != 41,]
dat$FAOST_CODE[dat$FAOST_CODE == 351] <- 41

map.plot <- left_join(map.df,dat)

# Add region key and subset
map.plot <- map.plot[which(map.plot[[region_to_report]]),]

cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","QV.PCNPV.FOOD.ID3D")]
cat_data$value_cat <- categories(x=cat_data$QV.PCNPV.FOOD.ID3D, n=5) # manualBreaks = c(0, 5, 15, 25, 35, 100),

map.plot <- left_join(map.plot,cat_data[c("FAOST_CODE","value_cat")])

# define map unit
map_unit <- "Percent"


create_map_here()

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
short_text <- "An adequate supply of food does not in itself guarantee household level food security. Access to food is primarily determined by incomes, food prices and the ability of households and individuals to obtain access to social support. Individuals’ access to food is also heavily influenced by social variables, including gender positioning and power hierarchies within households. In addition to economic affordability, physical access to food is also facilitated by adequate infrastructure, such as railway lines and paved roads."



## ---- P2accessData ----



## ---- P2accessTOPRIGHT ----
dat_plot <- df %>% filter(FAOST_CODE %in% c(5100,5300,5500,5205,5000)) %>%  select(FAOST_CODE,Year,FAO_TABLE_NAME,FS.OA.DOFD.KCD3D)

dat_plot$FAO_TABLE_NAME[dat_plot$FAO_TABLE_NAME == "Latin America and the Caribbean"] <- "Latin Am. and the Carib."

p <- ggplot(data = dat_plot, aes(x = Year, y = FS.OA.DOFD.KCD3D,group=FAO_TABLE_NAME,color=FAO_TABLE_NAME))
p <- p + geom_line()
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

dat <- arrange(dat, -Year, -FS.DEA.DFPLI.IND)

# limit the nro of printed for REU/RNE countries
if (region_to_report %in% c("REU","RNE")){
  max_nro_countries <- 8
} else max_nro_countries <- 20 


top15 <- dat %>% slice(1:max_nro_countries) %>% dplyr::mutate(color = "2014")
top91 <- dat %>% filter(FAOST_CODE %in% top15$FAOST_CODE, Year == 2000) %>% dplyr::mutate(color = "2000")
dat_plot <- rbind(top15,top91)

dat_plot$SHORT_NAME <- factor(dat_plot$SHORT_NAME, levels=arrange(top15, FS.DEA.DFPLI.IND)$SHORT_NAME)

p <- ggplot(dat_plot, aes(x=SHORT_NAME,y=FS.DEA.DFPLI.IND))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="percent")
p <- p + guides(color = guide_legend(nrow = 2))
p

# Caption
caption_text <- "Domestic food price level index (index), top 20 countries in 2014 (2000 to 2014)"
if (region_to_report == "RAF") caption_text <- "Domestic food price level index (index), top 20 countries in 2014 (2000 to 2014)"
if (region_to_report == "RAP") caption_text <- "Domestic food price level index (index), top 20 countries in 2014 (2000 to 2014)"
if (region_to_report == "REU") caption_text <- "Domestic food price level index (index), top 20 countries in 2014 (2000 to 2014)"
if (region_to_report == "RNE") caption_text <- "Domestic food price level index (index), top 20 countries in 2014 (2000 to 2014)"
if (region_to_report == "GLO") caption_text <- "Domestic food price level index (index), top 20 countries in 2014 (2000 to 2014)"



## ---- P2accessRIGHT ----

dat <- df[df$Year %in%  c(2000,2015) & df$FAOST_CODE < 5000,c("FAOST_CODE","Year","FAO_TABLE_NAME","FS.OA.POU.PCT3D1")]

dat <- dat[!is.na(dat$FS.OA.POU.PCT3D1),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

dat <- dat[which(dat[[region_to_report]]),]

dat <- arrange(dat, -Year, -FS.OA.POU.PCT3D1)

# limit the nro of printed for REU/RNE countries
if (region_to_report %in% c("REU","RNE")){
  max_nro_countries <- 8
} else max_nro_countries <- 20 


top15 <- dat %>% slice(1:max_nro_countries) %>% dplyr::mutate(color = "2014-16")
top91 <- dat %>% filter(FAOST_CODE %in% top15$FAOST_CODE, Year == 2000) %>% dplyr::mutate(color = "1999-2001")
dat_plot <- rbind(top15,top91)

dat_plot$SHORT_NAME <- factor(dat_plot$SHORT_NAME, levels=arrange(top15, FS.OA.POU.PCT3D1)$SHORT_NAME)

p <- ggplot(dat_plot, aes(x=SHORT_NAME,y=FS.OA.POU.PCT3D1))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="percent")
p <- p + guides(color = guide_legend(nrow = 2))
p

# Caption
caption_text <- "Prevalence of undernourishment, highest 20 countries in 2014-16 (3 year averages)"
if (region_to_report == "RAF") caption_text <- "Domestic food price level index (index), top 20 countries in 2014 (2000 to 2014)"
if (region_to_report == "RAP") caption_text <- "Domestic food price level index (index), top 20 countries in 2014 (2000 to 2014)"
if (region_to_report == "REU") caption_text <- "Domestic food price level index (index), top 20 countries in 2014 (2000 to 2014)"
if (region_to_report == "RNE") caption_text <- "Domestic food price level index (index), top 20 countries in 2014 (2000 to 2014)"
if (region_to_report == "GLO") caption_text <- "Domestic food price level index (index), top 20 countries in 2014 (2000 to 2014)"


## ---- P2accessBOTTOM ----

dat_plot <- df %>% filter(FAOST_CODE %in% c(5100,5300,5500,5205,5000)) %>%  select(FAOST_CODE,Year,FAO_TABLE_NAME,NY.GDP.PCAP.PP.KD)

dat_plot$FAO_TABLE_NAME[dat_plot$FAO_TABLE_NAME == "Latin America and the Caribbean"] <- "Latin Am. and the Carib."

p <- ggplot(data = dat_plot, aes(x = Year, y = NY.GDP.PCAP.PP.KD,group=FAO_TABLE_NAME,color=FAO_TABLE_NAME))
p <- p + geom_line()
p <- p + scale_color_manual(values = plot_colors(part = 1, length(unique(dat_plot$FAO_TABLE_NAME)))[["Sub"]])
p <- p + labs(y="percent", x="")
p <- p + guides(color = guide_legend(nrow = 3))
p

# Caption
caption_text <- "GDP per capita, PPP, constant 2011 international \\$"



## ---- P2accessMAP ----
dat <- df[df$Year %in%  2007:2011 & df$FAOST_CODE < 5000,c("Year","FAOST_CODE","IS.ROD.DNST.K2D")]

dat <- dat[dat$FAOST_CODE != 41,]
dat$FAOST_CODE[dat$FAOST_CODE == 351] <- 41

dat <- dat[!is.na(dat$IS.ROD.DNST.K2D),]
dat <- dat %>% group_by(FAOST_CODE) %>% filter(Year == max(Year))

map.plot <- left_join(map.df,dat)

# Add region key and subset
map.plot <- map.plot[which(map.plot[[region_to_report]]),]

cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","IS.ROD.DNST.K2D")]
cat_data$value_cat <- categories(x=cat_data$IS.ROD.DNST.K2D, n=5) # manualBreaks = c(0, 5, 15, 25, 35, 100),

map.plot <- left_join(map.plot,cat_data[c("FAOST_CODE","value_cat")])

# define map unit
map_unit <- "per 100 square km of land"


create_map_here()

# Caption
caption_text <- "Road density, per 100 square km of land area (2007 to 2011*)"


#   _____                       _           _             _       _   _   _   _
#  |  ___|   ___     ___     __| |    ___  | |_    __ _  | |__   (_) | | (_) | |_   _   _
#  | |_     / _ \   / _ \   / _` |   / __| | __|  / _` | | '_ \  | | | | | | | __| | | | |
#  |  _|   | (_) | | (_) | | (_| |   \__ \ | |_  | (_| | | |_) | | | | | | | | |_  | |_| |
#  |_|      \___/   \___/   \__,_|   |___/  \__|  \__,_| |_.__/  |_| |_| |_|  \__|  \__, |
#                                                                                   |___/

## ---- P2stabilityTEXT ----
spread_title <- "Economic and political stability"
short_text <- "Over the last ten years, food and agricultural markets have entered an unexpectedly turbulent phase, characterized by large supply shortfalls, price swings. Political and economic uncertainties, coupled with extreme weather conditions, can have direct and adverse impacts on food security. The poorer the household, the stronger the impact of external shocks, as poor households spend a proportionally higher share of their incomes on food."


## ---- P2stabilityData ----


## ---- P2stabilityTOPRIGHT ----
dat_plot <- df %>% filter(FAOST_CODE %in% c(5100,5300,5500,5205,5000)) %>%  select(FAOST_CODE,Year,FAO_TABLE_NAME,FS.DEA.PCFPV.IDD)

dat_plot$FAO_TABLE_NAME[dat_plot$FAO_TABLE_NAME == "Latin America and the Caribbean"] <- "Latin Am. and the Carib."

p <- ggplot(data = dat_plot, aes(x = Year, y = FS.DEA.PCFPV.IDD,group=FAO_TABLE_NAME,color=FAO_TABLE_NAME))
p <- p + geom_line()
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

dat <- arrange(dat, -Year, -FS.DS.PCFSV.KCDD)

# limit the nro of printed for REU/RNE countries
if (region_to_report %in% c("REU","RNE")){
  max_nro_countries <- 8
} else max_nro_countries <- 20 


top15 <- dat %>% slice(1:max_nro_countries) %>% dplyr::mutate(color = "2011")
top91 <- dat %>% filter(FAOST_CODE %in% top15$FAOST_CODE, Year == 2000) %>% dplyr::mutate(color = "2000")
dat_plot <- rbind(top15,top91)

dat_plot$SHORT_NAME <- factor(dat_plot$SHORT_NAME, levels=arrange(top15, FS.DS.PCFSV.KCDD)$SHORT_NAME)

p <- ggplot(dat_plot, aes(x=SHORT_NAME,y=FS.DS.PCFSV.KCDD))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="kcal/capita/day")
p <- p + guides(color = guide_legend(nrow = 2))
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

dat <- arrange(dat, -Year, -FS.DEA.DFPLIV.IND)

# limit the nro of printed for REU/RNE countries
if (region_to_report %in% c("REU","RNE")){
  max_nro_countries <- 8
} else max_nro_countries <- 20 


top15 <- dat %>% slice(1:max_nro_countries) %>% dplyr::mutate(color = "2011")
top91 <- dat %>% filter(FAOST_CODE %in% top15$FAOST_CODE, Year == 2000) %>% dplyr::mutate(color = "2000")
dat_plot <- rbind(top15,top91)

dat_plot$SHORT_NAME <- factor(dat_plot$SHORT_NAME, levels=arrange(top15, FS.DEA.DFPLIV.IND)$SHORT_NAME)

p <- ggplot(dat_plot, aes(x=SHORT_NAME,y=FS.DEA.DFPLIV.IND))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="index")
p <- p + guides(color = guide_legend(nrow = 2))
p

# Caption
caption_text <- "Domestic food price volatility index, top 20 countriesin 2014"
if (region_to_report == "RAF") caption_text <- "Domestic food price volatility index, top 20 countriesin 2014"
if (region_to_report == "RAP") caption_text <- "Domestic food price volatility index, top 20 countriesin 2014"
if (region_to_report == "REU") caption_text <- "Domestic food price volatility index, top 20 countriesin 2014"
if (region_to_report == "RNE") caption_text <- "Domestic food price volatility index, top 20 countriesin 2014"
if (region_to_report == "GLO") caption_text <- "Domestic food price volatility index, top 20 countriesin 2014"

## ---- P2stabilityBOTTOM ----
dat <- df %>% filter(FAOST_CODE %in% c(5100,5300,5500,5205,5000), Year %in% c(2000,2010)) %>%  select(FAOST_CODE,Year,FAO_TABLE_NAME,T.V.FEFS.PCT3D)

dat_plot <- dat[!is.na(dat$T.V.FEFS.PCT3D),]

# top ten

dat_plot$year_range[dat$Year == 2000] <- "1999-2001"
dat_plot$year_range[dat$Year == 2010] <- "2009-2011"

dat_plot$FAO_TABLE_NAME[dat_plot$FAO_TABLE_NAME == "Latin America and the Caribbean"] <- "Latin Am. and the Carib."

p <- ggplot(dat_plot, aes(x=FAO_TABLE_NAME,y=T.V.FEFS.PCT3D,fill=year_range))
p <- p + geom_bar(stat="identity",position="dodge")
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + labs(x=NULL,y="percent")
p <- p + theme(axis.text.x = element_text(angle=45))
p

caption_text <- "Value of food imports as a share of total merchandise exports (3 year averages)"


## ---- P2stabilityMAP ----
dat <- df[df$Year %in%  2013 & df$FAOST_CODE < 5000,c("Year","FAOST_CODE","G.GD.PSAVT.IN")]

dat <- dat[dat$FAOST_CODE != 41,]
dat$FAOST_CODE[dat$FAOST_CODE == 351] <- 41

dat <- dat[!is.na(dat$G.GD.PSAVT.IN),]

map.plot <- left_join(map.df,dat)

# Add region key and subset
map.plot <- map.plot[which(map.plot[[region_to_report]]),]

cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","G.GD.PSAVT.IN")]
cat_data$value_cat <- categories(x=cat_data$G.GD.PSAVT.IN, n=5) # manualBreaks = c(0, 5, 15, 25, 35, 100),

map.plot <- left_join(map.plot,cat_data[c("FAOST_CODE","value_cat")])

# define map unit
map_unit <- "index"


create_map_here()

# Caption
caption_text <- "Political stability and absence of violence/terrorism, index (2013)"



#   _____                       _             _     _   _   _                 _     _
#  |  ___|   ___     ___     __| |    _   _  | |_  (_) | | (_)  ____   __ _  | |_  (_)   ___    _ __
#  | |_     / _ \   / _ \   / _` |   | | | | | __| | | | | | | |_  /  / _` | | __| | |  / _ \  | '_ \
#  |  _|   | (_) | | (_) | | (_| |   | |_| | | |_  | | | | | |  / /  | (_| | | |_  | | | (_) | | | | |
#  |_|      \___/   \___/   \__,_|    \__,_|  \__| |_| |_| |_| /___|  \__,_|  \__| |_|  \___/  |_| |_|


## ---- P2utilizaTEXT ----
spread_title <- "Food utilization"
short_text <- "Utilization emphasizes the nutritional aspects of food security. It is commonly understood as the way the body makes the most of nutrients from food. Sufficient energy and nutrient intake includes nutritious and safe diets, a clean environment, access to health care, diversity of a diet and intra-household distribution of food. Poor utilization within a population can impose economic and social costs in countries at all economic levels."


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

print.xtable(xtable(tbl, caption = "Countries with highest share of children under 5 who are underweight, percent", digits = c(0,0,0,1),
                    align= "l{\raggedright\arraybackslash}p{1.6cm}rr"),
             type = "latex", table.placement = NULL, booktabs = TRUE, include.rownames = FALSE, size = "footnotesize", caption.placement = "top")



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
  max_nro_countries <- 8
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
p <- p + guides(color = guide_legend(nrow = 2))
p

# Caption
caption_text <- "Percentage of children under 5 who are stunted, highest 20 countries (2006 - 2014*)"
if (region_to_report == "RAF") caption_text <- "Average protein supply, top 20 countries in 2009-2011"
if (region_to_report == "RAP") caption_text <- "Average protein supply, top 20 countries in 2009-2011"
if (region_to_report == "REU") caption_text <- "Average protein supply, top 20 countries in 2009-2011"
if (region_to_report == "RNE") caption_text <- "Average protein supply, top 20 countries in 2009-2011"
if (region_to_report == "GLO") caption_text <- "Average protein supply, top 20 countries in 2009-2011"

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
  max_nro_countries <- 8
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
p <- p + guides(color = guide_legend(nrow = 2))
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
p <- p + geom_line()
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + labs(x=NULL,y="percent of population")
p <- p + scale_x_continuous(breaks=c(2000,2005,2010))
p

caption_text <- "Value of food imports as a share of total merchandise exports (3 year averages)"

## ---- P2utilizaMAP ----

dat <- df[df$Year %in%  2011 & df$FAOST_CODE < 5000,c("Year","FAOST_CODE","SH.ANM.CHLD.ZS")]

dat <- dat[dat$FAOST_CODE != 41,]
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


create_map_here()

# Caption
caption_text <- "Percentage of anaemia among children under 5, percent (2011)"