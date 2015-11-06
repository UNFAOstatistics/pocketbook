## ---- part5_setup ----

source(paste0(root.dir,'/input/code/plot/plot_color.R'))


syb_part <- 5

## Part 5
colPart5 <- plot_colors(part = syb_part, 8)
col.main1 <- colPart5[["Main"]][1]
## color for the grid
col.main2 <- colPart5[["Main"]][2]

source(paste0(root.dir,"/input/code/plot/theme.R"))

# map functions
source(paste0(root.dir,'/input/code/plot/map_categories.R'))
 # --------------------------------------------------------------- #


#    ___                                  _                   
#   / _ \  __   __   ___   _ __  __   __ (_)   ___  __      __
#  | | | | \ \ / /  / _ \ | '__| \ \ / / | |  / _ \ \ \ /\ / /
#  | |_| |  \ V /  |  __/ | |     \ V /  | | |  __/  \ V  V / 
#   \___/    \_/    \___| |_|      \_/   |_|  \___|   \_/\_/  
#                                                            



## ---- P5overTEXT ----
spread_title <- "Overview"
short_text <- "A combination of declining mortality rates, prolonged life expectancy and younger populations in regions with high fertility contributes to continuous, albeit declining population growth in the world. While rates continue to slow, the world’s population has nevertheless doubled since the late 1960s, to over 7 billion people today. Generally, population growth is high where income levels are low. This is especially true in cities. Since 2008, more people live in cities than in rural areas."
 # --------------------------------------------------------------- #

## ---- P5overData ----
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
df <- gather(dat, variable, value, 3:4)
 # --------------------------------------------------------------- #


## ---- P5overTOPRIGHT ----

if (region_to_report != "COF"){
  dat <- df %>% select(FAOST_CODE,Year,variable,value)
  dat <- left_join(dat,region_key)
  dat <- dat[which(dat[[region_to_report]]),]
  
  dat$variable <- as.character(dat$variable)
  dat$variable[dat$variable == "OA_3010_551"] <- "Rural population"
  dat$variable[dat$variable == "OA_3010_561"] <- "Urban population"
  dat <- dat %>% group_by(Year,variable) %>%  dplyr::summarise(value = sum(value, na.rm=TRUE)/1000000)
}

if (region_to_report == "COF"){
  dat <- df[df$FAOST_CODE == 5000,]
  dat$variable <- as.character(dat$variable)
  dat$variable[dat$variable == "OA_3010_551"] <- "Rural population"
  dat$variable[dat$variable == "OA_3010_561"] <- "Urban population"
  dat$value <- dat$value /1000000
}

# Draw the plot
p <- ggplot(dat, aes(x = Year, y = value))
p <- p + geom_area(aes(fill=variable), stat = "identity",position = "stack")
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + labs(x="",y="billion people")
p <- p + geom_vline(aes(xintercept=2015), color="grey20", linetype="dashed")
p <- p + scale_x_continuous(breaks=c(1961,2000,2015,2050))
p

cat("\\footnotesize{\\textit{Data after 2015 are projections}}")
cat("\\vspace{1mm}")

# Caption

caption_text <- "Rural and urban population"
# --------------------------------------------------------------- #


## ---- P5overLEFT ----
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
top10 <- dat %>% slice(1:10) %>% dplyr::mutate(color = "Countries with highest values")
bot10 <- dat %>% slice( (nrow(dat)-9):nrow(dat)) %>% dplyr::mutate(color = "Countries with lowest values")
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
 # --------------------------------------------------------------- #

## ---- P5overRIGHT ----

# data
dat <- syb.df %>% filter(Year %in% 2013) %>% select(FAOST_CODE,SP.DYN.LE00.IN)
dat <- dat[!is.na(dat$SP.DYN.LE00.IN),]

# Add region key and subset
dat <- left_join(dat,region_key)
dat <- dat[which(dat[[region_to_report]]),]

dat <- arrange(dat, -SP.DYN.LE00.IN)
top10 <- dat %>% slice(1:10) %>% dplyr::mutate(color = "Countries with highest values")
bot10 <- dat %>% slice( (nrow(dat)-9):nrow(dat)) %>% dplyr::mutate(color = "Countries with lowest values")
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



## ---- P5overBOTTOM ----
# data
dat <- syb.df %>% filter(Year %in% c(2000:2014), FAOST_CODE %in% c(5100,5200,5300,5400,5500)) %>% select(FAOST_CODE,Year,SHORT_NAME,OA.TEAPT.POP.PPL.NO)
dat <- dat[!is.na(dat$OA.TEAPT.POP.PPL.NO),]
dat <- dat[!is.na(dat$SHORT_NAME),]


dat$OA.TEAPT.POP.PPL.NO <- dat$OA.TEAPT.POP.PPL.NO / 1000000

dat_plot <- dat

p <- ggplot(dat_plot, aes(x=Year,y=OA.TEAPT.POP.PPL.NO,color=SHORT_NAME))
p <- p + geom_line(size = 1.1, alpha=.7)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, length(unique(dat_plot$SHORT_NAME)))[["Sub"]])
p <- p + labs(x="",y="million people")
#p <- p + guides(color = guide_legend(nrow = 2))
p <- p + scale_y_continuous(labels=space) 
p

# Caption
caption_text <- "Total economically active population (2000 to 2014)"
# --------------------------------------------------------------- #


## ---- P5overMAP ----
dat <- syb.df %>% filter(Year %in% 2014, FAOST_CODE < 5000) %>% select(FAOST_CODE,SHORT_NAME,OA.TPR.POP.PPL.SHP)

map.plot <- left_join(dat,map.df)

# Add region key and subset
#map.plot <- map.plot[which(map.plot[[region_to_report]]),]

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
# --------------------------------------------------------------- #


#   _____                                                    
#  | ____|   ___    ___    _ __     ___    _ __ ___    _   _ 
#  |  _|    / __|  / _ \  | '_ \   / _ \  | '_ ` _ \  | | | |
#  | |___  | (__  | (_) | | | | | | (_) | | | | | | | | |_| |
#  |_____|  \___|  \___/  |_| |_|  \___/  |_| |_| |_|  \__, |
#                                                      |___/ 
# 


## ---- P5econTEXT ----
spread_title <- "Economy"
short_text <- "Changes in the wider economy, including growing global integration – and therefore trade, currency movements and international prices – affect the performance of commodities in the agricultural sector, including coffee production. During the global economic downturn of the last decade, some sectors were particularly hard hit. Agriculture was affected too, but the coffee sector has demonstrated remarkable resilience. Since 2008, the value of coffee production has grown at over 3.5 percent per year, which is faster than 2.5 percent overall growth in agriculture."
 # --------------------------------------------------------------- #

## ---- P5econTOPRIGHT, eval=P5econ, top_right_plot=P5econ, fig.height=top_right_plot_height, fig.width=top_right_plot_width ----
dat <- syb.df %>% filter(Year %in% 2013, FAOST_CODE %in% c(5100,5200,5300,5400,5500)) %>% select(SHORT_NAME,NV.AGR.TOTL.ZS,NV.IND.TOTL.ZS,NV.SRV.TETC.ZS)

dat <- gather(dat, variable, value, 2:4)
dat$fill[dat$variable == "NV.AGR.TOTL.ZS"] <- "Agriculture"
dat$fill[dat$variable == "NV.IND.TOTL.ZS"] <- "Industry"
dat$fill[dat$variable == "NV.SRV.TETC.ZS"] <- "Services"

dat_plot <- dat

# reorder regions by the share of agricultural land
dat_plot$SHORT_NAME <- factor(dat_plot$SHORT_NAME,
                                  levels=arrange(dat_plot[dat_plot$fill == "Agriculture",],-value)$SHORT_NAME )

p <- ggplot(dat_plot, aes(x=SHORT_NAME, y=value, fill=fill))
p <- p + geom_bar(stat="identity", position="stack")
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, 3)[["Sub"]])
p <- p + labs(x="",y="percent")
p <- p + theme(axis.text.x = element_text(angle=45))
p

# Caption
caption_text <- "Value added, share of GDP (2013)"
# --------------------------------------------------------------- #


## ---- P5econLEFT, eval=P5econ, left_plot=P5econ, fig.height=left_plot_height, fig.width=left_plot_width ----
dat <- syb.df[syb.df$Year %in%  2013 & syb.df$FAOST_CODE < 5000,c("FAOST_CODE","Year","SHORT_NAME","EA.PRD.AGRI.KD")]

dat <- dat[!is.na(dat$EA.PRD.AGRI.KD),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

#and subset
dat <- dat[which(dat[[region_to_report]]),]

# top for this plot
dat <- arrange(dat, -EA.PRD.AGRI.KD)
dat_plot <- dat %>% slice(1:20) %>% dplyr::mutate(color = "2013")

p <- ggplot(dat_plot, aes(x=reorder(SHORT_NAME, EA.PRD.AGRI.KD),y=EA.PRD.AGRI.KD))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 1)[["Sub"]])
p <- p + theme(legend.position = "none") # hide legend as only one year plotted
p <- p + coord_flip()
p <- p + labs(x="",y="US$")
p <- p + guides(color = guide_legend(nrow = 2))
p <- p + scale_y_continuous(labels=space) 
p

# Caption
caption_text <- "Agriculture value added per worker, countries with the highest values"
# --------------------------------------------------------------- #

## ---- P5econRIGHT, eval=P5econ ,right_plot=P5econ, fig.height=right_plot_height, fig.width=right_plot_width ----

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
              dplyr::mutate(color = "Countries with highest values")

bot10 <- dat %>% arrange(FAOST_CODE,Year) %>%  
              group_by(FAOST_CODE) %>% dplyr::mutate(Growth=c(NA,exp(diff(log(NV.AGR.TOTL.KD)))-1)) %>% 
              group_by(SHORT_NAME) %>% 
              dplyr::summarise(growth_NV.AGR.TOTL.KD = mean(Growth, na.rm = TRUE)*100) %>% 
              arrange(growth_NV.AGR.TOTL.KD) %>% 
              slice(1:10) %>% 
              dplyr::mutate(color = "Countries with lowest values")
dat_plot <- rbind(top10,bot10)
  
p <- ggplot(dat_plot, aes(x=reorder(SHORT_NAME, growth_NV.AGR.TOTL.KD),y=growth_NV.AGR.TOTL.KD))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="percent")
p <- p + guides(color = guide_legend(nrow = 2))
p

# Caption
caption_text <- "Life expectancy at birth, countries with the highest and lowest values (2013)"
#if (region_to_report == "COF") dat <- dat[dat$FAOST_CODE == 5000,]

 # --------------------------------------------------------------- #


## ---- P5econBOTTOM_data, cache=TRUE ----
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
 # --------------------------------------------------------------- #



## ---- P5econBOTTOM, eval=P5econ, bottom_plot=P5econ, fig.height=bottom_plot_height, fig.width=bottom_plot_width ----

dat <- syb.df %>% filter(Year %in% 2000:2013, FAOST_CODE %in% c(5100,5200,5300,5400,5500)) %>% select(SHORT_NAME,Year,NV.AGR.TOTL.ZS)

dat_plot <- dat

p <- ggplot(data = dat_plot, aes(x = Year, y = NV.AGR.TOTL.ZS,group=SHORT_NAME,color=SHORT_NAME))
p <- p + geom_line(size = 1.1, alpha=.7)
p <- p + scale_color_manual(values = plot_colors(part = 1, length(unique(dat_plot$SHORT_NAME)))[["Sub"]])
p <- p + labs(y="percent", x="")
#p <- p + guides(color = guide_legend(nrow = 2))
p

# Caption
caption_text <- "Value added in agriculture as share of GDP"
 # --------------------------------------------------------------- #


## ---- P5econMAP, eval=P5econ, map_plot=P5econ, fig.width=map.fig.width, fig.height= map.fig.height ,out.width=map.out.width, out.height=map.out.height, out.extra=map.out.extra ----
dat <- syb.df %>% filter(Year %in% c(2010:2013), FAOST_CODE < 5000) %>%
                select(FAOST_CODE,SHORT_NAME,NV.AGR.TOTL.ZS) %>%
                group_by(FAOST_CODE) %>% dplyr::summarise(NV.AGR.TOTL.ZS = max(NV.AGR.TOTL.ZS)) %>% 
                ungroup()

map.plot <- left_join(dat,map.df)

# Add region key and subset
# map.plot <- map.plot[which(map.plot[[region_to_report]]),]

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
 # --------------------------------------------------------------- #


# 
#   ____                                  _           
#  |  _ \    ___   __   __   ___   _ __  | |_   _   _ 
#  | |_) |  / _ \  \ \ / /  / _ \ | '__| | __| | | | |
#  |  __/  | (_) |  \ V /  |  __/ | |    | |_  | |_| |
#  |_|      \___/    \_/    \___| |_|     \__|  \__, |
#                                               |___/ 


## ---- P5povertyTEXT, eval=P5poverty, short_text_ChartPage=P5poverty ----
spread_title <- "Poverty"
short_text <- "Most coffee is produced in developing countries. Yet the benefits from the expansion of the coffee industry in recent years have not always trickled down to the poorest. Among the largest coffee producing countries, many still have more than 10 percent of their populations living below US\\$1.25 a day. However, change is on the way, and coffee plays a pivotal role in this process. Coffee production and the proceeds from it have allowed a growing number of small and poor farmers to escape the poverty trap. By adapting new technologies or by switching to better varieties, many now harvest higher crops of better quality beans."
 # --------------------------------------------------------------- #


## ---- P5povertyData, cache=TRUE,results='hide', eval=P5poverty ----


# --------------------------------------------------------------- #



## ---- P5povertyTOPRIGHT, eval=P5poverty, top_right_minitable=P5poverty ----

# These figures come from the World Bank WDI 2015 page 35

dw <- data_frame(region <- c("East Asia & Pacific","Europe & Central Asia",
                             "Lat Amer & Caribbean","Mid East & N Africa",
                             "South Asia","Sub-Saharan Africa","World"),
                 Y1990 <- c(939,7,53,13,620,290,1923),
                 Y2015 <- c(86,1 ,27,7 ,311,403,836))


names(dw) <- c("","1990","2015*")
# Thousand separator for poverty table
dw[[2]]<- prettyNum(dw[[2]], big.mark=" ")
dw[[3]]<- prettyNum(dw[[3]], big.mark=" ")


# dw <- head(cars)
# dw$names <- "hahahaha"

print.xtable(xtable(dw, caption = "\\large{People living on less than 2005 PPP \\$1.25 a day (mln)}", digits = c(0,0,0,0),
                    align= "l{\raggedright\arraybackslash}p{1.7cm}rr"),
             type = "latex", table.placement = NULL, 
             booktabs = TRUE, include.rownames = FALSE, size = "footnotesize", caption.placement = "top")
cat("\\footnotesize{\\textit{* forecast}}")

 # --------------------------------------------------------------- #


## ---- P5povertyLEFT ----
dat <- filter(syb.df, Year %in%
                   c(2010:2014)) %>% 
                  group_by(FAOST_CODE,SHORT_NAME) %>% 
                  dplyr::summarise(SI.DST.05TH.20 = mean(SI.DST.05TH.20, na.rm=TRUE))
dat <- ungroup(dat)
dat <- dat[!is.na(dat$SI.DST.05TH.20),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[which(dat[[region_to_report]]),]

dat <- arrange(dat, -SI.DST.05TH.20)
top10 <- dat %>% slice(1:10) %>% dplyr::mutate(color = "Countries with highest values")
bot10 <- dat %>% slice( (nrow(dat)-9):nrow(dat)) %>% dplyr::mutate(color = "Countries with lowest values")
dat_plot <- rbind(top10,bot10)

p <- ggplot(dat_plot, aes(x=reorder(SHORT_NAME, SI.DST.05TH.20),y=SI.DST.05TH.20))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="percent")
p <- p + guides(color = guide_legend(nrow = 2))
p


# Caption
caption_text <- "Income share held by highest 20 (percent) (2010-2014)"
 # --------------------------------------------------------------- #

## ---- P5povertyRIGHT ----

dat <- filter(syb.df, Year %in%
                   c(2010:2014)) %>%
                  group_by(FAOST_CODE,SHORT_NAME) %>%
                  dplyr::summarise(SI.POV.NAHC = mean(SI.POV.NAHC, na.rm=TRUE))
dat <- ungroup(dat)
dat <- dat[!is.na(dat$SI.POV.NAHC),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[which(dat[[region_to_report]]),]

dat <- arrange(dat, -SI.POV.NAHC)
top10 <- dat %>% slice(1:10) %>% dplyr::mutate(color = "Countries with highest values")
bot10 <- dat %>% slice( (nrow(dat)-9):nrow(dat)) %>% dplyr::mutate(color = "Countries with lowest values")
dat_plot <- rbind(top10,bot10)

p <- ggplot(dat_plot, aes(x=reorder(SHORT_NAME, SI.POV.NAHC),y=SI.POV.NAHC))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="percent")
p <- p + guides(color = guide_legend(nrow = 2))
p


# Caption
caption_text <- "Poverty headcount ratio at national poverty line (population share)"
 # --------------------------------------------------------------- #


## ---- P5povertyBOTTOM ----


dat <- syb.df %>% filter(Year %in% c(2000:2014)) %>% 
  select(FAOST_CODE,Year,SHORT_NAME,SI.POV.DDAY,OA.TPBS.POP.PPL.NO)
dat <- dat[!is.na(dat$SI.POV.DDAY),]
dat <- dat[!is.na(dat$OA.TPBS.POP.PPL.NO),]
dat <- dat[!is.na(dat$SHORT_NAME),]


# DEFAULT GROUPING
df <- subgrouping(region_to_report = region_to_report)

# merge data with the region info
dat <- merge(dat,df[c("FAOST_CODE","subgroup")],by="FAOST_CODE")

# AGREGATE
dat_plot <- dat %>% group_by(subgroup,Year) %>% dplyr::summarise(value = weighted.mean(SI.POV.DDAY, OA.TPBS.POP.PPL.NO, na.rm=TRUE)) %>% ungroup()


# These figures come from the World Bank WDI 2015 page 35


dw <- data_frame(region = c("East Asia & Pacific","Europe & Central Asia",
                             "Lat Amer & Caribbean","Mid East & N Africa",
                             "South Asia","Sub-Saharan Africa","World"),
                 Y1990 = c(57,
                            1.5,
                            12.2,
                            5.8,
                            54.1,
                            56.6,
                            36.4),
                 Y1993 = c(51.7,
                            2.9,
                            11.9,
                            5.3,
                            52.1,
                            60.9,
                            35.1),
                 Y1996 = c(38.3,
                            4.3,
                            10.5,
                            4.8,
                            45.0,
                            59.7,
                            30.4),
                 Y1999 = c(35.9,
                            3.8,
                            11.0,
                            4.8,
                            45.0,
                            59.3,
                            29.1),
                 Y2002 = c(27.3,
                            2.1,
                            10.2,
                            3.8,
                            44.1,
                            57.1,
                            26.1),
                 Y2005 = c(16.7,
                            1.3,
                            7.3,
                            3.0,
                            39.3,
                            52.8,
                            21.1),
                 Y2008 = c(13.7,
                            0.5,
                            5.4,
                            2.1,
                            34.1,
                            49.7,
                            18.6),
                 Y2011 = c(7.9,
                            0.5,
                            4.6,
                            1.7,
                            24.5,
                            46.8,
                            14.5),
                 Y2015 = c(4.1,
                            0.3,
                            4.3,
                            2.0,
                            18.1,
                            40.9,
                            11.5))
ddw <- gather(dw, Year, value, 2:10)

ddw$Year <- str_replace_all(ddw$Year, "Y", "")

ddw$Year <- factor(ddw$Year)
ddw$Year <- as.numeric(levels(ddw$Year))[ddw$Year]

p <- ggplot(ddw, aes(x=Year,y=value,color=region,group=region))
p <- p + geom_line(size = 1.1, alpha=.7)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, length(unique(ddw$region)))[["Sub"]])
p <- p + labs(x="",y="% of population")
p <- p + guides(color = guide_legend(nrow = 2))
p

# Caption
caption_text <- "Share of population living on less than 2005 PPP \\$1.25 a day (\\%)"


 # --------------------------------------------------------------- #


## ---- P5povertyMAP ----

dat <- syb.df %>% filter(Year %in% 2007:2012, FAOST_CODE < 5000) %>%
                  select(FAOST_CODE,SHORT_NAME,SI.POV.DDAY) %>% 
                  group_by(FAOST_CODE) %>% 
                  dplyr::summarise(SI.POV.DDAY = mean(SI.POV.DDAY, na.rm=TRUE)) 

map.plot <- left_join(dat,map.df)

# Add region key and subset
# map.plot <- map.plot[which(map.plot[[region_to_report]]),]

cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","SI.POV.DDAY")]
cat_data$value_cat <- categories(x=cat_data$SI.POV.DDAY, n=5, method="jenks")

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
caption_text <- "Share of population living less than US\\$ 1.25 per day (2007 - 2012)"

 # --------------------------------------------------------------- #



#   ____   _        _                                                                                         _        
#  |  _ \ (_)  ___ | |_  __ _  _ __  _   _    ___  _ __    ___  _ __  __ _  _   _   ___  _   _  _ __   _ __  | | _   _ 
#  | | | || | / _ \| __|/ _` || '__|| | | |  / _ \| '_ \  / _ \| '__|/ _` || | | | / __|| | | || '_ \ | '_ \ | || | | |
#  | |_| || ||  __/| |_| (_| || |   | |_| | |  __/| | | ||  __/| |  | (_| || |_| | \__ \| |_| || |_) || |_) || || |_| |
#  |____/ |_| \___| \__|\__,_||_|    \__, |  \___||_| |_| \___||_|   \__, | \__, | |___/ \__,_|| .__/ | .__/ |_| \__, |
#                                    |___/                           |___/  |___/              |_|    |_|        |___/ 


## ---- P5desTEXT ----
spread_title <- "Dietary energy supply"
short_text <- "TThe dietary energy supply (DES) is the food available for human consumption, expressed in kilocalories per person per day. In 1961 the average global calorie availability was as low as 2 196 kcal/cap/day; by 2011, it had reached 2 870 kcal/cap/day, and consisted primarily of a base of staple grains, and meat and dairy products. Consumers of coffee have traditionally come from high-income countries but now include the growing middle classes in developing countries. In terms of calories, coffee and other stimulants still make up a very small part of food available for consumption."
 # --------------------------------------------------------------- #

## ---- P5desData,results='hide', cache=TRUE, eval=P5des ----
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

df.fsi <- dat[!duplicated(dat[c("FAOST_CODE","Year")]),]

# For despie graphs icn2.df
load(paste0(data.dir,"/icn2.RData"))

 # --------------------------------------------------------------- #


## ---- P5desTOPRIGHT ----


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

dat_s <- dat %>% group_by(var) %>%  dplyr::summarise(wmean = weighted.mean(mean, OA.TPBS.POP.PPL.NO, na.rm=FALSE)) %>% 
             dplyr::mutate(mean = wmean/sum(wmean)*100)

dat_plot <- dat_s  %>% dplyr::mutate(sum = sum(mean)) 

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
# p <- p + scale_fill_manual(values=rev(colPart1$Sub))
p <- p + scale_fill_manual(values = c("#8c510a","#d8b365","#f6e8c3","#c7eae5","#5ab4ac","#01665e"))
p <- p + theme(legend.title = element_blank())
p <- p + theme(legend.key.height = unit(7, "mm"))
p <- p + theme(legend.key.width = unit(3, "mm"))
p <- p + theme(panel.grid=element_blank(), panel.border=element_blank())
p <- p + labs(x=NULL, y=NULL)
p <- p + theme(plot.margin=unit(c(0,0,0,0),"mm"))
p

# Caption
caption_text <- "Share of dietary energy supply, kcal/capita/day (2009-2011)"
 # --------------------------------------------------------------- #

## ---- P5desLEFT ----
# data

dat <- df.fsi[df.fsi$Year %in%  c(2000,2015) & df.fsi$FAOST_CODE < 5000,c("FAOST_CODE","Year","FAO_TABLE_NAME","FBS.PCS.PDES.KCD3D")]

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

dat_plot$SHORT_NAME  <- factor(dat_plot$SHORT_NAME, levels=top2015[order(top2015$FBS.PCS.PDES.KCD3D),]$SHORT_NAME)

p <- ggplot(dat_plot, aes(x=SHORT_NAME,y=FBS.PCS.PDES.KCD3D))
#p <- ggplot(dat_plot, aes(x=SHORT_NAME,y=FBS.PCS.PDES.KCD3D))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="kcal/cap/day")
p <- p + guides(color = guide_legend(nrow = 1))
p <- p + scale_y_continuous(labels=space, breaks=c(2500,3000,3500)) 
p

# Caption
caption_text <- "Dietary energy supply, top 20 countries in 2015"
# --------------------------------------------------------------- #

## ---- P5desRIGHT ----

dat <- df.fsi[df.fsi$Year %in%  c(2000,2015) & df.fsi$FAOST_CODE < 5000,c("FAOST_CODE","Year","FAO_TABLE_NAME","FBS.PCS.PDES.KCD3D")]

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

dat_plot$SHORT_NAME  <- factor(dat_plot$SHORT_NAME, levels=bottom2015[order(bottom2015$FBS.PCS.PDES.KCD3D),]$SHORT_NAME)

p <- ggplot(dat_plot, aes(x=SHORT_NAME,y=FBS.PCS.PDES.KCD3D))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="kcal/cap/day")
p <- p + guides(color = guide_legend(nrow = 1))
p <- p + scale_y_continuous(labels=space, breaks=c(1800,2000,2200)) 
p

# Caption
caption_text <- "Dietary energy supply, bottom 20 countries in 2015"
# --------------------------------------------------------------- #


## ---- P5desBOTTOM ----
#dat <- df.fsi[df.fsi$Year %in%  c(2000:2015) & df.fsi$FAOST_CODE < 5000,c("FAOST_CODE","Year","FAO_TABLE_NAME","FBS.PCS.PDES.KCD3D")]

# dat <- dat[!is.na(dat$FBS.PCS.PDES.KCD3D),]
# # Add region key and subset
# dat <- left_join(dat,region_key)
# 
# dat <- dat[dat$FAOST_CODE != 348,]
# dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"
# 
# df <- subgrouping(region_to_report = region_to_report)
# 
# # merge data with the region info
# dat2 <- merge(dat,df[c("FAOST_CODE","subgroup")],by="FAOST_CODE")
# 
# dat <- syb.df %>% filter(Year %in% c(2000:2015)) %>% select(FAOST_CODE,Year,OA.TPBS.POP.PPL.NO)
# dat <- dat[!is.na(dat$OA.TPBS.POP.PPL.NO),]
# 
# dat3 <- left_join(dat2,dat)
# 
# # AGREGATE
# dat_plot <- dat3 %>% group_by(subgroup,Year) %>% dplyr::summarise(value = weighted.mean(FBS.PCS.PDES.KCD3D, OA.TPBS.POP.PPL.NO, na.rm=TRUE)) %>% ungroup()

dat_plot <- df.fsi[df.fsi$Year %in%  c(2000:2015) & df.fsi$FAOST_CODE %in% c(5000,5100,5205,5300,5500),
              c("FAOST_CODE","Year","FAO_TABLE_NAME","FBS.PCS.PDES.KCD3D")]

p <- ggplot(dat_plot, aes(x=Year,y=FBS.PCS.PDES.KCD3D,color=FAO_TABLE_NAME))
p <- p + geom_line(size = 1.1, alpha=.7)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 5)[["Sub"]])
p <- p + labs(x="",y="kcal/cap/day")
p <- p + scale_y_continuous(labels=space) 
p

# Caption
caption_text <- "Dietary energy supply"
#if (region_to_report == "COF") dat <- dat[dat$FAOST_CODE == 5000,]
 # --------------------------------------------------------------- #


## ---- P5desMAP ----

dat <- df.fsi[df.fsi$Year %in%  2015 & df.fsi$FAOST_CODE < 5000,c("Year","FAOST_CODE","FS.DA.ADESA.PCT3D")]

dat <- dat[dat$FAOST_CODE != 41,]
dat$FAOST_CODE[dat$FAOST_CODE == 351] <- 41

map.plot <- left_join(dat,map.df)

# Add region key and subset

# map.plot <- map.plot[which(map.plot[[region_to_report]]),]


cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","FS.DA.ADESA.PCT3D")]
cat_data$value_cat <- categories(x=cat_data$FS.DA.ADESA.PCT3D, n=5, manual=FALSE, method="jenks") # manualBreaks = c(0, 5, 15, 25, 35, 100),

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
 caption_text <- "Average dietary energy supply adequacy, percent (2014-2016)"
# --------------------------------------------------------------- #


