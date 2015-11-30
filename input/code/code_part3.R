## ---- part3_setup ----

source(paste0(root.dir,'/input/code/plot/plot_color.R'))

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
if (region_to_report == "RAF") short_text <- "The dietary energy supply (DES) is the food available for human consumption, expressed in kilocalories per person per day. At the country level, it is calculated as a measure of food available for human use after taking out all non-food utilization, including exports, industrial use, animal feed, seed, wastage and changes in stocks. In 1961 the average global calorie availability was as low as 2 196 kcal/cap/day; by 2011, it had reached 2 870 kcal/cap/day, and was centered more around a narrow base of staple grains as well as meat and dairy products."
if (region_to_report == "RAP") short_text <- "The dietary energy supply (DES) is the food available for human consumption, expressed in kilocalories per person per day. At the country level, it is calculated as a measure of food available for human use after taking out all non-food utilization, including exports, industrial use, animal feed, seed, wastage and changes in stocks. In 1961 the average global calorie availability was as low as 2 196 kcal/cap/day; by 2011, it had reached 2 870 kcal/cap/day, and was centered more around a narrow base of staple grains as well as meat and dairy products."
if (region_to_report == "REU") short_text <- "The dietary energy supply (DES) is the food available for human consumption, expressed in kilocalories per person per day. At the country level, it is calculated as a measure of food available for human use after taking out all non-food utilization, including exports, industrial use, animal feed, seed, wastage and changes in stocks. In 1961 the average global calorie availability was as low as 2 196 kcal/cap/day; by 2011, it had reached 2 870 kcal/cap/day, and was centered more around a narrow base of staple grains as well as meat and dairy products."
if (region_to_report == "RNE") short_text <- "The dietary energy supply (DES) is the food available for human consumption, expressed in kilocalories per person per day. At the country level, it is calculated as a measure of food available for human use after taking out all non-food utilization, including exports, industrial use, animal feed, seed, wastage and changes in stocks. In 1961 the average global calorie availability was as low as 2 196 kcal/cap/day; by 2011, it had reached 2 870 kcal/cap/day, and was centered more around a narrow base of staple grains as well as meat and dairy products."
if (region_to_report == "GLO") short_text <- "The dietary energy supply (DES) is the food available for human consumption, expressed in kilocalories per person per day. At the country level, it is calculated as a measure of food available for human use after taking out all non-food utilization, including exports, industrial use, animal feed, seed, wastage and changes in stocks. In 1961 the average global calorie availability was as low as 2 196 kcal/cap/day; by 2011, it had reached 2 870 kcal/cap/day, and was centered more around a narrow base of staple grains as well as meat and dairy products."

## ---- P3desData ----
# Retrieve data
load(paste0(data.dir,"/fsi_data.RData")) # manipulated in code_part2.R
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

# For despie graphs icn2.df
load(paste0(root.dir,"../ICN2PB14/Data/Processed/icn2.RData"))



## ---- P3desTOPRIGHT ----


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

dat <- dat %>% group_by(var) %>%  dplyr::summarise(wmean = weighted.mean(mean, OA.TPBS.POP.PPL.NO, na.rm=FALSE)) %>%
  dplyr::mutate(mean = wmean/sum(wmean)*100)

dat_plot <- dat  %>% dplyr::mutate(sum = sum(mean))

p <- ggplot(dat_plot, aes(x=sum/2, y = mean, fill = var, width = sum))
p <- p + geom_bar(position="fill", stat="identity")
p <- p + coord_polar("y")
p <- p + theme_minimal()
p <- p + theme(legend.position = "right")
p <- p + theme(axis.text = element_blank())
p <- p + theme(axis.title = element_blank())
p <- p + theme(axis.ticks = element_blank())
p <- p + theme(panel.grid.minor = element_blank())
p <- p + theme(panel.grid.major = element_blank())
p <- p + scale_fill_manual(values=rev(colPart3$Sub))
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
p <- p + theme(plot.margin=unit(c(0,0,0,0),"mm"))
p

# Caption
caption_text <- "Share of dietary energy supply, kcal/capita/day (2009-2011)"


## ---- P3desLEFT ----
# data

dat <- df[df$Year %in%  c(2000,2015) & df$FAOST_CODE < 5000,c("FAOST_CODE","Year","FAO_TABLE_NAME","FBS.PCS.PDES.KCD3D")]

dat <- dat[!is.na(dat$FBS.PCS.PDES.KCD3D),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

dat <- dat[which(dat[[region_to_report]]),]

# semi-standard data munging for two year dot-plots
# give name Value for value-col
names(dat)[names(dat)=="FBS.PCS.PDES.KCD3D"] <- "Value"
# Plot only as many countries as there are for particular region, max 20
nro_latest_cases <- nrow(dat[dat$Year == max(dat$Year),])
if (nro_latest_cases < 20) {ncases <- nro_latest_cases} else ncases <- 20
dat <- arrange(dat, -Year, -Value)
# slice the data for both years
top2015 <- dat %>% slice(1:ncases) %>% dplyr::mutate(color = "2015")
top2000 <- dat %>% filter(FAOST_CODE %in% top2015$FAOST_CODE, Year == 2000) %>% dplyr::mutate(color = "2000")
dat_plot <- rbind(top2015,top2000)
# levels based on newest year
dat_plot$SHORT_NAME <- factor(dat_plot$SHORT_NAME, levels=arrange(top2015,Value)$SHORT_NAME)
###############

p <- ggplot(dat_plot, aes(x=SHORT_NAME,y=Value))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="kcal/cap/day")
p <- p + guides(color = guide_legend(nrow = 1))
p <- p + scale_y_continuous(labels=space) 
p

# Caption
caption_text <- "Dietary energy supply, top 20 countries in 2015"

## ---- P3desRIGHT ----

dat <- df[df$Year %in%  c(2000,2015) & df$FAOST_CODE < 5000,c("FAOST_CODE","Year","FAO_TABLE_NAME","FBS.PCS.PDES.KCD3D")]

dat <- dat[!is.na(dat$FBS.PCS.PDES.KCD3D),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

dat <- dat[which(dat[[region_to_report]]),]

# semi-standard data munging for two year dot-plots
# give name Value for value-col
names(dat)[names(dat)=="FBS.PCS.PDES.KCD3D"] <- "Value"
# Plot only as many countries as there are for particular region, max 20
nro_latest_cases <- nrow(dat[dat$Year == max(dat$Year),])
if (nro_latest_cases < 20) {ncases <- nro_latest_cases} else ncases <- 20
dat <- arrange(dat, -Year, -Value)
# slice the data for both years
top2015 <- dat %>% slice(1:ncases) %>% dplyr::mutate(color = "2015")
top2000 <- dat %>% filter(FAOST_CODE %in% top2015$FAOST_CODE, Year == 2000) %>% dplyr::mutate(color = "2000")
dat_plot <- rbind(top2015,top2000)
# levels based on newest year
dat_plot$SHORT_NAME <- factor(dat_plot$SHORT_NAME, levels=arrange(top2015,Value)$SHORT_NAME)
###############

p <- ggplot(dat_plot, aes(x=SHORT_NAME,y=Value))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="kcal/cap/day")
p <- p + guides(color = guide_legend(nrow = 1))
p <- p + scale_y_continuous(labels=space) 
p

# Caption
caption_text <- "Dietary energy supply, bottom 20 countries in 2015"


## ---- P3desBOTTOM ----
dat <- df[df$Year %in%  c(2000:2015) & df$FAOST_CODE < 5000,c("FAOST_CODE","Year","FAO_TABLE_NAME","FBS.PCS.PDES.KCD3D")]

dat <- dat[!is.na(dat$FBS.PCS.PDES.KCD3D),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"


dat <- dat[which(dat[[region_to_report]]),]

dat <- arrange(dat, -Year, -FBS.PCS.PDES.KCD3D)
top5_FAOST_CODE <- head(dat$FAOST_CODE, 5)
dat_plot <- dat %>%  filter(FAOST_CODE %in% top5_FAOST_CODE)


p <- ggplot(dat_plot, aes(x=Year,y=FBS.PCS.PDES.KCD3D,color=SHORT_NAME))
p <- p + geom_line(size=1.1, alpha=.7)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 5)[["Sub"]])
p <- p + labs(x="",y="kcal/cap/day")
p <- p + scale_y_continuous(labels=space) 
p

# Caption
caption_text <- "Dietary energy supply in top 5 countries"

## ---- P3desMAP ----

dat <- df[df$Year %in%  2015 & df$FAOST_CODE < 5000,c("Year","FAOST_CODE","FS.DA.ADESA.PCT3D")]

map.plot <- left_join(map.df,dat) # so that each country in the region will be filled (value/NA)

# Add region key and subset

map.plot <- map.plot[which(map.plot[[region_to_report]]),]


cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","FS.DA.ADESA.PCT3D")]
cat_data$value_cat <- categories(x=cat_data$FS.DA.ADESA.PCT3D, n=5, manual=FALSE, method="jenks") # manualBreaks = c(0, 5, 15, 25, 35, 100),

map.plot <- left_join(map.plot,cat_data[c("FAOST_CODE","value_cat")])

# define map unit
map_unit <- "Percent"

create_map_here()

# Caption
caption_text <- "Average dietary energy supply adequacy, percent (2014-2016)"
if (region_to_report == "RAF") caption_text <- "Average dietary energy supply adequacy, percent (2014-2016)"
if (region_to_report == "RAP") caption_text <- "Average dietary energy supply adequacy, percent (2014-2016)"
if (region_to_report == "REU") caption_text <- "Average dietary energy supply adequacy, percent (2014-2016)"
if (region_to_report == "RNE") caption_text <- "Average dietary energy supply adequacy, percent (2014-2016)"
if (region_to_report == "GLO") caption_text <- "Average dietary energy supply adequacy, percent (2014-2016)"

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
if (region_to_report == "REU") short_text <- "The majority of people in developing countries live in rural areas, and most of them depend on agriculture for their livelihoods. Over the past 50 years, growth in crop production has been driven largely by higher yields per unit of land, and crop intensification. Trends are not uniform across regions, however. Most of the growth in wheat and rice production in Asia and Northern Africa has been from gains in yield, while expansion of harvested land has led to production growth of maize in Latin America and in sub-Saharan Africa."
if (region_to_report == "RNE") short_text <- "The majority of people in developing countries live in rural areas, and most of them depend on agriculture for their livelihoods. Over the past 50 years, growth in crop production has been driven largely by higher yields per unit of land, and crop intensification. Trends are not uniform across regions, however. Most of the growth in wheat and rice production in Asia and Northern Africa has been from gains in yield, while expansion of harvested land has led to production growth of maize in Latin America and in sub-Saharan Africa."
if (region_to_report == "GLO") short_text <- "The majority of people in developing countries live in rural areas, and most of them depend on agriculture for their livelihoods. Over the past 50 years, growth in crop production has been driven largely by higher yields per unit of land, and crop intensification. Trends are not uniform across regions, however. Most of the growth in wheat and rice production in Asia and Northern Africa has been from gains in yield, while expansion of harvested land has led to production growth of maize in Latin America and in sub-Saharan Africa."

## ---- P3cropproData ----

# This should be thought twice how to produce it for regional books!
if (!file.exists(paste0(data.dir,"/Production_Crops_E_All_Data_(Norm).csv"))){
  download.file("http://faostat3.fao.org/faostat-bulkdownloads/Production_Crops_E_All_Data_(Norm).zip",
                destfile = paste0(data.dir,"/Production_Crops_E_All_Data_(Norm).zip"))
  unzip(zipfile = paste0(data.dir,"/Production_Crops_E_All_Data_(Norm).zip"),
        exdir = data.dir)
  dat <- read_csv(paste0(data.dir,"/Production_Crops_E_All_Data_(Norm).csv"))
} else dat <- read_csv(paste0(data.dir,"/Production_Crops_E_All_Data_(Norm).csv"))

names(dat)[names(dat)=="Country Code"] <- "FAOST_CODE"
dat <- dat[dat$Year > 1999,]
# Add region key and subset
dat <- left_join(dat,region_key)


## ---- P3cropproTOPRIGHT ----
# rc <- dat %>%  filter(Year >= 2000, Element == "Production") %>% group_by(Item,Year) %>% dplyr::summarise(Value = sum(Value, na.rm = TRUE)) %>%
#   dplyr::mutate(Growth=c(NA,exp(diff(log(Value)))-1)) %>%
#   dplyr::summarise(mean_growth = mean(Growth, na.rm = TRUE)*100) %>%  filter(!is.infinite(mean_growth)) %>%
#   arrange(-mean_growth) %>% slice(1:5) %>% select(Item,mean_growth)

dat <- dat[which(dat[[region_to_report]]),]

growth <- data.frame()

gr_dat <- dat %>% filter(Year >= 2000,Element == "Production")
gr_dat <- gr_dat[!is.na(gr_dat$Value),]
gr_dat$Item <- as.character(gr_dat$Item)
for (fs in unique(gr_dat$Item)){
  d <- gr_dat[gr_dat$Item %in% fs,]
  if (sum(d$Value) == 0) next
  d <- d[d$Value > 0,]
  grate <- as.numeric((exp(coef(lm(log(d$Value) ~ Year, d))[2]) - 1) * 100)
  row <- data.frame(FAOST_CODE = fs,
                    growth_rate = grate)
  growth <- rbind(growth,row)
}
# items to exclude
growth <- growth[growth[[1]] != "Fruit, pome nes",] # leave the 1st, Fruit, pome nes , out from the table as pointed by Amy sep 18, 2015
rc <- growth %>% arrange(-growth_rate) %>% slice(1:5)


names(rc) <- c("","%")

print.xtable(xtable(rc, caption = "\\large{Fastest growing products based on quantities (average annual growth rate, 2000 to 2013)}", digits = c(0,0,0),
                    align= "l{\raggedright\arraybackslash}p{2.2cm}r"),
             type = table_type, table.placement = NULL, booktabs = TRUE,
             include.rownames = FALSE, size = "footnotesize", caption.placement = "top",
             html.table.attributes = 'class="table table-striped table-hover"')



## ---- P3cropproLEFT ----
# data
dat <- syb.df %>% filter(Year %in% c(2000,2012)) %>%  select(FAOST_CODE,Year,QV.NPCPV.CRPS.ID.SHP)

dat <- dat[!is.na(dat$QV.NPCPV.CRPS.ID.SHP),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

dat <- dat[which(dat[[region_to_report]]),]

# semi-standard data munging for two year dot-plots
# give name Value for value-col
names(dat)[names(dat)=="QV.NPCPV.CRPS.ID.SHP"] <- "Value"
# Plot only as many countries as there are for particular region, max 20
nro_latest_cases <- nrow(dat[dat$Year == max(dat$Year),])
if (nro_latest_cases < 20) {ncases <- nro_latest_cases} else ncases <- 20
dat <- arrange(dat, -Year, -Value)
# slice the data for both years
top2015 <- dat %>% slice(1:ncases) %>% dplyr::mutate(color = "2012")
top2000 <- dat %>% filter(FAOST_CODE %in% top2015$FAOST_CODE, Year == 2000) %>% dplyr::mutate(color = "2000")
dat_plot <- rbind(top2015,top2000)
# levels based on newest year
dat_plot$SHORT_NAME <- factor(dat_plot$SHORT_NAME, levels=arrange(top2015,Value)$SHORT_NAME)
###############

p <- ggplot(dat_plot, aes(x=SHORT_NAME,y=Value))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="constant 2004 - 2006 Int$")
p <- p + guides(color = guide_legend(nrow = 1))
p <- p + scale_y_continuous(labels=space) 
p

# Caption
caption_text <- "Top 20 crop producing countries in 2012 based on net per capita crop production value"


## ---- P3cropproRIGHT ----

# data
dat <- syb.df %>% filter(Year %in% c(2000,2012)) %>%  select(FAOST_CODE,Year,QV.GPCPV.FOOD.ID.SHP)

dat <- dat[!is.na(dat$QV.GPCPV.FOOD.ID.SHP),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

dat <- dat[which(dat[[region_to_report]]),]

# semi-standard data munging for two year dot-plots
# give name Value for value-col
names(dat)[names(dat)=="QV.GPCPV.FOOD.ID.SHP"] <- "Value"
# Plot only as many countries as there are for particular region, max 20
nro_latest_cases <- nrow(dat[dat$Year == max(dat$Year),])
if (nro_latest_cases < 20) {ncases <- nro_latest_cases} else ncases <- 20
dat <- arrange(dat, -Year, -Value)
# slice the data for both years
top2015 <- dat %>% slice(1:ncases) %>% dplyr::mutate(color = "2012")
top2000 <- dat %>% filter(FAOST_CODE %in% top2015$FAOST_CODE, Year == 2000) %>% dplyr::mutate(color = "2000")
dat_plot <- rbind(top2015,top2000)
# levels based on newest year
dat_plot$SHORT_NAME <- factor(dat_plot$SHORT_NAME, levels=arrange(top2015,Value)$SHORT_NAME)
###############

p <- ggplot(dat_plot, aes(x=SHORT_NAME,y=Value))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="constant 2004 - 2006 Int$")
p <- p + guides(color = guide_legend(nrow = 1))
# p <- p + scale_y_continuous(labels=space,breaks=c(1000,2000))
p

# Caption
caption_text <- "Top 20 food producing countries in 2012 based on net food per capita production value"

## ---- P3cropproBOTTOM ----
if (region_to_report == "RAF") dat <- syb.df %>% filter(Year >= 2000, FAOST_CODE %in% 12000) %>%
  select(FAOST_CODE,Area,Year,
         QC.PRD.CRLS.TN.NO,   # Cereals production (tonnes)
         QC.RHRV.CRLS.HA.NO,  # Cereals harvested area (ha)
         QC.YIELD.CRLS.HG.NO) # Cereals yield (hg/ha)
if (region_to_report == "RAP") dat <- syb.df %>% filter(Year >= 2000, FAOST_CODE %in% 13000) %>%
  select(FAOST_CODE,Area,Year,
         QC.PRD.CRLS.TN.NO,   # Cereals production (tonnes)
         QC.RHRV.CRLS.HA.NO,  # Cereals harvested area (ha)
         QC.YIELD.CRLS.HG.NO) # Cereals yield (hg/ha)
if (region_to_report == "REU") dat <- syb.df %>% filter(Year >= 2000, FAOST_CODE %in% 14000) %>%
  select(FAOST_CODE,Area,Year,
         QC.PRD.CRLS.TN.NO,   # Cereals production (tonnes)
         QC.RHRV.CRLS.HA.NO,  # Cereals harvested area (ha)
         QC.YIELD.CRLS.HG.NO) # Cereals yield (hg/ha)
if (region_to_report == "RNE") dat <- syb.df %>% filter(Year >= 2000, FAOST_CODE %in% 15000) %>%
  select(FAOST_CODE,Area,Year,
         QC.PRD.CRLS.TN.NO,   # Cereals production (tonnes)
         QC.RHRV.CRLS.HA.NO,  # Cereals harvested area (ha)
         QC.YIELD.CRLS.HG.NO) # Cereals yield (hg/ha)
if (region_to_report == "GLO") dat <- syb.df %>% filter(Year >= 2000, FAOST_CODE %in% 5000) %>%
  select(FAOST_CODE,Area,Year,
         QC.PRD.CRLS.TN.NO,   # Cereals production (tonnes)
         QC.RHRV.CRLS.HA.NO,  # Cereals harvested area (ha)
         QC.YIELD.CRLS.HG.NO) # Cereals yield (hg/ha)

# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

dat <- gather(dat,
              variable,
              value,
              4:6)
dat <- dat %>% filter(!is.na(value)) %>% arrange(variable,Year) %>% select(FAOST_CODE,Year,variable,value)

dat$variable <- as.character(dat$variable)

dat$grate <- as.numeric((exp(coef(lm(log(dat$value) ~ Year, dat))[2]) - 1) * 100)

dat_plot <- data.frame()
for (fs in unique(dat$variable)){
  d <- dat[dat$variable %in% fs,]
  if (sum(dat$value) == 0) next
  d <- d[d$value > 0,]
  grate <- as.numeric((exp(coef(lm(log(d$value) ~ Year, d))[2]) - 1) * 100)
  row <- data.frame(variable = fs,
                    growth_rate = grate,stringsAsFactors = FALSE)
  dat_plot <- rbind(dat_plot,row)
}

dat_plot$variable[dat_plot$variable == "QC.PRD.CRLS.TN.NO"] <- "Production"
dat_plot$variable[dat_plot$variable == "QC.RHRV.CRLS.HA.NO"] <- "Harvested area"
dat_plot$variable[dat_plot$variable == "QC.YIELD.CRLS.HG.NO"] <- "Yield"

p <- ggplot(dat_plot, aes(x=variable,y=growth_rate,fill=variable))
p <- p + geom_bar(stat="identity",position="dodge")
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, length(unique(dat_plot$variable)))[["Sub"]])
p <- p + labs(x="",y="percent")
p <- p + theme(legend.position = "none")
p

# Caption
caption_text <- "Average annual growth in cereals production (2000-13)"


## ---- P3cropproMAP ----
dat <- syb.df %>% filter(Year %in% 2013) %>% select(FAOST_CODE,
                                                    QV.NPCPV.CRPS.ID.SHP)

map.plot <- left_join(map.df,dat) # so that each country in the region will be filled (value/NA)

# Add region key and subset

map.plot <- map.plot[which(map.plot[[region_to_report]]),]

cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","QV.NPCPV.CRPS.ID.SHP")]
cat_data$value_cat <- categories(x=cat_data$QV.NPCPV.CRPS.ID.SHP, n=5, method="jenks")

map.plot <- left_join(map.plot,cat_data[c("FAOST_CODE","value_cat")])

# define map unit
map_unit <- "index"

create_map_here()

# Caption
caption_text <- "Crops, gross per capita production index (2004-06 = 100, 2013)"

#
#    ____
#   / ___| _ __  ___   _ __
#  | |    | '__|/ _ \ | '_ \
#  | |___ | |  | (_) || |_) |
#   \____||_|   \___/ | .__/
#                     |_|



## ---- P3cropTEXT ----
spread_title <- "Crops"
if (region_to_report == "RAF") short_text <- "Cereals, which include wheat, rice, barley, maize, rye, oats and millet, make up the majority of the production of the crop sector. They continue to be the most important food source for human consumption. Yet external factors, such as rising incomes and urbanization, are causing diets to shift towards diets that are higher in protein, fats and sugar. In addition, livestock and biofuel production have and will most likely grow at a faster rate than crop production. This is causing a shift away from crops, like wheat and rice, towards coarse grains and oilseeds to meet demands for food, feed and biofuel."
if (region_to_report == "RAP") short_text <- "Cereals, which include wheat, rice, barley, maize, rye, oats and millet, make up the majority of the production of the crop sector. They continue to be the most important food source for human consumption. Yet external factors, such as rising incomes and urbanization, are causing diets to shift towards diets that are higher in protein, fats and sugar. In addition, livestock and biofuel production have and will most likely grow at a faster rate than crop production. This is causing a shift away from crops, like wheat and rice, towards coarse grains and oilseeds to meet demands for food, feed and biofuel."
if (region_to_report == "REU") short_text <- "Cereals, which include wheat, rice, barley, maize, rye, oats and millet, make up the majority of the production of the crop sector. They continue to be the most important food source for human consumption. Yet external factors, such as rising incomes and urbanization, are causing diets to shift towards diets that are higher in protein, fats and sugar. In addition, livestock and biofuel production have and will most likely grow at a faster rate than crop production. This is causing a shift away from crops, like wheat and rice, towards coarse grains and oilseeds to meet demands for food, feed and biofuel."
if (region_to_report == "RNE") short_text <- "Cereals, which include wheat, rice, barley, maize, rye, oats and millet, make up the majority of the production of the crop sector. They continue to be the most important food source for human consumption. Yet external factors, such as rising incomes and urbanization, are causing diets to shift towards diets that are higher in protein, fats and sugar. In addition, livestock and biofuel production have and will most likely grow at a faster rate than crop production. This is causing a shift away from crops, like wheat and rice, towards coarse grains and oilseeds to meet demands for food, feed and biofuel."
if (region_to_report == "GLO") short_text <- "Cereals, which include wheat, rice, barley, maize, rye, oats and millet, make up the majority of the production of the crop sector. They continue to be the most important food source for human consumption. Yet external factors, such as rising incomes and urbanization, are causing diets to shift towards diets that are higher in protein, fats and sugar. In addition, livestock and biofuel production have and will most likely grow at a faster rate than crop production. This is causing a shift away from crops, like wheat and rice, towards coarse grains and oilseeds to meet demands for food, feed and biofuel."

## ---- P3cropData ----

# # This should be thought twice how to produce it for regional books!
# load(paste0(data.dir,"/Production_Crops_E_All_Data.RData"))
# names(dat)[names(dat)=="CountryCode"] <- "FAOST_CODE"
# dat <- dat[dat$Year > 1999,]
# 
# # Add region key and subset
# dat <- left_join(dat,region_key)

# This should be thought twice how to produce it for regional books!
if (!file.exists(paste0(data.dir,"/Production_Crops_E_All_Data_(Norm).csv"))){
  download.file("http://faostat3.fao.org/faostat-bulkdownloads/Production_Crops_E_All_Data_(Norm).zip",
                destfile = paste0(data.dir,"/Production_Crops_E_All_Data_(Norm).zip"))
  unzip(zipfile = paste0(data.dir,"/Production_Crops_E_All_Data_(Norm).zip"),
        exdir = data.dir)
  dat <- read_csv(paste0(data.dir,"/Production_Crops_E_All_Data_(Norm).csv"))
} else dat <- read_csv(paste0(data.dir,"/Production_Crops_E_All_Data_(Norm).csv"))

names(dat)[names(dat)=="Country Code"] <- "FAOST_CODE"
# Remove two items
dat <- dat[!dat$Item %in% c("Cereals (Rice Milled Eqv)","Vegetables Primary"),]


# dat$Value <- ifelse(dat$Unit %in% "1000 Head", dat$Value * 1000, dat$Value)
# Add region key and subset
dat <- left_join(dat,region_key)



## ---- P3cropTOPRIGHT ----
dat <- dat[which(dat[[region_to_report]]),]
d13 <- dat %>%  filter(Year == 2013, Element == "Production", FAOST_CODE < 5000) %>%
  filter(!grepl("Total",Item)) %>%
  group_by(Item) %>%
  dplyr::summarise(Value = sum(Value, na.rm = TRUE)) %>%
  arrange(-Value) %>%
  slice(1:5)
d00 <- dat %>% filter(Year == 2000, Element == "Production", Item %in% d13$Item ) %>%
  filter(!grepl("Total",Item)) %>%
  group_by(Item) %>%
  dplyr::summarise(Value = sum(Value, na.rm = TRUE)) %>%
  arrange(-Value) %>%
  slice(1:5)
gg <- merge(d00,d13,by="Item")
gg$Value.x <- gg$Value.x/1000
gg$Value.y <- gg$Value.y/1000
gg <- arrange(gg, -gg$Value.y)
names(gg) <- c("","2000", "2013")
gg[[2]] <- round(gg[[2]],0)
gg[[3]] <- round(gg[[3]],0)
gg[[2]]<- prettyNum(gg[[2]], big.mark=" ")
gg[[3]]<- prettyNum(gg[[3]], big.mark=" ")

print(xtable(gg, caption = "\\large{Top five items produced in 2013, thousand tonnes}", digits = c(0,0,0,0),
             align= "l{\raggedright\arraybackslash}p{1.0cm}rr"),
      type = table_type, table.placement = NULL,
      booktabs = TRUE, include.rownames = FALSE,
      size = "footnotesize", caption.placement = "top",
      html.table.attributes = 'class="table table-striped table-hover"')


## ---- P3cropLEFT ----

first <- as.character(gg[1,1])
# data

d <- dat %>% filter(Item %in% first, Element == "Production", Year %in% c(2000,2013)) %>%
  # select(FAOST_CODE,Year,Value,Unit,SHORT_NAME) %>%
  mutate(Value = Value * 1000) # into kilograms

# Add region key and subset

d <- d[d$FAOST_CODE != 348,]
d$SHORT_NAME[d$FAOST_CODE == 351] <- "China"

per_capita <- syb.df %>% filter(Year %in% c(2000,2013)) %>% select(FAOST_CODE,Year,OA.TPBS.POP.PPL.NO)

d <- left_join(d,per_capita)

d$Value <- d$Value / d$OA.TPBS.POP.PPL.NO
d <- d[!is.na(d$Value),]

d <- d[which(d[[region_to_report]]),]

# semi-standard data munging for two year dot-plots
# give name Value for value-col
# names(dat)[names(dat)=="QC.PRD.RICE.TN.SHP"] <- "Value"
# Plot only as many countries as there are for particular region, max 20
nro_latest_cases <- nrow(d[d$Year == max(d$Year),])
if (nro_latest_cases < 20) {ncases <- nro_latest_cases} else ncases <- 20
d <- arrange(d, -Year, -Value)
# slice the data for both years
top2015 <- d %>% slice(1:ncases) %>% dplyr::mutate(color = "2013")
top2000 <- d %>% filter(FAOST_CODE %in% top2015$FAOST_CODE, Year == 2000) %>% dplyr::mutate(color = "2000")
dat_plot <- rbind(top2015,top2000)
# levels based on newest year
dat_plot$SHORT_NAME <- factor(dat_plot$SHORT_NAME, levels=arrange(top2015,Value)$SHORT_NAME)
###############

p <- ggplot(dat_plot, aes(x=SHORT_NAME,y=Value))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="kg per capita")
p <- p + guides(color = guide_legend(nrow = 1))
p <- p + scale_y_continuous(labels=space) 
p

# Caption
caption_text <- paste("Top 20",tolower(first),"producing countries, per capita")


## ---- P3cropRIGHT ----

second <- as.character(gg[2,1])

d <- dat %>% filter(Item %in% second, Element == "Production", Year %in% c(2000,2013)) %>%
  # select(FAOST_CODE,Year,Value,Unit,SHORT_NAME) %>%
  mutate(Value = Value * 1000) # into kilograms


d <- d[d$FAOST_CODE != 348,]
d$SHORT_NAME[d$FAOST_CODE == 351] <- "China"

per_capita <- syb.df %>% filter(Year %in% c(2000,2013)) %>% select(FAOST_CODE,Year,OA.TPBS.POP.PPL.NO)

d <- left_join(d,per_capita)

d$Value <- d$Value / d$OA.TPBS.POP.PPL.NO
d <- d[!is.na(d$Value),]

d <- d[which(d[[region_to_report]]),]

# semi-standard data munging for two year dot-plots
# give name Value for value-col
# names(dat)[names(dat)=="QC.PRD.RICE.TN.SHP"] <- "Value"
# Plot only as many countries as there are for particular region, max 20
nro_latest_cases <- nrow(d[d$Year == max(d$Year),])
if (nro_latest_cases < 20) {ncases <- nro_latest_cases} else ncases <- 20
d <- arrange(d, -Year, -Value)
# slice the data for both years
top2015 <- d %>% slice(1:ncases) %>% dplyr::mutate(color = "2013")
top2000 <- d %>% filter(FAOST_CODE %in% top2015$FAOST_CODE, Year == 2000) %>% dplyr::mutate(color = "2000")
dat_plot <- rbind(top2015,top2000)
# levels based on newest year
dat_plot$SHORT_NAME <- factor(dat_plot$SHORT_NAME, levels=arrange(top2015,Value)$SHORT_NAME)
###############

p <- ggplot(dat_plot, aes(x=SHORT_NAME,y=Value))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="kg per capita")
p <- p + guides(color = guide_legend(nrow = 1))
p <- p + scale_y_continuous(labels=space) 
p

# Caption
caption_text <- paste("Top 20",tolower(second),"producing countries, per capita")


## ---- P3cropBOTTOM ----
if (region_to_report == "RAF") dat <- syb.df %>% filter(Year %in% 2000:2013, FAOST_CODE %in% 12001:12005) %>%
  select(SHORT_NAME,Area,Year,
         QC.YIELD.CRLS.HG.NO)   
if (region_to_report == "RAP") dat <- syb.df %>% filter(Year %in% 2000:2013, FAOST_CODE %in% 13001:13014) %>%
  select(SHORT_NAME,Area,Year,
         QC.YIELD.CRLS.HG.NO)   
if (region_to_report == "REU") dat <- syb.df %>% filter(Year %in% 2000:2014, FAOST_CODE %in% 14001:14007) %>%
  select(SHORT_NAME,Area,Year,
         QC.YIELD.CRLS.HG.NO)   
if (region_to_report == "RNE") dat <- syb.df %>% filter(Year %in% 2000:2012, FAOST_CODE %in% 15001:15003) %>%
  select(SHORT_NAME,Area,Year,
         QC.YIELD.CRLS.HG.NO)   
if (region_to_report == "GLO") dat <- syb.df %>% filter(Year %in% 2000:2012, FAOST_CODE %in% c(5100,5200,5300,5400,5500)) %>%
  select(SHORT_NAME,Area,Year,
         QC.YIELD.CRLS.HG.NO)   
dat_plot <- na.omit(dat)

p <- ggplot(data = dat_plot, aes(x = Year, y = QC.YIELD.CRLS.HG.NO,group=SHORT_NAME,color=SHORT_NAME))
p <- p + geom_line(size=1.1, alpha=.7)
p <- p + scale_color_manual(values = plot_colors(part = 1, length(unique(dat_plot$SHORT_NAME)))[["Sub"]])
p <- p + labs(y="hg/capita", x="")
p <- p + guides(color = guide_legend(nrow = 3))
p  <-p +  scale_x_continuous(breaks=c(2000,2003,2006,2009,2012))
p <- p + scale_y_continuous(labels=space) 
p

# Caption
caption_text <- "Cereals, yield"


## ---- P3cropMAP ----
dat <- syb.df %>% filter(Year %in% 2013) %>% select(FAOST_CODE,QC.PRD.CRLS.TN.SHP) %>% mutate(QC.PRD.CRLS.TN.SHP = QC.PRD.CRLS.TN.SHP * 1000)

dat <- dat[dat$FAOST_CODE != 351,]
dat$FAOST_CODE[dat$FAOST_CODE == 41] <- 351

map.plot <- left_join(map.df,dat) # so that each country in the region will be filled (value/NA)

# Add region key and subset

map.plot <- map.plot[which(map.plot[[region_to_report]]),]

cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","QC.PRD.CRLS.TN.SHP")]
cat_data$value_cat <- categories(x=cat_data$QC.PRD.CRLS.TN.SHP, n=5, method="jenks")

map.plot <- left_join(map.plot,cat_data[c("FAOST_CODE","value_cat")])

# define map unit
map_unit <- "kg/cap"

create_map_here()

# Caption
caption_text <- "Cereal production, kg/cap (2013)"



#   _      _                   _                _
#  | |    (_)__   __ ___  ___ | |_  ___    ___ | | __
#  | |    | |\ \ / // _ \/ __|| __|/ _ \  / __|| |/ /
#  | |___ | | \ V /|  __/\__ \| |_| (_) || (__ |   <
#  |_____||_|  \_/  \___||___/ \__|\___/  \___||_|\_\
#

## ---- P3livestockTEXT ----
spread_title <- "Livestock"
if (region_to_report == "RAF") short_text <- "The world food economy is being increasingly driven by the shift of diets towards animal-based products such as meat, milk and dairy. As a result, agriculture is being affected, not only through growth of livestock production, but also through linkages to other sectors that supply feeding stuffs, such as crops and fisheries. Globally livestock production is the largest user of agricultural land and therefore also leaves a significant imprint on the environment."
if (region_to_report == "RAP") short_text <- "The world food economy is being increasingly driven by the shift of diets towards animal-based products such as meat, milk and dairy. As a result, agriculture is being affected, not only through growth of livestock production, but also through linkages to other sectors that supply feeding stuffs, such as crops and fisheries. Globally livestock production is the largest user of agricultural land and therefore also leaves a significant imprint on the environment."
if (region_to_report == "REU") short_text <- "The world food economy is being increasingly driven by the shift of diets towards animal-based products such as meat, milk and dairy. As a result, agriculture is being affected, not only through growth of livestock production, but also through linkages to other sectors that supply feeding stuffs, such as crops and fisheries. Globally livestock production is the largest user of agricultural land and therefore also leaves a significant imprint on the environment."
if (region_to_report == "RNE") short_text <- "The world food economy is being increasingly driven by the shift of diets towards animal-based products such as meat, milk and dairy. As a result, agriculture is being affected, not only through growth of livestock production, but also through linkages to other sectors that supply feeding stuffs, such as crops and fisheries. Globally livestock production is the largest user of agricultural land and therefore also leaves a significant imprint on the environment."
if (region_to_report == "GLO") short_text <- "The world food economy is being increasingly driven by the shift of diets towards animal-based products such as meat, milk and dairy. As a result, agriculture is being affected, not only through growth of livestock production, but also through linkages to other sectors that supply feeding stuffs, such as crops and fisheries. Globally livestock production is the largest user of agricultural land and therefore also leaves a significant imprint on the environment."

## ---- P3livestockData ----

# This should be thought twice how to produce it for regional books!
if (!file.exists(paste0(data.dir,"/Production_Livestock_E_All_Data_(Norm).csv"))){
  download.file("http://faostat3.fao.org/faostat-bulkdownloads/Production_Livestock_E_All_Data_(Norm).zip",
                destfile = paste0(data.dir,"/Production_Livestock_E_All_Data_(Norm).zip"))
  unzip(zipfile = paste0(data.dir,"/Production_Livestock_E_All_Data_(Norm).zip"),
        exdir = data.dir)
  dat <- read_csv(paste0(data.dir,"/Production_Livestock_E_All_Data_(Norm).csv"))
} else dat <- read_csv(paste0(data.dir,"/Production_Livestock_E_All_Data_(Norm).csv"))

names(dat)[names(dat)=="Country Code"] <- "FAOST_CODE"

dat$Value <- ifelse(dat$Unit %in% "1000 Head", dat$Value * 1000, dat$Value)

# Add region key and subset
dat <- left_join(dat,region_key)

## ---- P3livestockTOPRIGHT ----
dat <- dat[which(dat[[region_to_report]]),]
d13 <- dat %>%  filter(Year %in% 2013, Unit %in% c("Head","1000 Head")) %>%
  filter(!grepl("Total",Item),
         !Item %in% c("Poultry Birds","Sheep and Goats","Cattle and Buffaloes")
         ) %>%
  group_by(Item) %>%
  dplyr::summarise(Value = sum(Value, na.rm = TRUE)) %>%
  arrange(-Value)
d00 <- dat %>%  filter(Year %in% 2000, Unit %in% c("Head","1000 Head")) %>%
  filter(!grepl("Total",Item),
         !Item %in% c("Poultry Birds","Sheep and Goats","Cattle and Buffaloes")
         ) %>%
  group_by(Item) %>%
  dplyr::summarise(Value = sum(Value, na.rm = TRUE)) %>%
  arrange(-Value)
gg <- merge(d00,d13,by="Item")
gg$Value.x <- gg$Value.x/1000
gg$Value.y <- gg$Value.y/1000
gg <- arrange(gg, -gg$Value.y)
gg <- gg[1:5,]
names(gg) <- c("","2000", "2013")
gg[[2]] <- round(gg[[2]],0)
gg[[3]] <- round(gg[[3]],0)
gg[[2]]<- prettyNum(gg[[2]], big.mark=" ")
gg[[3]]<- prettyNum(gg[[3]], big.mark=" ")

print.xtable(xtable(gg, caption = "\\large{Live animal number, top 5 in 2013 (thousand heads)}", digits = c(0,0,0,0),
                    align= "l{\raggedright\arraybackslash}p{1.0cm}rr"),
             type = table_type, table.placement = NULL, booktabs = TRUE,
             include.rownames = FALSE, size = "footnotesize", caption.placement = "top",
             html.table.attributes = 'class="table table-striped table-hover"')




## ---- P3livestockLEFT ----
# data
dat <- filter(syb.df, Year %in% 2012) %>% select(FAOST_CODE,Year,QL.PRD.MILK.TN.NO) %>%  mutate(QL.PRD.MILK.TN.NO = QL.PRD.MILK.TN.NO / 1000000)

# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[!is.na(dat$QL.PRD.MILK.TN.NO),]

dat <- dat[which(dat[[region_to_report]]),]

dat <- arrange(dat, -QL.PRD.MILK.TN.NO)
top10 <- dat %>% slice(1:10) %>% dplyr::mutate(color = "With highest values")
bot10 <- dat %>% slice( (nrow(dat)-9):nrow(dat)) %>% dplyr::mutate(color = "With lowest values")
dat_plot <- rbind(top10,bot10)

p <- ggplot(dat_plot, aes(x=reorder(SHORT_NAME, QL.PRD.MILK.TN.NO),y=QL.PRD.MILK.TN.NO))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="million tonnes")
p <- p + guides(color = guide_legend(nrow = 2))
p <- p + scale_y_continuous(labels=space) 
p

# Caption
caption_text <- "Total milk production, top and bottom 10 countries (2012)"

## ---- P3livestockRIGHT ----

dat <- filter(syb.df, Year %in% 2012) %>% select(FAOST_CODE,Year,QL.PRD.EGG.TN.NO) %>%  mutate(QL.PRD.EGG.TN.NO = QL.PRD.EGG.TN.NO / 1000000)

# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[!is.na(dat$QL.PRD.EGG.TN.NO),]

dat <- dat[which(dat[[region_to_report]]),]

dat <- arrange(dat, -QL.PRD.EGG.TN.NO)
top10 <- dat %>% slice(1:10) %>% dplyr::mutate(color = "With highest values")
bot10 <- dat %>% slice( (nrow(dat)-9):nrow(dat)) %>% dplyr::mutate(color = "With lowest values")
dat_plot <- rbind(top10,bot10)

p <- ggplot(dat_plot, aes(x=reorder(SHORT_NAME, QL.PRD.EGG.TN.NO),y=QL.PRD.EGG.TN.NO))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="mln tonnes")
p <- p + guides(color = guide_legend(nrow = 2))
p <- p + scale_y_continuous(labels=space) 
p

# Caption
caption_text <- "Total egg production, top and bottom 10 countries (2012)"


## ---- P3livestockBOTTOM ----
load(paste0(data.dir,"/Production_Livestock_E_All_Data.RData"))
names(dat)[names(dat)=="CountryCode"] <- "FAOST_CODE"
# Add region key and subset
dat <- left_join(dat,region_key)
dat <- dat[which(dat[[region_to_report]]),]

# DEFAULT GROUPING
df <- subgrouping(region_to_report = region_to_report)

# merge data with the region info
dat <- merge(dat,df[c("FAOST_CODE","subgroup")],by="FAOST_CODE")

d <- dat %>% filter(Item == "Pigs", Year %in% c(2000,2013)) %>% group_by(Year,subgroup) %>%
  dplyr::summarise(Value = sum(Value)) %>%
  dplyr::mutate(sum = sum(Value)) %>%
  dplyr::mutate(share = round(Value/sum*100,0)) %>%
  ungroup() %>%
  dplyr::mutate(subgroup = str_replace_all(subgroup, "\\ \\+\\ \\(Total\\)","")) %>%
  group_by(Year) %>%
  dplyr::mutate(pos = cumsum(share)- share/2)

p <- ggplot(d, aes(x=sum/2, y = share, fill = subgroup, width = sum))
p <- p + geom_bar(position="fill", stat="identity")
p <- p + facet_wrap(~Year)
p <- p + coord_polar("y")
p <- p + theme_minimal()
p <- p + theme(legend.position = "top")
p <- p + theme(axis.text = element_blank())
p <- p + theme(axis.title = element_blank())
p <- p + theme(axis.ticks = element_blank())
p <- p + theme(panel.grid.minor = element_blank())
p <- p + theme(panel.grid.major = element_blank())
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, length(unique(d$subgroup)))[["Sub"]])
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
p <- p + theme(plot.margin=unit(c(0,0,0,0),"mm"))
p <- p + guides(fill = guide_legend(nrow = 3))
p


# Caption
caption_text <- "Pig production (heads)"



## ---- P3livestockMAP ----
dat <- syb.df %>% filter(Year %in% 2012) %>% select(FAOST_CODE,
                                                    QA.STCK.CB.HD.SHL)
map.plot <- left_join(map.df,dat) # so that each country in the region will be filled (value/NA)

# Add region key and subset

map.plot <- map.plot[which(map.plot[[region_to_report]]),]

cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","QA.STCK.CB.HD.SHL")]
cat_data$value_cat <- categories(x=cat_data$QA.STCK.CB.HD.SHL, n=5, method="jenks",decimals = 1)

map.plot <- left_join(map.plot,cat_data[c("FAOST_CODE","value_cat")])

# define map unit
map_unit <- "head/ha"

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
caption_text <- "Cattle and buffaloes per ha of agricultural area, heads per ha (2012)"


#   _____  _       _                  _
#  |  ___|(_) ___ | |__    ___  _ __ (_)  ___  ___
#  | |_   | |/ __|| '_ \  / _ \| '__|| | / _ \/ __|
#  |  _|  | |\__ \| | | ||  __/| |   | ||  __/\__ \
#  |_|    |_||___/|_| |_| \___||_|   |_| \___||___/
#

## ---- P3fisheriesTEXT ----
spread_title <- "Fisheries"
if (region_to_report == "RAF") short_text <- "Fish is an important component in people’s diets, providing about 3.1 billion people with almost 20 percent of their average intake of animal protein. Capture fisheries continue to dominate world output, but aquaculture accounts for a growing percentage of total fish supply. Fishery sectors are particularly important in developing countries, providing both food and livelihoods"
if (region_to_report == "RAP") short_text <- "Fish is an important component in people’s diets, providing about 3.1 billion people with almost 20 percent of their average intake of animal protein. Capture fisheries continue to dominate world output, but aquaculture accounts for a growing percentage of total fish supply. Fishery sectors are particularly important in developing countries, providing both food and livelihoods"
if (region_to_report == "REU") short_text <- "Fish is an important component in people’s diets, providing about 3.1 billion people with almost 20 percent of their average intake of animal protein. Capture fisheries continue to dominate world output, but aquaculture accounts for a growing percentage of total fish supply. Fishery sectors are particularly important in developing countries, providing both food and livelihoods"
if (region_to_report == "RNE") short_text <- "Fish is an important component in people’s diets, providing about 3.1 billion people with almost 20 percent of their average intake of animal protein. Capture fisheries continue to dominate world output, but aquaculture accounts for a growing percentage of total fish supply. Fishery sectors are particularly important in developing countries, providing both food and livelihoods"
if (region_to_report == "GLO") short_text <- "Fish is an important component in people’s diets, providing about 3.1 billion people with almost 20 percent of their average intake of animal protein. Capture fisheries continue to dominate world output, but aquaculture accounts for a growing percentage of total fish supply. Fishery sectors are particularly important in developing countries, providing both food and livelihoods"

## ---- P3fisheriesData ----






## ---- P3fisheriesTOPRIGHT ----
if (region_to_report == "RAF") dat <- syb.df %>% filter(FAOST_CODE %in% 12000) %>%
  select(SHORT_NAME,Year,
         capture_fish_production,aquaculture_fish_production)
if (region_to_report == "RAP") dat <- syb.df %>% filter(FAOST_CODE %in% 13000) %>%
  select(SHORT_NAME,Year,
         capture_fish_production,aquaculture_fish_production)
if (region_to_report == "REU") dat <- syb.df %>% filter(FAOST_CODE %in% 14000) %>%
  select(SHORT_NAME,Year,
         capture_fish_production,aquaculture_fish_production)
if (region_to_report == "RNE") dat <- syb.df %>% filter(FAOST_CODE %in% 15000) %>%
  select(SHORT_NAME,Year,
         capture_fish_production,aquaculture_fish_production)
if (region_to_report == "GLO") dat <- syb.df %>% filter(FAOST_CODE %in% 5000) %>%
  select(SHORT_NAME,Year,
         capture_fish_production,aquaculture_fish_production)
dat <- na.omit(dat)

# Add region key and subset

dat <- gather(dat, variable, value, 3:4)
dat$fill[dat$variable == "capture_fish_production"] <- "From capture fishing"
dat$fill[dat$variable == "aquaculture_fish_production"] <- "From aquaculture"

dat$value <- dat$value / 1000000

dat_plot <- dat

p <- ggplot(dat_plot, aes(x=Year, y=value, color=fill))
p <- p + geom_line(size=1.1, alpha=.7)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + labs(x="",y="million tonnes")
p <- p + guides(color = guide_legend(nrow = 2))
p <- p + theme(axis.text.x = element_text(angle=45))
p

# Caption
caption_text <- "Fish production for aquaculture and capture fishing"


## ---- P3fisheriesLEFT ----
dat <- filter(syb.df, Year %in% 2012) %>% select(FAOST_CODE,Year,capture_fish_production) %>% mutate(capture_fish_production = capture_fish_production / 1000000)

# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[!is.na(dat$capture_fish_production),]

dat <- dat[which(dat[[region_to_report]]),]

dat <- arrange(dat, -capture_fish_production)
top10 <- dat %>% slice(1:10) %>% dplyr::mutate(color = "With highest values")
bot10 <- dat %>% slice( (nrow(dat)-9):nrow(dat)) %>% dplyr::mutate(color = "With lowest values")
dat_plot <- rbind(top10,bot10)

p <- ggplot(dat_plot, aes(x=reorder(SHORT_NAME, capture_fish_production),y=capture_fish_production))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="million tonnes")
p <- p + guides(color = guide_legend(nrow = 2))
p <- p + scale_y_continuous(labels=space) 
p

# Caption
caption_text <- "20 countries with highest value of capture production (2013)"


## ---- P3fisheriesRIGHT ----

dat <- filter(syb.df, Year %in% 2012) %>% select(FAOST_CODE,Year,aquaculture_fish_production) %>% mutate(aquaculture_fish_production = aquaculture_fish_production / 1000000)

# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[!is.na(dat$aquaculture_fish_production),]

dat <- dat[which(dat[[region_to_report]]),]

dat <- arrange(dat, -aquaculture_fish_production)
top10 <- dat %>% slice(1:10) %>% dplyr::mutate(color = "With highest values")
bot10 <- dat %>% slice( (nrow(dat)-9):nrow(dat)) %>% dplyr::mutate(color = "With lowest values")
dat_plot <- rbind(top10,bot10)

p <- ggplot(dat_plot, aes(x=reorder(SHORT_NAME, aquaculture_fish_production),y=aquaculture_fish_production))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="million tonnes")
p <- p + guides(color = guide_legend(nrow = 2))
p <- p + scale_y_continuous(labels=space) 
p

# Caption

caption_text <- "20 countries with highest value of aquaculture production (2013)"


## ---- P3fisheriesBOTTOM ----
if (region_to_report == "RAF") dat <- syb.df %>% filter(Year %in% 2000:2013, FAOST_CODE %in% 12001:12005) %>%
  select(SHORT_NAME,Area,Year,
         production_quantity_index)   # fish
if (region_to_report == "RAP") dat <- syb.df %>% filter(Year %in% 2000:2013, FAOST_CODE %in% 13001:13014) %>%
  select(SHORT_NAME,Area,Year,
         production_quantity_index)   # cereal export value
if (region_to_report == "REU") dat <- syb.df %>% filter(Year %in% 2000:2014, FAOST_CODE %in% 14001:14007) %>%
  select(SHORT_NAME,Area,Year,
         production_quantity_index)   # cereal export value
if (region_to_report == "RNE") dat <- syb.df %>% filter(Year %in% 2000:2012, FAOST_CODE %in% 15001:15003) %>%
  select(SHORT_NAME,Area,Year,
         production_quantity_index)   # cereal export value
if (region_to_report == "GLO") dat <- syb.df %>% filter(Year %in% 2000:2012, FAOST_CODE %in% c(5100,5200,5300,5400,5500)) %>%
  select(SHORT_NAME,Area,Year,
         production_quantity_index)   # cereal export value
dat_plot <- na.omit(dat)

p <- ggplot(data = dat_plot, aes(x = Year, y = production_quantity_index,group=SHORT_NAME,color=SHORT_NAME))
p <- p + geom_line(size=1.1, alpha=.7)
p <- p + scale_color_manual(values = plot_colors(part = 1, length(unique(dat_plot$SHORT_NAME)))[["Sub"]])
p <- p + labs(y="index", x="")
p <- p + guides(color = guide_legend(nrow = 3))
p  <-p +  scale_x_continuous(breaks=c(2000,2003,2006,2009,2012))
p

# Caption
caption_text <- "Fish production indices (2004-06=100)"



## ---- P3fisheriesMAP ----
dat <- filter(syb.df, Year %in% 2012) %>% select(FAOST_CODE,Year,net_fish_trade) %>%  mutate(net_fish_trade = net_fish_trade/ 1000)
dat <- dat[!is.na(dat$net_fish_trade),]

# dat <- dat[dat$FAOST_CODE != 351,]
# dat$FAOST_CODE[dat$FAOST_CODE == 41] <- 351

# set Robinson projection
map.plot <- left_join(map.df,dat) # so that each country in the region will be filled (value/NA)

# Subset
map.plot <- map.plot[which(map.plot[[region_to_report]]),]

cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","net_fish_trade")]
cat_data$value_cat <- categories(x=cat_data$net_fish_trade, n=5)

map.plot <- left_join(map.plot,cat_data[c("FAOST_CODE","value_cat")])

# define map unit
map_unit <- "1 000 US$"

create_map_here()

# Caption
caption_text <- "Net trade of fish in 2012"


#      _                 _               _  _                       _   _                    _
#     / \    __ _  _ __ (_)  ___  _   _ | || |_  _   _  _ __  __ _ | | | |_  _ __  __ _   __| |  ___
#    / _ \  / _` || '__|| | / __|| | | || || __|| | | || '__|/ _` || | | __|| '__|/ _` | / _` | / _ \
#   / ___ \| (_| || |   | || (__ | |_| || || |_ | |_| || |  | (_| || | | |_ | |  | (_| || (_| ||  __/
#  /_/   \_\\__, ||_|   |_| \___| \__,_||_| \__| \__,_||_|   \__,_||_|  \__||_|   \__,_| \__,_| \___|
#           |___/
#

## ---- P3tradeTEXT ----
spread_title <- "Agricultural trade"
if (region_to_report == "RAF") short_text <- "Most of the food consumed worldwide is grown locally. Where there is not enough local production to meet demand, trade has been instrumental in filling the gap. The scale of food and agricultural trade today is unprecedented. In real terms, the value of international flows has increased around fivefold over the past 50 years, reflecting global trends in the overall volume of trade. However, this expansion has been unevenly distributed across regions. High-income countries have generally outpaced developing regions, although several of the latter have comparative advantages in food and agricultural production."
if (region_to_report == "RAP") short_text <- "Most of the food consumed worldwide is grown locally. Where there is not enough local production to meet demand, trade has been instrumental in filling the gap. The scale of food and agricultural trade today is unprecedented. In real terms, the value of international flows has increased around fivefold over the past 50 years, reflecting global trends in the overall volume of trade. However, this expansion has been unevenly distributed across regions. High-income countries have generally outpaced developing regions, although several of the latter have comparative advantages in food and agricultural production."
if (region_to_report == "REU") short_text <- "Most of the food consumed worldwide is grown locally. Where there is not enough local production to meet demand, trade has been instrumental in filling the gap. The scale of food and agricultural trade today is unprecedented. In real terms, the value of international flows has increased around fivefold over the past 50 years, reflecting global trends in the overall volume of trade. However, this expansion has been unevenly distributed across regions. High-income countries have generally outpaced developing regions, although several of the latter have comparative advantages in food and agricultural production."
if (region_to_report == "RNE") short_text <- "Most of the food consumed worldwide is grown locally. Where there is not enough local production to meet demand, trade has been instrumental in filling the gap. The scale of food and agricultural trade today is unprecedented. In real terms, the value of international flows has increased around fivefold over the past 50 years, reflecting global trends in the overall volume of trade. However, this expansion has been unevenly distributed across regions. High-income countries have generally outpaced developing regions, although several of the latter have comparative advantages in food and agricultural production."
if (region_to_report == "GLO") short_text <- "Most of the food consumed worldwide is grown locally. Where there is not enough local production to meet demand, trade has been instrumental in filling the gap. The scale of food and agricultural trade today is unprecedented. In real terms, the value of international flows has increased around fivefold over the past 50 years, reflecting global trends in the overall volume of trade. However, this expansion has been unevenly distributed across regions. High-income countries have generally outpaced developing regions, although several of the latter have comparative advantages in food and agricultural production."

## ---- P3tradeData ----



## ---- P3tradeTOPRIGHT ----

if (region_to_report == "RAF") dat <- syb.df %>% filter(Year == 2012, FAOST_CODE %in% 12001:12005) %>%
  select(SHORT_NAME,Area,Year,
         TP.EXVAL.FOOD.USD.NO,   # food export value
         TP.IMVAL.FOOD.USD.NO) # food import value
if (region_to_report == "RAP") dat <- syb.df %>% filter(Year >= 2012, FAOST_CODE %in% 13001:13014) %>%
  select(SHORT_NAME,Area,Year,
         TP.EXVAL.FOOD.USD.NO,   # food export value
         TP.IMVAL.FOOD.USD.NO) # food import value
if (region_to_report == "REU") dat <- syb.df %>% filter(Year >= 2012, FAOST_CODE %in% 14001:14007) %>%
  select(SHORT_NAME,Area,Year,
         TP.EXVAL.FOOD.USD.NO,   # food export value
         TP.IMVAL.FOOD.USD.NO) # food import value
if (region_to_report == "RNE") dat <- syb.df %>% filter(Year >= 2012, FAOST_CODE %in% 15001:15003) %>%
  select(SHORT_NAME,Area,Year,
         TP.EXVAL.FOOD.USD.NO,   # food export value
         TP.IMVAL.FOOD.USD.NO) # food import value
if (region_to_report == "GLO") dat <- syb.df %>% filter(Year >= 2012, FAOST_CODE %in% c(5100,5200,5300,5400,5500)) %>%
  select(SHORT_NAME,Area,Year,
         TP.EXVAL.FOOD.USD.NO,   # food export value
         TP.IMVAL.FOOD.USD.NO) # food import value
dw <- na.omit(dat)

dw$TP.EXVAL.FOOD.USD.NO <- dw$TP.EXVAL.FOOD.USD.NO / 1000000000
dw$TP.IMVAL.FOOD.USD.NO <- dw$TP.IMVAL.FOOD.USD.NO / 1000000000

dw <- dw[order(-dw$TP.IMVAL.FOOD.USD.NO),c("SHORT_NAME","TP.EXVAL.FOOD.USD.NO","TP.IMVAL.FOOD.USD.NO")]

dw <- head(dw, 7) # to work with RAP too

names(dw) <- c("","Export value", "Import value")

dw[[2]] <- round(dw[[2]],0)
dw[[3]] <- round(dw[[3]],0)
dw[[2]]<- prettyNum(dw[[2]], big.mark=" ")
dw[[3]]<- prettyNum(dw[[3]], big.mark=" ")

print.xtable(xtable(dw, caption = "\\large{Exports and Imports of food, million US\\$ (2012)}", digits = c(0,0,0,0),
                    align= "l{\raggedright\arraybackslash}p{1.2cm}rr"),
             type = table_type, table.placement = NULL, booktabs = TRUE, include.rownames = FALSE,
             size = "footnotesize", caption.placement = "top",
             html.table.attributes = 'class="table table-striped table-hover"')


## ---- P3tradeLEFT ----
# data
dat <- syb.df %>% filter(Year %in% c(2000,2012)) %>%  select(FAOST_CODE,Year,TP.IMVAL.FOOD.USD.NO) %>% mutate(TP.IMVAL.FOOD.USD.NO = TP.IMVAL.FOOD.USD.NO / 1000000000)

dat <- dat[!is.na(dat$TP.IMVAL.FOOD.USD.NO),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

dat <- dat[which(dat[[region_to_report]]),]

# semi-standard data munging for two year dot-plots
# give name Value for value-col
names(dat)[names(dat)=="TP.IMVAL.FOOD.USD.NO"] <- "Value"
# Plot only as many countries as there are for particular region, max 20
nro_latest_cases <- nrow(dat[dat$Year == max(dat$Year),])
if (nro_latest_cases < 20) {ncases <- nro_latest_cases} else ncases <- 20
dat <- arrange(dat, -Year, -Value)
# slice the data for both years
top2015 <- dat %>% slice(1:ncases) %>% dplyr::mutate(color = "2012")
top2000 <- dat %>% filter(FAOST_CODE %in% top2015$FAOST_CODE, Year == 2000) %>% dplyr::mutate(color = "2000")
dat_plot <- rbind(top2015,top2000)
# levels based on newest year
dat_plot$SHORT_NAME <- factor(dat_plot$SHORT_NAME, levels=arrange(top2015,Value)$SHORT_NAME)
###############

p <- ggplot(dat_plot, aes(x=SHORT_NAME,y=Value))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="billion US$")
p <- p + guides(color = guide_legend(nrow = 1))
p

# Caption
caption_text <- "Top food importing countries in 2012"

## ---- P3tradeRIGHT ----

# data
dat <- syb.df %>% filter(Year %in% c(2000,2012)) %>%  select(FAOST_CODE,Year,TP.EXVAL.FOOD.USD.NO) %>% mutate(TP.EXVAL.FOOD.USD.NO = TP.EXVAL.FOOD.USD.NO / 1000000000)

dat <- dat[!is.na(dat$TP.EXVAL.FOOD.USD.NO),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

dat <- dat[which(dat[[region_to_report]]),]

# semi-standard data munging for two year dot-plots
# give name Value for value-col
names(dat)[names(dat)=="TP.EXVAL.FOOD.USD.NO"] <- "Value"
# Plot only as many countries as there are for particular region, max 20
nro_latest_cases <- nrow(dat[dat$Year == max(dat$Year),])
if (nro_latest_cases < 20) {ncases <- nro_latest_cases} else ncases <- 20
dat <- arrange(dat, -Year, -Value)
# slice the data for both years
top2015 <- dat %>% slice(1:ncases) %>% dplyr::mutate(color = "2012")
top2000 <- dat %>% filter(FAOST_CODE %in% top2015$FAOST_CODE, Year == 2000) %>% dplyr::mutate(color = "2000")
dat_plot <- rbind(top2015,top2000)
# levels based on newest year
dat_plot$SHORT_NAME <- factor(dat_plot$SHORT_NAME, levels=arrange(top2015,Value)$SHORT_NAME)
###############

p <- ggplot(dat_plot, aes(x=SHORT_NAME,y=Value))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="billion US$")
p <- p + guides(color = guide_legend(nrow = 1))
p

# Caption
caption_text <- "Top food exporting countries in 2012"


## ---- P3tradeBOTTOM ----
if (region_to_report == "RAF") dat <- syb.df %>% filter(Year %in% 2000:2012, FAOST_CODE %in% 12001:12005) %>%
  select(SHORT_NAME,Area,Year,
         TP.EXVAL.CRLS.USD.NO)   # cereal export value
if (region_to_report == "RAP") dat <- syb.df %>% filter(Year %in% 2000:2012, FAOST_CODE %in% 13001:13014) %>%
  select(SHORT_NAME,Area,Year,
         TP.EXVAL.CRLS.USD.NO)   # cereal export value
if (region_to_report == "REU") dat <- syb.df %>% filter(Year %in% 2000:2012, FAOST_CODE %in% 14001:14007) %>%
  select(SHORT_NAME,Area,Year,
         TP.EXVAL.CRLS.USD.NO)   # cereal export value
if (region_to_report == "RNE") dat <- syb.df %>% filter(Year %in% 2000:2012, FAOST_CODE %in% 15001:15003) %>%
  select(SHORT_NAME,Area,Year,
         TP.EXVAL.CRLS.USD.NO)   # cereal export value
if (region_to_report == "GLO") dat <- syb.df %>% filter(Year %in% 2000:2012, FAOST_CODE %in% c(5100,5200,5300,5400,5500)) %>%
  select(SHORT_NAME,Area,Year,
         TP.EXVAL.CRLS.USD.NO)   # cereal export value
dat_plot <- na.omit(dat)

dat_plot$value <- dat_plot$TP.EXVAL.CRLS.USD.NO / 1000000000

p <- ggplot(data = dat_plot, aes(x = Year, y = value,group=SHORT_NAME,color=SHORT_NAME))
p <- p + geom_line(size=1.1, alpha=.7)
p <- p + scale_color_manual(values = plot_colors(part = 1, length(unique(dat_plot$SHORT_NAME)))[["Sub"]])
p <- p + labs(y="billion constant 2005 US$", x="")
p <- p + guides(color = guide_legend(nrow = 3))
p <-p +  scale_x_continuous(breaks=c(2000,2002,2004,2006,2008,2010,2012))
p

# Caption
caption_text <- "Cereal exports"

## ---- P3tradeMAP ----
dat <- syb.df %>% filter(Year %in% 2011) %>% select(FAOST_CODE,
                                                    TI.IMVAL.FOOD.IN.NO)

dat <- dat[dat$FAOST_CODE != 351,]
dat$FAOST_CODE[dat$FAOST_CODE == 41] <- 351

map.plot <- left_join(map.df,dat) # so that each country in the region will be filled (value/NA)

# Add region key and subset

map.plot <- map.plot[which(map.plot[[region_to_report]]),]

cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","TI.IMVAL.FOOD.IN.NO")]
cat_data$value_cat <- categories(x=cat_data$TI.IMVAL.FOOD.IN.NO, n=5, method="jenks")

map.plot <- left_join(map.plot,cat_data[c("FAOST_CODE","value_cat")])

# define map unit
map_unit <- "index"


create_map_here()

# Caption
caption_text <- "Import value index (2004-2006 = 100, 2011)"
