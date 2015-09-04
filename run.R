#################################################################################################
# This is the main script used to control the production of FAO statistical pocketbook workflow #
#
################################################################################################
rm(list=ls(all=TRUE)) 
gc()

#options(scipen=999) # disable scientific number formatting

# set root directory
root.dir <- "~/btsync/faosync/pocketbooks/regional15/"
setwd(root.dir)
# set data directory
data.dir <- "~/btsync/faosync/pocketbooks/GSPB15/database/"

# Stuff you DO edit
# ----------------------------------------------------------------------------------

## ---- chapters_to_include ----


regionS_to_report <- c(
#                       "GLO" # Global
                             # , "RAP" # Asia and the Pacific
#                              ,"RAF"  # Africa
#                              ,"REU" # Europe and Central Asia
#                              ,"RNE" # Near East and North Africa
#                              ,"LAC" # Latin America and the Caribbean
                                 "COF" # Coffee
                      )
include_foreword <- F
include_part1 <- F
include_part2 <- F
include_part3 <- F
include_part4 <- F
include_part5 <- T
include_part6 <- T
include_country_profiles <- F
include_definitions <- T

# To be uploaded for comments or not
upload_to_server <- F

# Upgrade the comparison tables
broke_into_images <- F

# just for troubleshooting
region_to_report <- "RAF"

############################################################
############################################################

#   _ _ _                    _           
#  | (_) |__  _ __ __ _ _ __(_) ___  ___ 
#  | | | '_ \| '__/ _` | '__| |/ _ \/ __|
#  | | | |_) | | | (_| | |  | |  __/\__ \
#  |_|_|_.__/|_|  \__,_|_|  |_|\___||___/
#                                       

## ---- load_libraries

library(readr)
library(readxl)
library(magrittr)
library(lazyeval)
library(FAOSTAT)
library(dplyr)
#library(plyr) # to run certain computations using ddply. Should get rid of this
library(tidyr)
library(stringr)
library(rgdal)
library(gisfao)
library(grid)
library(scales)
library(ggplot2)
library(wesanderson)
library(xtable)
library(extrafont)
loadfonts()

# Source functions

source(paste0(root.dir,"input/code/subgroupings.R"))
source(paste0(root.dir,"input/code/plot/create_map_here.R"))


# _                 _       _       _        
# | | ___   __ _  __| |   __| | __ _| |_ __ _ 
# | |/ _ \ / _` |/ _` |  / _` |/ _` | __/ _` |
# | | (_) | (_| | (_| | | (_| | (_| | || (_| |
# |_|\___/ \__,_|\__,_|  \__,_|\__,_|\__\__,_|
#                                              

## ---- load_data ----

# load FAOcountryprofile data 
FAOcountryProfile <- read_csv(paste0(root.dir,"./input/data/FAOcountryProfile.csv"))

# Recode the Short Name Variables
## Abbreviate names
FAOcountryProfile[FAOcountryProfile[, "FAO_TABLE_NAME"] == "Latin America and the Caribbean"          & !is.na(FAOcountryProfile[, "FAO_TABLE_NAME"]), "FAO_TABLE_NAME"] <- "Latin America\nand the Caribbean"
FAOcountryProfile[FAOcountryProfile[, "FAO_TABLE_NAME"] == "Developed countries"                      & !is.na(FAOcountryProfile[, "FAO_TABLE_NAME"]), "FAO_TABLE_NAME"] <- "Developed\ncountries"
FAOcountryProfile[FAOcountryProfile[, "FAO_TABLE_NAME"] == "Developing countries"                     & !is.na(FAOcountryProfile[, "FAO_TABLE_NAME"]), "FAO_TABLE_NAME"] <- "Developing\ncountries"

FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Saint Vincent and the Grenadines"             & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Saint Vincent\nand the\nGrenadines"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Antigua and Barbuda"                          & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Antigua and\nBarbuda"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Trinidad and Tobago"                          & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Trinidad and\nTobago"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Republic of Moldova"                          & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Republic of\nMoldova"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Saint Helena, Ascension and Tristan da Cunha" & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Saint Helena"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Northern Mariana Islands"                     & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "N. Mariana\nIslands"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Wallis and Futuna Islands"                    & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Wallis and\nFutuna Is."
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "United Arab Emirates"                         & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "United Arab\nEmirates"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Turks and Caicos Islands"                     & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Turks and\nCaicos Is."
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Central African Republic"                     & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Central African\nRepublic"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Sao Tome and Principe"                        & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Sao Tome and\nPrincipe"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "United States of America"                     & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "United States\nof America"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Iran (Islamic Republic of)"                   & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Iran\n(Islamic Republic of)"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Bosnia and Herzegovina"                       & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Bosnia and\nHerzegovina"
FAOcountryProfile["SHORT_NAME"][FAOcountryProfile[, "FAOST_CODE"] == 107]                                                                                        <- "Côte d'Ivoire"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Falkland Islands (Malvinas)"                  & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Falkland Islands\n(Malvinas)"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Papua New Guinea"                             & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Papua New\nGuinea"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "American Samoa"                               & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "American\nSamoa"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Western Sahara"                               & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Western\nSahara"

FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Union of Soviet Socialist Republic"           & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Soviet Union"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Micronesia (Federated States of)"             & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Micronesia"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Svalbard and Jan Mayen Islands"               & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Svalbard"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Occupied Palestinian Territory"               & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Occupied\nPalestinian Territory"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "United States Virgin Islands"                 & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "U.S. Virgin Islands"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Saint Pierre and Miquelon"                    & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Saint Pierre\nand Miquelon"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Serbia and Montenegro"                        & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Serbia and\nMontenegro"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Saint Kitts and Nevis"                        & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Saint Kitts\nand Nevis"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Netherlands Antilles"                         & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Netherlands\nAntilles"
#FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == ""                                            & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- ""

# load SYB data
load(paste0(data.dir,"Data/Processed/SYB2015-08-18.RData"))
syb.df <- SYB.df; rm(SYB.df)

syb.df <- merge(syb.df, FAOcountryProfile[, c("FAOST_CODE", "SHORT_NAME")], by = "FAOST_CODE", all.x = TRUE)

# Fill missing values in SHORT_NAME with FAO_TABLE_NAME
syb.df$SHORT_NAME <- ifelse(is.na(syb.df$SHORT_NAME), syb.df$FAO_TABLE_NAME, syb.df$SHORT_NAME)

# REMOVE Western Sahara
syb.df <- syb.df[syb.df$FAOST_CODE != 205, ]

## Chinas
syb.df[syb.df[, "FAOST_CODE"] %in% c(357), "Area"] <- "China 357"
syb.df[syb.df[, "FAOST_CODE"] %in% c(41), "Area"] <-  "China 41"
syb.df[syb.df[, "FAOST_CODE"] %in% c(128), "Area"] <- "Macau"
syb.df[syb.df[, "FAOST_CODE"] %in% c(96), "Area"] <-  "Hong Kong"
syb.df[syb.df[, "FAOST_CODE"] %in% c(214), "Area"] <- "Taiwan"
## Occupied Palestinian Territory
syb.df[syb.df[, "SHORT_NAME"] == "Occupied Palestinian Territory" & !is.na(syb.df[, "SHORT_NAME"]), "SHORT_NAME"] <-   "West Bank and\nGaza Strip"
syb.df[syb.df[, "FAO_TABLE_NAME"] == "Occupied Palestinian Territory" & !is.na(syb.df[, "FAO_TABLE_NAME"]), "FAO_TABLE_NAME"] <-  "West Bank and Gaza Strip"


#  __  __                     _         _         
# |  \/  |  __ _  _ __     __| |  __ _ | |_  __ _ 
# | |\/| | / _` || '_ \   / _` | / _` || __|/ _` |
# | |  | || (_| || |_) | | (_| || (_| || |_| (_| |
# |_|  |_| \__,_|| .__/   \__,_| \__,_| \__|\__,_|

## ---- map_data

shape <- spTransform(fao_world, CRS("+proj=robin"))

# Fortify the shape
shape$id <- rownames(shape@data)
map.points <- fortify(shape, region = "id")
map.df <- merge(map.points, shape, by = "id")
map.df$FAOST_CODE[map.df$FAOST_CODE %in% 41] <- 351


#   ____          __  _                                  _                    
#  |  _ \   ___  / _|(_) _ __    ___   _ __  ___   __ _ (_)  ___   _ __   ___ 
#  | | | | / _ \| |_ | || '_ \  / _ \ | '__|/ _ \ / _` || | / _ \ | '_ \ / __|
#  | |_| ||  __/|  _|| || | | ||  __/ | |  |  __/| (_| || || (_) || | | |\__ \
#  |____/  \___||_|  |_||_| |_| \___| |_|   \___| \__, ||_| \___/ |_| |_||___/
#                                                 |___/                       

source(paste0(root.dir,"input/code/define_regions.R"))


##############################################################
##############################################################
## Pppulation threshold - currently disabled 
#############################################################
# pop_threshold <- 120000 # 
# small_countries <- syb.df[syb.df$OA.TPBS.POP.PPL.NO <= pop_threshold,c("FAOST_CODE","Year","SHORT_NAME","OA.TPBS.POP.PPL.NO")]
# #small_countries <- small_countries[!duplicated(small_countries[c("FAOST_CODE")]),]
# small_countries <- small_countries[small_countries$Year %in% 2013,]
# small_countries_FAOST_CODE <- unique(small_countries$FAOST_CODE)
# small_countries_FAOST_CODE <- small_countries_FAOST_CODE[!is.na(small_countries_FAOST_CODE)]
# syb.df <- syb.df[!(syb.df$FAOST_CODE %in% small_countries_FAOST_CODE), ]
# 
# na_countries <- syb.df[is.na(syb.df$OA.TPBS.POP.PPL.NO),c("FAOST_CODE","Year","SHORT_NAME","OA.TPBS.POP.PPL.NO")]
# na_countries <- na_countries[na_countries$Year %in% 2013,]
# na_countries_FAOST_CODE <- unique(na_countries$FAOST_CODE)
# syb.df <- syb.df[!(syb.df$FAOST_CODE %in% na_countries_FAOST_CODE), ]


source(paste0(root.dir,"input/code/process_the_book.R"))