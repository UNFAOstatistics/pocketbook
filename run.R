#################################################################################################
# This is the main script used to control the production of FAO statistical pocketbook workflow #
################################################################################################

options(scipen=999) # disable scientific number formatting

# set root directory
root.dir <- "~/faosync/pocketbooks/pocketbook/"

setwd(root.dir)
# set data directory
data.dir <- paste0(root.dir,"/input/data/database/")

# Stuff you DO edit
# ----------------------------------------------------------------------------------

## ---- chapters_to_include ----
regionS_to_report <- c(
                      # "GLO" # Global
                          # ,"RAP" # Asia and the Pacific
                         # "RAF"  # Africa
                      "REU" # Europe and Central Asia
                        # ,"RNE" # Near East and North Africa
                        # "COF" # Coffee
                        #,"LAC" # Latin America and the Caribbean
                      )
## Language
rulang <- T
itlang <- F
filang <- F

############################################################
# For print or for web or a4-print (in-house)
output_type <- "web" # web//a4g
# output_type <- "print" # web/print/a4
# output_type <- "a4" # web/print/a4


# Parts to include/exclude
# -------------------------------(heads)
include_covers           <- T
include_timestamp        <- T
include_disclaimer       <- T
include_foreword         <- T
include_introduction     <- T
include_acknowledgements <- T
include_overview_map     <- T
include_overview_tbl     <- T # do not include for coffee book
# -------------------------------
include_part1        <- T
include_part2        <- T
include_part3        <- T
include_part4        <- T
include_part5        <- F
include_part6        <- F
# include_part7        <- F # just a placeholder
# include_part8        <- F # just a placeholder
# include_part9        <- F # just a placeholder
# include_part10       <- F # just a placeholder
# -------------------------------
include_country_profiles <- T
include_definitions      <- T
# --------------------------- ----
# Upgrade the comparison tables 
broke_all_into_images         <- F
broke_only_tables_into_images <- F
broke_rus_translation_images  <- F
# -------------------------------
# To be uploaded for comments or not91830
upload_pdfs_to_server   <- F
upload_images_to_server <- F
# ------------------------------
# for latex tables etc. latex specific stuff
table_type <- "latex"

# special characters 
dag_char <- "\\textsuperscript{\\dag}"
ddag_char <- "\\textsuperscript{\\ddag}"


# just for troubleshooting
# region_to_report <- "RAF"

############################################################
# CACHE

cache_foreword <- F
cache_overview_map <- F
cache_overview_tbl <- F
cache_part1 <- F
cache_part2 <- F
cache_part3 <- F
cache_part4 <- F
cache_part5 <- F
cache_part6 <- F
cache_country_profiles <- F
cache_definitions <- F

###########################################################
# Data - Load SYB
# load("~/faosync/pocketbooks/pocketbook_database/output_data/2017-03-30-14/SYB2017-03-30-14.RData")
load("~/faosync/pocketbooks/pocketbook_database/output_data/2017-04-02-12/SYB2017-04-02-12.RData")

source("../pocketbook_database/code/read_functions/ReadMetadata.R")
meta.lst <- ReadMetadata(file = "../pocketbook_database/input_data/Metadata2015.csv", 
                         encoding = "UTF-8")
meta_full <- meta.lst[["FULL"]]
full_meta <- readRDS("~/local_data/faostat/metadata/meta_faostat.RDS")
csv_data <- readRDS("~/local_data/faostat/metadata/csv_data.RDS")

## Process the production data manuyally

if (!file.exists("~/local_data/faostat/temp/production.RDS")){
  
  dir.create("~/local_data/faostat/temp/", recursive = TRUE, showWarnings = FALSE)
  full_meta <- readRDS("~/local_data/faostat/metadata/meta_faostat.RDS")
  csv_data <- readRDS("~/local_data/faostat/metadata/csv_data.RDS")
  fao_bulk <- readRDS("~/local_data/faostat/rds/faostat.RDS")
  fao_bulk$subcat <- csv_data$subcat[match(fao_bulk$id, csv_data$id)]
  fao_bulk$FAOST_CODE <- fao_bulk$countrycode
  fao_bulk$Year <- fao_bulk$year
  fao_bulk %>% 
    filter(subcat %in% "production_crops_e_all_data_(normalized)") %>% 
    saveRDS(., "~/local_data/faostat/temp/production.RDS")
  fao_bulk %>% 
    filter(subcat %in% "production_livestock_e_all_data_(normalized)") %>% 
    saveRDS(., "~/local_data/faostat/temp/livestockproduction.RDS")
}




############################################################
############################################################

# _                       _       _   _                 
# | |_ _ __ __ _ _ __  ___| | __ _| |_(_) ___  _ __  ___ 
# | __| '__/ _` | '_ \/ __| |/ _` | __| |/ _ \| '_ \/ __|
# | |_| | | (_| | | | \__ \ | (_| | |_| | (_) | | | \__ \
#  \__|_|  \__,_|_| |_|___/_|\__,_|\__|_|\___/|_| |_|___/
# 
# Function for translations

translate_subgroups <- function(var, isfactor=FALSE,add_row_breaks=TRUE,abbreviate=TRUE){
  
  if (abbreviate & rulang){
    
    if (isfactor){
      
      levels(var)[levels(var) %in% "Andorra Israel Monaco and San Marino"] <- "Андорра, Израиль, \nМонако и Сан-Марино"
      levels(var)[levels(var) %in% "South Eastern Europe"] <- "Юго-Восточная \nЕвропа"
      levels(var)[levels(var) %in% "Caucasus and Turkey"] <- "Кавказ и \nТурция"
      levels(var)[levels(var) %in% "EU Other and EFTA"] <- "Др. cтр. \nЕС и ЕАСТ"
      levels(var)[levels(var) %in% "EU other and EFTA"] <- "Др. cтр. \nЕС и ЕАСТ"
      levels(var)[levels(var) %in% "CIS Europe"] <- "СНГ Европа"
      levels(var)[levels(var) %in% "EU Central Eastern"] <- "Центр. и Вост. \nчасть ЕС"
      levels(var)[levels(var) %in% "EU Central and Eastern"] <- "Центр. и Вост. \nчасть ЕС"
      levels(var)[levels(var) %in% "Central Asia"] <- "Центральная \nАзия"
      levels(var)[levels(var) %in% "Europe and Central Asia"] <- "Европа и \nЦентральная Азия"
      
    } else{
      
      var[var %in% "Andorra Israel Monaco and San Marino"] <- "Андорра, Израиль, \nМонако и Сан-Марино"
      var[var %in% "South Eastern Europe"] <- "Юго-Восточная \nЕвропа"
      var[var %in% "Caucasus and Turkey"] <- "Кавказ и \nТурция"
      var[var %in% "EU Other and EFTA"] <- "Др. cтр. \nЕС и ЕАСТ"
      var[var %in% "EU other and EFTA"] <- "Др. cтр. \nЕС и ЕАСТ"
      var[var %in% "CIS Europe"] <- "СНГ Европа"
      var[var %in% "EU Central Eastern"] <- "Центр. и Вост. \nчасть ЕС"
      var[var %in% "EU Central and Eastern"] <- "Центр. и Вост. \nчасть ЕС"
      var[var %in% "Central Asia"] <- "Центральная \nАзия"
      var[var %in% "Europe and Central Asia"] <- "Европа и \nЦентральная Азия"
      
      
    }
    

    
  } else {
    
    if (add_row_breaks){
      
      if (!isfactor){
        if (rulang){
          var[var %in% "Andorra Israel Monaco and San Marino"] <- "Андорра, Израиль, Монако и Сан-Марино"
          var[var %in% "South Eastern Europe"] <- "Юго-Восточная \nЕвропа"
          var[var %in% "Caucasus and Turkey"] <- "Кавказ и \nТурция"
          var[var %in% "EU Other and EFTA"] <- "Другие страны ЕС \nи ЕАСТ"
          var[var %in% "EU other and EFTA"] <- "Другие страны ЕС \nи ЕАСТ"
          var[var %in% "CIS Europe"] <- "СНГ Европа"
          var[var %in% "EU Central Eastern"] <- "Центральная и \nВосточная часть ЕС"
          var[var %in% "EU Central and Eastern"] <- "Центральная и \nВосточная часть ЕС"
          var[var %in% "Central Asia"] <- "Центральная \nАзия"
          var[var %in% "Europe and Central Asia"] <- "Европа и \nЦентральная Азия"
        }
        if (filang){
          var[var %in% "Andorra Israel Monaco and San Marino"] <- "Andorra, Israel, Monaco ja San Marino"
          var[var %in% "South Eastern Europe"] <- "Eteläinen Itä-Eurooppa"
          var[var %in% "Caucasus and Turkey"] <- "Kaukasus ja Turkki"
          var[var %in% "EU Other and EFTA"] <- "EU muu ja EFTA"
          var[var %in% "CIS Europe"] <- "IVY Eurooppa"
          var[var %in% "EU Central Eastern"] <- "EU keskinen ja itäinen"
          var[var %in% "Central Asia"] <- "Keski-Aasia"
        }
      } else {
        if (rulang){
          levels(var)[levels(var) %in% "Andorra Israel Monaco and San Marino"] <- "Андорра, Израиль, \nМонако и Сан-Марино"
          levels(var)[levels(var) %in% "South Eastern Europe"] <- "Юго-Восточная \nЕвропа"
          levels(var)[levels(var) %in% "Caucasus and Turkey"] <- "Кавказ \nи Турция"
          levels(var)[levels(var) %in% "EU Other and EFTA"] <- "Другие страны ЕС \nи ЕАСТ"
          levels(var)[levels(var) %in% "EU other and EFTA"] <- "Другие страны ЕС \nи ЕАСТ"
          levels(var)[levels(var) %in% "CIS Europe"] <- "СНГ Европа"
          levels(var)[levels(var) %in% "EU Central Eastern"] <- "Центральная и \nВосточная часть ЕС"
          levels(var)[levels(var) %in% "EU Central and Eastern"] <- "Центральная и \nВосточная часть ЕС"
          levels(var)[levels(var) %in% "Central Asia"] <- "Центральная \nАзия"
          levels(var)[levels(var) %in% "Europe and Central Asia"] <- "Европа и \nЦентральная Азия"
        }
      }
    } else {
      
      if (!isfactor){
        if (rulang){
          var[var %in% "Andorra Israel Monaco and San Marino"] <- "Андорра, Израиль, Монако и Сан-Марино"
          var[var %in% "South Eastern Europe"] <- "Юго-Восточная Европа"
          var[var %in% "Caucasus and Turkey"] <- "Кавказ и Турция"
          var[var %in% "EU Other and EFTA"] <- "Другие страны ЕС и ЕАСТ"
          var[var %in% "EU other and EFTA"] <- "Другие страны ЕС и ЕАСТ"
          var[var %in% "CIS Europe"] <- "СНГ Европа"
          var[var %in% "EU Central Eastern"] <- "Центральная и Восточная часть ЕС"
          var[var %in% "EU Central and Eastern"] <- "Центральная и Восточная часть ЕС"
          var[var %in% "Central Asia"] <- "Центральная Азия"
          var[var %in% "Europe and Central Asia"] <- "Европа и Центральная Азия"
        }
        if (filang){
          var[var %in% "Andorra Israel Monaco and San Marino"] <- "Andorra, Israel, Monaco ja San Marino"
          var[var %in% "South Eastern Europe"] <- "Eteläinen Itä-Eurooppa"
          var[var %in% "Caucasus and Turkey"] <- "Kaukasus ja Turkki"
          var[var %in% "EU Other and EFTA"] <- "EU muu ja EFTA"
          var[var %in% "CIS Europe"] <- "IVY Eurooppa"
          var[var %in% "EU Central Eastern"] <- "EU keskinen ja itäinen"
          var[var %in% "Central Asia"] <- "Keski-Aasia"
        }
      } else {
        if (rulang){
          levels(var)[levels(var) %in% "Andorra Israel Monaco and San Marino"] <- "Андорра, Израиль, Монако и Сан-Марино"
          levels(var)[levels(var) %in% "South Eastern Europe"] <- "Юго-Восточная Европа"
          levels(var)[levels(var) %in% "Caucasus and Turkey"] <- "Кавказ и Турция"
          levels(var)[levels(var) %in% "EU Other and EFTA"] <- "Другие страны ЕС и ЕАСТ"
          levels(var)[levels(var) %in% "EU other and EFTA"] <- "Другие страны ЕС и ЕАСТ"
          levels(var)[levels(var) %in% "CIS Europe"] <- "СНГ Европа"
          levels(var)[levels(var) %in% "EU Central Eastern"] <- "Центральная и Восточная часть ЕС"
          levels(var)[levels(var) %in% "EU Central and Eastern"] <- "Центральная и Восточная часть ЕС"
          levels(var)[levels(var) %in% "Central Asia"] <- "Центральная Азия"
          levels(var)[levels(var) %in% "Europe and Central Asia"] <- "Европа и Центральная Азия"
        }
      }
      
    }
    
  }
  

  return(var)
}



# if (region_to_report == "REU" & rulang) {
#   df$subgroup[df$subgroup %in% "Andorra Israel Monaco and San Marino"] <- "Андорра, Израиль, Монако и Сан-Марино"
#   df$subgroup[df$subgroup %in% "South Eastern Europe"] <- "Юго-Восточной Европы"
#   df$subgroup[df$subgroup %in% "Caucasus and Turkey"] <- "Кавказа и Турции"
#   df$subgroup[df$subgroup %in% "EU other and EFTA"] <- "ЕС другой и ЕАСТ"
#   df$subgroup[df$subgroup %in% "CIS Europe"] <- "СНГ Европа"
#   df$subgroup[df$subgroup %in% "EU Central and Eastern"] <- "Центральная и Восточная ЕС"
#   df$subgroup[df$subgroup %in% "Central Asia"] <- "Центральная Азия"
# }



#   _ _ _                    _
#  | (_) |__  _ __ __ _ _ __(_) ___  ___
#  | | | '_ \| '__/ _` | '__| |/ _ \/ __|
#  | | | |_) | | | (_| | |  | |  __/\__ \
#  |_|_|_.__/|_|  \__,_|_|  |_|\___||___/
#

## ---- load_libraries

library(knitr)
library(readxl)
library(magrittr)
library(lazyeval)
library(FAOSTAT)
#library(plyr) # to run certain computations using ddply. Should get rid of this
library(stringr)
library(rgdal)
library(gisfao)
library(grid)
library(scales)
library(WDI)
library(wesanderson)
library(xtable)
library(extrafont)
loadfonts()
library(forcats)
library(tidyverse)

# Source functions

source(paste0(root.dir,"/input/code/subgroupings.R"))
source(paste0(root.dir,"/input/code/plot/create_map_here.R"))


# _                 _       _       _
# | | ___   __ _  __| |   __| | __ _| |_ __ _
# | |/ _ \ / _` |/ _` |  / _` |/ _` | __/ _` |
# | | (_) | (_| | (_| | | (_| | (_| | || (_| |
# |_|\___/ \__,_|\__,_|  \__,_|\__,_|\__\__,_|
#

## ---- load_data ----

# load FAOcountryprofile data
FAOcountryProfile <- read_csv(paste0(root.dir,"/input/data/FAOcountryProfile.csv"))

# Russia is not part of FAO RAP Central Asia !!
FAOcountryProfile$FAO_RAP_SUB_REG[FAOcountryProfile[, "FAOST_CODE"] == 185] <- NA

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
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Laos"                         & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Lao PDR"



FAOcountryProfile$SHORT_NAME[FAOcountryProfile$FAOST_CODE == 116] <- "Korea, Dem Rep"


# North Korea
FAOcountryProfile$SHORT_NAME[FAOcountryProfile$FAOST_CODE == 116] <- "Korea, Dem Rep"


FAOcountryProfile$SHORT_NAME[FAOcountryProfile$FAOST_CODE == 154] <- "The former Yugoslav\nRepublic of Macedonia"



syb.df$FAOST_CODE[syb.df$FAOST_CODE %in% "LACregion"]         <- 11000
syb.df$FAOST_CODE[syb.df$FAOST_CODE %in% "LACCaribbean"]      <- 11001
syb.df$FAOST_CODE[syb.df$FAOST_CODE %in% "LACCentralAmerica"] <- 11002
syb.df$FAOST_CODE[syb.df$FAOST_CODE %in% "LACNorthAmerica"]   <- 11003
syb.df$FAOST_CODE[syb.df$FAOST_CODE %in% "LACSouthAmerica"]   <- 11004

syb.df$FAOST_CODE[syb.df$FAOST_CODE %in% "RAFregion"]         <- 12000
syb.df$FAOST_CODE[syb.df$FAOST_CODE %in% "RAFCentralAfrica"]  <- 12001
syb.df$FAOST_CODE[syb.df$FAOST_CODE %in% "RAFEastAfrica"]     <- 12002
syb.df$FAOST_CODE[syb.df$FAOST_CODE %in% "RAFNorthAfrica"]    <- 12003
syb.df$FAOST_CODE[syb.df$FAOST_CODE %in% "RAFSouthernAfrica"] <- 12004
syb.df$FAOST_CODE[syb.df$FAOST_CODE %in% "RAFWestAfrica"]     <- 12005


syb.df$FAOST_CODE[syb.df$FAOST_CODE %in% "RAPregion"]             <- 13000
syb.df$FAOST_CODE[syb.df$FAOST_CODE %in% "RAPEastAsia"]           <- 13001
# syb.df$FAOST_CODE[syb.df$FAOST_CODE %in% "RAPPacificIslands"]     <- 13002
syb.df$FAOST_CODE[syb.df$FAOST_CODE %in% "RAPSoutheastAsia"]      <- 13003
# syb.df$FAOST_CODE[syb.df$FAOST_CODE %in% "RAPSouthSouthwestAsia"] <- 13004
syb.df$FAOST_CODE[syb.df$FAOST_CODE %in% "RAPCentralAsia"]        <- 13005
syb.df$FAOST_CODE[syb.df$FAOST_CODE %in% "RAPOceania"]            <- 13006

syb.df$FAOST_CODE[syb.df$FAOST_CODE %in% "WAPregion"]             <- 23000 # FAO RAP member countries

# syb.df$FAOST_CODE[syb.df$FAOST_CODE %in% "RAPAustraliaNewZealand"]<- 13006
# syb.df$FAOST_CODE[syb.df$FAOST_CODE %in% "RAPFrance"]             <- 13007
# syb.df$FAOST_CODE[syb.df$FAOST_CODE %in% "RAPMelanesia"]          <- 13008
# syb.df$FAOST_CODE[syb.df$FAOST_CODE %in% "RAPMicronesia"]         <- 13009
# syb.df$FAOST_CODE[syb.df$FAOST_CODE %in% "RAPPolynesia"]          <- 13010
# syb.df$FAOST_CODE[syb.df$FAOST_CODE %in% "RAPRussianFederation"]  <- 13011
syb.df$FAOST_CODE[syb.df$FAOST_CODE %in% "RAPSouthernAsia"]       <- 13012
# syb.df$FAOST_CODE[syb.df$FAOST_CODE %in% "RAPUnitedStates"]       <- 13013
# syb.df$FAOST_CODE[syb.df$FAOST_CODE %in% "RAPWesternAsia"]        <- 13014

syb.df$FAOST_CODE[syb.df$FAOST_CODE %in% "RAPDeveloped"]          <- 13100
syb.df$FAOST_CODE[syb.df$FAOST_CODE %in% "RAPDevelopedCountries"] <- 13200
syb.df$FAOST_CODE[syb.df$FAOST_CODE %in% "RAPDeveloping"]         <- 13300

syb.df$FAOST_CODE[syb.df$FAOST_CODE %in% "REUregion"]                 <- 14000
syb.df$FAOST_CODE[syb.df$FAOST_CODE %in% "REUCaucAndTurkey"]          <- 14001
syb.df$FAOST_CODE[syb.df$FAOST_CODE %in% "REUCentralAsia"]            <- 14002
syb.df$FAOST_CODE[syb.df$FAOST_CODE %in% "REUEUCentralandEastern"]    <- 14003
syb.df$FAOST_CODE[syb.df$FAOST_CODE %in% "REUCISeurope"]              <- 14004
# syb.df$FAOST_CODE[syb.df$FAOST_CODE %in% "REUIsrael"]                 <- 14005
syb.df$FAOST_CODE[syb.df$FAOST_CODE %in% "REUAndorraIsraelMonacoandSanMarino"]                 <- 14999
# syb.df <- syb.df[syb.df$FAOST_CODE %in% "REUmisc",]
syb.df$FAOST_CODE[syb.df$FAOST_CODE %in% "REUEUOtherAndEFTA"]         <- 14006
syb.df$FAOST_CODE[syb.df$FAOST_CODE %in% "REUSouthEasternEurope"]     <- 14007

syb.df$FAOST_CODE[syb.df$FAOST_CODE %in% "RNEregion"] <- 15000
syb.df$FAOST_CODE[syb.df$FAOST_CODE %in% "RNEgccsy"]  <- 15001
syb.df$FAOST_CODE[syb.df$FAOST_CODE %in% "RNEna"]     <- 15002
syb.df$FAOST_CODE[syb.df$FAOST_CODE %in% "RNEome"]    <- 15003

# Sudan!!
# Sudan
syb.df <- syb.df[!(syb.df$FAOST_CODE == 276 & syb.df$Year < 2011),] # remove 
syb.df$FAOST_CODE <- ifelse(syb.df$FAOST_CODE == 206 & syb.df$Year < 2011, 276, syb.df$FAOST_CODE)

syb.df$FAOST_CODE <- factor(syb.df$FAOST_CODE)
syb.df$FAOST_CODE <- as.numeric(levels(syb.df$FAOST_CODE))[syb.df$FAOST_CODE]



syb.df <- merge(syb.df, FAOcountryProfile[, c("FAOST_CODE", "SHORT_NAME")], 
                by = "FAOST_CODE", all.x = TRUE)


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

syb.df$SHORT_NAME[syb.df$SHORT_NAME == "Gulf Cooperation Council States and Yemen"] <- "Gulf Cooperation\n Council States\n and Yemen"
syb.df$SHORT_NAME[syb.df$SHORT_NAME == "Other Near East countries"] <- "Other Near\n East countries"


# source(paste0(root.dir,"/input/code/process_fisheries_data.R"))

#   ____          __  _                                  _
#  |  _ \   ___  / _|(_) _ __    ___   _ __  ___   __ _ (_)  ___   _ __   ___
#  | | | | / _ \| |_ | || '_ \  / _ \ | '__|/ _ \ / _` || | / _ \ | '_ \ / __|
#  | |_| ||  __/|  _|| || | | ||  __/ | |  |  __/| (_| || || (_) || | | |\__ \
#  |____/  \___||_|  |_||_| |_| \___| |_|   \___| \__, ||_| \___/ |_| |_||___/
#                                                 |___/
fao_world <- gisfao::fao_world
source(paste0(root.dir,"/input/code/define_regions.R"))
# if you want to run aggregation tests
if (FALSE) save(region_key, file="~/faosync/pocketbooks/pocketbook_tests/data/region_key.RData")
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
map.df <- map.df[-22:-72]
map.df <- left_join(map.df,region_key)

##############################################################
##############################################################
## Pppulation threshold - currently disabled as the countries included are defined in "/input/code/define_regions.R"
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
# names(syb.df)

region_to_report="RNE" # debuggin

if (!exists("regional15_web")){ # because of the pocketbook_web
  source(paste0(root.dir,"/input/code/process_the_book.R"))
  # rm(list=ls(all=TRUE)) # 
  # gc()
  } 




