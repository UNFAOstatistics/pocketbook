#################################################################################################
# This is the main script used to control the production of FAO statistical pocketbook workflow #
#
################################################################################################
rm(list=ls(all=TRUE)) 

# set root directory
root.dir <- "~/btsync/fao_sync/pocketbooks/regional15/"
setwd(root.dir)
# set data directory
data.dir <- "~/btsync/fao_sync/pocketbooks/GSPB15/database/"

# Stuff you DO edit
# ----------------------------------------------------------------------------------


## Chapters to include

regionS_to_report <- c(
                      "GLO" # Global
                          ,"RAP" # Asia and the Pacific
                         ,"RAF"  # Africa
                        ,"REU" # Europe and Central Asia
                         ,"RNE" # Near East and North Africa
                      #,"COF" # Coffee
                      )

include_part1 <- T
include_part2 <- T
include_part3 <- T
include_part4 <- T
include_country_profiles <- T
include_metadata <- F

# To be uploaded for comments or not
upload_to_server <- T

# just for troubleshooting
region_to_report <- "GLO"

############################################################
############################################################

#   _ _ _                    _           
#  | (_) |__  _ __ __ _ _ __(_) ___  ___ 
#  | | | '_ \| '__/ _` | '__| |/ _ \/ __|
#  | | | |_) | | | (_| | |  | |  __/\__ \
#  |_|_|_.__/|_|  \__,_|_|  |_|\___||___/
#                                       


library(readr)
library(magrittr)
library(xtable)
library(lazyeval)
library(tidyr)
library(stringr)
library(scales)
library(ggplot2)
library(grid)
library(DT)
library(gisfao)
require(grid)
library(plyr)
library(dplyr)
library(rgdal)
library(ggplot2)
library(gisfao)
library(FAOSTAT)
library(extrafont)
loadfonts()


# _                 _       _       _        
# | | ___   __ _  __| |   __| | __ _| |_ __ _ 
# | |/ _ \ / _` |/ _` |  / _` |/ _` | __/ _` |
# | | (_) | (_| | (_| | | (_| | (_| | || (_| |
# |_|\___/ \__,_|\__,_|  \__,_|\__,_|\__\__,_|
#                                              


# load FAOcountryprofile data 
FAOcountryProfile <- read_csv("./input/data/FAOcountryProfile.csv")

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
FAOcountryProfile[FAOcountryProfile[, "FAOST_CODE"] == 107                                            & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Côte d'Ivoire"
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

# syb.df$nchar <- nchar(syb.df$SHORT_NAME)
# f <- syb.df %>% select(SHORT_NAME,FAO_TABLE_NAME,FAOST_CODE,nchar) %>% arrange(-nchar)
# f <- f[!duplicated(f[c("FAO_TABLE_NAME")]),]
# h(f,40)

# load SYB data
load(paste0(data.dir,"Data/Processed/SYB.RData"))
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
syb.df[syb.df[, "SHORT_NAME"] == "Occupied Palestinian Territory" & 
         !is.na(syb.df[, "SHORT_NAME"]), "SHORT_NAME"] <-   "West Bank and\nGaza Strip"
syb.df[syb.df[, "FAO_TABLE_NAME"] == "Occupied Palestinian Territory" & 
         !is.na(syb.df[, "FAO_TABLE_NAME"]), "FAO_TABLE_NAME"] <-  "West Bank and Gaza Strip"


#  ____          __  _                                  _                    
# |  _ \   ___  / _|(_) _ __    ___   _ __  ___   __ _ (_)  ___   _ __   ___ 
# | | | | / _ \| |_ | || '_ \  / _ \ | '__|/ _ \ / _` || | / _ \ | '_ \ / __|
# | |_| ||  __/|  _|| || | | ||  __/ | |  |  __/| (_| || || (_) || | | |\__ \
# |____/  \___||_|  |_||_| |_| \___| |_|   \___| \__, ||_| \___/ |_| |_||___/
#                                                 |___/                       

source(paste0(root.dir,"input/data/defining_countries_and_regions.R"))

# Replace the ad-hoc regional grouping with the one we have created
myvars <- names(fao_world@data) %in% c("RAF","LAC","RAP","REU","RNE")
fao_world@data <- fao_world@data[!myvars]
fao_world@data <- merge(fao_world@data,region_key,by="FAOST_CODE",all.x=TRUE)

##############################################################
##############################################################
## Pppulation threshold
#############################################################
pop_threshold <- 120000 # 
small_countries <- syb.df[syb.df$OA.TPBS.POP.PPL.NO <= pop_threshold,c("FAOST_CODE","Year","SHORT_NAME","OA.TPBS.POP.PPL.NO")]
#small_countries <- small_countries[!duplicated(small_countries[c("FAOST_CODE")]),]
small_countries <- small_countries[small_countries$Year %in% 2013,]
small_countries_FAOST_CODE <- unique(small_countries$FAOST_CODE)
small_countries_FAOST_CODE <- small_countries_FAOST_CODE[!is.na(small_countries_FAOST_CODE)]
syb.df <- syb.df[!(syb.df$FAOST_CODE %in% small_countries_FAOST_CODE), ]

na_countries <- syb.df[is.na(syb.df$OA.TPBS.POP.PPL.NO),c("FAOST_CODE","Year","SHORT_NAME","OA.TPBS.POP.PPL.NO")]
na_countries <- na_countries[na_countries$Year %in% 2013,]
na_countries_FAOST_CODE <- unique(na_countries$FAOST_CODE)
syb.df <- syb.df[!(syb.df$FAOST_CODE %in% na_countries_FAOST_CODE), ]

####################################################
####################################################
# Stuff You DO NOT edit
####################################################

#region_to_report <- "GLO"

# -- delete output/ -folder recursively
unlink(paste0(root.dir,"/output/process"), recursive = TRUE)

# -- Create output folder if not exists --- #
if (!file.exists(paste0(root.dir,"/output"))) dir.create(paste0(root.dir,"/output"))
if (!file.exists(paste0(root.dir,"/output/process"))) dir.create(paste0(root.dir,"/output/process"))
if (!file.exists(paste0(root.dir,"/output/pdf"))) dir.create(paste0(root.dir,"/output/pdf"))
if (!file.exists(paste0(root.dir,"/output/html"))) dir.create(paste0(root.dir,"/output/html"))

## Copy .Rnw files into process/-folder
flist <- list.files(paste0(root.dir,"input/"), 
                    "+[.]Rnw$", 
                    full.names = TRUE)
file.copy(flist, paste0(root.dir,"/output/process"), overwrite = TRUE)

## Copy everything from templates/-folder into process/folder
flist <- list.files(paste0(root.dir,"input/templates"), 
                    recursive = TRUE, 
                    include.dirs = TRUE, 
                    full.names = TRUE)
file.copy(flist, paste0(root.dir,"/output/process"), overwrite = TRUE)

setwd(paste0(root.dir,"output/process"))


###################################################################################3
#   _                           _                   _             
#  | |     ___    ___   _ __   | |__    ___   __ _ (_) _ __   ___ 
#  | |    / _ \  / _ \ | '_ \  | '_ \  / _ \ / _` || || '_ \ / __|
#  | |___| (_) || (_) || |_) | | |_) ||  __/| (_| || || | | |\__ \
#  |_____|\___/  \___/ | .__/  |_.__/  \___| \__, ||_||_| |_||___/
#                      |_|                   |___/                


for (region_to_report in regionS_to_report) {
  

  
  ### Which spreads
  spreads <- read_csv(paste0(root.dir,"/input/define_spreads.csv"))
  # subset to particular regions colunm 
  spreads <- spreads[c("SPREAD",region_to_report)]
  
  # 
  for (i in 1:nrow(spreads)) {
    if (spreads[[i,2]] == 0) value <- FALSE
    if (spreads[[i,2]] == 1) value <- TRUE
    assign(spreads[[i,1]],value,envir = globalenv())
  }
  
  # remove figures from previous region
  unlink(paste0(root.dir,"/output/process/figure"), recursive = TRUE)

  knitr::knit("syb_main.Rnw")
  # Embed fonts
  flist <- list.files(paste0(root.dir,"output/process/figure"), 
                      recursive = TRUE, 
                      include.dirs = TRUE, 
                      full.names = TRUE)
  
  for (plot in flist) {
    embed_fonts(plot)
  }

  system(paste0("pdflatex ",root.dir,"output/process/syb_main.tex"))
  system(paste0("pdflatex ",root.dir,"output/process/syb_main.tex"))
  system(paste0("cp ",root.dir,"output/process/syb_main.pdf ",root.dir,"output/process/syb_main_",region_to_report,".pdf"))
#   
  # Technical report
#   knitr::purl("syb_part1.Rnw","syb_part1.R")
#   knitr::spin("syb_part1.R")
  
  # knitr::purl("syb_part2.Rnw","syb_part2.R")
  # knitr::spin("syb_part2.R")
  # 
  # knitr::purl("syb_part3.Rnw","syb_part3.R")
  # knitr::spin("syb_part3.R")
  # 
  # knitr::purl("syb_part4.Rnw","syb_part4.R")
  # knitr::spin("syb_part4.R")
}

# copy the output -pdf's into the output/pdf-folder
flist <- list.files(paste0(root.dir,"output/process"), 
                    "+[.]pdf$", 
                    full.names = TRUE)

 # Exclude the covers etc files from being copied
flist <- flist[!grepl("cover", flist, ignore.case = TRUE)]
flist <- flist[!grepl("disclaimer", flist, ignore.case = TRUE)]
# Exclude the plain syb_main.pdf
flist <- flist[!grepl("syb_main.pdf", flist, ignore.case = TRUE)]

file.copy(flist, paste0(root.dir,"/output/pdf"), overwrite = TRUE)

# copy the output -html's into the output/html-folder
flist <- list.files(paste0(root.dir,"output/process"), 
                    "+[.]html$", 
                    full.names = TRUE)

file.copy(flist, paste0(root.dir,"/output/html"), overwrite = TRUE)


if (upload_to_server) {
  
#   pdftk GSPB15.pdf cat 19-20 output undernourishment.pdf
#   pdftk GSPB15.pdf cat 15-16 output investment.pdf
#   pdftk GSPB15.pdf cat 43-44 output energy.pdf
#   pdftk GSPB15.pdf cat 60-61 output tables.pdf
#   pdftk GSPB15.pdf cat 51-232 output tables_all.pdf
#   pdftk GSPB15.pdf cat 1-50 output spreads.pdf
#   pdftk GSPB15.pdf cat 134 output table.pdf
#   pdftk GSPB15.pdf cat 233-end output definitions.pdf
#   convert -density 200 table.pdf table.jpg
#   pdftk GSPB15.pdf cat 179 output table2.pdf
#   convert -density 200 table2.pdf table2.jpg
#   
#   pandoc comment_charts.md -o comment_charts.html
#   pandoc comment_tables.md -o comment_tables.html
#   pandoc comment_captions.md -o comment_captions.html
#   pandoc comment_definitions.md -o comment_definitions.html
  
#  upload the output pdf to kapsi
  pdfs <- list.files(paste0(root.dir,"output/pdf"), full.names = TRUE)
  system(paste("scp",paste(pdfs, collapse=" ")," output muuankarski@kapsi.fi:public_html/fao/RSPB15"))

}



#!/bin/bash




setwd(root.dir)
