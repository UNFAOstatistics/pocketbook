#################################################################################################
# This is the main script used to control the production of FAO statistical pocketbook workflow #
#
################################################################################################
rm(list=ls(all=TRUE)) 

library(readr)


# Stuff you DO edit
# ----------------------------------------------------------------------------------


## Chapters to include

region_to_report <- "RAF" # Africa
# region_to_report <- "RAP" # Asia and the Pacific
# region_to_report <- "REU" # Europe and Central Asia
# region_to_report <- "RNE" # Near East and North Africa
# region_to_report <- "COF" # Coffee
#region_to_report <- "GLO" # Global

include_part1 <- TRUE
include_part2 <- FALSE
include_part3 <- FALSE
include_part4 <- FALSE
include_country_profiles <- FALSE
include_metadata <- FALSE

# set root directory
root.dir <- "~/btsync/fao_sync/pocketbooks/regional15/"
setwd(root.dir)
# set data directory
data.dir <- "~/btsync/fao_sync/pocketbooks/GSPB15/database/"

############################################################
############################################################
# Customise SYB data for pocketbooks
# 
############################################################

# load FAOcountryprofile data 
FAOcountryProfile <- read_csv("./input/data/FAOcountryProfile.csv")

# load SYB data
load(paste0(data.dir,"Data/Processed/SYB.RData"))
syb.df <- SYB.df; rm(SYB.df)

syb.df <- 
  merge(syb.df, FAOcountryProfile[, c("FAOST_CODE", "SHORT_NAME")],
        by = "FAOST_CODE", all.x = TRUE)

## Abbreviate names
syb.df[syb.df[, "FAO_TABLE_NAME"] == "Latin America and the Caribbean" & 
         !is.na(syb.df[, "FAO_TABLE_NAME"]), "FAO_TABLE_NAME"] <- "Latin America\nand the Caribbean"
syb.df[syb.df[, "FAO_TABLE_NAME"] == "Developed countries" & 
         !is.na(syb.df[, "FAO_TABLE_NAME"]), "FAO_TABLE_NAME"] <- "Developed\ncountries"
syb.df[syb.df[, "FAO_TABLE_NAME"] == "Developing countries" & 
         !is.na(syb.df[, "FAO_TABLE_NAME"]), "FAO_TABLE_NAME"] <- "Developing\ncountries"
syb.df[syb.df[, "SHORT_NAME"] == "Saint Vincent and the Grenadines" & 
         !is.na(syb.df[, "SHORT_NAME"]), "SHORT_NAME"] <- "Saint Vincent\nand the\nGrenadines"
syb.df[syb.df[, "SHORT_NAME"] == "Antigua and Barbuda" & 
         !is.na(syb.df[, "SHORT_NAME"]), "SHORT_NAME"] <- "Antigua and\nBarbuda"
syb.df[syb.df[, "SHORT_NAME"] == "Trinidad and Tobago" & 
         !is.na(syb.df[, "SHORT_NAME"]), "SHORT_NAME"] <-  "Trinidad and\nTobago"
syb.df[syb.df[, "SHORT_NAME"] == "Republic of Moldova" & 
         !is.na(syb.df[, "SHORT_NAME"]), "SHORT_NAME"] <-  "Republic of\nMoldova"
# syb.df[syb.df[, "SHORT_NAME"] == "Saint Helena, Ascension and Tristan da Cunha" & 
#           !is.na(syb.df[, "SHORT_NAME"]), "SHORT_NAME"] <-
#   "Saint Helena,\nAscension and\nTristan da Cunha"
syb.df[syb.df[, "SHORT_NAME"] == "Saint Helena, Ascension and Tristan da Cunha" & 
         !is.na(syb.df[, "SHORT_NAME"]), "SHORT_NAME"] <-   "Saint Helena"
syb.df[syb.df[, "SHORT_NAME"] == "Northern Mariana Islands" & 
         !is.na(syb.df[, "SHORT_NAME"]), "SHORT_NAME"] <- "N. Mariana\nIslands"
syb.df[syb.df[, "SHORT_NAME"] == "Wallis and Futuna Islands" & 
         !is.na(syb.df[, "SHORT_NAME"]), "SHORT_NAME"] <- "Wallis and\nFutuna Is."
syb.df[syb.df[, "SHORT_NAME"] == "United Arab Emirates" & 
         !is.na(syb.df[, "SHORT_NAME"]), "SHORT_NAME"] <-   "United Arab\nEmirates"
syb.df[syb.df[, "SHORT_NAME"] == "Turks and Caicos Islands" & 
         !is.na(syb.df[, "SHORT_NAME"]), "SHORT_NAME"] <-   "Turks and\nCaicos Is."
syb.df[syb.df[, "SHORT_NAME"] == "Central African Republic" & 
         !is.na(syb.df[, "SHORT_NAME"]), "SHORT_NAME"] <-   "Central African\nRepublic"
syb.df[syb.df[, "SHORT_NAME"] == "Sao Tome and Principe" & 
         !is.na(syb.df[, "SHORT_NAME"]), "SHORT_NAME"] <-   "Sao Tome and\nPrincipe"
syb.df[syb.df[, "SHORT_NAME"] == "United States of America" & 
         !is.na(syb.df[, "SHORT_NAME"]), "SHORT_NAME"] <-   "United States\nof America"
syb.df[syb.df[, "SHORT_NAME"] == "Iran (Islamic Republic of)" & 
         !is.na(syb.df[, "SHORT_NAME"]), "SHORT_NAME"] <-   "Iran\n(Islamic Republic of)"
syb.df[syb.df[, "SHORT_NAME"] == "Bosnia and Herzegovina" & 
         !is.na(syb.df[, "SHORT_NAME"]), "SHORT_NAME"] <-   "Bosnia and\nHerzegovina"
syb.df[syb.df[, "FAOST_CODE"] == "107" & 
         !is.na(syb.df[, "SHORT_NAME"]), "SHORT_NAME"] <-   "Côte d'Ivoire"
syb.df[syb.df[, "SHORT_NAME"] == "Falkland Islands (Malvinas)" & 
         !is.na(syb.df[, "SHORT_NAME"]), "SHORT_NAME"] <-   "Falkland Islands\n(Malvinas)"
syb.df[syb.df[, "SHORT_NAME"] == "Papua New Guinea" & 
         !is.na(syb.df[, "SHORT_NAME"]), "SHORT_NAME"] <-   "Papua New\nGuinea"
syb.df[syb.df[, "SHORT_NAME"] == "American Samoa" & 
         !is.na(syb.df[, "SHORT_NAME"]), "SHORT_NAME"] <-   "American\nSamoa"
syb.df[syb.df[, "SHORT_NAME"] == "Western Sahara" & 
         !is.na(syb.df[, "SHORT_NAME"]), "SHORT_NAME"] <-   "Western\nSahara"
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

## Conditions

### Which spreads
spreads <- read_csv("./input/define_spreads.csv")
# subset to particular regions colunm 
spreads <- spreads[c("SPREAD",region_to_report)]

# 
for (i in 1:nrow(spreads)) {
  if (spreads[[i,2]] == 0) value <- FALSE
  if (spreads[[i,2]] == 1) value <- TRUE
  assign(spreads[[i,1]],value,envir = globalenv())
}

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

knitr::knit("syb_main.Rnw")
system(paste0("pdflatex ",root.dir,"output/process/syb_main.tex"))
system(paste0("pdflatex ",root.dir,"output/process/syb_main.tex"))
system(paste0("cp ",root.dir,"output/process/syb_main.pdf ",root.dir,"output/process/syb_main_",region_to_report,".pdf"))

# Technical report
knitr::purl("syb_part1.Rnw","syb_part1.R")
knitr::spin("syb_part1.R")
# 
# knitr::purl("syb_part2.Rnw","syb_part2.R")
# knitr::spin("syb_part2.R")
# 
# knitr::purl("syb_part3.Rnw","syb_part3.R")
# knitr::spin("syb_part3.R")
# 
# knitr::purl("syb_part4.Rnw","syb_part4.R")
# knitr::spin("syb_part4.R")

#system(paste0("pdflatex ",root.dir,"output/process/syb_technical_report.tex"))

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


setwd(root.dir)


#!/bin/bash

#cd ~/btsync/fao_sync/pocketbooks/GSPB15/publication/
#Rscript -e "library(knitr); knit('GSPB15.Rnw')"
#pdflatex GSPB15.tex

#pdftk GSPB15.pdf cat 19-20 output undernourishment.pdf
#pdftk GSPB15.pdf cat 15-16 output investment.pdf
#pdftk GSPB15.pdf cat 43-44 output energy.pdf
#pdftk GSPB15.pdf cat 60-61 output tables.pdf
#pdftk GSPB15.pdf cat 51-232 output tables_all.pdf
#pdftk GSPB15.pdf cat 1-50 output spreads.pdf
#pdftk GSPB15.pdf cat 134 output table.pdf
#pdftk GSPB15.pdf cat 233-end output definitions.pdf
#convert -density 200 table.pdf table.jpg
#pdftk GSPB15.pdf cat 179 output table2.pdf
#convert -density 200 table2.pdf table2.jpg

#pandoc comment_charts.md -o comment_charts.html
#pandoc comment_tables.md -o comment_tables.html
#pandoc comment_captions.md -o comment_captions.html
#pandoc comment_definitions.md -o comment_definitions.html

# upload the output pdf to kapsi
# scp book_RAF.pdf book_RAP.pdf book_REU.pdf book_RNE.pdf output muuankarski@kapsi.fi:public_html/fao/RSPB15

