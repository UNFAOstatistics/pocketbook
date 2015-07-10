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
# region_to_report <- "GLO" # Global

include_part1 <- TRUE
include_part2 <- TRUE
include_part3 <- FALSE
include_part4 <- FALSE
include_country_profiles <- FALSE
include_metadata <- FALSE

# set root directory
root.dir <- "~/btsync/fao_sync/pocketbooks/regional15/"
setwd(root.dir)

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

