####################################################
####################################################
# Stuff You DO NOT edit
####################################################


# -- delete output/ -folder recursively
unlink(paste0(root.dir,"/output/process"), recursive = TRUE)
unlink(paste0(root.dir,"/output/jpg"), recursive = TRUE)

# -- Create output folder if not exists --- #
if (!file.exists(paste0(root.dir,"/output"))) dir.create(paste0(root.dir,"/output"))
if (!file.exists(paste0(root.dir,"/output/process"))) dir.create(paste0(root.dir,"/output/process"))
if (!file.exists(paste0(root.dir,"/output/pdf"))) dir.create(paste0(root.dir,"/output/pdf"))
if (!file.exists(paste0(root.dir,"/output/html"))) dir.create(paste0(root.dir,"/output/html"))
if (!file.exists(paste0(root.dir,"/output/jpg"))) dir.create(paste0(root.dir,"/output/jpg"))

## Copy .Rnw files into process/-folder
flist <- list.files(paste0(root.dir,"/input/"),
                    "+[.]Rnw$",
                    full.names = TRUE)
file.copy(flist, paste0(root.dir,"/output/process"), overwrite = TRUE)


## Copy .md into jpg folder
flist <- list.files(paste0(root.dir,"/input/templates/jpg_comparison"),
                    "+[.]md$",
                    full.names = TRUE)
file.copy(flist, paste0(root.dir,"/output/jpg"), overwrite = TRUE)


setwd(paste0(root.dir,"/output/process"))


###################################################################################3
#   _                           _                   _
#  | |     ___    ___   _ __   | |__    ___   __ _ (_) _ __   ___
#  | |    / _ \  / _ \ | '_ \  | '_ \  / _ \ / _` || || '_ \ / __|
#  | |___| (_) || (_) || |_) | | |_) ||  __/| (_| || || | | |\__ \
#  |_____|\___/  \___/ | .__/  |_.__/  \___| \__, ||_||_| |_||___/
#                      |_|                   |___/

## ---- loop_begins ----

for (region_to_report in regionS_to_report) {

  
  ## Copy everything from templates/-folder into process/folder
  flist <- list.files(paste0(root.dir,"/input/templates"),
                      recursive = TRUE,
                      include.dirs = TRUE,
                      full.names = TRUE)
  file.copy(flist, paste0(root.dir,"/output/process"), overwrite = TRUE)
  
  # Lets automate some changes in latex classes depending on what type of doc we want
  
  if (output_type %in% "print"){
    system(paste0("sed -i -- 's/PLACE_DIMENSIONS_HERE/ % PLACE_DIMENSIONS_HERE /g' ",root.dir,"/output/process/faoyearbook.cls"))
  }
  if (output_type %in% "web"){
    system(paste0("sed -i -- 's/PLACE_DIMENSIONS_HERE/ \\\\PassOptionsToPackage{papersize={10cm,21cm},top=1cm,bottom=1cm, left=1cm, right=1cm, twoside}{geometry} /g' ",root.dir,"/output/process/faoyearbook.cls"))
  }
  if (output_type %in% "a4"){
    system(paste0("sed -i -- 's/PLACE_DIMENSIONS_HERE/ \\\\PassOptionsToPackage{papersize={220mm,307mm},layout=a4paper,layouthoffset=5mm,layoutvoffset=5mm,twoside}{geometry} /g' ",root.dir,"/output/process/faoyearbook.cls"))
  }
  
  if (output_type %in% "print"){
    system(paste0("sed -i -- 's/PLACE_PRINT_OR_WEB_HERE/ \\\\documentclass[print]{faofactbook} /g' ",root.dir,"/output/process/syb_main.Rnw"))
  }
  if (output_type %in% c("web","a4")){
    system(paste0("sed -i -- 's/PLACE_PRINT_OR_WEB_HERE/ \\\\documentclass[twoside,web]{faofactbook} /g' ",root.dir,"/output/process/syb_main.Rnw"))
  }
  

  if (region_to_report == "COF"){
    system(paste0("sed -i -- 's/REPLACE_THIS _WITH_PROPER_COLOR/ \\\\colorbox{part6}{\\\\parbox{\\\\dimexpr\\\\columnwidth+2ex}{\\\\Large\\\\color{white}\\\\hypertarget{#1}{#1}}}\\\\par} /g' ",root.dir,"/output/process/faofactbook.cls"))
  } else {
    system(paste0("sed -i -- 's/REPLACE_THIS _WITH_PROPER_COLOR/ \\\\colorbox{FAOblue}{\\\\parbox{\\\\dimexpr\\\\columnwidth-2ex}{\\\\Large\\\\color{white}\\\\hypertarget{#1}{#1}}}\\\\par} /g' ",root.dir,"/output/process/faofactbook.cls"))
  }


  # region_to_report <- regionS_to_report[1]

  ### Which spreads
  spreads <- read_csv(paste0(root.dir,"/input/define_spreads.csv"))
  # subset to particular regions colunm
  spreads <- spreads[c("SPREAD",region_to_report)]

  # lets incorporate both condition the include_partX and spread based on Px found in spread id.
  spreadsP1  <- spreads[grep("P1",  spreads$SPREAD),]
  spreadsP2  <- spreads[grep("P2",  spreads$SPREAD),]
  spreadsP3  <- spreads[grep("P3",  spreads$SPREAD),]
  spreadsP4  <- spreads[grep("P4",  spreads$SPREAD),]
  spreadsP5  <- spreads[grep("P5",  spreads$SPREAD),]
  spreadsP6  <- spreads[grep("P6",  spreads$SPREAD),]
  # spreadsP7  <- spreads[grep("P7",  spreads$SPREAD),]
  # spreadsP8  <- spreads[grep("P8",  spreads$SPREAD),]
  # spreadsP9  <- spreads[grep("P9",  spreads$SPREAD),]
  # spreadsP10 <- spreads[grep("P10", spreads$SPREAD),]

  # if part marked not included, them give value 0 for each spread

  if (!include_part1) spreadsP1[2]   <- 0
  if (!include_part2) spreadsP2[2]   <- 0
  if (!include_part3) spreadsP3[2]   <- 0
  if (!include_part4) spreadsP4[2]   <- 0
  if (!include_part5) spreadsP5[2]   <- 0
  if (!include_part6) spreadsP6[2]   <- 0
  # if (!include_part7) spreadsP7[2]   <- 0
  # if (!include_part8) spreadsP8[2]   <- 0
  # if (!include_part9) spreadsP9[2]   <- 0
  # if (!include_part10) spreadsP10[2] <- 0

  # Create logical objects for each spread to be given for EVAL
  spreads_for_parts <- apropos("spreadsP")
  #
  for (spr in spreads_for_parts){

    for (i in 1:nrow(get(spr))) {
      if (get(spr)[[i,2]] == 0) value <- FALSE
      if (get(spr)[[i,2]] == 1) value <- TRUE
      assign(get(spr)[[i,1]],value,envir = globalenv())
    }

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

  # create jpg's for web comparisons


if (broke_only_tables_into_images){

    if (region_to_report == "RAF"){
      system("pdftk syb_main.pdf cat 7 output agg_pic.pdf") # subregion
      system("pdftk syb_main.pdf cat 30 output table_pic.pdf") # Ethiopia
    }
    if (region_to_report == "RAP"){
      system("pdftk syb_main.pdf cat 7 output agg_pic.pdf") # subregion
      system("pdftk syb_main.pdf cat 25 output table_pic.pdf") # Bangladesh
    }
    if (region_to_report == "RNE"){
      system("pdftk syb_main.pdf cat 7 output agg_pic.pdf") # subregion
      system("pdftk syb_main.pdf cat 22 output table_pic.pdf") # Saudi-Arabia
    }
    if (region_to_report == "REU"){
      system("pdftk syb_main.pdf cat 7 output agg_pic.pdf") # subregion
      system("pdftk syb_main.pdf cat 59 output table_pic.pdf") # Turkey
    }

  system(paste0("convert -density 150 agg_pic.pdf ",root.dir,"output/jpg/",region_to_report,"_agg",".jpg"))
  system(paste0("convert -density 150 table_pic.pdf ",root.dir,"output/jpg/",region_to_report,"_tbl",".jpg"))

}
if (broke_all_into_images) system(paste0("convert -density 150 syb_main.pdf ",root.dir,"output/jpg/",region_to_report,".jpg"))



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
flist <- flist[!grepl("barcode", flist, ignore.case = TRUE)]
flist <- flist[!grepl("book\\.", flist, ignore.case = TRUE)]
# Exclude the plain syb_main.pdf
flist <- flist[!grepl("syb_main.pdf", flist, ignore.case = TRUE)]

file.copy(flist, paste0(root.dir,"/output/pdf"), overwrite = TRUE)




if (broke_all_into_images | broke_only_tables_into_images){

  # copy the output -html's into the output/html-folder
  flist <- list.files(paste0(root.dir,"/output/process"),
                      "+[.]html$",
                      full.names = TRUE)
  file.copy(flist, paste0(root.dir,"/output/html"), overwrite = TRUE)

  # convert the index.md into html in jpog comparison

  system(paste0("pandoc ",root.dir,"output/jpg/regional_book_comparison.md -o ",     root.dir,"output/jpg/regional_book_comparison.html"))
  system(paste0("pandoc ",root.dir,"output/jpg/regional_book_comparison_reg.md -o ", root.dir,"output/jpg/regional_book_comparison_reg.html"))
  system(paste0("pandoc ",root.dir,"output/jpg/regional_table_comparison.md -o ",    root.dir,"output/jpg/regional_table_comparison.html"))
  if (region_to_report == "COF") system(paste0("pandoc ",root.dir,"output/jpg/coffee_comparison.md -o ",root.dir,"output/jpg/coffee_comparison.html"))


  system(paste0("pandoc ",root.dir,"output/jpg/regional_book_comparison_raf.md -o ",root.dir,"output/jpg/regional_book_comparison_raf.html"))
  system(paste0("pandoc ",root.dir,"output/jpg/regional_book_comparison_rap.md -o ",root.dir,"output/jpg/regional_book_comparison_rap.html"))
  system(paste0("pandoc ",root.dir,"output/jpg/regional_book_comparison_reu.md -o ",root.dir,"output/jpg/regional_book_comparison_reu.html"))
  system(paste0("pandoc ",root.dir,"output/jpg/regional_book_comparison_rne.md -o ",root.dir,"output/jpg/regional_book_comparison_rne.html"))


}



if (upload_pdfs_to_server) {

  #  upload the output pdf to kapsi
  pdfs <- list.files(paste0(root.dir,"/output/pdf"), full.names = TRUE)
  pdfs <- c(pdfs,"/home/aurelius/btsync/faosync/pocketbooks/pocketbook//output/pdf/agg_pic.pdf")
  pdfs <- pdfs[!(pdfs %in% c("/home/aurelius/btsync/faosync/pocketbooks/pocketbook//output/pdf/agg_pic.pdf",
                             "/home/aurelius/btsync/faosync/pocketbooks/pocketbook//output/pdf/table_pic.pdf"))]
  system(paste("scp",paste(pdfs, collapse=" ")," output muuankarski@kapsi.fi:public_html/fao/RSPB15"))
}


if (upload_images_to_server) {
    comparison <- list.files(paste0(root.dir,"/output/jpg"), full.names = TRUE)
    system(paste("scp",paste(comparison, collapse=" ")," output muuankarski@kapsi.fi:public_html/fao/RSPB15/comparison/"))
}


setwd(root.dir)
