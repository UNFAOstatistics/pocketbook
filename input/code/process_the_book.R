####################################################
####################################################
# Stuff You DO NOT edit
####################################################


# -- delete output/ -folder recursively
unlink(paste0(root.dir,"/output/process"), recursive = TRUE, force=TRUE) 
# unlink(paste0(root.dir,"/output/jpg"), recursive = TRUE)# lets not do this in order for russian translation to work..

# -- Create output folder if not exists --- #
dir.create(paste0(root.dir,"/output/pdf"), showWarnings = FALSE, recursive = TRUE)
dir.create(paste0(root.dir,"/output/process"), showWarnings = FALSE, recursive = TRUE)

## Copy .Rnw files into process/-folder
flist <- list.files(paste0(root.dir,"/input/"),
                    "+[.]Rnw$",
                    full.names = TRUE)
file.copy(flist, paste0(root.dir,"/output/process"), overwrite = TRUE)

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
  
  # Font sizes for Russian translation
  if (rulang){
    system(paste0("sed -i -- 's/PLACE_FONTSIZE_NORMALSIZE_HERE/ \\\\@setfontsize\\\\normalsize{6\\\\p@}{6} /g' ",root.dir,"/output/process/faofactbook.cls"))
    system(paste0("sed -i -- 's/PLACE_FONTSIZE_SMALL_HERE/ \\\\@setfontsize\\\\small{5.5\\\\p@}{5.5} /g' ",root.dir,"/output/process/faofactbook.cls"))
    system(paste0("sed -i -- 's/PLACE_FONTSIZE_LARGE_HERE/ \\\\renewcommand\\\\large{\\\\@setfontsize\\\\large{6.2}{6.2}} /g' ",root.dir,"/output/process/faofactbook.cls"))
  } else {
    system(paste0("sed -i -- 's/PLACE_FONTSIZE_NORMALSIZE_HERE/ \\\\@setfontsize\\\\normalsize{6\\\\p@}{7} /g' ",root.dir,"/output/process/faofactbook.cls"))
    system(paste0("sed -i -- 's/PLACE_FONTSIZE_SMALL_HERE/ \\\\@setfontsize\\\\small{5.5\\\\p@}{6} /g' ",root.dir,"/output/process/faofactbook.cls"))
    system(paste0("sed -i -- 's/PLACE_FONTSIZE_LARGE_HERE/ \\\\renewcommand\\\\large{\\\\@setfontsize\\\\large{6.5}{7.0}} /g' ",root.dir,"/output/process/faofactbook.cls"))
  }
  
  
  # Definition environments for Russian translation
  if (rulang){
    system(paste0("sed -i -- 's/ADD_SOURCE_HERE/ \\\\def\\\\source_ru##1{\\\\par\\\\penalty10000\\\\emph{Источник: }##1\\\\par\\\\penalty10000}% /g' ",root.dir,"/output/process/faoyearbook.cls"))
    system(paste0("sed -i -- 's/ADD_OWNER_HERE/  \\\\def\\\\owner_ru##1{\\\\par\\\\penalty10000\\\\emph{Владелец авторского права: }##1\\\\par\\\\penalty10000}% /g' ",root.dir,"/output/process/faoyearbook.cls"))
    
  } else {
    system(paste0("sed -i -- 's/ADD_SOURCE_HERE/  \\\\def\\\\source##1{\\\\par\\\\penalty10000\\\\emph{Source: }##1\\\\par\\\\penalty10000}% /g' ",root.dir,"/output/process/faoyearbook.cls"))
    system(paste0("sed -i -- 's/ADD_OWNER_HERE/  \\\\def\\\\owner##1{\\\\par\\\\penalty10000\\\\emph{Owner: }##1\\\\par\\\\penalty10000}% /g' ",root.dir,"/output/process/faoyearbook.cls"))
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


  knitr::knit("syb_main.Rnw", encoding = "utf-8")
  # Embed fonts
  flist <- list.files(paste0(root.dir,"output/process/figure"),
                      recursive = TRUE,
                      include.dirs = TRUE,
                      full.names = TRUE)

  for (plot in flist) {
    embed_fonts(plot)
  }
  
  # Manual translations
  if ( rulang) system(paste0("sed -i -- 's/\\\\newcommand\\\\chartname{Chart}/ \\\\newcommand\\\\chartname{Диаграмма} /g' ",root.dir,"/output/process/faoyearbook.cls"))
  if ( rulang) system(paste0("sed -i -- 's/\\\\@makeschapterhead{\\\\normalfont \\\\LARGE Contents}/ \\\\@makeschapterhead{\\\\normalfont \\\\LARGE Содержание} /g' ",root.dir,"/output/process/faofactbook.cls"))
  
  

  # system(paste0("pdflatex ",root.dir,"output/process/syb_main.tex"))
  # system(paste0("pdflatex ",root.dir,"output/process/syb_main.tex"))
  system(paste0("pdflatex ",root.dir,"output/process/syb_main.tex"))
  system(paste0("pdflatex ",root.dir,"output/process/syb_main.tex"))
  if (!rulang) system(paste0("cp ",
                             root.dir,"output/process/syb_main.pdf ",
                             root.dir,"output/process/syb_main_",region_to_report,".pdf"))
  if (rulang) system(paste0("cp ",
                            root.dir,"output/process/syb_main.pdf ",
                            root.dir,"output/process/syb_main_",region_to_report,"_ru.pdf"))
  # Technical report
  #   knitr::purl("syb_part1.Rnw","syb_part1.R")
  #   knitr::spin("syb_part1.R")

  # create jpg's for web comparisons
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

setwd(root.dir)
