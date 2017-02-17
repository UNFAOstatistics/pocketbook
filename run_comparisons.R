# This  script converst the pdfs into images and makes comparisons etc

####################################################
####################################################
# Stuff You DO NOT edit
####################################################


books <- c(
          "GLO"
          ,"GLO15"
          ,"RAP"
          ,"RAF"
          ,"REU"
          ,"REU_ru"
          ,"RNE"
          )


for (region_to_report in books){
  path <- paste0(root.dir,"/output/pdf/",region_to_report)
  dir.create(path, showWarnings = FALSE)
  setwd(paste0(root.dir,"/output/pdf"))
  # download.file(url = "http://www.fao.org/3/a-i4691e.pdf", destfile = paste0(root.dir,"/input/templates/a-i4691e.pdf"))
  # file.copy(paste0(root.dir,"/input/templates/a-i4691e.pdf"), "./syb_main_GLO15.pdf", overwrite = TRUE)
  # system(paste0("convert -density 150 -alpha remove ",root.dir,"/input/templates/a-i4691e.pdf ",root.dir,"input/templates/GLO15/page.jpg"))
  if (region_to_report %in% "GLO15"){
    glo15_files <- list.files(paste0(root.dir,"input/templates/GLO15/"),pattern = ".jpg", full.names = TRUE)
    file.copy(from = glo15_files, 
              to = "./GLO15", overwrite = TRUE, recursive = TRUE)
  } 
  if (!file.exists(paste0("syb_main_",region_to_report,".pdf"))) next()
  system(paste0("convert -density 150 -alpha remove syb_main_",region_to_report,".pdf ",region_to_report,"/page.jpg"))
  setwd(root.dir)
}



# ------------------------------------
# regional comparison with global
# ------------------------------------
for (region_to_report in books){
  setwd(paste0(root.dir,"/output/pdf"))
  if (!file.exists(paste0("syb_main_",region_to_report,".pdf"))) next()
  fileOut <- paste0(region_to_report,".html")
  file.create(fileOut, showWarnings = FALSE)
  cat(paste('
 <body bgcolor="#669999">
            
<h1> spreads<h1>
              
<table>
<tr>
<th>Global 2016</th><th>',region_to_report,' 2016</th><th>Comments</th>
</tr>
<tr>
<td>            
'),
file = fileOut, append = TRUE) 

# number of images in the folder
nr_of_pages1 <- length(list.files(path = "GLO/", pattern = ".jpg"))
nr_of_pages2 <- length(list.files(path = paste0(region_to_report,"/"), pattern = ".jpg"))
if (nr_of_pages1 >= nr_of_pages2){
  nr_of_pages <- nr_of_pages2
} else nr_of_pages <- nr_of_pages1

for (i in 1:nr_of_pages){
    cat(paste0('
<img src="GLO/page-',i-1,'.jpg" vspace="10" height="800"/></br>
'),
        file = fileOut, append = TRUE) 
}
  
cat(paste('
</td>
<td>
'),
file = fileOut, append = TRUE) 
  
# number of images in the folder
  for (i in 1:nr_of_pages){
    cat(paste0('
<img src="',region_to_report,'/page-',i-1,'.jpg" vspace="10"  height="800"/></br>
'),
        file = fileOut, append = TRUE) 
}

cat(paste0("
</td>
<td>
<iframe name='embed_readwrite' src='http://pad.okfn.org/p/faopocketbook2016",region_to_report,"?showControls=true&showChat=true&showLineNumbers=true&useMonospaceFont=false' width=500 height=",nr_of_pages*(800+20),"></iframe> 
</td>
</table>

</body>
"),
    file = fileOut, append = TRUE) 
}

# ------------------------------------
# global with global
# ------------------------------------
setwd(paste0(root.dir,"/output/pdf"))
if (!file.exists(paste0("syb_main_",region_to_report,".pdf"))) next()
fileOut <- "GLO15_GLO.html"
file.create(fileOut, showWarnings = FALSE)
cat(paste('
<body bgcolor="#669999">

<h1> spreads<h1>

<table>
<tr>
<th>Global 2015</th><th>Global 2016</th><th>Comments</th>
</tr>
<tr>
<td>            
'),
file = fileOut, append = TRUE) 

# number of images in the folder
nr_of_pages <- length(list.files(path = "GLO15/", pattern = ".jpg"))
nr_of_pages <- nr_of_pages - 7
for (i in 1:nr_of_pages){
    cat(paste0('
<img src="GLO15/page-',i,'.jpg" vspace="10"/ height="800"></br>
               '),
        file = fileOut, append = TRUE) 
}
  
cat(paste('
</td>
<td>
          '),
file = fileOut, append = TRUE) 
  
  for (i in 1:nr_of_pages){
    cat(paste0('
<img src="GLO/page-',i+5,'.jpg" vspace="10"/ height="800"></br>
               '),
        file = fileOut, append = TRUE) 
  }
cat(paste("
</td>
<td>
<iframe name='embed_readwrite' src='https://pad.okfn.org/p/faopocketbook2016GLO?showControls=true&showChat=true&showLineNumbers=true&useMonospaceFont=false' width=500 height=",nr_of_pages*(800+20),"></iframe> 
</td>
</table>

</body>
"),
    file = fileOut, append = TRUE) 


# ------------------------------------
# REU ru vs REU
# ------------------------------------
setwd(paste0(root.dir,"/output/pdf"))
if (!file.exists(paste0("syb_main_",region_to_report,".pdf"))) next()
fileOut <- "REU_ru_REU.html"
file.create(fileOut, showWarnings = FALSE)
cat(paste('
<body bgcolor="#669999">

<h1> spreads<h1>

<table>
<tr>
<th>REU 2016</th><th>REU ru 2016</th><th>Comments</th>
</tr>
<tr>
<td>            
'),
    file = fileOut, append = TRUE) 

# number of images in the folder
nr_of_pages1 <- length(list.files(path = "REU/", pattern = ".jpg"))
nr_of_pages2 <- length(list.files(path = "REU_ru/", pattern = ".jpg"))
if (nr_of_pages1 >= nr_of_pages2){
  nr_of_pages <- nr_of_pages2
} else nr_of_pages <- nr_of_pages1

for (i in 1:nr_of_pages){
  cat(paste0('
<img src="REU/page-',i-1,'.jpg" vspace="10" height="800"/></br>
               '),
      file = fileOut, append = TRUE) 
}

cat(paste('
</td>
<td>
          '),
    file = fileOut, append = TRUE) 

for (i in 1:nr_of_pages){
  cat(paste0('
<img src="REU_ru/page-',i-1,'.jpg" vspace="10" height="800"/></br>
               '),
      file = fileOut, append = TRUE) 
}
cat(paste("
</td>
<td>
<iframe name='embed_readwrite' src='https://pad.okfn.org/p/faopocketbook2016REU_ru_REU?showControls=true&showChat=true&showLineNumbers=true&useMonospaceFont=false' width=500 height=",nr_of_pages*(800+20),"></iframe> 
</td>
</table>

</body>
"),
    file = fileOut, append = TRUE) 



# ---------------------------------------------
# Regional books all compared
# ---------------------------------------------
setwd(paste0(root.dir,"/output/pdf"))
if (!file.exists(paste0("syb_main_",region_to_report,".pdf"))) next()
fileOut <- "regional_books16.html"
file.create(fileOut, showWarnings = FALSE)
cat(paste('
<body bgcolor="#669999">

<h1> spreads<h1>

<table>
<tr>
<th>REU</th><th>RAP</th><th>RAF</th><th>RNE</th><th>Comments</th>
</tr>
<tr>
<td>            
'),
file = fileOut, append = TRUE) 

# number of images in the folder
nr_of_pages <- length(list.files(path = "RNE/", pattern = ".jpg"))

for (i in 1:nr_of_pages){
cat(paste0('
<img src="REU/page-',i-1,'.jpg" vspace="10" height="620"/></br>
'),
file = fileOut, append = TRUE) 
}
cat(paste('
</td>
<td>
'),
file = fileOut, append = TRUE) 
for (i in 1:nr_of_pages){
cat(paste0('
<img src="RAP/page-',i-1,'.jpg" vspace="10" height="620"/></br>
'),
file = fileOut, append = TRUE) 
}
cat(paste('
</td>
<td>
'),
file = fileOut, append = TRUE) 
for (i in 1:nr_of_pages){
cat(paste0('
<img src="RAF/page-',i-1,'.jpg" vspace="10" height="620"/></br>
             '),
file = fileOut, append = TRUE) 
}
cat(paste('
</td>
<td>
          '),
file = fileOut, append = TRUE) 

for (i in 1:nr_of_pages){
cat(paste0('
<img src="RNE/page-',i-1,'.jpg" vspace="10" height="620"/></br>
'),
file = fileOut, append = TRUE) 
}

cat(paste("
</td>
<td>
<iframe name='embed_readwrite' src='https://pad.okfn.org/p/faopocketbook2016regions?showControls=true&showChat=true&showLineNumbers=true&useMonospaceFont=false' width=500 height=",nr_of_pages*(620+20),"></iframe> 
</td>
</table>

</body>
"),
file = fileOut, append = TRUE) 


# ---------------------------------------------
# All 2016 books compared
# ---------------------------------------------
setwd(paste0(root.dir,"/output/pdf"))
if (!file.exists(paste0("syb_main_",region_to_report,".pdf"))) next()
fileOut <- "all_books16.html"
file.create(fileOut, showWarnings = FALSE)
cat(paste('
<body bgcolor="#669999">

<h1> spreads<h1>

<table>
<tr>
<th>Comments</th><th>REU</th><th>REU_ru</th><th>RAP</th><th>RAF</th><th>RNE</th><th>GLO</th>
</tr>
<tr>
<td>            
'),
    file = fileOut, append = TRUE) 

# number of images in the folder
nr_of_pages <- length(list.files(path = "RNE/", pattern = ".jpg"))


cat(paste0("
<iframe name='embed_readwrite' src='https://pad.okfn.org/p/faopocketbook2016all?showControls=true&showChat=true&showLineNumbers=true&useMonospaceFont=false' width=500 height=",nr_of_pages*(620+20),"></iframe> 
</td>
<td>
"), file=fileOut, append=TRUE)


for (i in 1:nr_of_pages){
  cat(paste0('
<img src="REU/page-',i-1,'.jpg" vspace="10" height="620"/></br>
'),
      file = fileOut, append = TRUE) 
}
cat(paste('
</td>
          <td>
          '),
    file = fileOut, append = TRUE) 
for (i in 1:nr_of_pages){
  cat(paste0('
             <img src="REU_ru/page-',i-1,'.jpg" vspace="10" height="620"/></br>
             '),
      file = fileOut, append = TRUE) 
}

cat(paste('
</td>
<td>
'),
    file = fileOut, append = TRUE) 
for (i in 1:nr_of_pages){
  cat(paste0('
<img src="RAP/page-',i-1,'.jpg" vspace="10" height="620"/></br>
'),
      file = fileOut, append = TRUE) 
}
cat(paste('
</td>
<td>
'),
    file = fileOut, append = TRUE) 
for (i in 1:nr_of_pages){
  cat(paste0('
<img src="RAF/page-',i-1,'.jpg" vspace="10" height="620"/></br>
             '),
      file = fileOut, append = TRUE) 
}
cat(paste('
</td>
          <td>
          '),
    file = fileOut, append = TRUE) 
for (i in 1:nr_of_pages){
  cat(paste0('
             <img src="RNE/page-',i-1,'.jpg" vspace="10" height="620"/></br>
             '),
      file = fileOut, append = TRUE) 
}
cat(paste('
</td>
<td>
          '),
    file = fileOut, append = TRUE) 

for (i in 1:nr_of_pages){
  cat(paste0('
<img src="GLO/page-',i-1,'.jpg" vspace="10" height="620"/></br>
'),
      file = fileOut, append = TRUE) 
}

cat(paste("
</td>
</table>

</body>
"),
    file = fileOut, append = TRUE) 



# if (broke_only_tables_into_images){
#   
#   # if (region_to_report == "RAF"){
#   #   system("pdftk syb_main.pdf cat 7 output agg_pic.pdf") # subregion
#   #   system("pdftk syb_main.pdf cat 30 output table_pic.pdf") # Ethiopia
#   # }
#   # if (region_to_report == "RAP"){
#   #   system("pdftk syb_main.pdf cat 7 output agg_pic.pdf") # subregion
#   #   system("pdftk syb_main.pdf cat 25 output table_pic.pdf") # Bangladesh
#   # }
#   # if (region_to_report == "RNE"){
#   #   system("pdftk syb_main.pdf cat 7 output agg_pic.pdf") # subregion
#   #   system("pdftk syb_main.pdf cat 22 output table_pic.pdf") # Saudi-Arabia
#   # }
#   # if (region_to_report == "REU"){
#   #   system("pdftk syb_main.pdf cat 7 output agg_pic.pdf") # subregion
#   #   system("pdftk syb_main.pdf cat 59 output table_pic.pdf") # Turkey
#   # }
#   
#   system(paste0("convert -density 150 -alpha remove agg_pic.pdf ",root.dir,"output/jpg/",region_to_report,"_agg",".jpg"))
#   system(paste0("convert -density 150 -alpha remove table_pic.pdf ",root.dir,"output/jpg/",region_to_report,"_tbl",".jpg"))
#   
# }
# if (broke_all_into_images) system(paste0("convert -density 150 -alpha remove syb_main_",region_to_report,".pdf ",root.dir,"output/jpg/",region_to_report,".jpg"))
# 
# 
# if (broke_all_into_images | broke_only_tables_into_images){
#   
#   # copy the output -html's into the output/html-folder
#   flist <- list.files(paste0(root.dir,"/output/process"),
#                       "+[.]html$",
#                       full.names = TRUE)
#   file.copy(flist, paste0(root.dir,"/output/html"), overwrite = TRUE)
#   
#   # convert the index.md into html in jpog comparison
#   
#   system(paste0("pandoc ",root.dir,"output/jpg/regional_book_comparison.md -o ",     root.dir,"output/jpg/regional_book_comparison.html"))
#   system(paste0("pandoc ",root.dir,"output/jpg/regional_book_comparison_reg.md -o ", root.dir,"output/jpg/regional_book_comparison_reg.html"))
#   system(paste0("pandoc ",root.dir,"output/jpg/regional_table_comparison.md -o ",    root.dir,"output/jpg/regional_table_comparison.html"))
#   if (region_to_report == "COF") system(paste0("pandoc ",root.dir,"output/jpg/coffee_comparison.md -o ",root.dir,"output/jpg/coffee_comparison.html"))
#   
# }
# 
# if (broke_rus_translation_images){
#   
#   
#   if (!rulang) system(paste0("convert -density 150 -alpha remove syb_main_REU.pdf ",root.dir,"output/jpg/REU.jpg"))
#   if (rulang) system(paste0("convert -density 150 -alpha remove syb_main_REU_ru.pdf ",root.dir,"output/jpg/REURU.jpg"))
#   
#   # copy the output -html's into the output/html-folder
#   flist <- list.files(paste0(root.dir,"/output/process"),
#                       "+[.]html$",
#                       full.names = TRUE)
#   file.copy(flist, paste0(root.dir,"/output/html"), overwrite = TRUE)
#   
# }

# Add data from bulk_download for Amanda
# file.copy("~/local_data/faostat/csv/faostat.csv", paste0(root.dir,"/output/data"), overwrite = TRUE)
# file.copy("~/faosync/syb_bulk_database/metadata/meta_faostat.csv", paste0(root.dir,"/output/data"), overwrite = TRUE)

file.copy(paste0(root.dir,"/input/templates/toc.Rmd"), to = paste0(root.dir,"/output"), overwrite = TRUE)
setwd(paste0(root.dir,"/output"))
rmarkdown::render("toc.Rmd", output_file = "index.html")
file.remove("./toc.Rmd")
setwd(root.dir)

unlink(paste0(root.dir,"/output/process"), recursive = TRUE, force = TRUE) 

# Sync to kapsi!!
system("/home/aurelius/faosync/pocketbooks/pocketbook/sync.sh")
