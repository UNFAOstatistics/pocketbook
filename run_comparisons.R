#!/usr/bin/r
setwd("~/faosync/pocketbooks/pocketbook")
# set root directory
root.dir <- "~/faosync/pocketbooks/pocketbook/"
# This  script converst the pdfs into images and makes comparisons etc

####################################################
####################################################
# Stuff You DO NOT edit
####################################################
amanda <- FALSE

books <- c(
  # "GLO"
  # ,"GLO15"
  "RAP"
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
file.remove(fileOut)
file.create(fileOut, showWarnings = FALSE)

# number of images in the folder
nr_of_pages1 <- length(list.files(path = "GLO/", pattern = ".jpg"))
nr_of_pages2 <- length(list.files(path = paste0(region_to_report,"/"), pattern = ".jpg"))
if (nr_of_pages1 >= nr_of_pages2){
nr_of_pages <- nr_of_pages2
} else nr_of_pages <- nr_of_pages1
nr_of_pages <- nr_of_pages -2
cat(paste0("
 <body bgcolor='#669999'>
 
 <h1> spreads<h1>
 
 <table>
 <tr>
 <th>Comments</th><th>latest</th><th>As in 20170601 - before new data, after Staceys comments</th><th>As in 20170328</th><th>20170306 - prior Staceys comments</th><th>20170221 - prior bulk</th>
 </tr>
 <tr>
 <td>
   <iframe name='embed_readwrite' src='http://pad.okfn.org/p/faopocketbook2016",region_to_report,"?showControls=true&showChat=true&showLineNumbers=true&useMonospaceFont=false' width=500 height=",nr_of_pages*(800+20),"></iframe> 
   </td>
   <td>
   "),
file = fileOut, append = TRUE) 


# number of images in the folder
for (i in 1:nr_of_pages){
cat(paste0('
     <img src="',region_to_report,'/page-',i,'.jpg" vspace="10"  height="800"/></br>
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
     <img src="../pdf_tmp_20170601/',region_to_report,'/page-',i,'.jpg" vspace="10"  height="800"/></br>
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
     <img src="../pdf_tmp_20170328/',region_to_report,'/page-',i,'.jpg" vspace="10"  height="800"/></br>
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
     <img src="../pdf_tmp_20170306/',region_to_report,'/page-',i,'.jpg" vspace="10"  height="800"/></br>
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
     <img src="../pdf_tmp_20170221/',region_to_report,'/page-',i-1,'.jpg" vspace="10"  height="800"/></br>
     '),
file = fileOut, append = TRUE) 
}

# cat(paste('
# </td>
# <td>
# '),
#     file = fileOut, append = TRUE) 
# 
# for (i in 1:nr_of_pages){
# cat(paste0('
# <img src="../pdf_tmp_20170221/GLO15/page-',i-1,'.jpg" vspace="10" height="800"/></br>
# '),
#     file = fileOut, append = TRUE) 
# }

cat(paste0("
   </td>
   </table>
   </body>
   "),
file = fileOut, append = TRUE) 

}



# # ------------------------------------
# # global with global
# # ------------------------------------
# setwd(paste0(root.dir,"/output/pdf"))
# if (!file.exists(paste0("syb_main_",region_to_report,".pdf"))) next()
# fileOut <- "GLO15_GLO.html"
# file.create(fileOut, showWarnings = FALSE)
# cat(paste('
# <body bgcolor="#669999">
# 
# <h1> spreads<h1>
# 
# <table>
# <tr>
# <th>Global 2015</th><th>Global 2016</th><th>Comments</th>
# </tr>
# <tr>
# <td>            
# '),
# file = fileOut, append = TRUE) 
# 
# # number of images in the folder
# nr_of_pages <- length(list.files(path = "GLO15/", pattern = ".jpg"))
# nr_of_pages <- nr_of_pages - 7
# for (i in 1:nr_of_pages){
#     cat(paste0('
# <img src="GLO15/page-',i,'.jpg" vspace="10"/ height="800"></br>
#                '),
#         file = fileOut, append = TRUE) 
# }
#   
# cat(paste('
# </td>
# <td>
#           '),
# file = fileOut, append = TRUE) 
#   
#   for (i in 1:nr_of_pages){
#     cat(paste0('
# <img src="GLO/page-',i+5,'.jpg" vspace="10"/ height="800"></br>
#                '),
#         file = fileOut, append = TRUE) 
#   }
# cat(paste("
# </td>
# <td>
# <iframe name='embed_readwrite' src='https://pad.okfn.org/p/faopocketbook2016GLO?showControls=true&showChat=true&showLineNumbers=true&useMonospaceFont=false' width=500 height=",nr_of_pages*(800+20),"></iframe> 
# </td>
# </table>
# 
# </body>
# "),
#     file = fileOut, append = TRUE) 


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


# 
# # ---------------------------------------------
# # Regional books all compared
# # ---------------------------------------------
# setwd(paste0(root.dir,"/output/pdf"))
# if (!file.exists(paste0("syb_main_",region_to_report,".pdf"))) next()
# fileOut <- "regional_books16.html"
# file.create(fileOut, showWarnings = FALSE)
# cat(paste('
# <body bgcolor="#669999">
# 
# <h1> spreads<h1>
# 
# <table>
# <tr>
# <th>REU</th><th>RAP</th><th>RAF</th><th>RNE</th><th>Comments</th>
# </tr>
# <tr>
# <td>            
# '),
# file = fileOut, append = TRUE) 
# 
# # number of images in the folder
# nr_of_pages <- length(list.files(path = "RNE/", pattern = ".jpg"))
# 
# for (i in 1:nr_of_pages){
# cat(paste0('
# <img src="REU/page-',i-1,'.jpg" vspace="10" height="620"/></br>
# '),
# file = fileOut, append = TRUE) 
# }
# cat(paste('
# </td>
# <td>
# '),
# file = fileOut, append = TRUE) 
# for (i in 1:nr_of_pages){
# cat(paste0('
# <img src="RAP/page-',i-1,'.jpg" vspace="10" height="620"/></br>
# '),
# file = fileOut, append = TRUE) 
# }
# cat(paste('
# </td>
# <td>
# '),
# file = fileOut, append = TRUE) 
# for (i in 1:nr_of_pages){
# cat(paste0('
# <img src="RAF/page-',i-1,'.jpg" vspace="10" height="620"/></br>
#              '),
# file = fileOut, append = TRUE) 
# }
# cat(paste('
# </td>
# <td>
#           '),
# file = fileOut, append = TRUE) 
# 
# for (i in 1:nr_of_pages){
# cat(paste0('
# <img src="RNE/page-',i-1,'.jpg" vspace="10" height="620"/></br>
# '),
# file = fileOut, append = TRUE) 
# }
# 
# cat(paste("
# </td>
# <td>
# <iframe name='embed_readwrite' src='https://pad.okfn.org/p/faopocketbook2016regions?showControls=true&showChat=true&showLineNumbers=true&useMonospaceFont=false' width=500 height=",nr_of_pages*(620+20),"></iframe> 
# </td>
# </table>
# 
# </body>
# "),
# file = fileOut, append = TRUE) 


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



# ------------------------------------
# Country profile tables 
# ------------------------------------
setwd(paste0(root.dir,"/output/pdf"))
fileOut <- "country_profiles_20170221.html"
file.remove(fileOut)
file.create(fileOut, showWarnings = FALSE)
cat(paste('
<body bgcolor="#669999">

<h1> spreads<h1>

<table>
<tr>
<th>Comments</th><th>current</th><th>original in 20170221</th><th>global book2015</th>
</tr>
<tr>
<td>            
'),
file = fileOut, append = TRUE) 

cat("
# <iframe name='embed_readwrite' src='https://pad.okfn.org/p/faopocketbook2016countryprofiles?showControls=true&showChat=true&showLineNumbers=true&useMonospaceFont=false' width=500 height=23500></iframe> 
</td>
<td>
", file=fileOut, append=TRUE)

# number of images in the folder
cat('
<h3>REU-book</h3>
<img src="REU/page-56.jpg" vspace="10" height="1200"/></br>
<img src="REU/page-67.jpg" vspace="10" height="1200"/></br>
<img src="REU/page-77.jpg" vspace="10" height="1200"/></br>
<h3>REU_ru-book</h3>
<img src="REU_ru/page-57.jpg" vspace="10" height="1200"/></br>
<img src="REU_ru/page-67.jpg" vspace="10" height="1200"/></br>
<img src="REU_ru/page-77.jpg" vspace="10" height="1200"/></br>
<h3>RAF-book</h3>
<img src="RAF/page-56.jpg" vspace="10" height="1200"/></br>
<img src="RAF/page-77.jpg" vspace="10" height="1200"/></br>
<img src="RAF/page-79.jpg" vspace="10" height="1200"/></br>
<h3>RAP-book</h3>
<img src="RAP/page-56.jpg" vspace="10" height="1200"/></br>
<img src="RAP/page-68.jpg" vspace="10" height="1200"/></br>
<img src="RAP/page-72.jpg" vspace="10" height="1200"/></br>
<img src="RAP/page-94.jpg" vspace="10" height="1200"/></br>
<h3>RNE-book</h3>
<img src="RNE/page-56.jpg" vspace="10" height="1200"/></br>
<img src="RNE/page-63.jpg" vspace="10" height="1200"/></br>
<img src="RNE/page-73.jpg" vspace="10" height="1200"/></br>
<h3>GLO-book</h3>
<img src="GLO/page-67.jpg" vspace="10" height="1200"/></br>
<img src="GLO/page-134.jpg" vspace="10" height="1200"/></br>
<img src="GLO/page-229.jpg" vspace="10" height="1200"/></br>
',
file = fileOut, append = TRUE) 


# number of images in the folder
cat('
<h3>REU-book</h3>
<img src="../pdf_tmp_20170221/REU/page-55.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170221/REU/page-66.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170221/REU/page-76.jpg" vspace="10" height="1200"/></br>
<h3>REU_ru-book</h3>
<img src="../pdf_tmp_20170221/REU_ru/page-55.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170221/REU_ru/page-66.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170221/REU_ru/page-76.jpg" vspace="10" height="1200"/></br>
<h3>RAF-book</h3>
<img src="../pdf_tmp_20170221/RAF/page-55.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170221/RAF/page-76.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170221/RAF/page-78.jpg" vspace="10" height="1200"/></br>
<h3>RAP-book</h3>
<img src="../pdf_tmp_20170221/RAP/page-55.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170221/RAP/page-67.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170221/RAP/page-71.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170221/RAP/page-93.jpg" vspace="10" height="1200"/></br>
<h3>RNE-book</h3>
<img src="../pdf_tmp_20170221/RNE/page-55.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170221/RNE/page-62.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170221/RNE/page-72.jpg" vspace="10" height="1200"/></br>
<h3>GLO-book</h3>
<img src="../pdf_tmp_20170221/GLO/page-66.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170221/GLO/page-133.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170221/GLO/page-228.jpg" vspace="10" height="1200"/></br>
',
file = fileOut, append = TRUE)

cat(paste('
</td>
<td>
'),
file = fileOut, append = TRUE) 

cat('
<h3>GLO-book</h3>
<img src="../pdf_tmp_20170221/GLO15/page-50.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170221/GLO15/page-63.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170221/GLO15/page-109.jpg" vspace="10" height="1200"/></br>
<h3>GLO15-book</h3>
<img src="../pdf_tmp_20170221/GLO15/page-50.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170221/GLO15/page-51.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170221/GLO15/page-66.jpg" vspace="10" height="1200"/></br>
<h3>GLO15-book</h3>
<img src="../pdf_tmp_20170221/GLO15/page-51.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170221/GLO15/page-103.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170221/GLO15/page-107.jpg" vspace="10" height="1200"/></br>
<h3>GLO15-book</h3>
<img src="../pdf_tmp_20170221/GLO15/page-52.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170221/GLO15/page-87.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170221/GLO15/page-125.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170221/GLO15/page-204.jpg" vspace="10" height="1200"/></br>
<h3>GLO15-book</h3>
<img src="../pdf_tmp_20170221/GLO15/page-51.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170221/GLO15/page-127.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170221/GLO15/page-184.jpg" vspace="10" height="1200"/></br>
<h3>GLO15-book</h3>
<img src="../pdf_tmp_20170221/GLO15/page-65.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170221/GLO15/page-131.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170221/GLO15/page-215.jpg" vspace="10" height="1200"/></br>
',
file = fileOut, append = TRUE)




cat(paste("
</td>
</table>

</body>
"),
file = fileOut, append = TRUE) 




setwd(paste0(root.dir,"/output/pdf"))
fileOut <- "country_profiles_20170301.html"
file.remove(fileOut)
file.create(fileOut, showWarnings = FALSE)
cat(paste('
<body bgcolor="#669999">

<h1> spreads<h1>

<table>
<tr>
<th>Comments</th><th>current</th><th>as in 20170328</th><th>original in 20170306</th><th>global book2015</th>
</tr>
<tr>
<td>            
'),
file = fileOut, append = TRUE) 

cat("
# <iframe name='embed_readwrite' src='https://pad.okfn.org/p/faopocketbook2016countryprofiles20170301?showControls=true&showChat=true&showLineNumbers=true&useMonospaceFont=false' width=500 height=23500></iframe> 
</td>
<td>
", file=fileOut, append=TRUE)

# number of images in the folder
cat('
<h3>REU-book</h3>
<img src="REU/page-56.jpg" vspace="10" height="1200"/></br>
<img src="REU/page-67.jpg" vspace="10" height="1200"/></br>
<img src="REU/page-77.jpg" vspace="10" height="1200"/></br>
<h3>REU_ru-book</h3>
<img src="REU_ru/page-57.jpg" vspace="10" height="1200"/></br>
<img src="REU_ru/page-67.jpg" vspace="10" height="1200"/></br>
<img src="REU_ru/page-77.jpg" vspace="10" height="1200"/></br>
<h3>RAF-book</h3>
<img src="RAF/page-56.jpg" vspace="10" height="1200"/></br>
<img src="RAF/page-77.jpg" vspace="10" height="1200"/></br>
<img src="RAF/page-79.jpg" vspace="10" height="1200"/></br>
<h3>RAP-book</h3>
<img src="RAP/page-56.jpg" vspace="10" height="1200"/></br>
<img src="RAP/page-68.jpg" vspace="10" height="1200"/></br>
<img src="RAP/page-72.jpg" vspace="10" height="1200"/></br>
<img src="RAP/page-94.jpg" vspace="10" height="1200"/></br>
<h3>RNE-book</h3>
<img src="RNE/page-56.jpg" vspace="10" height="1200"/></br>
<img src="RNE/page-63.jpg" vspace="10" height="1200"/></br>
<img src="RNE/page-73.jpg" vspace="10" height="1200"/></br>
<h3>GLO-book</h3>
<img src="GLO/page-67.jpg" vspace="10" height="1200"/></br>
<img src="GLO/page-134.jpg" vspace="10" height="1200"/></br>
<img src="GLO/page-229.jpg" vspace="10" height="1200"/></br>
',
file = fileOut, append = TRUE) 

cat(paste('
</td>
<td>
'),
file = fileOut, append = TRUE) 

# number of images in the folder
# <img src="../pdf_tmp_20170328/REU/page-56.jpg" vspace="10" height="1200"/></br>
cat('
<h3>REU-book</h3>
<img src="../pdf_tmp_20170328/REU/page-56.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170328/REU/page-67.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170328/REU/page-77.jpg" vspace="10" height="1200"/></br>
<h3>REU_ru-book</h3>
<img src="../pdf_tmp_20170328/REU_ru/page-57.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170328/REU_ru/page-67.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170328/REU_ru/page-77.jpg" vspace="10" height="1200"/></br>
<h3>RAF-book</h3>
<img src="../pdf_tmp_20170328/RAF/page-56.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170328/RAF/page-77.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170328/RAF/page-79.jpg" vspace="10" height="1200"/></br>
<h3>RAP-book</h3>
<img src="../pdf_tmp_20170328/RAP/page-56.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170328/RAP/page-68.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170328/RAP/page-72.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170328/RAP/page-94.jpg" vspace="10" height="1200"/></br>
<h3>RNE-book</h3>
<img src="../pdf_tmp_20170328/RNE/page-56.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170328/RNE/page-63.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170328/RNE/page-73.jpg" vspace="10" height="1200"/></br>
<h3>GLO-book</h3>
<img src="../pdf_tmp_20170328/GLO/page-67.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170328/GLO/page-134.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170328/GLO/page-229.jpg" vspace="10" height="1200"/></br>
',
file = fileOut, append = TRUE) 

cat(paste('
</td>
<td>
'),
file = fileOut, append = TRUE) 

# number of images in the folder
cat('
<h3>REU-book</h3>
<img src="../pdf_tmp_20170306/REU/page-56.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170306/REU/page-67.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170306/REU/page-77.jpg" vspace="10" height="1200"/></br>
<h3>REU_ru-book</h3>
<img src="../pdf_tmp_20170306/REU_ru/page-57.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170306/REU_ru/page-67.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170306/REU_ru/page-77.jpg" vspace="10" height="1200"/></br>
<h3>RAF-book</h3>
<img src="../pdf_tmp_20170306/RAF/page-56.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170306/RAF/page-77.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170306/RAF/page-79.jpg" vspace="10" height="1200"/></br>
<h3>RAP-book</h3>
<img src="../pdf_tmp_20170306/RAP/page-56.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170306/RAP/page-68.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170306/RAP/page-72.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170306/RAP/page-94.jpg" vspace="10" height="1200"/></br>
<h3>RNE-book</h3>
<img src="../pdf_tmp_20170306/RNE/page-56.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170306/RNE/page-63.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170306/RNE/page-73.jpg" vspace="10" height="1200"/></br>
<h3>GLO-book</h3>
<img src="../pdf_tmp_20170306/GLO/page-67.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170306/GLO/page-134.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170306/GLO/page-229.jpg" vspace="10" height="1200"/></br>
',
file = fileOut, append = TRUE)

cat(paste('
</td>
<td>
'),
file = fileOut, append = TRUE) 

cat('
<h3>GLO-book</h3>
<img src="../pdf_tmp_20170221/GLO15/page-50.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170221/GLO15/page-63.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170221/GLO15/page-109.jpg" vspace="10" height="1200"/></br>
<h3>GLO15-book</h3>
<img src="../pdf_tmp_20170221/GLO15/page-50.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170221/GLO15/page-51.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170221/GLO15/page-66.jpg" vspace="10" height="1200"/></br>
<h3>GLO15-book</h3>
<img src="../pdf_tmp_20170221/GLO15/page-51.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170221/GLO15/page-103.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170221/GLO15/page-107.jpg" vspace="10" height="1200"/></br>
<h3>GLO15-book</h3>
<img src="../pdf_tmp_20170221/GLO15/page-52.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170221/GLO15/page-87.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170221/GLO15/page-125.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170221/GLO15/page-204.jpg" vspace="10" height="1200"/></br>
<h3>GLO15-book</h3>
<img src="../pdf_tmp_20170221/GLO15/page-51.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170221/GLO15/page-127.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170221/GLO15/page-184.jpg" vspace="10" height="1200"/></br>
<h3>GLO15-book</h3>
<img src="../pdf_tmp_20170221/GLO15/page-65.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170221/GLO15/page-131.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170221/GLO15/page-215.jpg" vspace="10" height="1200"/></br>
',
file = fileOut, append = TRUE)




cat(paste("
</td>
</table>

</body>
"),
file = fileOut, append = TRUE) 



setwd(paste0(root.dir,"/output/pdf"))
fileOut <- "country_profiles_20170601.html"
file.remove(fileOut)
file.create(fileOut, showWarnings = FALSE)
cat(paste('
<body bgcolor="#669999">

<h1> spreads<h1>

<table>
<tr>
<th>Comments</th><th>current</th><th>as in 20170328</th><th>original in 20170306</th><th>global book2015</th>
</tr>
<tr>
<td>            
'),
file = fileOut, append = TRUE) 

cat("
# <iframe name='embed_readwrite' src='https://pad.okfn.org/p/faopocketbook2016countryprofiles20170601?showControls=true&showChat=true&showLineNumbers=true&useMonospaceFont=false' width=500 height=23500></iframe> 
</td>
<td>
", file=fileOut, append=TRUE)

# number of images in the folder
cat('
<h3>REU-book</h3>
<img src="REU/page-56.jpg" vspace="10" height="1200"/></br>
<img src="REU/page-67.jpg" vspace="10" height="1200"/></br>
<img src="REU/page-77.jpg" vspace="10" height="1200"/></br>
<h3>REU_ru-book</h3>
<img src="REU_ru/page-57.jpg" vspace="10" height="1200"/></br>
<img src="REU_ru/page-67.jpg" vspace="10" height="1200"/></br>
<img src="REU_ru/page-77.jpg" vspace="10" height="1200"/></br>
<h3>RAF-book</h3>
<img src="RAF/page-56.jpg" vspace="10" height="1200"/></br>
<img src="RAF/page-77.jpg" vspace="10" height="1200"/></br>
<img src="RAF/page-79.jpg" vspace="10" height="1200"/></br>
<h3>RAP-book</h3>
<img src="RAP/page-56.jpg" vspace="10" height="1200"/></br>
<img src="RAP/page-68.jpg" vspace="10" height="1200"/></br>
<img src="RAP/page-72.jpg" vspace="10" height="1200"/></br>
<img src="RAP/page-94.jpg" vspace="10" height="1200"/></br>
<h3>RNE-book</h3>
<img src="RNE/page-56.jpg" vspace="10" height="1200"/></br>
<img src="RNE/page-63.jpg" vspace="10" height="1200"/></br>
<img src="RNE/page-73.jpg" vspace="10" height="1200"/></br>
<h3>GLO-book</h3>
<img src="GLO/page-67.jpg" vspace="10" height="1200"/></br>
<img src="GLO/page-134.jpg" vspace="10" height="1200"/></br>
<img src="GLO/page-229.jpg" vspace="10" height="1200"/></br>
',
file = fileOut, append = TRUE) 

cat(paste('
</td>
<td>
'),
file = fileOut, append = TRUE) 

cat(paste('
</td>
<td>
'),
file = fileOut, append = TRUE) 

# number of images in the folder
cat('
<h3>REU-book</h3>
<img src="../pdf_tmp_20170601/REU/page-56.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170601/REU/page-67.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170601/REU/page-77.jpg" vspace="10" height="1200"/></br>
<h3>REU_ru-book</h3>
<img src="../pdf_tmp_20170601/REU_ru/page-56.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170601/REU_ru/page-67.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170601/REU_ru/page-77.jpg" vspace="10" height="1200"/></br>
<h3>RAF-book</h3>
<img src="../pdf_tmp_20170601/RAF/page-56.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170601/RAF/page-77.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170601/RAF/page-79.jpg" vspace="10" height="1200"/></br>
<h3>RAP-book</h3>
<img src="../pdf_tmp_20170601/RAP/page-56.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170601/RAP/page-68.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170601/RAP/page-72.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170601/RAP/page-94.jpg" vspace="10" height="1200"/></br>
<h3>RNE-book</h3>
<img src="../pdf_tmp_20170601/RNE/page-56.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170601/RNE/page-63.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170601/RNE/page-73.jpg" vspace="10" height="1200"/></br>
<h3>GLO-book</h3>
<img src="../pdf_tmp_20170601/GLO/page-67.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170601/GLO/page-134.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170601/GLO/page-229.jpg" vspace="10" height="1200"/></br>
',
file = fileOut, append = TRUE)

cat(paste('
</td>
<td>
'),
file = fileOut, append = TRUE) 

# number of images in the folder
# <img src="../pdf_tmp_20170328/REU/page-56.jpg" vspace="10" height="1200"/></br>
cat('
<h3>REU-book</h3>
<img src="../pdf_tmp_20170328/REU/page-56.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170328/REU/page-67.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170328/REU/page-77.jpg" vspace="10" height="1200"/></br>
<h3>REU_ru-book</h3>
<img src="../pdf_tmp_20170328/REU_ru/page-57.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170328/REU_ru/page-67.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170328/REU_ru/page-77.jpg" vspace="10" height="1200"/></br>
<h3>RAF-book</h3>
<img src="../pdf_tmp_20170328/RAF/page-56.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170328/RAF/page-77.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170328/RAF/page-79.jpg" vspace="10" height="1200"/></br>
<h3>RAP-book</h3>
<img src="../pdf_tmp_20170328/RAP/page-56.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170328/RAP/page-68.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170328/RAP/page-72.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170328/RAP/page-94.jpg" vspace="10" height="1200"/></br>
<h3>RNE-book</h3>
<img src="../pdf_tmp_20170328/RNE/page-56.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170328/RNE/page-63.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170328/RNE/page-73.jpg" vspace="10" height="1200"/></br>
<h3>GLO-book</h3>
<img src="../pdf_tmp_20170328/GLO/page-67.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170328/GLO/page-134.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170328/GLO/page-229.jpg" vspace="10" height="1200"/></br>
',
file = fileOut, append = TRUE) 

cat(paste('
</td>
<td>
'),
file = fileOut, append = TRUE) 

# number of images in the folder
cat('
<h3>REU-book</h3>
<img src="../pdf_tmp_20170306/REU/page-56.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170306/REU/page-67.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170306/REU/page-77.jpg" vspace="10" height="1200"/></br>
<h3>REU_ru-book</h3>
<img src="../pdf_tmp_20170306/REU_ru/page-57.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170306/REU_ru/page-67.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170306/REU_ru/page-77.jpg" vspace="10" height="1200"/></br>
<h3>RAF-book</h3>
<img src="../pdf_tmp_20170306/RAF/page-56.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170306/RAF/page-77.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170306/RAF/page-79.jpg" vspace="10" height="1200"/></br>
<h3>RAP-book</h3>
<img src="../pdf_tmp_20170306/RAP/page-56.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170306/RAP/page-68.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170306/RAP/page-72.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170306/RAP/page-94.jpg" vspace="10" height="1200"/></br>
<h3>RNE-book</h3>
<img src="../pdf_tmp_20170306/RNE/page-56.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170306/RNE/page-63.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170306/RNE/page-73.jpg" vspace="10" height="1200"/></br>
<h3>GLO-book</h3>
<img src="../pdf_tmp_20170306/GLO/page-67.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170306/GLO/page-134.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170306/GLO/page-229.jpg" vspace="10" height="1200"/></br>
',
file = fileOut, append = TRUE)

cat(paste('
</td>
<td>
'),
file = fileOut, append = TRUE) 

cat('
<h3>GLO-book</h3>
<img src="../pdf_tmp_20170221/GLO15/page-50.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170221/GLO15/page-63.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170221/GLO15/page-109.jpg" vspace="10" height="1200"/></br>
<h3>GLO15-book</h3>
<img src="../pdf_tmp_20170221/GLO15/page-50.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170221/GLO15/page-51.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170221/GLO15/page-66.jpg" vspace="10" height="1200"/></br>
<h3>GLO15-book</h3>
<img src="../pdf_tmp_20170221/GLO15/page-51.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170221/GLO15/page-103.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170221/GLO15/page-107.jpg" vspace="10" height="1200"/></br>
<h3>GLO15-book</h3>
<img src="../pdf_tmp_20170221/GLO15/page-52.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170221/GLO15/page-87.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170221/GLO15/page-125.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170221/GLO15/page-204.jpg" vspace="10" height="1200"/></br>
<h3>GLO15-book</h3>
<img src="../pdf_tmp_20170221/GLO15/page-51.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170221/GLO15/page-127.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170221/GLO15/page-184.jpg" vspace="10" height="1200"/></br>
<h3>GLO15-book</h3>
<img src="../pdf_tmp_20170221/GLO15/page-65.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170221/GLO15/page-131.jpg" vspace="10" height="1200"/></br>
<img src="../pdf_tmp_20170221/GLO15/page-215.jpg" vspace="10" height="1200"/></br>
',
file = fileOut, append = TRUE)




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

if (amanda){

tmp <- readRDS("~/local_data/faostat/rds/faostat.RDS")
write_csv(tmp[1:10000,], paste0(root.dir,"/output/data/faostat.csv"))

tmp <- readRDS("~/local_data/faostat/metadata/meta_faostat.RDS")
write_csv(tmp, paste0(root.dir,"/output/data/meta_faostat.csv"))

write_csv(syb.df, paste0(root.dir,"/output/data/data_for_book.csv"))
}


file.copy(paste0(root.dir,"/input/templates/toc.Rmd"), to = paste0(root.dir,"/output"), overwrite = TRUE)
file.copy(paste0(root.dir,"/input/templates/datatests.html"), to = paste0(root.dir,"/output"), overwrite = TRUE)
file.copy(paste0(root.dir,"/input/templates/github_browser_edit.gif"), to = paste0(root.dir,"/output"), overwrite = TRUE)
file.copy(paste0(root.dir,"/input/templates/datacheck_amandasql.Rmd"), to = paste0(root.dir,"/output"), overwrite = TRUE)

setwd(paste0(root.dir,"/output"))
rmarkdown::render("toc.Rmd", output_file = "index.html")
rmarkdown::render("datacheck_amandasql.Rmd", output_file = "datacheck_amandasql.html")
file.remove("./toc.Rmd")
file.remove("./datacheck_amandasql.Rmd")

fly <- readLines("./datacheck_amandasql.html")
fly <- gsub("max-width: 940px;", "max-width: 1240px;", fly)
writeLines(fly, "./datacheck_amandasql.html")

setwd(root.dir)

unlink(paste0(root.dir,"/output/process"), recursive = TRUE, force = TRUE) 

# Sync to kapsi!!
if (Sys.info()["nodename"] =="markus-desktop-mint18" & Sys.info()["user"] == "aurelius")     system('rsync -lptDvzhe "ssh -i /home/aurelius/avaimet/nuc-rsync-key" --progress --delete --recursive /home/aurelius/faosync/pocketbooks/pocketbook/output/ pi@82.181.175.116:/var/www/html/fao/pocketbooks/')
if (Sys.info()["nodename"] =="markus-x220") system('rsync -lptDvzhe "ssh -i /home/aurelius/avaimet/x220-rsync-key" --recursive --progress --delete /home/aurelius/faosync/pocketbooks/pocketbook/output/ pi@82.181.175.116:/var/www/html/fao/pocketbooks/')
if (Sys.info()["nodename"] == "markus-T430")              system('rsync -lptDvzhe "ssh -i /home/aurelius/avaimet/t430-rsync-key" --progress /home/aurelius/faosync/pocketbooks/pocketbook/output/ pi@82.181.175.116:/var/www/html/fao/pocketbooks/')
if (Sys.info()["nodename"] =="markus-desktop-mint18" & Sys.info()["user"] == "amanda")     system('rsync -lptDvzhe "ssh -i /home/amanda/avaimet/amanda-rsync-key" --progress --delete --recursive /home/amanda/faosync/pocketbooks/pocketbook/output/ pi@82.181.175.116:/var/www/html/fao/pocketbooks/')

