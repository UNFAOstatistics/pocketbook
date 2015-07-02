#!/bin/bash

# Optional way to have text writing in etherpad and read the current text while compiling the latex project.
# Perhaps this should be part of the latex-project..

# add systime
systemtime <- paste("Compiled at",as.character(Sys.time()))
writeLines(systemtime, con = paste0(textsOutput,"systemtime.tex"))

library(RCurl)
library(stringr)

if (!(exists("local_text"))) local_text <- "./Text/TextOfAnalysis/gsyb2015.txt" # for the cronjob to run properly

download.file("https://pad.okfn.org/p/gsyb2015/export/txt", destfile = local_text, method = "curl")

tx <- readLines(local_text,encoding="UTF-8")

tx <- tx[-grep("^<!--",tx)] # exclude the rows that with begin "<!--"
title_rows <- grep("^§§", tx)

for (i in 1:length(title_rows)) {
  file_name <- str_trim(str_replace(tx[title_rows[i]], pattern = "§§", replacement = ""))
  file_name_raw <- str_replace(file_name, pattern = ".tex", replacement = "")
  if (i < length(title_rows)) content <- tx[(title_rows[i]+1):(title_rows[i+1]-1)]
  if (i == length(title_rows)) content <- tx[(title_rows[i]+1):length(tx)]
#   for (nr in 1:length(content)) {
#     content <- append(x = content, "", content[nr])
#   }
  writeLines(content, con = paste0(textsOutput,file_name_raw,".txt"), sep = "\n", useBytes = FALSE)
  system(paste("pandoc -o", paste0(textsOutput,file_name_raw,".tex"),
                paste0(textsOutput,file_name_raw,".txt")))
  
}


txt_files_to_remove <- list.files(path = textsOutput, pattern = ".txt", full.names = TRUE)
file.remove(txt_files_to_remove)
