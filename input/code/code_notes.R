d_list <- yaml::yaml.load_file(paste0(root.dir,"/input/data/notes.yaml"))
d <- plyr::ldply (d_list, data.frame, stringsAsFactors=FALSE)

par1 <- d[d$id == region_to_report,"paragraph1"]
par2 <- d[d$id == region_to_report,"paragraph2"]
par3 <- d[d$id == region_to_report,"paragraph3"]
par4 <- d[d$id == region_to_report,"paragraph4"]
par5 <- d[d$id == region_to_report,"paragraph5"]
par6 <- d[d$id == region_to_report,"paragraph6"]
par7 <- d[d$id == region_to_report,"paragraph7"]
par8 <- d[d$id == region_to_report,"paragraph8"]




if (table_type == "latex"){
  
  if (nchar(par2) == 0) par2 <- "\n \\vspace{-2mm} \n\n"
  
    cat(paste0(par1,
              "\n \\vspace{2mm} \n\n",
              par2,
              "\n \\vspace{2mm} \n\n",
              par3,
              "\n \\vspace{2mm} \n\n",
              par4,
              "\n \\vspace{2mm} \n\n",
              par5,
              "\n \\vspace{2mm} \n\n",
              par6,
              "\n \\vspace{2mm} \n\n",
              par7,
              "\n \\vspace{2mm} \n\n",
              par8))
    
  }

if (table_type == "html"){
  
  if (nchar(par2) != 0) par2 <- paste(par2, "<br><br>")
  
  par1 <- gsub(par1, pattern = "\\\\url\\{", replacement = "<")
  par1 <- gsub(par1, pattern = "\\}", replacement = ">")
  par5 <- gsub(par5, pattern = "\\\\textasciitilde\\{\\}", replacement = "~")
  par6 <- gsub(par6, pattern = "\\\\begin\\{itemize\\} \\\\item", replacement = "<ul><li>")
  par6 <- gsub(par6, pattern = "\\\\end\\{itemize\\}", replacement = "</li></ul>")
  par6 <- gsub(par6, pattern = "\\\\item", replacement = "</li><li>")
  par6 <- gsub(par6, pattern = "\\\\", replacement = "")
  
    cat(paste0(par1,
               "<br><br>",
               par2,
               par3,
               "<br><br>",
               par4,
               "<br><br>",
               par5,
               "<br><br>",
               par6,
               "<br>",
               par7,
               "<br><br>",
               par8))
    

}