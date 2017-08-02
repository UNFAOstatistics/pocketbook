# Read in the data for a particular publication

region_to_report <- "REU"
rulang <-  FALSE

year1 <- 1990
year2 <- 2000
year3 <- 2015

temp <- read_csv(paste0("http://fenixservices.fao.org/faostat/static/bulkdownloads/",region_to_report,"_cp_data_final.csv"))

# reformat the data a bit
temp <- temp %>% setNames(tolower(names(.))) %>% 
  rename(FAOST_CODE = areacode)

# create a .tex file for the output

dir.create("./output/process", showWarnings = FALSE, recursive = TRUE)
fileOut <- paste0("./output/process/countryprofile_",region_to_report,".tex")
file.create(fileOut)

# Few presets for the whole table section!!
tbl_row_height <- 1.12
if (region_to_report == "COF") tbl_row_height <- 1.42
if (region_to_report == "RAF") tbl_row_height <- 1.18
if (region_to_report == "RAP") tbl_row_height <- 1.12
if (region_to_report == "REU" & rulang) tbl_row_height <- .80
if (region_to_report == "REU" & !rulang) tbl_row_height <- 1.06
if (region_to_report == "RNE") tbl_row_height <- 1.12
if (region_to_report == "GLO") tbl_row_height <- 1.22

cat(paste0("\\renewcommand{\\arraystretch}{",tbl_row_height,"}\n"),
    file = fileOut, append = TRUE)

cat("\\setlength{\\tabcolsep}{4pt}\n",
    file = fileOut, append = TRUE) ## Reduce the space between columns
cat("\\normalsize\n",
    file = fileOut, append = TRUE)


# Loop across the countries in the data

uniq_faost_code <- unique(temp$FAOST_CODE)

for (i in 1:length(uniq_faost_code)){
  
  ctemp <- temp[temp$FAOST_CODE == uniq_faost_code[i],]
  # 
  cname <- FAOcountryProfile %>% filter(FAOST_CODE == uniq_faost_code[i]) %>% pull(SHORT_NAME)
  # read in the file specifying in indicators for a particular book
  cinds <- read_csv(paste0("./input/data/country_profile_indicators_",region_to_report,".csv"))

  # set names for indicators (based on indicator codes!)
  ctemp$name <- cinds$SERIES_NAME_SHORT[match(cinds$INDICATOR1,ctemp$indicator)]
  
  tbl_print <- ctemp %>% 
    select(name,year,value_char) %>% 
    mutate(value_char = ifelse(is.na(value_char) , "NA", value_char)) %>% 
    spread(data = ., key = year, value = value_char)

  # conditional row colors ------------------------------
  row_color <- "FAOblue"
  if (region_to_report == "COF") row_color <- "part7"
  define_row_color <- paste0("\\rowcolors{1}{",row_color,"!10}{white}")
  
  # if (rulang){
    
    # if (M49countries[i,"SHORT_NAME"] %in% REU_reg_names_ru){
    #   cat("\\CountryData{", M49countries[i, "SHORT_NAME"], "\\textsuperscript{\\ddag} }", # asterisk for France, Russia & US
    #       define_row_color,
    #       "\\begin{tabular}{L{4.20cm} R{1cm} R{1cm} R{1cm}}
    #         \\toprule
    #         \\multicolumn{1}{c}{} & \\multicolumn{1}{c}{", year1, "} & \\multicolumn{1}{c}{", year2, "} & \\multicolumn{1}{c}{", year3, "} \\\\
    #         \\midrule\n",
    #       file = fileOut, append = TRUE)
    # }
    # if (!M49countries[i,"SHORT_NAME"] %in% REU_reg_names_ru){
    #   cat("\\CountryData{", M49countries[i, "SHORT_NAME"], "}",
    #       define_row_color,
    #       "\\begin{tabular}{L{4.20cm} R{1cm} R{1cm} R{1cm}}
    #           \\toprule
    #           \\multicolumn{1}{c}{} & \\multicolumn{1}{c}{", year1, "} & \\multicolumn{1}{c}{", year2, "} & \\multicolumn{1}{c}{", year3, "} \\\\
    #           \\midrule\n",
    #       file = fileOut, append = TRUE)
    # }
    
  # } else {
    
    # if (M49countries[i,"SHORT_NAME"] %in% REU_reg_names){
    #   cat("\\CountryData{", M49countries[i, "SHORT_NAME"], "\\textsuperscript{\\ddag} }", # asterisk for France, Russia & US
    #       define_row_color,
    #       "\\begin{tabular}{L{4.20cm} R{1cm} R{1cm} R{1cm}}
    #         \\toprule
    #         \\multicolumn{1}{c}{} & \\multicolumn{1}{c}{", year1, "} & \\multicolumn{1}{c}{", year2, "} & \\multicolumn{1}{c}{", year3, "} \\\\
    #         \\midrule\n",
    #       file = fileOut, append = TRUE)
    # }
    # if (!M49countries[i,"SHORT_NAME"] %in% REU_reg_names){
  cat("\\CountryData{", cname, "}",
      define_row_color,
      "\\begin{tabular}{L{4.20cm} R{1cm} R{1cm} R{1cm}}
          \\toprule
          \\multicolumn{1}{c}{} & \\multicolumn{1}{c}{", year1, "} & \\multicolumn{1}{c}{", year2, "} & \\multicolumn{1}{c}{", year3, "} \\\\
          \\midrule\n",
      file = fileOut, append = TRUE)
  
  
  
  
    # }
    
  # }
  
  print(xtable(tbl_print), include.rownames = FALSE, file = fileOut, append = TRUE)
  
  
}

