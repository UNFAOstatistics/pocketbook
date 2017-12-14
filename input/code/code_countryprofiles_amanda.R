###########################################################################
## Country profiles
###########################################################################
# Read in the data for a particular publication
#temp <- read_csv(paste0("http://fenixservices.fao.org/faostat/static/bulkdownloads/",
#                        region_to_report,
#                        "_cp_data_final.csv"))

# read from Excel instead of csv
library(readxl)
url <- paste0("http://fenixservices.fao.org/faostat/static/bulkdownloads/",region_to_report,"_CP_data_final.xlsx")
destfile <- paste0(root.dir,"input_data/",region_to_report,"_CP_data_final.xlsx")
curl::curl_download(url, destfile)
temp <- read_excel(destfile, col_types = c("text", "text", "text", "text", 
                                           "text", "text"))

# Spesify the years!
year1 <- "1990"
year2 <- "2000"
year3 <- "2015"

# reformat the data a bit
temp <- temp %>% setNames(tolower(names(.))) %>% 
  rename(FAOST_CODE = areacode)

# read in the file specifying in indicators for a particular book
cinds <- read_csv(paste0(root.dir,"input/data/country_profile_indicators_",region_to_report,".csv"))

# Swap the english and russian topic & indicator names in the case of REU
if (region_to_report == "REU" & rulang){
  cinds <- cinds %>% 
    mutate(PART = PART_RU) %>% 
    rename(SERIES_NAME_SHORT_DAG = SERIES_NAME_SHORT) %>% 
    mutate(SERIES_NAME_SHORT = SERIES_NAME_SHORT_RU)
}

# set names for indicators (based on indicator codes!)
temp <- left_join(temp,
                  cinds %>% select(INDICATOR1,SERIES_NAME_SHORT,ORDER),
                  by = c("indicator" = "INDICATOR1")) %>% 
  rename(name = SERIES_NAME_SHORT)

# Add country names
temp$SHORT_NAME <- FAOcountryProfile$SHORT_NAME[match(temp$FAOST_CODE,FAOcountryProfile$FAOST_CODE)]

# Names for Regions and Subregions
## RAF

if (region_to_report == "RAF"){
  reg_names <- c("Africa",
                     "Central Africa",
                     "Eastern Africa",
                     "Northern Africa",
                     "Southern Africa",
                     "Western Africa")
  reg_codes <- 12000:12005
  reg_data <- data_frame(SHORT_NAME = reg_names,
                         FAOST_CODE = reg_codes)
}


## RAP
if (region_to_report == "RAP"){
  reg_names <- c("Asia and the Pacific",
                     "East Asia",
                     "Southeast Asia",
                     "Central Asia",
                     "Oceania",
                     "Southern Asia"
  )
  reg_codes <- c(13000, # Regional Office for Asia and the Pacific
                     13001, # East Asia
                     13003, # Southeast Asia
                     13005, # Central Asia
                     13006, # Oceania
                     13012 # Southhern Asia
  )
  reg_data <- data_frame(SHORT_NAME = reg_names,
                         FAOST_CODE = reg_codes)  
}

# REU
if (region_to_report == "REU"){
  reg_names <- c("Europe and Central Asia",
                 "Central Asia",
                 "Caucasus and Turkey",
                 "EU Central and Eastern",
                 "CIS Europe",
                 "EU Other and EFTA",
                 "South Eastern Europe")
  # translate into Russian
  if (rulang){
    reg_names <- translate_subgroups(reg_names, 
                                     isfactor = FALSE, 
                                     add_row_breaks = FALSE, 
                                     abbreviate = FALSE)
  }
  
  reg_codes <- c(445,5833,5840,5843,5841,5844,5842) # new codes by Amanda
  reg_data <- data_frame(SHORT_NAME = reg_names,
                         FAOST_CODE = reg_codes)
}


## RNE
if (region_to_report == "RNE"){
  reg_names <- c("Near East and North Africa",
                     "Gulf Cooperation\n Council States\n and Yemen",
                     "North Africa",
                     "Other Near\n East countries")
  reg_codes <- 15000:15003  
  reg_data <- data_frame(SHORT_NAME = reg_names,
                         FAOST_CODE = reg_codes)  
}
# Reorder in alphabetical order (forget "the")
temp_cntry <- temp %>% 
  mutate(FAOST_CODE = as.integer(FAOST_CODE)) %>% 
  mutate(ordervar = gsub("^the ", "", SHORT_NAME)) %>% 
  arrange(ordervar) %>% 
  select(-ordervar) %>% 
  # !!!!! remove aggregates as we dont have names from them yet!
  filter(FAOST_CODE <= 400)

temp_reg <- temp %>%
  mutate(FAOST_CODE = as.integer(FAOST_CODE)) %>% 
  filter(FAOST_CODE >= 400, FAOST_CODE != 5839) %>% 
  select(-SHORT_NAME) %>% 
  left_join(.,reg_data)

temp_all <- bind_rows(temp_reg,temp_cntry)
temp <- temp_all

## Perhaps Amanda can include these countries automatically and 
## all this have to do is to add footnotes!
# 
# data.frame(FAOST_CODE = c(68, # France
#                           185,  # Russian Federation
#                           231 # United States
# ),
# SHORT_NAME = c("France",
#                "Russian Federation",
#                "United States")







## =====================================================
## CUSTOM FUNCTIONS


## Sanitize the expression for Latex code
sanitizeToLatex <- function(str, html=FALSE, type=c("text","table")) {
  
  type <- match.arg(type)
  
  result <- as.character(str)
  
  result <- gsub("\\\\-","TEX.BACKSLASH",result)
  result <- gsub("\\\\","SANITIZE.BACKSLASH",result)
  result <- gsub("$","\\$",result,fixed=TRUE)
  result <- gsub(">","$>$",result,fixed=TRUE)
  result <- gsub("<","$<$",result,fixed=TRUE)
  result <- gsub("|","$|$",result,fixed=TRUE)
  result <- gsub("{","\\{",result,fixed=TRUE)
  result <- gsub("}","\\}",result,fixed=TRUE)
  result <- gsub("%","\\%",result,fixed=TRUE)
  result <- gsub("&","\\&",result,fixed=TRUE)
  result <- gsub("_","\\_",result,fixed=TRUE)
  ## result <- gsub("_", "\\textsubscript", result, fixed = TRUE)
  result <- gsub("#","\\#",result,fixed=TRUE)
  result <- gsub("^", ifelse(type == "table", "\\verb|^|",
                             "\\textsuperscript "), result,fixed = TRUE)
  result <- gsub("~","\\~{}",result,fixed=TRUE)
  result <- gsub("Ã´","\\^{o}",result,fixed=TRUE)
  result <- gsub("?","\\^{o}",result,fixed=TRUE)
  result <- gsub("Ã¢","\\^{a}",result,fixed=TRUE)
  result <- gsub("Ã¨","\\`{e}",result,fixed=TRUE)
  result <- gsub("?","\\`{e}",result,fixed=TRUE)
  result <- gsub("Ã©","\\'{e}",result,fixed=TRUE)
  result <- gsub("?","\\'{e}",result,fixed=TRUE)
  result <- gsub("?","\\'{o}",result,fixed=TRUE)
  result <- gsub("?","\\`{o}",result,fixed=TRUE)
  result <- gsub("?","\\'{i}",result,fixed=TRUE)
  result <- gsub("?","\\`{i}",result,fixed=TRUE)
  result <- gsub("?","\\'{I}",result,fixed=TRUE)
  result <- gsub("?","\\`{I}",result,fixed=TRUE)
  result <- gsub("?","\\r{A}",result,fixed=TRUE)
  result <- gsub("?","\\c{c}",result,fixed=TRUE)
  result <- gsub("?","\\'{a}",result,fixed=TRUE)
  result <- gsub("?","\\`{a}",result,fixed=TRUE)
  result <- gsub("?","\\'{A}",result,fixed=TRUE)
  result <- gsub("?","\\`{A}",result,fixed=TRUE)
  result <- gsub("?","\\'{u}",result,fixed=TRUE)
  result <- gsub("?","\\`{u}",result,fixed=TRUE)
  result <- gsub("?","\\~{n}",result,fixed=TRUE)
  result <- gsub("SANITIZE.BACKSLASH","$\\backslash$",result,fixed=TRUE)
  result <- gsub("TEX.BACKSLASH","\\-",result,fixed=TRUE)
  if(html) {
    result <- gsub("( www.[0-9A-Za-z./\\-\\_]*)"," \\\\url{\\1}",result)
    result <- gsub("(http://(www.)*[0-9A-Za-z./\\-\\_]*)","\\\\url{\\1}",result)
    dotSlash<-grepl("\\url\\{.*\\.}",result)
    result[dotSlash] <- gsub("\\.\\}","\\}\\.",result[dotSlash])
  }
  
  ## special expressions
  result <- gsub("km2", "km\\textsuperscript{2}", result, fixed = TRUE)
  result <- gsub("m3", "m\\textsuperscript{3}", result, fixed = TRUE)
  result <- gsub("CO2", "CO\\textsubscript{2}", result, fixed = TRUE)
  
  return(result)
}
## =====================================================



## ========================================================
## CREATE EMPTY TEX-FILE AND START POPULATING IT!"
dir.create("./output/process", showWarnings = FALSE, recursive = TRUE)
fileOut <- paste0(root.dir,"output/process/CountryProfiles.tex")
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
## ========================================================

## ========================================================
## LOOP ACROSS THE COUNTRIES IN THE DATA
uniq_faost_code <- unique(temp$FAOST_CODE)

for (i in 1:length(uniq_faost_code)){
  
  # country level data
  dtbl <- temp %>% filter(FAOST_CODE == uniq_faost_code[i]) %>% 
    select(FAOST_CODE,name,indicator,year,value,flag,ORDER,SHORT_NAME) %>% 
    arrange(ORDER)
  
  if (region_to_report == "REU" & rulang){
    dtbl$SHORT_NAME <- countrycode.multilang::countrycode(dtbl$FAOST_CODE, 
                                                             "fao", 
                                                             "country.name.russian.fao")
  } 

  ## ==================================================================
  ## LATEX-PREFIXES TO BE PRINTED BEFORE EACH COUNTRY TABLE
  row_color <- "FAOblue"
  if (region_to_report == "COF") row_color <- "part7"
  define_row_color <- paste0("\\rowcolors{1}{",row_color,"!10}{white}")
  
  cat("\\CountryData{", unique(dtbl$SHORT_NAME), # Country name
      "}",
      define_row_color,
      "\\begin{tabular}{L{4.20cm} R{1cm} R{1cm} R{1cm}}
          \\toprule
          \\multicolumn{1}{c}{} & \\multicolumn{1}{c}{", year1, "} & \\multicolumn{1}{c}{", year2, "} & \\multicolumn{1}{c}{", year3, "} \\\\
          \\midrule\n",
      file = fileOut, append = TRUE)
  ## ==================================================================
  
  
  ## ==================================================================
  ## CONTENT OF THE TABLES!

  for (ind1 in unique(dtbl$indicator)){
   
    # Value for year1
    value1 <- dtbl %>% filter(indicator %in% ind1,
                              year %in% year1) %>% 
      mutate(new_value = ifelse(flag == "i", paste0("\\textit{",value,"}"), value)) %>% pull(new_value)
    # Value for year2
    value2 <- dtbl %>% filter(indicator %in% ind1,
                              year %in% year2) %>% 
      mutate(new_value = ifelse(flag == "i", paste0("\\textit{",value,"}"), value)) %>% pull(new_value)
    # Value for year3
    value3 <- dtbl %>% filter(indicator %in% ind1,
                              year %in% year3) %>% 
      mutate(new_value = ifelse(flag == "i", paste0("\\textit{",value,"}"), value)) %>% pull(new_value)
    # Indicator name
    ind_name <- dtbl %>% filter(indicator %in% ind1) %>% distinct(name) %>% pull(name)
    
    # Write it all to the file!
    cat("\t ~ ", sanitizeToLatex(ind_name), " & ", value1, " ~ \\ \\ & ", value2, " ~ \\ \\ & ", value3, " ~ \\ \\ \\\\ \n",
        file = fileOut, append = TRUE, sep = "")
    
  

  }
  ## ==================================================================
  
  
  

  ## ==================================================================
  ## LATEX-POSTFIXES TO BE PRINTED AFTER EACH COUNTRY TABLE!
  cat("\ \ \ \ \ \ \ \\toprule
      \\end{tabular}
      \\clearpage\n",
      file = fileOut, append = TRUE)
  ## ==================================================================
}

