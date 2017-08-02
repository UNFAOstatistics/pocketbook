###########################################################################
## This script generates the ICN2 Statistical Pocketbook 2014
###########################################################################

###########################################################################
## Country profiles
###########################################################################

temp <- syb.df

if (!exists("temp$missing")) {

  temp$missing <- "not def"

}

library(dplyr)
library(xtable)
library(lazyeval)
library(tidyr)
library(stringr)
library(scales)
library(ggplot2)
library(readxl)
library(zoo)

##' Sanitize the expression for Latex code
##'
##' The function sanitize an expression for the Latex code
##'
##' @param str A string to be sanitized
##' @param html
##' @param type Specify whether the expression is for text or table
##' @export

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


sanitizeToHTML <- function(str, html=FALSE, type=c("text","table")) {

  type <- match.arg(type)

  result <- as.character(str)

  # result <- gsub("^", ifelse(type == "table", "\\verb|^|",
  #                            "\\textsuperscript "), result,fixed = TRUE)

  ## special expressions
  # result <- gsub("km2", "km\\textsuperscript{2}", result, fixed = TRUE)
  # result <- gsub("m3", "m\\textsuperscript{3}", result, fixed = TRUE)
  # result <- gsub("CO2", "CO\\textsubscript{2}", result, fixed = TRUE)

  return(result)
}




## -------------------------------------------------------------------------------------
# merge with FSI with temp for country profiles


if (!("FS.DA.ADESA.PCT3D" %in% names(temp))) {

  dat <- readRDS(paste0(data.dir,"/fsi_data.RDS")) # manipulated in code_part2.R
  # dat <- readRDS(paste0(data.dir,"/fsi_data_old.RDS")) %>% # Old fsi data

  dat <- dat[!duplicated(dat[c("FAOST_CODE","Year")]),]
  vars_to_exclude <- names(temp)[names(temp) %in% names(dat)][c(-1:-4,-14)]
  # myvars <- names(temp) %in% vars_to_exclude
  # temp <- temp[!myvars]
  myvars <- names(dat) %in% vars_to_exclude
  dat <- dat[!myvars]

  vars_to_exclude <- c("FAO_TABLE_NAME","SHORT_NAME","Area")
  # myvars <- names(dat) %in% vars_to_exclude
  # dat <- dat[!myvars]
  myvars <- names(temp) %in% vars_to_exclude
  temp <- temp[!myvars]


  temp <- merge(temp,dat,by=c("FAOST_CODE","Year"),all.x=TRUE)
}


# temp %>% select(FAOST_CODE,Year,
#                   # FS.OU.VAD.PCT,
#                   # SI.POV.DDAY,
#                   # GN_6808_72182,
#                   ILO_female_emp_agri) %>%
#   # filter(!is.na(FS.OU.VAD.PCT)) %>%
#   # filter(!is.na(SI.POV.DDAY)) %>%
#   filter(!is.na(ILO_female_emp_agri)) %>%
#   # group_by(FAOST_CODE) %>% filter(Year == max(Year)) %>%
#   left_join(.,FAOcountryProfile[c("FAOST_CODE","FAO_TABLE_NAME")]) %>%
#   arrange(FAO_TABLE_NAME) %>%
#   filter(FAO_TABLE_NAME == "China") %>%
#   as.data.frame(.)


# An awful china hack

temp_357 <- temp %>% filter(FAOST_CODE == 357)
temp_41 <- temp %>% filter(FAOST_CODE == 41)
temp_351 <- temp %>% filter(FAOST_CODE == 351)

for (y in unique(temp_351$Year)) {
  for (i in unique(names(temp_351))) {
    temp_351[temp_351$Year == y, i] <- ifelse(is.na(temp_351[temp_351$Year == y,i]),
                                                    temp_41[temp_41$Year == y, i],
                                                    temp_351[temp_351$Year == y, i])
    
  }
}

for (y in unique(temp_351$Year)) {
  for (i in unique(names(temp_351))) {
    temp_351[temp_351$Year == y, i] <- ifelse(is.na(temp_351[temp_351$Year == y,i]),
                                                  temp_357[temp_357$Year == y, i],
                                                  temp_351[temp_351$Year == y, i])
    
  }
}

# sum(colSums(is.na(temp_357)))
# sum(colSums(is.na(temp_41)))
# sum(colSums(is.na(temp_351)))

temp_tmp <- temp %>% filter(FAOST_CODE != 351) 
temp <- bind_rows(temp_tmp,temp_351)
  
  # 
# temp_351
# 
# for (i in names(temp)){
#   for (y in 1990:2020){
#           
#     temp[[i]] <- ifelse(temp$FAOSTAT_CODE == 351 & temp$Year == y & is.na(temp[[i]]),
#                           temp_357[temp_357$Year == y, i],
#                           temp[i])
#     temp[[i]] <- ifelse(temp$FAOSTAT_CODE == 351 & temp$Year == y & is.na(temp[[i]]),
#                           temp_41[temp_41$Year == y, i],
#                           temp[i])
#   }
# }



if (!("cropping_intensity_ratio" %in% names(temp))) {

  temp$cropping_intensity_ratio <- temp$area_harvested / temp$RL.AREA.AGR.HA.NO
}

## Fertilisers
if (!("nitrogen_tonnes_per_ha" %in% names(temp))) {

  temp$phosphate_tonnes_per_ha <- temp$phosphate_tonnes / temp$RL.AREA.AGR.HA.NO
  temp$potash_tonnes_per_ha <- temp$potash_tonnes / temp$RL.AREA.AGR.HA.NO
  temp$nitrogen_tonnes_per_ha <- temp$nitrogen_tonnes / temp$RL.AREA.AGR.HA.NO
  temp$total_nutrients_tonnes_per_ha <- (temp$phosphate_tonnes +
                                             temp$potash_tonnes +
                                             temp$nitrogen_tonnes) / temp$RL.AREA.AGR.HA.NO
  
}


# if (!("aqua_culture_share" %in% names(temp))) {
#
#   temp$aqua_culture_share <- temp$FI.PRD.AQ.TN.NO / (temp$FI.PRD.AQ.TN.NO + temp$FI.PRD.CAPT.TN.NO) *100
# }

if (!("rural_pop_share" %in% names(temp))) {

  temp$rural_pop_share <- temp$OA.TPR.POP.PPL.NO / temp$OA.TPBS.POP.PPL.NO *100
}

temp$agricultural_exports_share <- temp$TP_5922_1882 / temp$NY.GDP.MKTP.CD  * 100


## Water indicators for China

# Markus, please describe this
water_vars <- names(temp)[grep("^AQ.", names(temp))]
water_vars_plus <- c("Year","FAOST_CODE",water_vars)
water_vars.df <- temp[water_vars_plus]
water_vars.df <- water_vars.df[water_vars.df$FAOST_CODE != 351,]
water_vars.df$FAOST_CODE[water_vars.df$FAOST_CODE == 357] <- 351

myvars <- names(temp) %in% water_vars
temp <- temp[!myvars]

water_vars.df <- water_vars.df[!duplicated(water_vars.df[c("FAOST_CODE","Year")]),]

temp <- dplyr::left_join(temp,water_vars.df)


#########################################################
#                                                       #
#    REU regional book vars                             #
#                                                       #
#########################################################


if (!("agr_employment_male_female" %in% names(temp)) & region_to_report == "REU"){

  # MALE FEMALE STUFF

  temp$overweight <- paste(round(temp$overweight_MLE,1),round(temp$overweight_FMLE,1), sep="/")
  temp$overweight[temp$overweight %in% "NA/NA"] <- NA

  temp$obesity <- paste(round(temp$obesity_MLE,1),round(temp$obesity_FMLE,1), sep="/")
  temp$obesity[temp$obesity %in% "NA/NA"] <- NA

  temp$tot_pop_male_female <- paste(round((temp$OA.TPM.POP.PPL.NO/1000000),0),round((temp$OA.TPF.POP.PPL.NO/1000000),0),sep="/")
  temp$tot_pop_male_female[temp$tot_pop_male_female %in% "NA/NA"] <- NA

  #temp$rural_pop_gender_shares <- paste(round(temp$rural_male_share,1),round(temp$rural_female_share,1), sep="/")
  # New source ILO
  temp$rural_pop_gender_shares <- paste(round(temp$rural_male_share,1),round(temp$rural_female_share,1), sep="/")
  temp$rural_pop_gender_shares[temp$rural_pop_gender_shares %in% "NA/NA"] <- NA

  temp$agr_employment_male_female <- paste(round(temp$SL.AGR.EMPL.MA.ZS,1),round(temp$SL.AGR.EMPL.FE.ZS,1),sep="/")
  temp$agr_employment_male_female[temp$agr_employment_male_female %in% "NA/NA"] <- NA

}

# LEts remove TEMPORARILY the 
# "SH.STA.STNT.ZS.x"
# "SH.STA.WAST.ZS.x" 
# "SH.STA.STNT.ZS.y"
# "SH.STA.WAST.ZS.y" 

if ("SH.STA.STNT.ZS.y" %in% names(temp)){
  temp <- temp %>% select(-SH.STA.STNT.ZS.y,-SH.STA.WAST.ZS.y)
  names(temp)[names(temp) %in% "SH.STA.STNT.ZS.x"] <- "SH.STA.STNT.ZS"
  names(temp)[names(temp) %in% "SH.STA.WAST.ZS.x"] <- "SH.STA.WAST.ZS"
  }


# Countries ---------------------------------------------------------------



# if (region_to_report == "COF"){
# M49countries <- region_key[which(region_key[[region_to_report]]),c("FAOST_CODE","SHORT_NAME")]
# this used to be short name as above, but we shall use fao_table_name instead. However, everything hereafter is hard
# coded for SHORT_NAME, so we just rename the FAO_TABLE_NAME into SHORT_NAME

M49countries <- region_key[which(region_key[[region_to_report]]),c("FAOST_CODE","FAO_TABLE_NAME")]
M49countries <- as.data.frame(M49countries)
names(M49countries)[names(M49countries)=="FAO_TABLE_NAME"] <- "SHORT_NAME"

# Reorder in alphabetical order (forget "the")
M49countries <- M49countries %>% 
  mutate(ordervar = gsub("^the ", "", SHORT_NAME)) %>% 
  arrange(ordervar) %>% 
  select(-ordervar) 

# } else {
# M49countries <- FAOcountryProfile %>% filter(UNSD_WORLD_REG %in% "World") %>%
#   select(FAOST_CODE, SHORT_NAME) %>%  arrange(SHORT_NAME) %>%  na.omit()
# M49countries <- as.data.frame(M49countries)
# }

## Remove old countries
OldCountries <-
  data.frame(FAOST_CODE = c(15,51,62,151,164,186,228,206,247,246,248,
                            17,83,196,191),
             COUNTRY_NAME = c("Belgium-Luxembourg", "Czechoslovakia",
                              "Ethiopia PDR", "Netherlands Antilles",
                              "Pacific Islands",
                              "Serbia and Montenegro", "Soviet Union",
                              "Sudan (former)", "Yemen (former)",
                              "Yemen (old)", "Yugoslav SFR",
                              "Bermuda","Kiribati","Seychelles","Saint Vincent and the Grenadines "),
             stringsAsFactors = FALSE)
M49countries <-
  M49countries[!M49countries[, "FAOST_CODE"] %in%
                 OldCountries[, "FAOST_CODE"],]
## Remove the countries we do not want to show: ,
tbr.df <-
  data.frame(COUNTRY_NAME = c("Aland Island",
                              "American Samoa",
                              # "Andorra",
                              "Anguilla",
                              "Antigua and Barbuda",
                              "Aruba",
                              "Bahamas",
                              "Bonaire",
                              "British Virgin Islands",
                              "Cayman",
                              # "Cook Islands",
                              "Curacao",
                              "Dominica",
                              "Equatorial Guinea",
                              "Falkland Islands (Malvinas)",
                              "Faroe",
                              "French Guiana",
                              "French Polynesia",
                              "Gibraltar",
                              "Greenland",
                              "Grenada",
                              "Guadeloupe",
                              "Guam",
                              "Guernsey",
                              "Holy See",
                              "Isle of Man",
                              "Jersey",
                              "Liechtenstein",
                              # "Marshall Islands",
                              "Martinique",
                              "Mayotte",
                              # "Micronesia",
                              # "Monaco",
                              "Montserrat",
                              # "Nauru",
                              "New Caledonia",
                              # "Niue",
                              "Norfolk Island",
                              "Northern Mariana Islands",
                              #"Oman",
                              #"Palau",
                              #"Papua New Guinea",
                              "Pitcairn Islands",
                              "Puerto Rico", 
                              #"Qatar",
                              "Reunion",
                              "Saint Barthelemi",
                              "Saint Helena",
                              "Saint Kitts",
                              "Saint Lucia",
                              "Saint Pierre",
                              # "San Marino",
                              "Sark",
                              "Saint-Martin (French Part)",
                              "Sint Maarten (Dutch Part)",
                              "Singapore",
                              "Svalbard and Jan Mayen Islands",
                              "Tokelau",
                              #"Tonga",
                              "Turks and Caicos Islands",
                              #"Tuvalu",
                              "United States Virgin Islands",
                              "Wallis and Futuna Islands",
                              "Western Sahara"),
             FAOST_CODE = c(284,
                            5,
                            # 6, # Andorra
                            258,
                            8,
                            22,
                            12,
                            278,
                            239,
                            36,
                            #47, # Cook Islands
                            279,
                            55,
                            61,
                            65,
                            64,
                            69,
                            70,
                            82,
                            85,
                            86,
                            87,
                            88,
                            274,
                            94,
                            264,
                            283,
                            125,
                            # 127, # Marshall Islands
                            135,
                            270,
                            # 145, # Micronesia
                            # 140, # Monaco
                            142,
                            # 148, # Nauru
                            153,
                            # 160, # Niue
                            161,
                            163,
                            #221, # Oman
                            # 180, # Palau
                            # 168, # Papua New Guinea
                            172,
                            177,
                            #179, # Qatar
                            182,
                            282,
                            187,
                            188,
                            189,
                            190,
                            #192, # San Marino
                            285,
                            281,
                            280,
                            200,
                            260,
                            218,
                            #219, # Tonga
                            224,
                            # 227, # Tuvalu
                            240,
                            243,
                            205),
             stringsAsFactors = FALSE)

M49countries <-
  M49countries[!M49countries[, "FAOST_CODE"] %in% tbr.df[, "FAOST_CODE"],]
## Occupied Palestinian Territory
M49countries[M49countries[, "FAOST_CODE"] == 299, "SHORT_NAME"] <-
  "West Bank and Gaza Strip"
## Chinas
M49countries <-
  M49countries[!M49countries[, "FAOST_CODE"] %in% c(41,128,96,357,214),]

# M49countries <- head(M49countries)

## Add aggregates NOT

RAF_reg_names <- c("Africa",
                   "Central Africa",
                   "Eastern Africa",
                   "Northern Africa",
                   "Southern Africa",
                   "Western Africa")
RAP_reg_names <- c("Asia and the Pacific",
                   "East Asia",
                   "Southeast Asia",
                   "Central Asia",
                   "Oceania",
                   "Southern Asia"
                   )
REU_reg_names <- c("Europe and Central Asia",
                   "Central Asia",
                   "Caucasus and Turkey",
                   "EU Central and Eastern",
                   "CIS Europe",
                   "EU Other and EFTA",
                   "South Eastern Europe")

REU_reg_names_ru <- translate_subgroups(REU_reg_names, 
                                        isfactor = FALSE, 
                                        add_row_breaks = FALSE, 
                                        abbreviate = FALSE)

RNE_reg_names <- c("Near East and North Africa",
                   "Gulf Cooperation\n Council States\n and Yemen",
                   "North Africa",
                   "Other Near\n East countries")

if (region_to_report == "RAF"){
  M49countries <-
    rbind(data.frame(FAOST_CODE = 12000:12005,
                     SHORT_NAME = RAF_reg_names,
                     stringsAsFactors = FALSE),
          M49countries)
}
if (region_to_report == "RAP"){
  # Combine region countries with extra countries France, Russia, US

  M49countries <-
    rbind(
    M49countries,
    data.frame(FAOST_CODE = c(68, # France
                              185,  # Russian Federation
                              231 # United States
    ),
    SHORT_NAME = c("France",
                   "Russian Federation",
                   "United States"),
    stringsAsFactors = FALSE))

  # Into alphabetical order
  M49countries <- arrange(M49countries, SHORT_NAME)

  M49countries <-
    rbind(data.frame(FAOST_CODE = c(13000, # Regional Office for Asia and the Pacific
                                    13001, # East Asia
                                    13003, # Southeast Asia
                                    13005, # Central Asia
                                    # 13006, # Australia and New Zealand
                                    13006, # Oceania
                                    13012 #, # Southhern Asia
                                    # excludinf for now
                                    # 23000 # FAO RAP member countries #**
                                    # 68, # France
                                    # 13008, #Melanesia
                                    # 13009, #Micronesia
                                    # 13010, # Polynesia
                                    # 185,  # Russian Federation
                                    # 231#, # United States
                                    # 13014 # Western Asia
                                    ),
                     SHORT_NAME = c(RAP_reg_names#,
                                    #"FAO RAP member countries" #**
                                    ),
                     stringsAsFactors = FALSE),
          M49countries)
}
if (region_to_report == "REU" & !rulang){
  M49countries <-
    rbind(data.frame(FAOST_CODE = c(14000,14001,14002,14003,14004,
                                    #14005, # Exclude Israel from country groupings
                                    14006,14007),
                     SHORT_NAME = REU_reg_names,
                     stringsAsFactors = FALSE),
          M49countries)
}
if (region_to_report == "REU" & rulang){
  M49countries <-
    rbind(data.frame(FAOST_CODE = c(14000,14001,14002,14003,14004,
                                    #14005, # Exclude Israel from country groupings
                                    14006,14007),
                     SHORT_NAME = REU_reg_names_ru,
                     stringsAsFactors = FALSE),
          M49countries)
  M49countries$SHORT_NAME_RU <- countrycode.multilang::countrycode(M49countries$FAOST_CODE, "fao", "country.name.russian.fao")
  M49countries$SHORT_NAME <- ifelse(!is.na(M49countries$SHORT_NAME_RU), M49countries$SHORT_NAME_RU, M49countries$SHORT_NAME)
}
if (region_to_report == "RNE"){
  M49countries <-
    rbind(data.frame(FAOST_CODE = 15000:15003,
                     SHORT_NAME = RNE_reg_names,
                     stringsAsFactors = FALSE),
          M49countries)
}


# M49countries <-
#   rbind(data.frame(FAOST_CODE = c(5000,5100,5300,5205,5500),
#                    SHORT_NAME = c("World",
#                                       "Africa", "Asia",
#                                       "Latin America and the Caribbean",
#                                       "Oceania"),
#                    stringsAsFactors = FALSE),
#         M49countries)

# Subset countries per region -----------------------

# Add region key and subset


# Indicators --------------------------------------------------------------

## Load the indicators we want to show
indicators.df <- read.csv(paste0(root.dir,"input/data/country_profile_indicators_",region_to_report,".csv"), na.strings = "", stringsAsFactors = FALSE)


if (rulang){
  indicators.df$PART <- NULL
  names(indicators.df)[names(indicators.df) %in% "PART_RU"] <- "PART"
  indicators.df$SERIES_NAME_SHORT_DAG <- indicators.df$SERIES_NAME_SHORT
  indicators.df$SERIES_NAME_SHORT <- NULL
  names(indicators.df)[names(indicators.df) %in% "SERIES_NAME_SHORT_RU"] <- "SERIES_NAME_SHORT"
}

# M49countries <- M49countries[1:10,]

# source(paste0(root.dir,"input/code/table/tableInfo.R"))
if (table_type == "latex"){

  ## Create the new .tex file
  fileOut <- paste0(root.dir,"output/process/CountryProfiles.tex")
  if(file.exists(fileOut)) file.remove(fileOut)
  file.create(fileOut)
  ## Subset the dataset
  CountryProfile.df <-
    temp[, colnames(temp) %in% c("FAOST_CODE", "SHORT_NAME", "Year",
                                     na.omit(indicators.df[, "INDICATOR1"]),
                                     na.omit(indicators.df[, "INDICATOR2"]),
                                     na.omit(indicators.df[, "INDICATOR3"]))]
  if ("OA.TPBS.POP.PPL.NO" %in% names(CountryProfile.df)) CountryProfile.df[, "OA.TPBS.POP.PPL.NO"] <- CountryProfile.df[, "OA.TPBS.POP.PPL.NO"]/1000000
  if ("OA.TPR.POP.PPL.NO" %in% names(CountryProfile.df)) CountryProfile.df[, "OA.TPR.POP.PPL.NO"] <- CountryProfile.df[, "OA.TPR.POP.PPL.NO"]/1000000




  # Multiplying
  indicators.df$MULTIPLIER <- factor(indicators.df$MULTIPLIER)
  indicators.df$MULTIPLIER <- as.numeric(levels(indicators.df$MULTIPLIER))[indicators.df$MULTIPLIER]

  multip.df <- indicators.df[!is.na(indicators.df$MULTIPLIER),]


  for (name in names(CountryProfile.df)) {
    if (name %in% multip.df$INDICATOR1) CountryProfile.df[[name]] <- CountryProfile.df[[name]] / multip.df[multip.df$INDICATOR1 %in% name,]$MULTIPLIER
  }


  # TeX file ----------------------------------------------------------------

  ## Years to be shown in the country profile
  year1 = 1990
  year2 = 2000
  year3 = 2015
  ## This script creates the latex file

  ## Set the rowheight for cprofiles for each book

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
  for (i in 1:nrow(M49countries)) {
    ## header

    # conditional row colors ------------------------------
    row_color <- "FAOblue"
    if (region_to_report == "COF") row_color <- "part7"
    define_row_color <- paste0("\\rowcolors{1}{",row_color,"!10}{white}")
    # conditional row colors ------------------------------

    if (region_to_report %in% c("COF","GLO")){
      cat("\\CountryData{", M49countries[i, "SHORT_NAME"], "}",
          define_row_color,
          "\\begin{tabular}{L{4.6cm} R{0.9cm} R{0.9cm} R{0.9cm}}
          \\toprule
          \\multicolumn{1}{c}{} & \\multicolumn{1}{c}{", year1, "} & \\multicolumn{1}{c}{", year2, "} & \\multicolumn{1}{c}{", year3, "} \\\\
          \\midrule\n",
          file = fileOut, append = TRUE)
    }

    
    # RAP  customatisation
    if (region_to_report %in% "RAP"){

      if (M49countries[i,"SHORT_NAME"] %in% c("France","United States","Russian Federation",RAP_reg_names)){
        cat("\\CountryData{", M49countries[i, "SHORT_NAME"], "\\textsuperscript{\\ddag} }", # asterisk for France, Russia & US
            define_row_color,
            "\\begin{tabular}{L{4.05cm} R{1cm} R{1cm} R{1cm}}
          \\toprule
          \\multicolumn{1}{c}{} & \\multicolumn{1}{c}{", year1, "} & \\multicolumn{1}{c}{", year2, "} & \\multicolumn{1}{c}{", year3, "} \\\\
          \\midrule\n",
            file = fileOut, append = TRUE)
      }
      if (!M49countries[i,"SHORT_NAME"] %in% c("France","United States","Russian Federation",RAP_reg_names)){
        cat("\\CountryData{", M49countries[i, "SHORT_NAME"], "}",
            define_row_color,
            "\\begin{tabular}{L{4.05cm} R{1cm} R{1cm} R{1cm}}
            \\toprule
            \\multicolumn{1}{c}{} & \\multicolumn{1}{c}{", year1, "} & \\multicolumn{1}{c}{", year2, "} & \\multicolumn{1}{c}{", year3, "} \\\\
            \\midrule\n",
            file = fileOut, append = TRUE)
      }
    }
    if (region_to_report %in% "RAF"){

      if (M49countries[i,"SHORT_NAME"] %in% RAF_reg_names){
        cat("\\CountryData{", M49countries[i, "SHORT_NAME"], "\\textsuperscript{\\ddag} }", # asterisk for France, Russia & US
            define_row_color,
            "\\begin{tabular}{L{4.0cm} R{1cm} R{1cm} R{1cm}}
          \\toprule
          \\multicolumn{1}{c}{} & \\multicolumn{1}{c}{", year1, "} & \\multicolumn{1}{c}{", year2, "} & \\multicolumn{1}{c}{", year3, "} \\\\
          \\midrule\n",
            file = fileOut, append = TRUE)
      }
      if (!M49countries[i,"SHORT_NAME"] %in% RAF_reg_names){
        cat("\\CountryData{", M49countries[i, "SHORT_NAME"], "}",
            define_row_color,
            "\\begin{tabular}{L{4.0cm} R{1cm} R{1cm} R{1cm}}
            \\toprule
            \\multicolumn{1}{c}{} & \\multicolumn{1}{c}{", year1, "} & \\multicolumn{1}{c}{", year2, "} & \\multicolumn{1}{c}{", year3, "} \\\\
            \\midrule\n",
            file = fileOut, append = TRUE)
      }
    }
    if (region_to_report %in% "RNE"){

      if (M49countries[i,"SHORT_NAME"] %in% RNE_reg_names){
          cat("\\CountryData{", M49countries[i, "SHORT_NAME"], "\\textsuperscript{\\ddag} }", # asterisk for France, Russia & US
              define_row_color,
              "\\begin{tabular}{L{4.0cm} R{1cm} R{1cm} R{1cm}}
            \\toprule
            \\multicolumn{1}{c}{} & \\multicolumn{1}{c}{", year1, "} & \\multicolumn{1}{c}{", year2, "} & \\multicolumn{1}{c}{", year3, "} \\\\
            \\midrule\n",
              file = fileOut, append = TRUE)
      }
      if (!M49countries[i,"SHORT_NAME"] %in% RNE_reg_names){
        cat("\\CountryData{", M49countries[i, "SHORT_NAME"], "}",
            define_row_color,
            "\\begin{tabular}{L{4.0cm} R{1cm} R{1cm} R{1cm}}
              \\toprule
              \\multicolumn{1}{c}{} & \\multicolumn{1}{c}{", year1, "} & \\multicolumn{1}{c}{", year2, "} & \\multicolumn{1}{c}{", year3, "} \\\\
              \\midrule\n",
            file = fileOut, append = TRUE)
      }
    }
    if (region_to_report %in% "REU"){
      
      if (rulang){
        
        if (M49countries[i,"SHORT_NAME"] %in% REU_reg_names_ru){
          cat("\\CountryData{", M49countries[i, "SHORT_NAME"], "\\textsuperscript{\\ddag} }", # asterisk for France, Russia & US
              define_row_color,
              "\\begin{tabular}{L{4.20cm} R{1cm} R{1cm} R{1cm}}
            \\toprule
            \\multicolumn{1}{c}{} & \\multicolumn{1}{c}{", year1, "} & \\multicolumn{1}{c}{", year2, "} & \\multicolumn{1}{c}{", year3, "} \\\\
            \\midrule\n",
              file = fileOut, append = TRUE)
        }
        if (!M49countries[i,"SHORT_NAME"] %in% REU_reg_names_ru){
          cat("\\CountryData{", M49countries[i, "SHORT_NAME"], "}",
              define_row_color,
              "\\begin{tabular}{L{4.20cm} R{1cm} R{1cm} R{1cm}}
              \\toprule
              \\multicolumn{1}{c}{} & \\multicolumn{1}{c}{", year1, "} & \\multicolumn{1}{c}{", year2, "} & \\multicolumn{1}{c}{", year3, "} \\\\
              \\midrule\n",
              file = fileOut, append = TRUE)
        }
        
      } else {
        
        if (M49countries[i,"SHORT_NAME"] %in% REU_reg_names){
          cat("\\CountryData{", M49countries[i, "SHORT_NAME"], "\\textsuperscript{\\ddag} }", # asterisk for France, Russia & US
              define_row_color,
              "\\begin{tabular}{L{4.20cm} R{1cm} R{1cm} R{1cm}}
            \\toprule
            \\multicolumn{1}{c}{} & \\multicolumn{1}{c}{", year1, "} & \\multicolumn{1}{c}{", year2, "} & \\multicolumn{1}{c}{", year3, "} \\\\
            \\midrule\n",
              file = fileOut, append = TRUE)
        }
        if (!M49countries[i,"SHORT_NAME"] %in% REU_reg_names){
          cat("\\CountryData{", M49countries[i, "SHORT_NAME"], "}",
              define_row_color,
              "\\begin{tabular}{L{4.20cm} R{1cm} R{1cm} R{1cm}}
              \\toprule
              \\multicolumn{1}{c}{} & \\multicolumn{1}{c}{", year1, "} & \\multicolumn{1}{c}{", year2, "} & \\multicolumn{1}{c}{", year3, "} \\\\
              \\midrule\n",
              file = fileOut, append = TRUE)
        }
        
      }
    }
    ## data
    tmp = CountryProfile.df[CountryProfile.df[, "FAOST_CODE"] == M49countries[i, "FAOST_CODE"], ]
    tmp <- tmp[!is.na(tmp$Year),]
    
    
    
    for (part in 1:length(unique(indicators.df[, "PART"]))) {
      #     cat("\t\\multicolumn{4}{l}{\\textcolor{",paste0("part", part),"}{\\textbf{\\large{", unique(indicators.df$PART)[part], "}}}} \\\\ \n",
      #         file = fileOut, append = TRUE, sep = "")

      if (region_to_report %in% c("COF","GLO")){
        if (part %in% c(3,4)) {
          cat("\t\\multicolumn{4}{l}{\\textit{\\normalsize{", unique(indicators.df$PART)[part], "}}} \\\\ \n",
              file = fileOut, append = TRUE, sep = "")
        } else cat("\t\\multicolumn{4}{l}{\\textcolor{",row_color,"}{\\textbf{\\large{", unique(indicators.df$PART)[part], "}}}} \\\\ \n",
                   file = fileOut, append = TRUE, sep = "")
      }
      if (region_to_report == "RAF"){
        if (part %in% c(4)) {
          cat("\t\\multicolumn{4}{l}{\\textit{\\normalsize{", unique(indicators.df$PART)[part], "}}} \\\\ \n",
              file = fileOut, append = TRUE, sep = "")
        }
        if (part %in% c(5)) {
          cat("\t\\multicolumn{4}{l}{\\textit{\\normalsize{", unique(indicators.df$PART)[part], "\\S}}} \\\\ \n",
              file = fileOut, append = TRUE, sep = "")
        }
        if (!part %in% c(4,5)) {
          cat("\t\\multicolumn{4}{l}{\\textcolor{",row_color,"}{\\textbf{\\large{", unique(indicators.df$PART)[part], "}}}} \\\\ \n",
                   file = fileOut, append = TRUE, sep = "")
        }
      }
      if (region_to_report == "RAP"){
        if (part %in% c(4)) {
          cat("\t\\multicolumn{4}{l}{\\textit{\\normalsize{", unique(indicators.df$PART)[part], "\\S}}} \\\\ \n",
              file = fileOut, append = TRUE, sep = "")
        } 
        if (part %in% c(5)) {
          cat("\t\\multicolumn{4}{l}{\\textit{\\normalsize{", unique(indicators.df$PART)[part], "}}} \\\\ \n",
              file = fileOut, append = TRUE, sep = "")
        } 
        
        if (!part %in% c(4,5)) {
          cat("\t\\multicolumn{4}{l}{\\textcolor{",row_color,"}{\\textbf{\\large{", unique(indicators.df$PART)[part], "}}}} \\\\ \n",
                   file = fileOut, append = TRUE, sep = "")
        }
      }
      if (region_to_report == "REU"){
        if (part %in% c(4)) {
          cat("\t\\multicolumn{4}{l}{\\textit{\\normalsize{", unique(indicators.df$PART)[part], "\\S}}} \\\\ \n",
              file = fileOut, append = TRUE, sep = "")
        }
        if (part %in% c(5)) {
          cat("\t\\multicolumn{4}{l}{\\textit{\\normalsize{", unique(indicators.df$PART)[part], "}}} \\\\ \n",
              file = fileOut, append = TRUE, sep = "")
        }
        if (!part %in% c(4,5)) {
          cat("\t\\multicolumn{4}{l}{\\textcolor{",row_color,"}{\\textbf{\\large{", unique(indicators.df$PART)[part], "}}}} \\\\ \n",
                   file = fileOut, append = TRUE, sep = "")
        }
      }
      if (region_to_report == "RNE"){
        if (part %in% c(4)) {
          cat("\t\\multicolumn{4}{l}{\\textit{\\normalsize{", unique(indicators.df$PART)[part], "\\S}}} \\\\ \n",
              file = fileOut, append = TRUE, sep = "")
        } 
        if (part %in% c(5)) {
          cat("\t\\multicolumn{4}{l}{\\textit{\\normalsize{", unique(indicators.df$PART)[part], "}}} \\\\ \n",
              file = fileOut, append = TRUE, sep = "")
        } 
        if (!part %in% c(4,5)) {
          cat("\t\\multicolumn{4}{l}{\\textcolor{",row_color,"}{\\textbf{\\large{", unique(indicators.df$PART)[part], "}}}} \\\\ \n",
                   file = fileOut, append = TRUE, sep = "")
        }
      }


      subindicators.df = indicators.df[indicators.df[, "PART"] == unique(indicators.df$PART)[part], ]
      

      for (j in 1:nrow(subindicators.df)) {
        y1 = tmp[tmp[, "Year"] == year1, subindicators.df[j, "INDICATOR1"]]
        if (length(y1) == 1) {
          if (!is.na(y1)) {
            if (is.numeric(y1)) {
              chunk1 = format(round(y1, digits = subindicators.df[j, "DIGITS"]), nsmall = 0, big.mark = ",")
            } else {
              chunk1 = y1
            }
          } else {
            lya = na.locf(tmp[tmp[, "Year"] %in% c((year1-2):(year1+3)), subindicators.df[j, "INDICATOR1"]], na.rm = FALSE)[6]
            if (!is.na(lya)) {
              if (is.numeric(lya)) {
                chunk1 = paste0("\\textit{", format(round(lya, digits = subindicators.df[j, "DIGITS"]), nsmall = 0, big.mark = ","), "}")
              } else {
                chunk1 = paste0("\\textit{", lya, "}")
              }
            } else {
              chunk1 = "{\\scriptsize\\textit{NA}}"
              if (rulang) chunk1 = "{\\scriptsize\\textit{НП}}"
            }
          }
        } else {
          chunk1 = "{\\scriptsize\\textit{NA}}"
          if (rulang) chunk1 = "{\\scriptsize\\textit{НП}}"
        }
        y2 = tmp[tmp[, "Year"] == year2, subindicators.df[j, "INDICATOR1"]]
        if (length(y2) == 1) {
          if (!is.na(y2)) {
            if (is.numeric(y2)) {
              chunk2 = format(round(y2, digits = subindicators.df[j, "DIGITS"]), nsmall = 0, big.mark = ",")
            } else {
              chunk2 = y2
            }
          } else {
            lya = na.locf(tmp[tmp[, "Year"] %in% c((year2-7):(year2+3)), subindicators.df[j, "INDICATOR1"]], na.rm = FALSE)[11]
            if (!is.na(lya)) {
              if (is.numeric(lya)) {
                chunk2 = paste0("\\textit{", format(round(lya, digits = subindicators.df[j, "DIGITS"]), nsmall = 0, big.mark = ","), "}")
              } else {
                chunk2 = paste0("\\textit{", lya, "}")
              }
            } else {
              chunk2 = "{\\scriptsize\\textit{NA}}"
              if (rulang) chunk2 = "{\\scriptsize\\textit{НП}}"
            }
          }
        } else {
          chunk2 = "{\\scriptsize\\textit{NA}}"
          if (rulang) chunk2 = "{\\scriptsize\\textit{НП}}"
        }
        y3 = tmp[tmp[, "Year"] == year3, subindicators.df[j, "INDICATOR1"]]
        if (length(y3) == 1) {
          if (!is.na(y3)) {
            if (is.numeric(y3)) {
              chunk3 = format(round(y3, digits = subindicators.df[j, "DIGITS"]), nsmall = 0, big.mark = ",")
            } else {
              chunk3 = y3
            }
          } else {
            lya = na.locf(tmp[tmp[, "Year"] %in% c((year3-9):(year3+2)), subindicators.df[j, "INDICATOR1"]], na.rm = FALSE)[12]
            if (!is.na(lya)) {
              if (is.numeric(lya)) {
                chunk3 = paste0("\\textit{", format(round(lya, digits = subindicators.df[j, "DIGITS"]), nsmall = 0, big.mark = ","), "}")
              } else {
                chunk3 = paste0("\\textit{", lya, "}")
              }
            } else {
              chunk3 = "{\\scriptsize\\textit{NA}}"
              if (rulang) chunk3 = "{\\scriptsize\\textit{НП}}"
            }
          }
        } else {
          chunk3 = "{\\scriptsize\\textit{NA}}"
          if (rulang) chunk3 = "{\\scriptsize\\textit{НП}}"
        }
        chunk1 <- gsub(pattern = ",", replacement = "\\\\,", x = chunk1)
        chunk2 <- gsub(pattern = ",", replacement = "\\\\,", x = chunk2)
        chunk3 <- gsub(pattern = ",", replacement = "\\\\,", x = chunk3)
        
        
        # Add asterisk if SOFI indicator
        fsi_meta <- read_csv(paste0(data.dir,"/FSI2015_DisseminationMetadata.csv"))
        fsi_meta <- as.data.frame(fsi_meta)

          if (subindicators.df[j, "INDICATOR1"] %in% fsi_meta[["NAME"]] & M49countries[i,"SHORT_NAME"] %in% c(RAP_reg_names,
                                                                                                              RAF_reg_names,
                                                                                                              REU_reg_names,
                                                                                                              REU_reg_names_ru,
                                                                                                              RNE_reg_names)){
            cat("\t ~ ", sanitizeToLatex(subindicators.df[j, "SERIES_NAME_SHORT"]),dag_char, " & ", chunk1, " ~ \\ \\ & ", chunk2, " ~ \\ \\ & ", chunk3, " ~ \\ \\ \\\\ \n",
                file = fileOut, append = TRUE, sep = "")
            
          } else {
            cat("\t ~ ", sanitizeToLatex(subindicators.df[j, "SERIES_NAME_SHORT"]), " & ", chunk1, " ~ \\ \\ & ", chunk2, " ~ \\ \\ & ", chunk3, " ~ \\ \\ \\\\ \n",
                file = fileOut, append = TRUE, sep = "")
          }

      }
    }
    ## tail
    
    if (region_to_report %in% c("COF","GLO")){
      
      if (M49countries[i,"SHORT_NAME"] %in% RNE_reg_names){
        
        cat("\ \ \ \ \ \ \ \\toprule
        \n\\end{tabular}
        \\textsuperscript{\\ddag} Aggregation based on the country groupings defined in table 'Classification of Countries' on page xi.
        \\clearpage\n",
            file = fileOut, append = TRUE)
      }
      if (!M49countries[i,"SHORT_NAME"] %in% RNE_reg_names){
        
        cat("\ \ \ \ \ \ \ \\toprule
        \\end{tabular}
        \\clearpage\n",
            file = fileOut, append = TRUE)
      }
      
    }
    
    if (region_to_report == "RAP"){

      if (M49countries[i,"SHORT_NAME"] %in% RAP_reg_names){
        cat("\ \ \ \ \ \ \ \\toprule
      \n\\end{tabular}
      \\textsuperscript{\\ddag} Aggregation based on the country groupings defined in table 'Classification of Countries' on page xi.
      \\clearpage\n",
            file = fileOut, append = TRUE)
      }
      if (M49countries[i,"SHORT_NAME"] %in% c("France")){

        cat("\ \ \ \ \ \ \ \\toprule
      \n\\end{tabular}
      \\textsuperscript{\\ddag} France is included in this publication as it has territories in the Region. However the data refer to the entire country, irrespective of geographic location.
      \\clearpage\n",
            file = fileOut, append = TRUE)
      }
      if (M49countries[i,"SHORT_NAME"] %in% c("Russian Federation")){

        cat("\ \ \ \ \ \ \ \\toprule
      \n\\end{tabular}
      \\textsuperscript{\\ddag} Russian Federation is included in this publication as it is geographically included in both Europe and Asia and is also a member of the FAO Regional Conference for Asia and the Pacific.
      \\clearpage\n",
            file = fileOut, append = TRUE)
      }
      if (M49countries[i,"SHORT_NAME"] %in% c("United States")){

        cat("\ \ \ \ \ \ \ \\toprule
      \n\\end{tabular}
      \\textsuperscript{\\ddag} United States is included in this publication as it has territories in the Region. However the data refer to  the entire country, irrespective of geographic location of its territorial areas.
      \\clearpage\n",
            file = fileOut, append = TRUE)

      }
      if (!M49countries[i,"SHORT_NAME"] %in% c("France",
                                              "Russian Federation",
                                              "United States",
                                              RAP_reg_names)){
      cat("\ \ \ \ \ \ \ \\toprule
      \\end{tabular}
      \\clearpage\n",
            file = fileOut, append = TRUE)
      }
    }

    if (region_to_report == "RAF"){

      if (M49countries[i,"SHORT_NAME"] %in% RAF_reg_names){

      cat("\ \ \ \ \ \ \ \\toprule
      \n\\end{tabular}
      \\textsuperscript{\\ddag} Aggregation based on the country groupings defined in table 'Classification of Countries' on page xi.
      \\clearpage\n",
            file = fileOut, append = TRUE)
      }
      if (!M49countries[i,"SHORT_NAME"] %in% RAF_reg_names){

        cat("\ \ \ \ \ \ \ \\toprule
      \\end{tabular}
      \\clearpage\n",
            file = fileOut, append = TRUE)
      }

    }

    if (region_to_report == "REU"){

      if (rulang){
        
        if (M49countries[i,"SHORT_NAME"] %in% REU_reg_names_ru){
          cat("\ \ \ \ \ \ \ \\toprule
      \n\\end{tabular}
      \\textsuperscript{\\ddag} Совокупные показатели сформированы на основе групп стран, определенных в таблице «Классификация стран» на стр. xi.
      \\clearpage\n",
              file = fileOut, append = TRUE)
        }
        if (!M49countries[i,"SHORT_NAME"] %in% REU_reg_names_ru){
          cat("\ \ \ \ \ \ \ \\toprule
      \\end{tabular}
      \\clearpage\n",
              file = fileOut, append = TRUE)
        }
      }  else {
        
        if (M49countries[i,"SHORT_NAME"] %in% REU_reg_names){
          cat("\ \ \ \ \ \ \ \\toprule
      \n\\end{tabular}
      \\textsuperscript{\\ddag} Aggregation based on the country groupings defined in table 'Classification of Countries' on page xi.
      \\clearpage\n",
              file = fileOut, append = TRUE)
        }
        if (!M49countries[i,"SHORT_NAME"] %in% REU_reg_names){
          cat("\ \ \ \ \ \ \ \\toprule
      \\end{tabular}
      \\clearpage\n",
              file = fileOut, append = TRUE)
        }
      }
        
      }
      
    
    if (region_to_report == "RNE"){

      if (M49countries[i,"SHORT_NAME"] %in% RNE_reg_names){

        cat("\ \ \ \ \ \ \ \\toprule
        \n\\end{tabular}
        \\textsuperscript{\\ddag} Aggregation based on the country groupings defined in table 'Classification of Countries' on page xi.
        \\clearpage\n",
        file = fileOut, append = TRUE)
        }
      if (!M49countries[i,"SHORT_NAME"] %in% RNE_reg_names){

        cat("\ \ \ \ \ \ \ \\toprule
        \\end{tabular}
        \\clearpage\n",
        file = fileOut, append = TRUE)
      }

    }

  }
}

if (table_type == "html"){

  ## Create the new .tex file
  fileOut <- paste0(root.dir,"output/process/CountryProfiles.html")
  if(file.exists(fileOut)) file.remove(fileOut)
  file.create(fileOut)
  ## Subset the dataset
  CountryProfile.df <-
    temp[, colnames(temp) %in% c("FAOST_CODE", "SHORT_NAME", "Year",
                                     na.omit(indicators.df[, "INDICATOR1"]),
                                     na.omit(indicators.df[, "INDICATOR2"]),
                                     na.omit(indicators.df[, "INDICATOR3"]))]
  if ("OA.TPBS.POP.PPL.NO" %in% names(CountryProfile.df)) CountryProfile.df[, "OA.TPBS.POP.PPL.NO"] <- CountryProfile.df[, "OA.TPBS.POP.PPL.NO"]/1000000
  if ("OA.TPR.POP.PPL.NO" %in% names(CountryProfile.df)) CountryProfile.df[, "OA.TPR.POP.PPL.NO"] <- CountryProfile.df[, "OA.TPR.POP.PPL.NO"]/1000000




  # Multiplying
  multip.df <- indicators.df[!is.na(indicators.df$MULTIPLIER),]

  for (name in names(CountryProfile.df)) {
    if (name %in% multip.df$INDICATOR1) CountryProfile.df[[name]] <- CountryProfile.df[[name]] / multip.df[multip.df$INDICATOR1 %in% name,]$MULTIPLIER
  }


  # TeX file ----------------------------------------------------------------

  ## Years to be shown in the country profile
  year1 = 1990
  year2 = 2000
  year3 = 2014
  ## This script creates the latex file

  ## Set the rowheight for cprofiles for each book

  for (i in 1:nrow(M49countries)) {

    cat(paste0('<h1>',M49countries[i, "SHORT_NAME"],'</h1> \n'),
        file = fileOut, append = TRUE)

    # header
    cat(paste0('<table class="table table-striped table-hover"> \n'),
        file = fileOut, append = TRUE)


    cat('<tr><th></th><th>',year1,'</th><th>',year2,'</th><th>',year3,'</th></tr>  \n',
        file = fileOut, append = TRUE)

    ## data
    tmp = CountryProfile.df[CountryProfile.df[, "FAOST_CODE"] == M49countries[i, "FAOST_CODE"], ]
    for (part in 1:length(unique(indicators.df[, "PART"]))) {

      cat('<tr><th>', unique(indicators.df$PART)[part], '</th><th> </th><th> </th><th> </th></tr> \n',
              file = fileOut, append = TRUE, sep = "")

      subindicators.df = indicators.df[indicators.df[, "PART"] == unique(indicators.df$PART)[part], ]
      for (j in 1:nrow(subindicators.df)) {
        y1 = tmp[tmp[, "Year"] == year1, subindicators.df[j, "INDICATOR1"]]
        if (length(y1) == 1) {
          if (!is.na(y1)) {
            if (is.numeric(y1)) {
              chunk1 = format(round(y1, digits = subindicators.df[j, "DIGITS"]), nsmall = 0, big.mark = ",")
            } else {
              chunk1 = y1
            }
          } else {
            lya = na.locf(tmp[tmp[, "Year"] %in% c((year1-2):(year1+3)), subindicators.df[j, "INDICATOR1"]], na.rm = FALSE)[6]
            if (!is.na(lya)) {
              if (is.numeric(lya)) {
                chunk1 = paste0("<i>", format(round(lya, digits = subindicators.df[j, "DIGITS"]), nsmall = 0, big.mark = ","), "</i>")
              } else {
                chunk1 = paste0("<i>", lya, "</i>")
              }
            } else {
              chunk1 = ""
            }
          }
        } else {
          chunk1 = ""
        }
        y2 = tmp[tmp[, "Year"] == year2, subindicators.df[j, "INDICATOR1"]]
        if (length(y2) == 1) {
          if (!is.na(y2)) {
            if (is.numeric(y2)) {
              chunk2 = format(round(y2, digits = subindicators.df[j, "DIGITS"]), nsmall = 0, big.mark = ",")
            } else {
              chunk2 = y2
            }
          } else {
            lya = na.locf(tmp[tmp[, "Year"] %in% c((year2-7):(year2+3)), subindicators.df[j, "INDICATOR1"]], na.rm = FALSE)[11]
            if (!is.na(lya)) {
              if (is.numeric(lya)) {
                chunk2 = paste0("<i>", format(round(lya, digits = subindicators.df[j, "DIGITS"]), nsmall = 0, big.mark = ","), "</i>")
              } else {
                chunk2 = paste0("<i>", lya, "</i>")
              }
            } else {
              chunk2 = ""
            }
          }
        } else {
          chunk2 = ""
        }
        y3 = tmp[tmp[, "Year"] == year3, subindicators.df[j, "INDICATOR1"]]
        if (length(y3) == 1) {
          if (!is.na(y3)) {
            if (is.numeric(y3)) {
              chunk3 = format(round(y3, digits = subindicators.df[j, "DIGITS"]), nsmall = 0, big.mark = ",")
            } else {
              chunk3 = y3
            }
          } else {
            lya = na.locf(tmp[tmp[, "Year"] %in% c((year3-9):(year3+2)), subindicators.df[j, "INDICATOR1"]], na.rm = FALSE)[12]
            if (!is.na(lya)) {
              if (is.numeric(lya)) {
                chunk3 = paste0("<i>", format(round(lya, digits = subindicators.df[j, "DIGITS"]), nsmall = 0, big.mark = ","), "</i>")
              } else {
                chunk3 = paste0("<i>", lya, "</i>")
              }
            } else {
              chunk3 = ""
            }
          }
        } else {
          chunk3 = ""
        }
        cat('<tr> <td align="left">', sanitizeToHTML(subindicators.df[j, "SERIES_NAME_SHORT"]), '</td> <td align="left">', chunk1, '</td> <td align="left">', chunk2, '</td> <td align="left">', chunk3, "</td> </tr> \n",
            file = fileOut, append = TRUE, sep = "")

      }

    }
    cat(paste0('</table> \n </br></br> \n'),
        file = fileOut, append = TRUE)
    }
}


# if (table_type == "csv"){
  
  ## Create the new .tex file
  dir.create(paste0(root.dir,"/output/data/"), showWarnings = FALSE, recursive = TRUE)
  fileOut <- paste0(root.dir,"/output/data/CountryProfiles.csv")
  if(file.exists(fileOut)) file.remove(fileOut)
  file.create(fileOut)
  ## Subset the dataset
  CountryProfile.df <-
    temp[, colnames(temp) %in% c("FAOST_CODE", "SHORT_NAME", "Year",
                                     na.omit(indicators.df[, "INDICATOR1"]),
                                     na.omit(indicators.df[, "INDICATOR2"]),
                                     na.omit(indicators.df[, "INDICATOR3"]))]
  if ("OA.TPBS.POP.PPL.NO" %in% names(CountryProfile.df)) CountryProfile.df[, "OA.TPBS.POP.PPL.NO"] <- CountryProfile.df[, "OA.TPBS.POP.PPL.NO"]/1000000
  if ("OA.TPR.POP.PPL.NO" %in% names(CountryProfile.df)) CountryProfile.df[, "OA.TPR.POP.PPL.NO"] <- CountryProfile.df[, "OA.TPR.POP.PPL.NO"]/1000000
  
  
  
  
  # Multiplying
  multip.df <- indicators.df[!is.na(indicators.df$MULTIPLIER),]
  
  for (name in names(CountryProfile.df)) {
    if (name %in% multip.df$INDICATOR1) CountryProfile.df[[name]] <- CountryProfile.df[[name]] / multip.df[multip.df$INDICATOR1 %in% name,]$MULTIPLIER
  }
  
  
  # TeX file ----------------------------------------------------------------
  
  ## Years to be shown in the country profile
  year1 = 1990
  year2 = 2000
  year3 = 2014
  ## This script creates the latex file
  
  ## Set the rowheight for cprofiles for each book
  
  cat(paste0('"FAOST_CODE";','"country";','"part";','"indicator";',year1,';',year2,';',year3,'\n'),
      file = fileOut, append = TRUE)
  
  for (i in 1:nrow(M49countries)) {

    cat(paste0('"',M49countries[i, "FAOST_CODE"],'";"',M49countries[i, "SHORT_NAME"],'";;;;;\n'),
        file = fileOut, append = TRUE)
    ## data
    tmp = CountryProfile.df[CountryProfile.df[, "FAOST_CODE"] == M49countries[i, "FAOST_CODE"], ]
    for (part in 1:length(unique(indicators.df[, "PART"]))) {
      
      cat(';;"', unique(indicators.df$PART)[part], '";;;;\n',
          file = fileOut, append = TRUE, sep = "")
      
      subindicators.df = indicators.df[indicators.df[, "PART"] == unique(indicators.df$PART)[part], ]
      for (j in 1:nrow(subindicators.df)) {
        y1 = tmp[tmp[, "Year"] == year1, subindicators.df[j, "INDICATOR1"]]
        if (length(y1) == 1) {
          if (!is.na(y1)) {
            if (is.numeric(y1)) {
              chunk1 = format(round(y1, digits = subindicators.df[j, "DIGITS"]), nsmall = 0, big.mark = ",")
            } else {
              chunk1 = y1
            }
          } else {
            lya = na.locf(tmp[tmp[, "Year"] %in% c((year1-2):(year1+3)), subindicators.df[j, "INDICATOR1"]], na.rm = FALSE)[6]
            if (!is.na(lya)) {
              if (is.numeric(lya)) {
                chunk1 = paste0("<i>", format(round(lya, digits = subindicators.df[j, "DIGITS"]), nsmall = 0, big.mark = ","), "</i>")
              } else {
                chunk1 = paste0("<i>", lya, "</i>")
              }
            } else {
              chunk1 = ""
            }
          }
        } else {
          chunk1 = ""
        }
        y2 = tmp[tmp[, "Year"] == year2, subindicators.df[j, "INDICATOR1"]]
        if (length(y2) == 1) {
          if (!is.na(y2)) {
            if (is.numeric(y2)) {
              chunk2 = format(round(y2, digits = subindicators.df[j, "DIGITS"]), nsmall = 0, big.mark = ",")
            } else {
              chunk2 = y2
            }
          } else {
            lya = na.locf(tmp[tmp[, "Year"] %in% c((year2-7):(year2+3)), subindicators.df[j, "INDICATOR1"]], na.rm = FALSE)[11]
            if (!is.na(lya)) {
              if (is.numeric(lya)) {
                chunk2 = paste0("<i>", format(round(lya, digits = subindicators.df[j, "DIGITS"]), nsmall = 0, big.mark = ","), "</i>")
              } else {
                chunk2 = paste0("<i>", lya, "</i>")
              }
            } else {
              chunk2 = ""
            }
          }
        } else {
          chunk2 = ""
        }
        y3 = tmp[tmp[, "Year"] == year3, subindicators.df[j, "INDICATOR1"]]
        if (length(y3) == 1) {
          if (!is.na(y3)) {
            if (is.numeric(y3)) {
              chunk3 = format(round(y3, digits = subindicators.df[j, "DIGITS"]), nsmall = 0, big.mark = ",")
            } else {
              chunk3 = y3
            }
          } else {
            lya = na.locf(tmp[tmp[, "Year"] %in% c((year3-9):(year3+2)), subindicators.df[j, "INDICATOR1"]], na.rm = FALSE)[12]
            if (!is.na(lya)) {
              if (is.numeric(lya)) {
                chunk3 = paste0("<i>", format(round(lya, digits = subindicators.df[j, "DIGITS"]), nsmall = 0, big.mark = ","), "</i>")
              } else {
                chunk3 = paste0("<i>", lya, "</i>")
              }
            } else {
              chunk3 = ""
            }
          }
        } else {
          chunk3 = ""
        }
        cat(';;;"', sanitizeToHTML(subindicators.df[j, "SERIES_NAME_SHORT"]), '";"', chunk1, '";"', chunk2, '";"', chunk3, '"\n',
            file = fileOut, append = TRUE, sep = "")
        
      }
      
    }
    # cat(paste0('</table> \n </br></br> \n'),
    #     file = fileOut, append = TRUE)
  }
# }

csv_table <- read.csv(fileOut, sep=";", stringsAsFactors = FALSE)
csv_table[csv_table==""] <- NA 
library(zoo)
csv_table[1:3] <- na.locf(csv_table[1:3])
csv_table <- csv_table[!is.na(csv_table$indicator),]

# Add metadata about the indicators - basicly joining what is directly available
indicators.df <- read.csv(paste0(root.dir,"input/data/country_profile_indicators_",region_to_report,".csv"), na.strings = "", stringsAsFactors = FALSE)

const <- read_csv("/home/aurelius/faosync/pocketbooks/pocketbook_database/input_data/Construction2015.csv")
meta <- read_csv("/home/aurelius/faosync/pocketbooks/pocketbook_database/input_data/Metadata2015.csv")


csv_table <- left_join(csv_table,
          indicators.df[c("SERIES_NAME_SHORT","INDICATOR1")],
          by=c("indicator" = "SERIES_NAME_SHORT"))

csv_table <- left_join(csv_table,
                    meta[c("STS_ID","SOURCE", "DATA_TYPE", "SQL_DOMAIN_CODE", "SQL_ELEMENT_CODE", "SQL_ITEM_CODE", "WDINAME")],
                    by=c("INDICATOR1" = "STS_ID"))

csv_table <- left_join(csv_table,
                        const[c("STS_ID", "STS_ID_CONSTR1", "STS_ID_CONSTR2", "STS_ID_WEIGHT", "CONSTRUCTION_TYPE", "GROWTH_RATE_FREQ", "GROWTH_TYPE")],
                        by=c("INDICATOR1" = "STS_ID"))

write.csv(csv_table, file=paste0(root.dir,"output/data/countryprofile",region_to_report,".csv"),row.names = FALSE)

# -- in case we need footnotes under each of the country profile table
# system("~/faosync/pocketbooks/pocketbook/input/code/countryprofile_footnote_RAP.sh")

# system("sed -i 's#{ 2014 }#{ 2014* }#' ./output/process/CountryProfiles.tex && sed -i 's#\\end{tabular}#\*We can add a footnote for each table like this..\n\\end{tabular}#' ./output/process/CountryProfiles.tex")
# sed -i 's#Net food#Net food**#' ./publication/Tables/CountryProfiles.tex && sed -i 's#\\end{tabular}#\n\**excluding fish\n\\end{tabular}#' ./publication/Tables/CountryProfiles.tex
temp <- merge(temp,FAOcountryProfile[c("FAOST_CODE","SHORT_NAME","FAO_TABLE_NAME")],by="FAOST_CODE",all=TRUE)
save(temp, file="~/faosync/pocketbooks/pocketbook_tests/data/regiona_sybdata.RData")


# temp <- temp
