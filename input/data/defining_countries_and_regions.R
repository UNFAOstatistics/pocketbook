################################################################################################
# This script defines the macro regions and subregions used in regional statistical pockebooks 2015
# Markus Kainu 20150717
# 
# The aim is to create a region_key that has FAOST_CODE and SHORT_NAME AND dummy vars for each region and subregion - belongs T/F

# First lets download the raw FAOcountryprofile and begin with the m49 classificaion defined there "UNSD_MACRO_REG","UNSD_SUB_REG" variables

library(dplyr)
library(stringr)

fao_prof <- read.csv("./input/data/FAOcountryProfile.csv", stringsAsFactors = FALSE)

unsd <- fao_prof[,c("FAOST_CODE","FAO_TABLE_NAME",
                    "UNSD_MACRO_REG","UNSD_SUB_REG", # M49 For RAP
                    "FAO_REU_REG","FAO_REU_SUB_REG", # old REU for REU
                    "FAO_RNE_REG","FAO_RNE_SUB_REG"#, # old RNE for RNE
                    #"FAO_RNE_REG","FAO_RNE_SUB_REG", # old RAF for RAF ???
                    )]

# create dummies for each macroregion and subregion
for (var in names(unsd)[-1:-2]) {
  for(t in unique(unsd[[var]])) {
    unsd[paste("M49",t,sep="_")] <- ifelse(unsd[[var]]==t,TRUE,FALSE)
    }
}
names(unsd) <- str_replace_all(names(unsd), " ", ".")


country_region <- unsd
write.csv(country_region,paste0(root.dir,"/input/data/country_region.csv"), row.names = FALSE)

## Create summaries for Amy

country_data <- data.frame(colx = rep("Middle-earth", 284))
for (col in 9:48){
  colname <- names(country_region)[col]
  vector <- ifelse(country_region[[colname]],country_region[[2]],NA)
  vector <- vector[!is.na(vector)]
  if (length(vector) < 284) vector <- c(vector, rep("empty", (284-length(vector))))
  vector <- as.data.frame(vector)
  country_data <- bind_cols(country_data,vector)
  names(country_data)[col-7] <- colname
}

faost_code_data <- data.frame(colx = rep(666, 284))
for (col in 9:48){
  colname <- names(country_region)[col]
  vector <- ifelse(country_region[[colname]],country_region[[1]],NA)
  vector <- vector[!is.na(vector)]
  if (length(vector) < 284) vector <- c(vector, rep("empty", (284-length(vector))))
  vector <- as.data.frame(vector)
  faost_code_data <- bind_cols(faost_code_data,vector)
  names(faost_code_data)[col-7] <- colname
}

#   ____          __  _                                  _                    
#  |  _ \   ___  / _|(_) _ __    ___   _ __  ___   __ _ (_)  ___   _ __   ___ 
#  | | | | / _ \| |_ | || '_ \  / _ \ | '__|/ _ \ / _` || | / _ \ | '_ \ / __|
#  | |_| ||  __/|  _|| || | | ||  __/ | |  |  __/| (_| || || (_) || | | |\__ \
#  |____/  \___||_|  |_||_| |_| \___| |_|   \___| \__, ||_| \___/ |_| |_||___/
#                                                 |___/                       



# 
# ____      _    _____ 
# |  _ \    / \  |  ___|
# | |_) |  / _ \ | |_   
# |  _ <  / ___ \|  _|  
# |_| \_\/_/   \_\_|    
#   (I seemed to have lost the email from Eloi. I am waiting for him to re-send the information)

# cat(paste(shQuote(country_data$RAF, type="cmd"), collapse=", "))
# FAO_TABLE_NAME

RAF <-  c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cameroon", "Cabo Verde", "Central African Republic", "Chad", "the Comoros", "Congo", "Côte d'Ivoire", "the Democratic Republic of the Congo", "Djibouti", "Egypt", "Equatorial Guinea", "Eritrea", "Ethiopia", "Ethiopia PDR", "Gabon", "the Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Kenya", "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius", "Mayotte", "Morocco", "Mozambique", "Namibia", "the Niger", "Nigeria", "Réunion", "Rwanda", "Saint Helena, Ascension and Tristan da Cunha", "Sao Tome and Principe", "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa", "South Sudan", "the Sudan", "the Sudan", "Swaziland", "Togo", "Tunisia", "Uganda", "the United Republic of Tanzania", "Western Sahara", "Zambia", "Zimbabwe")




# ____      _    ____  
# |  _ \    / \  |  _ \ 
# | |_) |  / _ \ | |_) |
# |  _ <  / ___ \|  __/ 
# |_| \_\/_/   \_\_|    
# RAP
# - See T:\Team_working_folder\F\FAOSYBs\PB15\RAP15\countrylist.xls for the list
# - For country profiles, please include all APRC countries plus Brunei, Singapore and Tokelau
# - The aggregate pages in the country profiles should include: World, Asia as defined in M49, Central Asia, Eastern Asia, Southern Asia, SE Asia, Oceania and FAORAP countries


# cat(paste(shQuote(country_data$M49_REUSouthEasternEurope, type="cmd"), collapse=", "))
# cat(paste(faost_code_data$M49_REUSouthEasternEurope, collapse=","))  

RAP_Central_Asia <- c()


RAP_Eastern_Asia  <- c()



RAP_Southern_Asia  <- c()



RAP_South_Eastern_Asia  <- c()



RAP_Western_Asia  <- c()



RAP_Austrial_and_New_Zealand  <- c()



RAP_Melanesia  <- c()


RAP_Micronesia  <- c()


RAP_Polynesia  <- c()


RAP_Russian_Federation  <- c()


RAP_France  <- c()


RAP_United_States  <- c()


RAP <- c()


# ____  _____ _   _ 
# |  _ \| ____| | | |
# | |_) |  _| | | | |
# |  _ <| |___| |_| |
# |_| \_\_____|\___/ 
# REU
# - Please use the groupings from the REU Yearbook
# - Please add EU region as well

# cat(paste(shQuote(country_data$M49_REUSouthEasternEurope, type="cmd"), collapse=", "))
# cat(paste(faost_code_data$M49_REUSouthEasternEurope, collapse=","))

REU_South_Eastern_Europe <- c(3, # "Albania"
                              80, # Bosnia and Herzegovina
                              98, # "Croatia"
                              273, # "Montenegro"
                              272, # "Serbia"
                              186, # "Serbia and Montenegro"
                              154, # "The former Yugoslav Republic of Macedonia"
                              248) # "Yugoslav SFR"



# cat(paste(country_data$M49_REUOtherAndEFTA, collapse=","))
# cat(paste(faost_code_data$M49_REUOtherAndEFTA, collapse=","))

REU_Other_and_EFTA <- c(6, #Andorra
                        11, #Austria
                        255, #Belgium
                        15, #Belgium-Luxembourg
                        50, #Cyprus
                        54, #Denmark
                        67, #Finland
                        68, # France
                        79, # Germany
                        84, # Greece
                        99, # Iceland
                        104, # Ireland
                        106, # Italy
                        256, #Luxembourg
                        134, # Malta
                        140, # Monaco
                        150, # the Netherlands
                        162, # Norway
                        174, # Portugal
                        192, # San Marino
                        203, # Spain
                        210, # Sweden
                        211, # Switzerland
                        229) # the United Kingdom of Great Britain and Northern Ireland

# cat(paste(country_data$M49_REUCaucAndTurkey, collapse=","))
# cat(paste(faost_code_data$M49_REUCaucAndTurkey, collapse=","))

REU_Caucasus_and_Turkey <- c(1, # Armenia
                          52, # Azerbaijan
                          73, # Georgia
                          223) # Turkey

# cat(paste(country_data$M49_REUCISeurope, collapse=","))
# cat(paste(faost_code_data$M49_REUCISeurope, collapse=","))

REU_CIS_Europe <- c(57, # Belarus
                      146, # Republic of Moldova
                      185, # the Russian Federation
                      230) # Ukraine

# cat(paste(country_data$M49_REUCentralEasternEurope, collapse=","))
# cat(paste(faost_code_data$M49_REUCentralEasternEurope, collapse=","))

REU_Central_Eastern_Europe <- c(27,  # Bulgaria
                                 167, # the Czech Republic
                                 51,  # Czechoslovakia
                                 97,  # Hungary
                                 119, # Latvia
                                 126, # Lithuania
                                 173, # Poland
                                 183, # Romania
                                 199, # Slovakia
                                 198) # Slovenia

# cat(paste(country_data$M49_REUIsrael, collapse=","))
# cat(paste(faost_code_data$M49_REUIsrael, collapse=","))


REU_Israel <- c(105) # Israel

# cat(paste(country_data$M49_REUCentralAsia, collapse=","))
# cat(paste(faost_code_data$M49_REUCentralAsia, collapse=","))

REU_Central_Asia <- c(108, # Kazakhstan
                      113, # Kyrgyzstan
                      208, # Tajikistan
                      213, # Turkmenistan
                      235) # Uzbekistan


REU <- c(REU_South_Eastern_Europe,
         REU_Other_And_EFTA,
         REU_Caucasus_and_Turkey,
         REU_CIS_Europe,
         REU_Central_Eastern_Europe,
         REU_Israel,
         REU_Central_Asia)  



# ____  _   _ _____ 
# |  _ \| \ | | ____|
# | |_) |  \| |  _|  
# |  _ <| |\  | |___ 
# |_| \_\_| \_|_____|
# RNE
# -    Please use the groupings from the RNE Yearbook
  

# cat(paste(country_data$M49_RNEregion, collapse=","))
# cat(paste(faost_code_data$M49_RNEregion, collapse=","))


# cat(paste(country_data$M49_RNEgccsy, collapse=","))
# cat(paste(faost_code_data$M49_RNEgccsy, collapse=","))

RNE_gccsy <- c(13, # Bahrain
               118, # Kuwait
               221, # Oman
               179, # Qatar
               194, # Saudi Arabia
               225, # the United Arab Emirates
               249, # Yemen
               247, # Democratic Yemen
               246) # Yemen (old)


# cat(paste(country_data$M49_RNEome, collapse=","))
# cat(paste(faost_code_data$M49_RNEome, collapse=","))

RNE_gccsy <- c(59, # Egypt
             102, # Iran (Islamic Republic of)
             103, # Iraq
             112, # Jordan
             121, # Lebanon
             206, # the Sudan
             276, # the Sudan
             212) # the Syrian Arab Republic



RNE <- c(RNE_gccsy,
         RNE_gccsy)


## Define the macro regions for the book

# RAF
country_region$RAF <- ifelse(country_region$M49_Africa, TRUE, FALSE) 
# -> Africa is same as M49 Africa

# REU
country_region$REU <- ifelse(country_region$M49_Europe | country_region$M49_Central.Asia, TRUE, FALSE) 
# countries belonging to M49_Europe or M49_Central_Asia belong to REU 

# RAP
country_region$RAP <- ifelse(country_region$M49_Asia | country_region$M49_Oceania, TRUE, FALSE) 
# Cojuntries from M49_Asia OR M49_Oceania belong to RAP

# RAP
country_region$RNE <- ifelse(country_region$M49_Northern.Africa | country_region$M49_Western.Asia, TRUE, FALSE) 
# Countries from M49_Northern.Africa OR M49_Western.Asia Belong to this

# RAP 
country_region$GLO <- TRUE
# All countries

country_region <- country_region[c(1,2,3,4,33,34,35,36,37,5:32)]

