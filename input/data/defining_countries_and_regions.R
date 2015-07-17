################################################################################################
# This script defines the macro regions and subregions used in regional statistical pockebooks 2015
# Markus Kainu 20150717
# 
# The aim is to create a region_key that has FAOST_CODE and SHORT_NAME AND dummy vars for each region and subregion - belongs T/F

# First lets download the raw FAOcountryprofile and begin with the m49 classificaion defined there "UNSD_MACRO_REG","UNSD_SUB_REG" variables

# library(dplyr)
# library(stringr)
# 
# fao_prof <- FAOcountryProfile
# 
# unsd <- fao_prof[,c("FAOST_CODE","FAO_TABLE_NAME",
#                     "UNSD_MACRO_REG","UNSD_SUB_REG", # M49 For RAP
#                     "FAO_REU_REG","FAO_REU_SUB_REG", # old REU for REU
#                     "FAO_RNE_REG","FAO_RNE_SUB_REG"#, # old RNE for RNE
#                     #"FAO_RNE_REG","FAO_RNE_SUB_REG", # old RAF for RAF ???
#                     )]
# 
# # create dummies for each macroregion and subregion
# for (var in names(unsd)[-1:-2]) {
#   for(t in unique(unsd[[var]])) {
#     unsd[paste("M49",t,sep="_")] <- ifelse(unsd[[var]]==t,TRUE,FALSE)
#     }
# }
# names(unsd) <- str_replace_all(names(unsd), " ", ".")
# 
# 
# country_region <- unsd
# write.csv(country_region,paste0(root.dir,"/input/data/country_region.csv"), row.names = FALSE)
# 
# ## Create data frames with subgrou/macrogroup name as variable name and names of the countirs as values!!!
# 
# country_data <- data.frame(colx = rep("Middle-earth", 284))
# for (col in 9:48){
#   colname <- names(country_region)[col]
#   vector <- ifelse(country_region[[colname]],country_region[[2]],NA)
#   vector <- vector[!is.na(vector)]
#   if (length(vector) < 284) vector <- c(vector, rep("empty", (284-length(vector))))
#   vector <- as.data.frame(vector)
#   country_data <- bind_cols(country_data,vector)
#   names(country_data)[col-7] <- colname
# }
# 
# faost_code_data <- data.frame(colx = rep(666, 284))
# for (col in 9:48){
#   colname <- names(country_region)[col]
#   vector <- ifelse(country_region[[colname]],country_region[[1]],NA)
#   vector <- vector[!is.na(vector)]
#   if (length(vector) < 284) vector <- c(vector, rep("empty", (284-length(vector))))
#   vector <- as.data.frame(vector)
#   faost_code_data <- bind_cols(faost_code_data,vector)
#   names(faost_code_data)[col-7] <- colname
# }

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

#RAF <-  c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cameroon", "Cabo Verde", "Central African Republic", "Chad", "the Comoros", "Congo", "Côte d'Ivoire", "the Democratic Republic of the Congo", "Djibouti", "Egypt", "Equatorial Guinea", "Eritrea", "Ethiopia", "Ethiopia PDR", "Gabon", "the Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Kenya", "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius", "Mayotte", "Morocco", "Mozambique", "Namibia", "the Niger", "Nigeria", "Réunion", "Rwanda", "Saint Helena, Ascension and Tristan da Cunha", "Sao Tome and Principe", "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa", "South Sudan", "the Sudan", "the Sudan", "Swaziland", "Togo", "Tunisia", "Uganda", "the United Republic of Tanzania", "Western Sahara", "Zambia", "Zimbabwe")

# cat(paste(country_data$M49_Middle.Africa, collapse=","))
# cat(paste(faost_code_data$M49_Middle.Africa, collapse=","))  

RAF_Central_Africa <- c(7, # Angola
                       32, # Cameroon
                       37, # Central African Republic
                       39, # Chad
                       46, # Congo
                       250, # the Democratic Republic of the Congo
                       61, # Equatorial Guinea
                       74, # Gabon
                       193) # Sao Tome and Principe


# cat(paste(country_data$M49_Northern.Africa, collapse=","))
# cat(paste(faost_code_data$M49_Northern.Africa, collapse=","))  

RAF_North_Africa <- c(4, # Algeria
                      59, # Egypt
                      124, # Libya
                      143, # Morocco
                      277, # South Sudan
                      206, # the Sudan
                      276, # the Sudan
                      222, # Tunisia
                      205) # Western Sahara

# cat(paste(country_data$M49_Western.Africa, collapse=","))
# cat(paste(faost_code_data$M49_Western.Africa, collapse=","))  

RAF_West_Africa <- c(53, # Benin
                     233, # Burkina Faso
                     35, # Cabo Verde
                     107, # Côte d'Ivoire
                     75, # the Gambia
                     81, # Ghana
                     90, # Guinea
                     175, # Guinea-Bissau
                     123, # Liberia
                     133, # Mali
                     136, # Mauritania
                     158, # the Niger
                     159, # Nigeria
                     187, # Saint Helena, Ascension and Tristan da Cunha
                     195, # Senegal
                     197, # Sierra Leone
                     217) # Togo


# cat(paste(country_data$M49_Southern.Africa, collapse=","))
# cat(paste(faost_code_data$M49_Southern.Africa, collapse=","))  

RAF_South_Africa <- c(20, # Botswana
                      122, # Lesotho
                      147, # Namibia
                      202, # South Africa
                      209) # Swaziland

# cat(paste(country_data$M49_Eastern.Africa, collapse=","))
# cat(paste(faost_code_data$M49_Eastern.Africa, collapse=","))  

RAF_East_Africa <- c(29, # Burundi
                     45, # the Comoros
                     72, # Djibouti
                     178, # Eritrea
                     238, # Ethiopia
                     62, # Ethiopia PDR
                     114, # Kenya
                     129, # Madagascar
                     130, # Malawi
                     137, # Mauritius
                     270, # Mayotte
                     144, # Mozambique
                     182, # Réunion
                     184, # Rwanda
                     196, # Seychelles
                     201, # Somalia
                     226, # Uganda
                     215, # the United Republic of Tanzania
                     251, # Zambia
                     181) # Zimbabwe

RAF <- c(RAF_Central_Africa,
         RAF_East_Africa,
         RAF_North_Africa,
         RAF_South_Africa,
         RAF_West_Africa)

#  ____      _    ____  
#  |  _ \    / \  |  _ \ 
#  | |_) |  / _ \ | |_) |
#  |  _ <  / ___ \|  __/ 
#  |_| \_\/_/   \_\_|    
#  

# RAP
# - See T:\Team_working_folder\F\FAOSYBs\PB15\RAP15\countrylist.xls for the list
# - For country profiles, please include all APRC countries plus Brunei, Singapore and Tokelau
# - The aggregate pages in the country profiles should include: World, Asia as defined in M49, Central Asia, Eastern Asia, Southern Asia, SE Asia, Oceania and FAORAP countries


# cat(paste(shQuote(country_data$M49_Central.Asia, type="cmd"), collapse=", "))
# cat(paste(faost_code_data$M49_Central.Asia, collapse=","))  

RAP_Central_Asia <- c(108, # Kazakhstan
                      113, # Kyrgyzstan
                      208, # Tajikistan
                      213, # Turkmenistan
                      235) # Uzbekistan

# cat(paste(shQuote(country_data$M49_Eastern.Asia, type="cmd"), collapse=", "))
# cat(paste(faost_code_data$M49_Eastern.Asia, collapse=","))  

RAP_Eastern_Asia  <- c(351, # China
                       116, # Democratic People's Republic of Korea
                       110, # Japan
                       141, # Mongolia
                       117) # Republic of Korea

# cat(paste(country_data$M49_Southern.Asia, collapse=","))
# cat(paste(faost_code_data$M49_Southern.Asia, collapse=","))  

RAP_Southern_Asia  <- c(2, # Afghanistan
                        16, # Bangladesh
                        18, # Bhutan
                        100, # India
                        102, # Iran (Islamic Republic of)
                        132, # Maldives
                        149, # Nepal
                        165, # Pakistan
                        38) # Sri Lanka

# cat(paste(country_data$'M49_South-Eastern.Asia', collapse=","))
# cat(paste(faost_code_data$'M49_South-Eastern.Asia', collapse=","))  

RAP_South_Eastern_Asia  <- c(26, # Brunei Darussalam
                             115, # Cambodia
                             101, # Indonesia
                             120, # the Lao People's Democratic Republic
                             131, # Malaysia
                             28, # Myanmar
                             171, # the Philippines
                             200, # Singapore
                             216, # Thailand
                             176, # Timor-Leste
                             237) # Viet Nam

# cat(paste(country_data$M49_Western.Asia, collapse=","))
# cat(paste(faost_code_data$M49_Western.Asia, collapse=","))  

RAP_Western_Asia  <- c(1, # Armenia
                       52, # Azerbaijan
                       13, # Bahrain
                       50, # Cyprus
                       73, # Georgia
                       103, # Iraq
                       105, # Israel
                       112, # Jordan
                       118, # Kuwait
                       121, # Lebanon
                       299, # Occupied Palestinian Territory
                       221, # Oman
                       179, # Qatar
                       194, # Saudi Arabia
                       212, # the Syrian Arab Republic
                       223, # Turkey
                       225, # the United Arab Emirates
                       249, # Yemen
                       247, # Democratic Yemen
                       246) # Yemen (old)


# cat(paste(country_data$M49_Australia.and.New.Zealand, collapse=","))
# cat(paste(faost_code_data$M49_Australia.and.New.Zealand, collapse=","))  

RAP_Austriala_and_New_Zealand  <- c(10,  # Australia
                                    156, # New Zealand
                                    161) # Norfolk Island


# cat(paste(country_data$M49_Melanesia, collapse=","))
# cat(paste(faost_code_data$M49_Melanesia, collapse=","))  


RAP_Melanesia  <- c(66, # Fiji
                    153, # New Caledonia
                    168, # Papua New Guinea
                    25, # Solomon Islands
                    155) # Vanuatu

# cat(paste(country_data$M49_Micronesia, collapse=","))
# cat(paste(faost_code_data$M49_Micronesia, collapse=","))  


RAP_Micronesia  <- c(88, # Guam
                     83, # Kiribati
                     127, # the Marshall Islands
                     145, # Micronesia (Federated States of)
                     148, # Nauru
                     163, # Northern Mariana Islands
                     164, # Pacific Islands
                     180) # Trust Territory of,Palau


# cat(paste(country_data$M49_Polynesia, collapse=","))
# cat(paste(faost_code_data$M49_Polynesia, collapse=","))  

RAP_Polynesia  <- c(5, # American Samoa
                    47, # the Cook Islands
                    70, # French Polynesia
                    160, # Niue
                    172, # Pitcairn Islands
                    244, # Samoa
                    218, # Tokelau
                    219, # Tonga
                    227, # Tuvalu
                    243) # Wallis and Futuna Islands


RAP_Russian_Federation  <- c(185)


RAP_France  <- c(68)

# cat(paste(country_data$M49_Melanesia, collapse=","))
# cat(paste(faost_code_data$M49_Melanesia, collapse=","))  

RAP_United_States  <- c(231)

# cat(paste(country_data$M49_Melanesia, collapse=","))
# cat(paste(faost_code_data$M49_Melanesia, collapse=","))  

RAP <- c(RAP_Central_Asia,
         RAP_Eastern_Asia,
         RAP_Southern_Asia,
         RAP_South_Eastern_Asia,
         RAP_Western_Asia,
         RAP_Austriala_and_New_Zealand,
         RAP_Melanesia,
         RAP_Micronesia,
         RAP_Polynesia,
         RAP_Russian_Federation,
         RAP_France,
         RAP_United_States)


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
         REU_Other_and_EFTA,
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
  

# cat(paste(country_data$M49_RNEgccsy, collapse=","))
# cat(paste(faost_code_data$M49_RNEgccsy, collapse=","))

RNE_Gulf_Cooperation_Council_States_and_Yemen <- c(13, # Bahrain
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

RNE_Other_Near_East_countries <- c(59, # Egypt
                                   102, # Iran (Islamic Republic of)
                                   103, # Iraq
                                   112, # Jordan
                                   121, # Lebanon
                                   206, # the Sudan
                                   276, # the Sudan
                                   212) # the Syrian Arab Republic

# cat(paste(country_data$M49_RNEna, collapse=","))
# cat(paste(faost_code_data$M49_RNEna, collapse=","))

RNE_North_Africa <- c(4,   # Algeria
                      124, # Libya
                      136, # Mauritania
                      143, # Morocco
                      222) # Tunisia

RNE <- c(RNE_Gulf_Cooperation_Council_States_and_Yemen,
         RNE_Other_Near_East_countries,
         RNE_North_Africa)


##########################################################################################################
# Create dummy vars 

region_key <- FAOcountryProfile[c("FAOST_CODE","FAO_TABLE_NAME","SHORT_NAME")]
region_key$RAF                <- ifelse(region_key$FAOST_CODE %in% RAF, TRUE, FALSE)
region_key$RAF_Central_Africa <- ifelse(region_key$FAOST_CODE %in% RAF_Central_Africa, TRUE, FALSE) 
region_key$RAF_East_Africa    <- ifelse(region_key$FAOST_CODE %in% RAF_East_Africa, TRUE, FALSE) 
region_key$RAF_North_Africa   <- ifelse(region_key$FAOST_CODE %in% RAF_North_Africa, TRUE, FALSE) 
region_key$RAF_South_Africa   <- ifelse(region_key$FAOST_CODE %in% RAF_South_Africa, TRUE, FALSE) 
region_key$RAF_West_Africa    <- ifelse(region_key$FAOST_CODE %in% RAF_West_Africa, TRUE, FALSE) 

region_key$RAP                              <- ifelse(region_key$FAOST_CODE %in% RAP, TRUE, FALSE)
region_key$RAP_Central_Asia                 <- ifelse(region_key$FAOST_CODE %in% RAP_Central_Asia, TRUE, FALSE)
region_key$RAP_Eastern_Asia                 <- ifelse(region_key$FAOST_CODE %in% RAP_Eastern_Asia, TRUE, FALSE)
region_key$RAP_Southern_Asia                <- ifelse(region_key$FAOST_CODE %in% RAP_Southern_Asia, TRUE, FALSE)
region_key$RAP_South_Eastern_Asia           <- ifelse(region_key$FAOST_CODE %in% RAP_South_Eastern_Asia, TRUE, FALSE)
region_key$RAP_Western_Asia                 <- ifelse(region_key$FAOST_CODE %in% RAP_Western_Asia, TRUE, FALSE)
region_key$RAP_Austriala_and_New_Zealand    <- ifelse(region_key$FAOST_CODE %in% RAP_Austriala_and_New_Zealand, TRUE, FALSE)
region_key$RAP_Melanesia                    <- ifelse(region_key$FAOST_CODE %in% RAP_Melanesia, TRUE, FALSE)
region_key$RAP_Micronesia                   <- ifelse(region_key$FAOST_CODE %in% RAP_Micronesia, TRUE, FALSE)
region_key$RAP_Polynesia                    <- ifelse(region_key$FAOST_CODE %in% RAP_Polynesia, TRUE, FALSE)
region_key$RAP_Russian_Federation           <- ifelse(region_key$FAOST_CODE %in% RAP_Russian_Federation, TRUE, FALSE)
region_key$RAP_France                       <- ifelse(region_key$FAOST_CODE %in% RAP_France, TRUE, FALSE)
region_key$RAP_United_States                <- ifelse(region_key$FAOST_CODE %in% RAP_United_States, TRUE, FALSE)

region_key$REU                          <- ifelse(region_key$FAOST_CODE %in% REU, TRUE, FALSE)
region_key$REU_South_Eastern_Europe     <- ifelse(region_key$FAOST_CODE %in% REU_South_Eastern_Europe, TRUE, FALSE)
region_key$REU_Other_And_EFTA           <- ifelse(region_key$FAOST_CODE %in% REU_Other_and_EFTA, TRUE, FALSE)
region_key$REU_Caucasus_and_Turkey      <- ifelse(region_key$FAOST_CODE %in% REU_Caucasus_and_Turkey, TRUE, FALSE)
region_key$REU_CIS_Europe               <- ifelse(region_key$FAOST_CODE %in% REU_CIS_Europe, TRUE, FALSE)
region_key$REU_Central_Eastern_Europe   <- ifelse(region_key$FAOST_CODE %in% REU_Central_Eastern_Europe, TRUE, FALSE)
region_key$REU_Israel                   <- ifelse(region_key$FAOST_CODE %in% REU_Israel, TRUE, FALSE)
region_key$REU_Central_Asia             <- ifelse(region_key$FAOST_CODE %in% REU_Central_Asia, TRUE, FALSE)

region_key$RNE                                            <- ifelse(region_key$FAOST_CODE %in% RNE, TRUE, FALSE)
region_key$RNE_Gulf_Cooperation_Council_States_and_Yemen  <- ifelse(region_key$FAOST_CODE %in% RNE_Gulf_Cooperation_Council_States_and_Yemen, TRUE, FALSE)
region_key$RNE_Other_Near_East_countries                  <- ifelse(region_key$FAOST_CODE %in% RNE_Other_Near_East_countries, TRUE, FALSE)
region_key$RNE_North_Africa                               <- ifelse(region_key$FAOST_CODE %in% RNE_North_Africa, TRUE, FALSE)

region_key$GLO <- TRUE
save(region_key, file=paste0(root.dir,"input/data/region_key.RData"))



## Define the macro regions for the book

# # RAF
# country_region$RAF <- ifelse(country_region$M49_Africa, TRUE, FALSE) 
# # -> Africa is same as M49 Africa
# 
# # REU
# country_region$REU <- ifelse(country_region$M49_Europe | country_region$M49_Central.Asia, TRUE, FALSE) 
# # countries belonging to M49_Europe or M49_Central_Asia belong to REU 
# 
# # RAP
# country_region$RAP <- ifelse(country_region$M49_Asia | country_region$M49_Oceania, TRUE, FALSE) 
# # Cojuntries from M49_Asia OR M49_Oceania belong to RAP
# 
# # RAP
# country_region$RNE <- ifelse(country_region$M49_Northern.Africa | country_region$M49_Western.Asia, TRUE, FALSE) 
# # Countries from M49_Northern.Africa OR M49_Western.Asia Belong to this
# 
# # RAP 
# country_region$GLO <- TRUE
# # All countries
# 
# country_region <- country_region[c(1,2,3,4,33,34,35,36,37,5:32)]
# 
