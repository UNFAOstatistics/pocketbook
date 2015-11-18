

# 
# ____      _    _____ 
# |  _ \    / \  |  ___|
# | |_) |  / _ \ | |_   
# |  _ <  / ___ \|  _|  
# |_| \_\/_/   \_\_|    
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

RAF_Northern_Africa <- c(4, # Algeria
                         59, # Egypt
                         124, # Libya
                         143, # Morocco
                         277, # South Sudan
                         206, # the Sudan
                         276, # the Sudan
                         222 # Tunisia
                         #                     205 # Western Sahara
)

# cat(paste(country_data$M49_Western.Africa, collapse=","))
# cat(paste(faost_code_data$M49_Western.Africa, collapse=","))  

RAF_Western_Africa <- c(53, # Benin
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
                        #187, # Saint Helena, Ascension and Tristan da Cunha
                        195, # Senegal
                        197, # Sierra Leone
                        217) # Togo


# cat(paste(country_data$M49_Southern.Africa, collapse=","))
# cat(paste(faost_code_data$M49_Southern.Africa, collapse=","))  

RAF_Southern_Africa <- c(20, # Botswana
                         122, # Lesotho
                         147, # Namibia
                         202, # South Africa
                         209) # Swaziland

# cat(paste(country_data$M49_Eastern.Africa, collapse=","))
# cat(paste(faost_code_data$M49_Eastern.Africa, collapse=","))  

RAF_Eastern_Africa <- c(29, # Burundi
                        45, # the Comoros
                        72, # Djibouti
                        178, # Eritrea
                        238, # Ethiopia
                        62, # Ethiopia PDR
                        114, # Kenya
                        129, # Madagascar
                        130, # Malawi
                        137, # Mauritius
                        #270, # Mayotte
                        144, # Mozambique
                        #182, # Réunion
                        184, # Rwanda
                        196, # Seychelles
                        201, # Somalia
                        226, # Uganda
                        215, # the United Republic of Tanzania
                        251, # Zambia
                        181 # Zimbabwe
)

RAF <- c(RAF_Central_Africa,
         RAF_Eastern_Africa,
         RAF_Northern_Africa,
         RAF_Southern_Africa,
         RAF_Western_Africa)

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
                       #41, # China
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
                       246 # Yemen (old)
)


# cat(paste(country_data$M49_Australia.and.New.Zealand, collapse=","))
# cat(paste(faost_code_data$M49_Australia.and.New.Zealand, collapse=","))  

RAP_Austriala_and_New_Zealand  <- c(10,  # Australia
                                    156, # New Zealand
                                    161 # Norfolk Island
)


# cat(paste(country_data$M49_Melanesia, collapse=","))
# cat(paste(faost_code_data$M49_Melanesia, collapse=","))  


RAP_Melanesia  <- c(66, # Fiji
                    153, # New Caledonia
                    168, # Papua New Guinea
                    25, # Solomon Islands
                    155 # Vanuatu
)

# cat(paste(country_data$M49_Micronesia, collapse=","))
# cat(paste(faost_code_data$M49_Micronesia, collapse=","))  


RAP_Micronesia  <- c(88, # Guam
                     83, # Kiribati
                     127, # the Marshall Islands
                     145, # Micronesia (Federated States of)
                     148, # Nauru
                     163, # Northern Mariana Islands
                     164, # Pacific Islands
                     180 # Trust Territory of,Palau
)


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
                    243 # Wallis and Futuna Islands
)


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
         RAP_United_States
)


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

REU_EU_other_and_EFTA <- c(6, #Andorra
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

REU_EU_Central_and_Eastern <- c(27,  # Bulgaria
                                167, # the Czech Republic
                                51,  # Czechoslovakia
                                63, # Estonia
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
         REU_EU_other_and_EFTA,
         REU_Caucasus_and_Turkey,
         REU_CIS_Europe,
         REU_EU_Central_and_Eastern,
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
                      #67, #Finland
                      222) # Tunisia

RNE <- c(RNE_Gulf_Cooperation_Council_States_and_Yemen,
         RNE_Other_Near_East_countries,
         RNE_North_Africa)


# _        _    ____ 
# | |      / \  / ___|
# | |     / _ \| |    
# | |___ / ___ \ |___ 
# |_____/_/   \_\____|
#   

LAC_Caribbean <- c(8, # Antigua and Barbuda
                   12, # Bahamas
                   14, # Barbados
                   49, # Cuba
                   55, # Dominica
                   56, # Dominican Republic
                   86, # Grenada
                   93, # Haiti
                   188, # Saint Kitts and Nevis
                   189, # Saint Lucia
                   191, # Saint Vincent and the Grenadines
                   220  # Trinidad and Tobago 
)



LAC_Central_America <- c(23, # Belize
                         48, # Costa Rica
                         60, # El Salvador
                         89, # Guatemala
                         95, # Honduras
                         138, # Mexico
                         157, # Nicaragua
                         166 # Panama
)


LAC_South_America <- c(9, # Argentina
                       19, # Bolivia (Plurinational State of)
                       21, # Brazil
                       40, # Chile
                       44, # Colombia
                       58, # Ecuador
                       91, # Guyana
                       169, # Paraguay
                       170, # Peru
                       207, # Suriname
                       234, # Uruguay
                       236 # Venezuela (Bolivarian Republic of)
                       
)



LAC_North_America <- c(33, # Canada
                       #67, #Finland
                       231 # United States of America
)


LAC <- c(LAC_Caribbean,
         LAC_Central_America,
         LAC_South_America,
         LAC_North_America)






# ____  _      ___  
# / ___|| |    / _ \ 
# | |  _ | |   | | | |
# | |_| || |___| |_| |
# \____||_____|\___/ 
#   


GLO_Europe <- REU
GLO_Asia <- RAP
GLO_Africa <- RAF
GLO_North_Africa_and_Middle_East <- RNE
GLO_Latin_America_and_the_Caribbean <- LAC

GLO <- c(GLO_Europe,
         GLO_Asia,
         GLO_Africa,
         GLO_North_Africa_and_Middle_East,
         GLO_Latin_America_and_the_Caribbean
         #          GLO_Oceania,
         #          GLO_Americas
)




# __  __  _  _    ___  
# |  \/  || || |  / _ \ 
# | |\/| || || |_| (_) |
# | |  | ||__   _|\__, |
# |_|  |_|   |_|    /_/ 
#   


M49_Europe <- c(284,	 # Åland Islands
                3,	 # Albania
                6,	 # Andorra
                11,	 # Austria
                57,	 # Belarus
                255,	 # Belgium
                15,	 # Belgium-Luxembourg
                80,	 # Bosnia and Herzegovina
                27,	 # Bulgaria
                98,	 # Croatia
                167,	 # the Czech Republic
                51,	 # Czechoslovakia
                54,	 # Denmark
                63,	 # Estonia
                64,	 # Faroe Islands
                67,	 # Finland
                68,	 # France
                79,	 # Germany
                82,	 # Gibraltar
                84,	 # Greece
                274,	 # Guernsey
                94,	 # Holy See
                97,	 # Hungary
                99,	 # Iceland
                104,	 # Ireland
                264,	 # Isle of Man
                106,	 # Italy
                283,	 # Jersey
                119,	 # Latvia
                125,	 # Liechtenstein
                126,	 # Lithuania
                256,	 # Luxembourg
                134,	 # Malta
                140,	 # Monaco
                273,	 # Montenegro
                150,	 # the Netherlands
                162,	 # Norway
                173,	 # Poland
                174,	 # Portugal
                146,	 # Republic of Moldova
                183,	 # Romania
                185,	 # the Russian Federation
                192,	 # San Marino
                285,	 # Sark
                272,	 # Serbia
                186,	 # Serbia and Montenegro
                199,	 # Slovakia
                198,	 # Slovenia
                228,	 # Union of Soviet Socialist Republic
                203,	 # Spain
                260,	 # Svalbard and Jan Mayen Islands
                210,	 # Sweden
                211,	 # Switzerland
                154,	 # The former Yugoslav Republic of Macedonia
                230,	 # Ukraine
                229,	 # the United Kingdom of Great Britain and Northern Ireland
                248	 # Yugoslav SFR
)

# cat(paste(country_data$M49_Asia, collapse=","))
# cat(paste(faost_code_data$M49_Asia, collapse=","))

M49_Asia <- c(2,	 # Afghanistan
              1,	 # Armenia
              52,	 # Azerbaijan
              13,	 # Bahrain
              16,	 # Bangladesh
              18,	 # Bhutan
              26,	 # Brunei Darussalam
              115,	 # Cambodia
              351,	 # China
              #41, # China
              50,	 # Cyprus
              116,	 # Democratic People's Republic of Korea
              73,	 # Georgia
              100,	 # India
              101,	 # Indonesia
              102,	 # Iran (Islamic Republic of)
              103,	 # Iraq
              105,	 # Israel
              110,	 # Japan
              112,	 # Jordan
              108,	 # Kazakhstan
              118,	 # Kuwait
              113,	 # Kyrgyzstan
              120,	 # the Lao People's Democratic Republic
              121,	 # Lebanon
              131,	 # Malaysia
              132,	 # Maldives
              141,	 # Mongolia
              28,	 # Myanmar
              149,	 # Nepal
              299,	 # Occupied Palestinian Territory
              221,	 # Oman
              165,	 # Pakistan
              171,	 # the Philippines
              179,	 # Qatar
              117,	 # Republic of Korea
              194,	 # Saudi Arabia
              200,	 # Singapore
              38,	 # Sri Lanka
              212,	 # the Syrian Arab Republic
              208,	 # Tajikistan
              216,	 # Thailand
              176,	 # Timor-Leste
              223,	 # Turkey
              213,	 # Turkmenistan
              225,	 # the United Arab Emirates
              235,	 # Uzbekistan
              237,	 # Viet Nam
              249,	 # Yemen
              247,	 # Democratic Yemen
              246	 # Yemen (old)
)

# cat(paste(country_data$M49_Africa, collapse=","))
# cat(paste(faost_code_data$M49_Africa, collapse=","))

M49_Africa <- c(4,	 # Algeria
                7,	 # Angola
                53,	 # Benin
                20,	 # Botswana
                233,	 # Burkina Faso
                29,	 # Burundi
                32,	 # Cameroon
                35,	 # Cabo Verde
                37,	 # Central African Republic
                39,	 # Chad
                45,	 # the Comoros
                46,	 # Congo
                107,	 # Côte d'Ivoire
                250,	 # the Democratic Republic of the Congo
                72,	 # Djibouti
                59,	 # Egypt
                61,	 # Equatorial Guinea
                178,	 # Eritrea
                238,	 # Ethiopia
                62,	 # Ethiopia PDR
                74,	 # Gabon
                75,	 # the Gambia
                81,	 # Ghana
                90,	 # Guinea
                175,	 # Guinea-Bissau
                114,	 # Kenya
                122,	 # Lesotho
                123,	 # Liberia
                124,	 # Libya
                129,	 # Madagascar
                130,	 # Malawi
                133,	 # Mali
                136,	 # Mauritania
                137,	 # Mauritius
                270,	 # Mayotte
                143,	 # Morocco
                144,	 # Mozambique
                147,	 # Namibia
                158,	 # the Niger
                159,	 # Nigeria
                182,	 # Réunion
                184,	 # Rwanda
                187,	 # Saint Helena, Ascension and Tristan da Cunha
                193,	 # Sao Tome and Principe
                195,	 # Senegal
                196,	 # Seychelles
                197,	 # Sierra Leone
                201,	 # Somalia
                202,	 # South Africa
                277,	 # South Sudan
                206,	 # the Sudan
                276,	 # the Sudan
                209,	 # Swaziland
                217,	 # Togo
                222,	 # Tunisia
                226,	 # Uganda
                215,	 # the United Republic of Tanzania
                205,	 # Western Sahara
                251,	 # Zambia
                181	 # Zimbabwe
)

# cat(paste(country_data$M49_Oceania, collapse=","))
# cat(paste(faost_code_data$M49_Oceania, collapse=","))

M49_Oceania <- c(5,	 # American Samoa
                 10,	 # Australia
                 47,	 # the Cook Islands
                 66,	 # Fiji
                 70,	 # French Polynesia
                 88,	 # Guam
                 83,	 # Kiribati
                 127,	 # the Marshall Islands
                 145,	 # Micronesia (Federated States of)
                 148,	 # Nauru
                 153,	 # New Caledonia
                 156,	 # New Zealand
                 160,	 # Niue
                 161,	 # Norfolk Island
                 163,	 # Northern Mariana Islands
                 164,	 # Pacific Islands, Trust Territory of
                 180,	 # Palau
                 168,	 # Papua New Guinea
                 172,	 # Pitcairn Islands
                 244,	 # Samoa
                 25,	 # Solomon Islands
                 218,	 # Tokelau
                 219,	 # Tonga
                 227,	 # Tuvalu
                 155,	 # Vanuatu
                 243	 # Wallis and Futuna Islands
)


# cat(paste(country_data$M49_Americas, collapse=","))
# cat(paste(faost_code_data$M49_Americas, collapse=",")

M49_Americas <- c(258,	 # Anguilla
                  8,	 # Antigua and Barbuda
                  9,	 # Argentina
                  22,	 # Aruba
                  12,	 # the Bahamas
                  14,	 # Barbados
                  23,	 # Belize
                  17,	 # Bermuda
                  19,	 # Bolivia (Plurinational State of)
                  278,	 # Bonaire, Sint Eustatius and Saba
                  21,	 # Brazil
                  239,	 # British Virgin Islands
                  33,	 # Canada
                  36,	 # Cayman Islands
                  40,	 # Chile
                  44,	 # Colombia
                  48,	 # Costa Rica
                  49,	 # Cuba
                  279,	 # Curaçao
                  55,	 # Dominica
                  56,	 # the Dominican Republic
                  58,	 # Ecuador
                  60,	 # El Salvador
                  65,	 # Falkland Islands (Malvinas)
                  69,	 # French Guiana
                  85,	 # Greenland
                  86,	 # Grenada
                  87,	 # Guadeloupe
                  89,	 # Guatemala
                  91,	 # Guyana
                  93,	 # Haiti
                  95,	 # Honduras
                  109,	 # Jamaica
                  135,	 # Martinique
                  138,	 # Mexico
                  142,	 # Montserrat
                  151,	 # Netherlands Antilles
                  157,	 # Nicaragua
                  166,	 # Panama
                  169,	 # Paraguay
                  170,	 # Peru
                  177,	 # Puerto Rico
                  282,	 # Saint Barthélemy
                  188,	 # Saint Kitts and Nevis
                  189,	 # Saint Lucia
                  190,	 # Saint Pierre and Miquelon
                  191,	 # Saint Vincent and the Grenadines
                  281,	 # Saint-Martin (French Part)
                  280,	 # Sint Maarten (Dutch Part)
                  207,	 # Suriname
                  220,	 # Trinidad and Tobago
                  224,	 # Turks and Caicos Islands
                  231,	 # the United States of America
                  240,	 # United States Virgin Islands
                  234,	 # Uruguay
                  236	 # Venezuela (Bolivarian Republic of)
)






#  ____  ___   _____ 
# / ___|/ _ \ |  ___|
# | |   | | | || |_   
# | |___| |_| ||  _|  
# \____|\___/ |_|    
#   

COF_Coffee_producers_April <- c(7, # Angola
                                19, # Bolivia
                                21, # Brazil
                                29, # Burundi
                                58, # Ecuador
                                101, # Indonesia
                                129, # Madagascar
                                130, # Malawi
                                168, # Papua New Guinea
                                169, # Paraguay
                                170, # Peru
                                184, # Rwanda
                                176, # Timor-Leste
                                181 # Zimbabwe
)

COF_Coffee_producers_July <- c(46, # the Republic of the Congo
                               #19, # the Democratic Republic of the Congo
                               49, # Cuba
                               56, # Dominican Republic
                               93, # Haiti
                               171, # Philippines
                               215, # Tanzania
                               251 # Zambia
)


COF_Coffee_producers_October <- c(53, # Benin
                                  32, # Cameroon
                                  37, # Central African Republic
                                  44, # Colombia
                                  250, # the Democratic Republic of the Congo
                                  48, # Costa Rica
                                  107, # Côte d'Ivoire
                                  60, # El Salvador
                                  61, # Equatorial Guinea
                                  238, # Ethiopia
                                  74, # Gabon
                                  81, # Ghana
                                  89, # Guatemala
                                  90, # Guinea
                                  91, # Guyana
                                  95, # Honduras
                                  100, # India
                                  109, # Jamaica
                                  114, # Kenya
                                  120, # the Lao People's Democratic Republic
                                  123, # Liberia
                                  138, # Mexico
                                  149, # Nepal
                                  157, # Nicaragua
                                  159, # Nigeria
                                  166, # Panama
                                  197, # Sierra Leone
                                  38, # Sri Lanka
                                  216, # Thailand
                                  217, # Togo
                                  197, # Trinidad
                                  220, # Trinidad & Tobago
                                  226, # Uganda
                                  236, # Venezuela
                                  237, # Vietnam
                                  246 # Yemen
)


COF_Nonmember_countries_Africa <- c(4,	# Algeria
                                    20,	# Botswana
                                    233,	# Burkina Faso
                                    35,	# Cape Verde
                                    39,	# Chad
                                    45,	# Comoros
                                    72,	# Djibouti
                                    59,	# Egypt
                                    75,	# Gambia
                                    175,	# Guinea-Bissau
                                    122,	# Lesotho
                                    124,	# Libya
                                    133,	# Mali
                                    136,	# Mauritania
                                    137,	# Mauritius
                                    143,	# Morocco
                                    144,	# Mozambique
                                    147,	# Namibia
                                    158,	# Niger
                                    193,	# Sao Tome and Principe
                                    195,	# Senegal
                                    196,	# Seychelles
                                    201,	# Somalia
                                    202,	# South Africa, Rep.of
                                    276,	# Sudan
                                    209	# Swaziland
)

COF_Nonmember_countries_Asia_and_Oceania <- c(
  2,	# Afghanistan
  1,	 # Armenia
  10,	 # Australia
  52,	 # Azerbaijan
  13,	 # Bahrain
  16,	 # Bangladesh
  26,	 # Brunei Darussalam
  115,	 # Cambodia
  351,	 # China, People's Republic of
  47,	 # Cook Islands
  66,	 # Fiji
  102,	 # Iran, Islamic Rep. of
  103,	 # Iraq
  105,	 # Israel
  112,	 # Jordan
  108,	 # Kazakhstan
  83,	 # Kiribati
  116,	 # Korea, Dem. People's Rep. of
  117,	 # Korea, Rep. of
  118,	 # Kuwait
  113,	 # Kyrgyz Republic
  121,	 # Lebanon
  131,	 # Malaysia
  141,	 # Mongolia
  28,	 # Myanmar
  156,	 # New Zealand
  221,	 # Oman
  165,	 # Pakistan
  179,	 # Qatar
  244,	 # Samoa
  194,	 # Saudi Arabia
  200,	 # Singapore
  25,	 # Solomon Islands
  212,	 # Syrian Arab Republic
  NA,	 # Taiwan
  219,	 # Tonga
  225,	 # United Arab Emirates
  225,	 #    United Arab Emirates
  155	 # Vanuatu
)

COF_Nonmember_countries_Caribbean <- c(258, # Anguila
                                       8, # Antigua
                                       22, # Aruba
                                       12, # Bahamas
                                       14, # Barbados
                                       36, # Cayman
                                       55, # Dominica
                                       86, # Grenada
                                       142, # Montserrat
                                       278, # Bonaire, Sint Eustatius and Saba
                                       177, # Puerto Rico
                                       188, # Saint Kitts and Nevis
                                       189, # Saint Lucia
                                       191, # Saint Vincent
                                       239 # Virgin Is
)

COF_Nonmember_countries_Central_America_and_Mexico <- c(23, # Belize
                                                        17 # Bermuda
                                                        
)

COF_Nonmember_countries_Europe <- c(3, # Albania
                                    57, # Belarus
                                    80, # Bosnia
                                    73, # Georgia
                                    82, # Gibraltar
                                    94, # Holy see
                                    99, # Iceland
                                    154, # Macedonia
                                    146, # Moldova
                                    273, # Montenegro
                                    272, # Serbia
                                    185, # Russian Fede
                                    230 # Ukraine
                                    
)

COF_Nonmember_countries_North_America <- c(33 # Canada
)

COF_Nonmember_countries_South_America <- c(234, # Uruguya
                                           40, # Chile
                                           207, # Suriname
                                           234 # Uruguay
)





COF_nonmembers_underdeveloped <- c(COF_Nonmember_countries_Caribbean,
                                   COF_Nonmember_countries_Central_America_and_Mexico,
                                   COF_Nonmember_countries_South_America,
                                   COF_Nonmember_countries_Africa,
                                   COF_Nonmember_countries_Asia_and_Oceania)

COF_nonmembers_developed <- c(COF_Nonmember_countries_Europe,
                              COF_Nonmember_countries_North_America
)


COF <- c(M49_Americas,
         M49_Oceania,
         M49_Africa,
         M49_Asia,
         M49_Europe)


##########################################################################################################
# Create dummy vars 

if (!(exists("run_only_regions"))) region_key <- FAOcountryProfile[c("FAOST_CODE","FAO_TABLE_NAME","SHORT_NAME")]
if (exists("run_only_regions")) region_key <- FAOcountryProfile[c("FAOST_CODE","FAO_TABLE_NAME")]

# include only countries listed above
region_key <- region_key[region_key$FAOST_CODE %in% GLO,]

region_key$RAF                <- ifelse(region_key$FAOST_CODE %in% RAF, TRUE, FALSE)
region_key$RAF_Central_Africa <- ifelse(region_key$FAOST_CODE %in% RAF_Central_Africa, TRUE, FALSE) 
region_key$RAF_Eastern_Africa    <- ifelse(region_key$FAOST_CODE %in% RAF_Eastern_Africa, TRUE, FALSE) 
region_key$RAF_Northern_Africa   <- ifelse(region_key$FAOST_CODE %in% RAF_Northern_Africa, TRUE, FALSE) 
region_key$RAF_Southern_Africa   <- ifelse(region_key$FAOST_CODE %in% RAF_Southern_Africa, TRUE, FALSE) 
region_key$RAF_Western_Africa    <- ifelse(region_key$FAOST_CODE %in% RAF_Western_Africa, TRUE, FALSE) 

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
region_key$REU_EU_other_and_EFTA        <- ifelse(region_key$FAOST_CODE %in% REU_EU_other_and_EFTA, TRUE, FALSE)
region_key$REU_Caucasus_and_Turkey      <- ifelse(region_key$FAOST_CODE %in% REU_Caucasus_and_Turkey, TRUE, FALSE)
region_key$REU_CIS_Europe               <- ifelse(region_key$FAOST_CODE %in% REU_CIS_Europe, TRUE, FALSE)
region_key$REU_EU_Central_and_Eastern   <- ifelse(region_key$FAOST_CODE %in% REU_EU_Central_and_Eastern, TRUE, FALSE)
region_key$REU_Israel                   <- ifelse(region_key$FAOST_CODE %in% REU_Israel, TRUE, FALSE)
region_key$REU_Central_Asia             <- ifelse(region_key$FAOST_CODE %in% REU_Central_Asia, TRUE, FALSE)

region_key$RNE                                            <- ifelse(region_key$FAOST_CODE %in% RNE, TRUE, FALSE)
region_key$RNE_Gulf_Cooperation_Council_States_and_Yemen  <- ifelse(region_key$FAOST_CODE %in% RNE_Gulf_Cooperation_Council_States_and_Yemen, TRUE, FALSE)
region_key$RNE_Other_Near_East_countries                  <- ifelse(region_key$FAOST_CODE %in% RNE_Other_Near_East_countries, TRUE, FALSE)
region_key$RNE_North_Africa                               <- ifelse(region_key$FAOST_CODE %in% RNE_North_Africa, TRUE, FALSE)

region_key$LAC                  <- ifelse(region_key$FAOST_CODE %in% LAC, TRUE, FALSE)
region_key$LAC_Caribbean        <- ifelse(region_key$FAOST_CODE %in% LAC_Caribbean, TRUE, FALSE)
region_key$LAC_Central_America  <- ifelse(region_key$FAOST_CODE %in% LAC_Central_America, TRUE, FALSE)
region_key$LAC_South_America    <- ifelse(region_key$FAOST_CODE %in% LAC_South_America, TRUE, FALSE)
region_key$LAC_North_America    <- ifelse(region_key$FAOST_CODE %in% LAC_North_America, TRUE, FALSE)


region_key$GLO             <- ifelse(region_key$FAOST_CODE %in% GLO, TRUE, FALSE)
region_key$GLO_Asia        <- ifelse(region_key$FAOST_CODE %in% GLO_Asia, TRUE, FALSE)
region_key$GLO_Africa      <- ifelse(region_key$FAOST_CODE %in% GLO_Africa, TRUE, FALSE)
region_key$GLO_Europe      <- ifelse(region_key$FAOST_CODE %in% GLO_Europe, TRUE, FALSE)
region_key$GLO_North_Africa_and_Middle_East <- ifelse(region_key$FAOST_CODE %in% GLO_North_Africa_and_Middle_East, TRUE, FALSE)
region_key$GLO_Latin_America_and_the_Caribbean <- ifelse(region_key$FAOST_CODE %in% GLO_Latin_America_and_the_Caribbean, TRUE, FALSE)

region_key$COF          <- ifelse(region_key$FAOST_CODE %in% COF,          TRUE, FALSE)
region_key$COF_Americas <- ifelse(region_key$FAOST_CODE %in% M49_Americas, TRUE, FALSE)
region_key$COF_Oceania  <- ifelse(region_key$FAOST_CODE %in% M49_Oceania,  TRUE, FALSE)
region_key$COF_Africa   <- ifelse(region_key$FAOST_CODE %in% M49_Africa,   TRUE, FALSE)
region_key$COF_Asia     <- ifelse(region_key$FAOST_CODE %in% M49_Asia,     TRUE, FALSE)
region_key$COF_Europe   <- ifelse(region_key$FAOST_CODE %in% M49_Europe,   TRUE, FALSE)


if (!(exists("run_only_regions"))){
  
  # Replace the ad-hoc regional grouping with the one we have created
  myvars <- names(fao_world@data) %in% c("RAF","LAC","RAP","REU","RNE")
  fao_world@data <- fao_world@data[!myvars]
  
  # View(region_key[!(region_key$FAOST_CODE %in% fao_world@data$FAOST_CODE),])
  fao_world$FAOST_CODE[fao_world$FAOST_CODE %in% 41] <- 351
  
  attribute_data <- region_key[region_key$FAOST_CODE %in% fao_world@data$FAOST_CODE,]
  
  
  FAOST_CODE <- as.character(fao_world$FAOST_CODE)
  VarX <- rep(NA, length(FAOST_CODE))
  dat <- data.frame(FAOST_CODE,VarX)
  # then we shall merge this with region_key data.frame
  dat2 <- merge(dat,attribute_data,by="FAOST_CODE", all.x=TRUE)
  ## merge this manipulated attribute data with the spatialpolygondataframe
  ## rownames
  row.names(dat2) <- dat2$FAOST_CODE
  row.names(fao_world) <- as.character(fao_world$FAOST_CODE)
  ## order data
  dat2 <- dat2[order(row.names(dat2)), ]
  fao_world <- fao_world[order(row.names(fao_world)), ]
  ## join
  library(maptools)
  dat2$FAOST_CODE <- NULL
  fao_world <- spCbind(fao_world, dat2)
  
}

