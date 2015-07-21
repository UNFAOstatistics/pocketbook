################################################################################################
# This script defines the macro regions and subregions used in regional statistical pockebooks 2015
# Markus Kainu 20150717
# 
# The aim is to create a region_key that has FAOST_CODE and SHORT_NAME AND dummy vars for each region and subregion - belongs T/F

# First lets download the raw FAOcountryprofile and begin with the m49 classificaion defined there "UNSD_MACRO_REG","UNSD_SUB_REG" variables
# 
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
