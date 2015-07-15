
fao_prof <- read.csv("./input/data/FAOcountryProfile.csv", encoding = "UTF-8", stringsAsFactors = FALSE)

unsd <- fao_prof[,c("FAOST_CODE","FAO_TABLE_NAME","UNSD_MACRO_REG","UNSD_SUB_REG")]

for (var in c("UNSD_MACRO_REG","UNSD_SUB_REG")) {
  for(t in unique(unsd[[var]])) {
    unsd[paste("M49",t,sep="_")] <- ifelse(unsd[[var]]==t,TRUE,FALSE)
    }
}
country_region <- unsd
write.csv(country_region,paste0(root.dir,"/input/data/country_region.csv"), row.names = FALSE)
###


#      _                               _ _ _         _                   
#     / \   _ __ ___  _   _    ___  __| (_) |_ ___  | |__   ___ _ __ ___ 
#    / _ \ | '_ ` _ \| | | |  / _ \/ _` | | __/ __| | '_ \ / _ \ '__/ _ \
#   / ___ \| | | | | | |_| | |  __/ (_| | | |_\__ \ | | | |  __/ | |  __/
#  /_/   \_\_| |_| |_|\__, |  \___|\__,_|_|\__|___/ |_| |_|\___|_|  \___|
#                     |___/                                             


### Read Amys edits
country_region <- read.csv(paste0(root.dir,"/input/data/country_region.csv"), stringsAsFactors = FALSE)

# Define the final regions
# RAF
country_region$RAF <- ifelse(country_region$M49_Africa, TRUE, FALSE) 

# REU
country_region$REU <- ifelse(country_region$M49_Europe | country_region$M49_Central.Asia, TRUE, FALSE) 

# RAP
country_region$RAP <- ifelse(country_region$M49_Asia | country_region$M49_Oceania, TRUE, FALSE) 

# RAP
country_region$RNE <- ifelse(country_region$M49_Northern.Africa | country_region$M49_Western.Asia, TRUE, FALSE) 
country_region$GLO <- TRUE

country_data <- data.frame(colx = rep("gaga", 284))
for (col in 5:37){
  colname <- names(country_region)[col]
  vector <- ifelse(country_region[[colname]],country_region[[2]],NA)
  vector <- vector[!is.na(vector)]
  if (length(vector) < 284) vector <- c(vector, rep("empty", (284-length(vector))))
  vector <- as.data.frame(vector)
  country_data <- bind_cols(country_data,vector)
  names(country_data)[col-3] <- colname
}




country_region$FAO_TABLE_NAME <- NULL
save(country_region, file=paste0(root.dir,"input/data/country_region.RData"))