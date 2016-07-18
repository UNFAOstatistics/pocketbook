# Function for subgroups

#` This is a bit complicated one. However, the idea is construct a data.frame
# with FAOST_CODE and subgroup columns that tells into which subgroup a particular country
# belongs to. If a country belongs to multiple groups, you want something like
# gather = TRUE
# Russia | Asia and the Pacific
# Russia | Europe and the Central Asia
# Or gather = FALSE
# Russia | Asia and the Pacific AND Europe and the Central Asia

subgrouping <- function(region_to_report, gather=TRUE){
  df <- region_key[c("FAOST_CODE","SHORT_NAME",names(region_key)[grep(region_to_report, names(region_key))])]
  df <- df[which(df[[region_to_report]]),]
  # subregions for this region
  subregion <- names(df)[grep(paste0(region_to_report,"_"), names(df))]
  n_subregion <- length(subregion)
  # create n_subregions number of empty vars
  if (n_subregion >= 1) df$subgroup1 <- ""
  if (n_subregion >= 2) df$subgroup2 <- ""
  if (n_subregion >= 3) df$subgroup3 <- ""
  if (n_subregion >= 4) df$subgroup4 <- ""
  if (n_subregion >= 5) df$subgroup5 <- ""
  if (n_subregion >= 6) df$subgroup6 <- ""
  if (n_subregion >= 7) df$subgroup7 <- ""
  if (n_subregion >= 8) df$subgroup8 <- ""
  if (n_subregion >= 9) df$subgroup9 <- ""
  if (n_subregion >= 10) df$subgroup10 <- ""
  if (n_subregion >= 11) df$subgroup11 <- ""
  if (n_subregion >= 12) df$subgroup12 <- ""
  if (n_subregion >= 13) df$subgroup13 <- ""
  if (n_subregion >= 14) df$subgroup14 <- ""
  if (n_subregion >= 15) df$subgroup15 <- ""
  if (n_subregion >= 16) stop("more than 16 subregions. Current code cannot handle it!")
  
  for (i in 1:n_subregion) {
    df[[paste0("subgroup",i)]] <- ifelse(df[[subregion[i]]] & df[[paste0("subgroup",i)]] == "",
                                         subregion[i],
                                         df[[paste0("subgroup",i)]]) 
  }
  for (i in 1:n_subregion) {
    df[[paste0("subgroup",i)]] <- str_replace_all(df[[paste0("subgroup",i)]], paste0(region_to_report,"_"), "")
    df[[paste0("subgroup",i)]] <- str_replace_all(df[[paste0("subgroup",i)]], "_", " ")
  }
  df$subgroup <- ""
  if (gather){
    df_x <- data.frame()
    for (i in 1:n_subregion) {
      FAOST_CODE <- ifelse(df[[paste0("subgroup",i)]] != "",df$FAOST_CODE, NA)
      subgroup <- ifelse(df[[paste0("subgroup",i)]] != "",df[[paste0("subgroup",i)]], NA)
      df_x <- rbind(df_x,data.frame(FAOST_CODE,subgroup))
    }
    df <- na.omit(df_x)
  } else {
    for (i in 1:n_subregion) {
      df$subgroup <- paste(df$subgroup, df[[paste0("subgroup",i)]], sep="+")
    }
    df$subgroup <- str_trim(df$subgroup)
    df$subgroup <- str_replace_all(df$subgroup, "^\\++", "")
    df$subgroup <- str_replace_all(df$subgroup, "\\++$", "")
    df$subgroup <- str_replace_all(df$subgroup, "\\++", " AND ")
    
  }
  df$subgroup <- as.character(df$subgroup)
  
  if (region_to_report == "RNE") {
    df$subgroup[df$subgroup %in% "Gulf Cooperation Council States and Yemen"] <- "Gulf Cooperation\nCouncil States\nand Yemen"
    df$subgroup[df$subgroup %in% "Other Near East countries"] <- "Other Near East\ncountries"
  }
  
  if (region_to_report == "REU" & !rulang) {
    df$subgroup[df$subgroup %in% "Andorra Israel Monaco and San Marino"] <- "Andorra, Israel, Monaco and San Marino"
  }
  
  
  
  df[c("FAOST_CODE","subgroup")]
}