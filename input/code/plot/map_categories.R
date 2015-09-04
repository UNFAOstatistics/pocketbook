categories <- function(x, n=5,method="jenks",manual=FALSE,manual_breaks = NULL) {
  
  library(stringr)
  library(classInt)
  if (manual) {
    levs <- as.data.frame(levels(cut(x, 
                                     breaks=manual_breaks,
                                     include.lowest=T,
                                     dig.lab=5)))
  } else {
    levs <- as.data.frame(levels(cut(x, 
                                     breaks=data.frame(classIntervals(x,n=n,method=method)[2])[,1],
                                     include.lowest=T,
                                     dig.lab=5)))
  } 
  names(levs) <- "orig"
  levs$mod <- str_replace_all(levs$orig, "\\[", "")
  levs$mod <- str_replace_all(levs$mod, "\\]", "")
  levs$mod <- str_replace_all(levs$mod, "\\(", "")
  levs$lower <- gsub(",.*$","", levs$mod)
  levs$upper <- gsub(".*,","", levs$mod)
  
  levs$lower <- factor(levs$lower)
  levs$lower <- round(as.numeric(levels(levs$lower))[levs$lower],0)
  levs$lower <- prettyNum(levs$lower, big.mark=" ")
  
  levs$upper <- factor(levs$upper)
  levs$upper <- round(as.numeric(levels(levs$upper))[levs$upper],0)
  levs$upper <- prettyNum(levs$upper, big.mark=" ")
  
  levs$labs <- paste(levs$lower,levs$upper, sep=" ~< ")
  
  labs <- as.character(c(levs$labs))
  if (manual) {
    y <- cut(x, breaks = manual_breaks,
             include.lowest=T,
             dig.lab=5, labels = labs)
    rm(manual_breaks)
  } else {
    y <- cut(x, breaks = data.frame(classIntervals(x,n=n,method=method)[2])[,1],
             include.lowest=T,
             dig.lab=5, labels = labs)
  }
  y <- as.character(y)
  #if (is.na(y)) {
  y[is.na(y)] <- "No Data"
  y <- factor(y, levels=c("No Data",labs[1:n]))
  #} else y <- factor(y, levels=c(labs[1:n]))
  y
}