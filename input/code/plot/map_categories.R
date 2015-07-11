# Creating a custom function for creating the breaks and makeing them look neat
categories <- function(x, cat=5) {

  library(stringr)
  levs <- as.data.frame(as.character(levels(cut_number(x, cat))))
  names(levs) <- "orig"
  levs$mod <- str_replace_all(levs$orig, "\\[", "")
  levs$mod <- str_replace_all(levs$mod, "\\]", "")
  levs$mod <- str_replace_all(levs$mod, "\\(", "")
  levs$lower <- gsub(",.*$","", levs$mod)
  levs$upper <- gsub(".*,","", levs$mod)

  levs$lower <- factor(levs$lower)
  levs$lower <- round(as.numeric(levels(levs$lower))[levs$lower],0)

  levs$upper <- factor(levs$upper)
  levs$upper <- round(as.numeric(levels(levs$upper))[levs$upper],0)

  levs$labs <- paste(levs$lower,levs$upper, sep=" - ")

  labs <- as.character(c(levs$labs))
  y <- cut_number(x, cat, right = FALSE, labels = labs)
  y <- as.character(y)
  y[is.na(y)] <- "No Data"
  y <- factor(y, levels=c("No Data",labs[1:cat]))
}