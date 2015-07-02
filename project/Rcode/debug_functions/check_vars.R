
check_vars <- function(var="varname") {
  library(DT)
  summary(gsyb15.df[[var]])
  dx <- gsyb15.df[,c("FAOST_CODE","Year",var)]
  print(dim(dx))
  dx <- dx[!is.na(dx$Year),]
  print(dim(dx))
  dx <- dx[!is.na(dx[[var]]),]
  print(dim(dx))
  print(summary(dx$Year))
  print(table(dx$Year))
  dd <- translateCountryCode(dx, "FAOST_CODE", "FAO_TABLE_NAME", "FAOST_CODE")
  datatable(dd,  options = list(pageLength = 100))
  }