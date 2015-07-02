##' Function for getting the year label
##'
##' The function returns a label for the year
##'
##' @param yrStart Starting year.
##' @param yrEnd Ending year.
##' @export

yearLabel = function(yrStart, yrEnd) {
  
  if (!is.na(yrStart) & !is.na(yrEnd)) {
    ## year fields are filled
    if (yrStart == yrEnd) {
      ## yearStart == yearEnd
      yearLab = yrEnd
    } else {
      ## different display of the year
      th1 = paste(strsplit(as.character(yrStart), "")[[1]][1], 
                  strsplit(as.character(yrStart), "")[[1]][2], sep = "")
      th2 = paste(strsplit(as.character(yrEnd), "")[[1]][1], 
                  strsplit(as.character(yrEnd), "")[[1]][2], sep = "")
      if (th1 == th2) {
        ## e.g. 1990-95 or 2000-10
        th3 = paste(strsplit(as.character(yrEnd), "")[[1]][3], 
                    strsplit(as.character(yrEnd), "")[[1]][4], sep = "")
        yearLab = paste(yrStart, th3, sep = "-")
      } else {
        ## e.g. 1990-2000
        yearLab = paste(yrStart, yrEnd, sep = "-")
      }
    }
  } else {
    ## year fields are not filled
    yearLab = "NotAv"
  }
  return(yearLab)
}
  

##' Function for getting the unit label
##'
##' The function returns a label for the unit
##'
##' @param qnt Quantity.
##' @param unt Unit.
##' @export

unitLabel = function(qnt, unt) {
  if (is.na(qnt)){
    unitLab = unt
  } else {
    unitLab = paste(qnt, unt, sep = " ")
  }
  return(unitLab)
}


##' Function for selecting the the countries to be shown in the table
##'
##' The function returns a list of countries
##'
##' @param fullList The full list of countries and aggregates normally shown
##' @param criterion For the moment "World" or "TOP50" 
##' @param data The dataset.
##' @param topVar Variable needed to order the dataset for the TOP50.
##' @param topYear Year needed to order the dataset for the TOP50.
##' @param codingSystem The coding system used.
##' @param namingSystem The naming system used.
##' @export

selectBase = function(fullList, criterion, data, topVar, topYear, 
                      codingSystem, namingSystem, aggregates) {

  if (criterion %in% c("World", "RAF", "RAP", "LAC", "RNE", "REU")) {
    finalTableData = fullList
    finalTableData$order = 1:nrow(finalTableData)
  } else {
    finalTableData = subset(data, 
                            subset = Year == topYear & 
                              !Area %in% c("World", "Region", "subRegion"), 
                            select = c(codingSystem, namingSystem, topVar))
    finalTableData = 
      finalTableData[order(finalTableData[, topVar], 
                           na.last = NA, decreasing = TRUE), ][1:40, ]
    finalTableData = finalTableData[, c(codingSystem, namingSystem)]
    finalTableData$order = 1:nrow(finalTableData)
    topAggs = data.frame(FAO_TABLE_NAME = aggregates,
                         order = (nrow(finalTableData)+1):(nrow(finalTableData)+length(aggregates)))
    topAggs = merge(topAggs, 
                    unique(subset(data, 
                                  subset = FAO_TABLE_NAME %in% aggregates, 
                                  select = c(codingSystem, namingSystem))),
                    by = "FAO_TABLE_NAME",
#                     all.x = TRUE)[, c("UN_CODE", "OFFICIAL_FAO_NAME", "order")]
                    all.x = TRUE)[, c(codingSystem, namingSystem, "order")]
    finalTableData = rbind(finalTableData, topAggs)
  }
  return(finalTableData)
}


##' Function for getting info & data to create tables
##'
##' The function returns metadata and data info needed to create tables
##'
##' @param tableName Name to give to the table.
##' @param area Choose between: "World", "TOP50".
##' @param tableTitle Title to give to the table.
##' @param caption1 Caption, first level.
##' @param caption2 Caption, second level.
##' @param caption3 Caption, third level.
##' @param yearStart Starting year.
##' @param yearEnd Ending year.
##' @param interval If we want to calculate the latest year in a certain period.
##' @param quantity Quantity.
##' @param unit Unit.
##' @param variable The variables.
##' @param data The dataset.
##' @param base The base for the list of countries.
##' @param codingSystem The merging code.
##' @export

getTableInfo <- function(tableName, tableTitle,
                         area = c("World", "TOP50", "RAF", "RNE", "LAC",
                                  "RAP", "REU"), 
                         caption1, caption2, caption3, yearStart, yearEnd, 
                         interval, digits, 
                         quantity, unit, variable, data, base, 
                         codingSystem, namingSystem,
                         aggregates, topVar = NA, topYear = NA) {
  
  area = match.arg(area)
  ## number of columns in the table
  noc = length(tableName)
  ## name of the table
  tableName = unique(tableName)[1]
  ## empty captions
  caption2[is.na(caption2)] = " "
  caption3[is.na(caption3)] = " "
  ## top50
  top50 = ifelse(area == "TOP50", TRUE, FALSE)
  ## digits
  digits = digits
  ## year
  year = c()
  finunit = c()
  for (vr in 1:noc) {
    ## get year label
    year[length(year)+1] = yearLabel(yrStart = yearStart[vr], 
                                     yrEnd = yearEnd[vr])
    ## get unit label
    finunit[length(finunit) + 1] = unitLabel(qnt = quantity[vr], 
                                             unt = unit[vr])
  }
  ## select the base
  tableBase = selectBase(fullList = base, criterion = area, 
                         data = data, topVar = topVar, 
                         topYear = topYear, codingSystem = codingSystem,
                         namingSystem = namingSystem, aggregates = aggregates)
  
  finalTableData = tableBase
  ## data
  for (j in 1:noc) {
    if (is.na(variable[j])) {
      ## variable not specified
      tmp = data.frame(Code = tableBase[, codingSystem], Variable = NA)
      colnames(tmp) = c(codingSystem, paste(tableName, j, sep = "_"))
    } else {
      if (!variable[j] %in% colnames(data)) {
        if (variable[j] == "YEAR") {
          ## year variable
          tmpData.sub = na.omit(subset(data,
                                       select = c(codingSystem, "Year", variable[j+1]),
                                       subset = Year %in% eval(parse(text = paste(yearStart[j], yearEnd[j], sep = ":"))) &
                                         data[, codingSystem] %in% tableBase[, codingSystem]))
          tmp = tmpData.sub[, c(codingSystem, "Year")]
          ## I am adding the year in the name of the column because in the table 
          ## we could have the same variable with different years
          colnames(tmp) = c(codingSystem, paste(variable[j], variable[j+1], sep = "_"))
        } else {
          ## variable not in the dataset
          tmp = data.frame(Code = tableBase[, codingSystem], Variable = NA)
          ## I am adding the year in the name of the column because in the table 
          ## we could have the same variable with different years
          colnames(tmp) = c(codingSystem, paste(variable[j], yearEnd[j], sep = "_"))
        }
      } else {
        ## variable in the dataset
        if (!is.na(interval[j])) {
          ## latest year variable
          yrstrt = yearEnd[j]-as.numeric(interval[j])
          tmpData.sub = na.omit(subset(data,
                                       select = c(codingSystem, "Year", variable[j]),
                                       subset = Year %in% eval(parse(text = paste(yrstrt, yearEnd[j], sep = ":"))) &
                                         data[, codingSystem] %in% tableBase[, codingSystem]))
          tmpData.resh = dcast(tmpData.sub, Year~eval(parse(text = codingSystem)))
#           tmpData.resh = dcast(tmpData.sub, Year~UN_CODE)
          tmpData.locf = na.locf(tmpData.resh)
          maxYear = max(tmpData.locf$Year)
          tmpData.locf = subset(tmpData.locf, Year == maxYear)
          tmpData = melt(tmpData.locf, id = "Year")[, c("variable", "value")]
          tmpData[, 1] = as.character(tmpData[, 1])
#           tmpData[, 1] = as.numeric(tmpData[, 1])
          tmpData[, 2] = as.numeric(tmpData[, 2])
          tmp = tmpData
          ## I am adding the year in the name of the column because in the table 
          ## we could have the same variable with different years
          colnames(tmp) = c(codingSystem, paste(variable[j], yearEnd[j], sep = "_"))
          year[j] = paste(year[j], "*", sep = "")
        } else {
          ## "classical" variable
          tmp = subset(data,
                       subset = data[, codingSystem] %in% tableBase[, codingSystem] & 
                         Year == yearEnd[j],
                       select = c(codingSystem, variable[j]))       
          ## I am adding the year in the name of the column because in the table 
          ## we could have the same variable with different years
          colnames(tmp) = c(codingSystem, paste(variable[j], yearEnd[j], sep = "_"))
        }
        
        ## scaling
        scaling = 1/ifelse (!is.na(quantity[j]),
                            translateUnit(quantity[j]), 1)
        tmp[, 2] = tmp[, 2]*scaling
      }
    }
    
    finalTableData = merge(finalTableData, tmp, by = codingSystem, all.x = TRUE, sort = FALSE)
    
  }
  
  finalTableData = finalTableData[order(finalTableData$order, na.last = NA, decreasing = FALSE), ]
  finalTableData = subset(finalTableData,
                          select = !colnames(finalTableData) %in% 
                            c(codingSystem, "order"))
  colnames(finalTableData) = c(" ", caption1)
  
  assign(tableName, list(Description = tableTitle[1],
                         Caption2 = c(" ", caption2),
                         Caption3 = c(" ", caption3),
                         Year = c(" ", year),
                         Unit = c(" ", finunit),
                         Key = c(" ", variable),
                         Data = finalTableData,
                         top50 = top50,
                         digits = c(0, digits)))
  
  return(eval(parse(text = unique(tableName))))
}