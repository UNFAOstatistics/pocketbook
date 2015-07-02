##' Map information
##'
##' Returns the info needed to feed the map function. This 
##' function is built on the basis of the current structure
##' of the SYB process.
##'
##' @param dissemination The dissemination file.
##' @param mapName Name of the map in the dissemination file.
##' @param data The dataset.
##' @param ncol Number of the colours in the map.
##' @param scheme Choose among sequential, diverging, and qualitative.
##' @param refCode The country coding system used in the shapefile.
##' @param mapArea Geographical level (usually "Territory").
##' @param revIntensity Logical. Option to reverse the color intensity.   
##' @return Returns the info needed to feed the map function.
##' @export

map_info = function(dissemination = diss.df, mapName, data, nCol = 5, mapArea,
                    scheme = c("sequential", "diverging", "qualitative"),
                    refCode = mapCountryCode, revIntensity = FALSE) {
  
  scheme = match.arg(scheme)
  specMap = subset(dissemination, subset = OBJECT_NAME == mapName)
  
  ## colors
  if (scheme == "sequential") {
    colPart = plot_colors(part = specMap[,"PART"], 12)
    mapColFun = colorRampPalette(c("white", colPart[["Main"]][1]))
    tmpCol = mapColFun(nCol)[2]
    mapColFun = colorRampPalette(c(tmpCol, colPart[["Main"]][1]))
    mapColors = mapColFun(nCol)
    if (revIntensity) mapColors = rev(mapColors)
  } else if (scheme == "qualitative"){
    mapColors = plot_colors(part = specMap[,"PART"], nCol)[["Sub"]]
  } else if (scheme == "diverging") {
#     lenghtWings = round(nCol/2, digits = 0) + 1
#     divCols = plot_colors(part = specMap[,"PART"], 3)[["Sub"]]
#     centralColFun = colorRampPalette(c("white", divCols[2]))
#     centralCol = centralColFun(5)[3]
#     leftColFun = colorRampPalette(c(divCols[1], centralCol))
#     leftCol = leftColFun(lenghtWings)[1:(lenghtWings-1)]
#     rightColFun = colorRampPalette(c(centralCol, divCols[3]))
#     rightCol = rightColFun(lenghtWings)[2:(lenghtWings)]
#     mapColors = c(leftCol, centralCol, rightCol)
    mapColors = c(rgb(202,0,32,maxColorValue=255),
                  rgb(244,165,130,maxColorValue=255),
                  rgb(247,247,247,maxColorValue=255),
                  rgb(146,197,222,maxColorValue=255), 
                  rgb(5,113,176,maxColorValue=255))
  }

  ## year
  mapYear = specMap[, "YEAR_END"]
  
  ## scaling
  scaling = ifelse(!is.na(specMap[, "QUANTITY"]), specMap[, "QUANTITY"], NA)
  scaling = 1/ifelse (!is.na(scaling) & scaling != "", translateUnit(scaling), 1)
  
  ## data
  timeSlot = ifelse(!is.na(specMap[, "INTERVAL"]), specMap[, "INTERVAL"], NA)
  territoryData = subset(data, subset = Area %in% c(mapArea))
  if (!is.na(timeSlot)) {
    mapYear = c(specMap[, "YEAR_START"]:specMap[, "YEAR_END"])
    tmpData.sub = na.omit(subset(territoryData,                                 
                                 select = c(refCode, "Year", specMap[, "DATA_KEY1"]),
                                 subset = Year %in% eval(parse(text = paste(specMap[, "YEAR_START"], specMap[, "YEAR_END"], sep = ":")))))
    tmpData.resh = dcast(tmpData.sub, 
                         formula = eval(parse(text = paste("Year ~ ",  refCode, sep = ""))))
    tmpData.locf = na.locf(tmpData.resh)
    maxYear = max(tmpData.locf$Year)
    tmpData.locf = subset(tmpData.locf, Year == maxYear)
    tmpData = melt(tmpData.locf, id = "Year")[, c("variable", "Year", "value")]
    colnames(tmpData)[grep("variable", colnames(tmpData))] = refCode
    colnames(tmpData)[grep("value", colnames(tmpData))] = specMap[, "DATA_KEY1"]
    tmpData[, 1] = as.character(tmpData[, 1])
    tmpData[, 1] = as.numeric(tmpData[, 1])
    tmpData[, "Area"] = mapArea
  } else {
    tmpData = subset(territoryData, 
                     subset = Year == specMap[, "YEAR_END"],
                     select = c(refCode, "Year", "Area", specMap[, "DATA_KEY1"]))
  }
  ## return
  mapInfo = list(mapName = mapName, mapVariable = specMap[, "DATA_KEY1"], 
                 mapColors = mapColors, scaling = scaling, mapData = tmpData,
                 mapYear = mapYear, mapArea = mapArea, mapScaling = scaling)
}
