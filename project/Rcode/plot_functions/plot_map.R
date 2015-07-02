##'A function for plotting choropleth map
##'
##' @export

library(scales)
library(sp)

plot_map = function (shpFile, var, data, 
                     countryCode = "FAOST_CODE",
                     n = 5, 
                     style = "jenks", 
                     manualBreaks, 
                     col = c("#F5F5F5", "#C8E2DE", "#9CCFC7", "#70BCB0", "#44AA99"), 
                     missCol = "#8B8878", 
                     countryCodeTransp = NULL,
                     missLabel = "No data available", 
                     subset = TRUE,
                     scale = 1, 
                     shpProj = "+proj=robin +ellps=WGS84", 
                     outProj = "+proj=robin"){
  
  if(!missing(shpFile)){
    ## Projection and read shapefile
    llCRS = CRS(projargs = shpProj)
    projCRS = CRS(outProj)
    #raw.sp = readShapePoly(shpFile, proj4string = llCRS)
    #raw.sp <- readOGR(dsn = "../../GSYB2015/shape/Common/GAULRobinson2013/", layer = "WorldMap_Robin_g2015_S2")
    raw.sp <- fao_world # from gisfao-package
    transformed.sp = spTransform(raw.sp, CRSobj = projCRS)
    transformed.df = fortify(transformed.sp, region = countryCode)
    transformed.df$id = as.numeric(transformed.df$id)
  } else {
    transformed.sp = spTransform(GAULspatialPolygon,
                                 CRSobj = CRS(proj4string(GAULspatialPolygon)))
    transformed.df = fortify(transformed.sp, region = countryCode)
    transformed.df$id = as.numeric(transformed.df$id)
    cat("\nNOTE: GAUL border used as default\n")
  }
  transformed.df$order = 1:NROW(transformed.df$order)
  
  ## Subset and scale data
  subset = substitute(subset)
  sub_data = subset.data.frame(data, subset = eval(subset),
                               select = c(countryCode, var))
  sub_data[, var] = sub_data[, var] * scale
  sub_data = unique(sub_data)
  
  ## determine the breaks of the legend and color
  if(missing(manualBreaks)){
    brks = map_breaks(sub_data[, var], n = n, style = style)
  } else {
    brks = manualBreaks
  }
  
  sub_data$fillColor = as.character(findInterval(sub_data[, var],
                                                 brks, rightmost.closed = TRUE))
  final.df = merge(sub_data, transformed.df, by.x = countryCode,
                   by.y = "id", all = TRUE)
  final.df = arrange(final.df, order)
  final.df[is.na(final.df[, var]) & !final.df[, countryCode] %in% countryCodeTransp, "fillColor"] = "0"
  
  ## Match the colors and create the legend
  if(any(is.na(final.df[, var]) & !final.df[, countryCode] %in% countryCodeTransp)){
    uVal = c(sort(unique(final.df$fillColor)))
    uCol = c(missCol, col[sort(as.numeric(unique(final.df$fillColor)))])
    uBrks = c(missLabel,
              formatC(brks[sort(as.numeric(unique(final.df$fillColor))) + 1],
                      format = "fg", big.mark = " "))
    
    nBrks = length(uBrks)
    endMar = rep(0, nBrks)
    endMar[3:(nBrks - 1)] = ifelse(uBrks[3:(nBrks - 1)] >= 10, 1, 0.01)
    
    legendLab = paste(c(uBrks[-nBrks]), c("", rep(" ~ < ", nBrks - 3), " ~ "),
                      c("", uBrks[3:nBrks]), sep = "")
    
    ## ## Format the legend labels
    ## brkNames = c(missLabel, formatC(as.numeric(uBrks[-1]), format = "fg"))
    ## endVal = formatC(c(0, as.numeric(uBrks[-1]) - endMar[-1]),
    ##     format = "fg")
    ## legendLab = paste(c("", brkNames[-c(1, nBrks, nBrks + 1)], ""),
    ##     c(" < ", rep(" - ", nBrks - 2), " > "),
    ##     c(endVal[-c(1, nBrks + 1)], endVal[nBrks]), " (",
    ##     table(sub_data$fillColor), ")", sep = "")
    
  } else {
    uVal = sort(unique(final.df$fillColor))
    uCol = col[sort(as.numeric(unique(final.df$fillColor)))]
    uBrks = formatC(brks[c(sort(as.numeric(unique(final.df$fillColor))),
                           length(brks))], format = "fg", big.mark = " ")
    
    nBrks = length(uBrks)
    endMar = rep(0, nBrks)
    endMar[3:(nBrks - 1)] = ifelse(uBrks[3:(nBrks - 1)] >= 10, 1, 0.01)
    legendLab = paste(c(uBrks[-nBrks]), c(rep(" ~ < ", nBrks - 2), " ~ "),
                      c(uBrks[2:nBrks]), sep = "")    
#     legendLab = paste(c(uBrks[-nBrks]), c("", rep(" ~ < ", nBrks - 3), " ~ "),
#                       c("", uBrks[3:nBrks]), sep = "")
    ## ## Format the legend labels
    ## brkNames = formatC(uBrks, format = "fg")
    ## endVal = formatC(uBrks - endMar, format = "fg")
    ## legendLab = paste(c("", brkNames[-c(1, nBrks, nBrks + 1)], ""),
    ##     c(" < ", rep(" - ", nBrks - 2), " > "),
    ##     c(endVal[-c(1, nBrks + 1)], endVal[nBrks]), " (",
    ##     table(sub_data$fillColor), ")", sep = "")
  }
  
  if (!is.null(countryCodeTransp)) {
    final.df[final.df[, countryCode] %in% countryCodeTransp, "fillColor"] = 
      "transparent"
  }

  # Remove Antartica from maps
  #final.df <- final.df[final.df$FAO_CODE != 480,]


  print(head(final.df))
  print(summary(final.df))
  table(factor(final.df$fillColor))
  
  ## Plot the map
  map <- ggplot(data = final.df, aes(x = long, y = lat, group = group))
    
    #  ---- grid below the countries ------------------------
  map <- map + geom_path(data = grat_df_robin, 
              aes(long, lat, group = group, fill = NULL), 
              linetype = "solid", color = col.main2, size = 0.001)
    #  ---- rest of the content on top ----------------------
    map <- map +  geom_polygon(aes(fill = fillColor))
    map <- map +  geom_path(colour = alpha("white", 1/2),  size=.2)
    map <- map +  coord_equal()
    map <- map +  theme(legend.position = c(0.13,0.17), 
                          legend.justification=c(0,0),
                          legend.key.size=unit(8,'mm'),
                          legend.direction = "vertical",
                          legend.background=element_rect(colour=NA, fill=alpha("white", 2/3)),
                          legend.text=element_text(size=16), 
                          panel.background = element_blank(), 
                          panel.grid = element_blank(),
                          plot.background = element_blank(), 
                          axis.text = element_blank(), 
                          axis.ticks = element_blank(), 
                          legend.title = element_blank(), 
                          plot.margin = unit(c(-3,-1.5, -3, -1.5), "cm"))
    map <- map + xlab(NULL)
    map <- map + ylab(NULL)
    # map <- map + geom_polygon(data = missing , fill="grey70") # missing data on top
  if (!is.null(countryCodeTransp)) {
    map <- map + scale_fill_manual(labels = c(legendLab, ""),
                                  values = c(uCol, "transparent"),
                                  breaks = c(uVal, "transparent"))
  } else {
    map <- map + scale_fill_manual(labels = legendLab,
                                  values = uCol,
                                  breaks = uVal)
  }
  map  
}


utils::globalVariables(names = c("GAULspatialPolygon", "long", "lat", "group",
                                 "fillColor"))
