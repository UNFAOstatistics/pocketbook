##' Plot information
##'
##' Returns the info needed to feed the plot_syb function
##'
##' @param dissemination The dissemination file.
##' @param plotName Name of the plot in the dissemination file.
##' @return Returns the info needed to feed the plot_syb function.
##' @export

plot_info = function(dissemination = diss.df,
                     metadata = meta.lst$FULL, 
                     plotName) {
  specPlot = subset(dissemination, subset = OBJECT_NAME == plotName)
  variables = specPlot[, paste("DATA_KEY", 1:10, sep = "")]
  variables = variables[!is.na(variables) & variables != ""]
  ## Y axis
  if (specPlot[, "PLOT_OBJECTIVE"] %in% c("top_20_bar", "bot_20_bar", "top_bot_bar",
                                          "top_dot", "bot_dot", "top_bot_dot")) {
    yAxis = specPlot[, "X_AXIS"]
  } else {
    yAxis = variables
  }
  ## X axis
  if (specPlot[, "PLOT_OBJECTIVE"] %in% c("top_20_bar", "bot_20_bar", "top_bot_bar",
                                          "top_dot", "bot_dot", "top_bot_dot")) {
    xAxis = variables
  } else {
    xAxis = specPlot[, "X_AXIS"]
  }  
  ## legend labels
  ## NOTE (FILIPPO): we could use the %in% function, but then we would have
  ## problems with order.
  legendLabels = character()
  for (i in variables) {
    legendLabels[length(legendLabels) + 1] = 
      subset(meta.lst$FULL, subset = STS_ID == i)[, "TITLE_STS_SHORT"]
  }
  ## years
  if (specPlot[, "PLOT_OBJECTIVE"] %in% c("reg_uni_bar", "multi_stack_bar","reg_uni_dot","multi_dodge_dot")) {
    yrs = unique(c(specPlot[, "YEAR_START"], specPlot[, "YEAR_END"]))
  } else {
    yrs = eval(parse(text = paste(specPlot[, "YEAR_START"], specPlot[, "YEAR_END"], sep = ":")))
  }
  ## area
  area = strsplit(specPlot[, "AREA"], "+ ")
  area = area[[1]][area[[1]] != "+"]
  ## Y_LAB
  if (!is.na(specPlot[, "Y_LAB"])) {
    yPlotLab = specPlot[, "Y_LAB"]
  } else {
    ou = ifelse(!is.na(specPlot[, "DISS_ORIG_UNIT"]) & 
                  specPlot[, "DISS_ORIG_UNIT"] != "", specPlot[, "DISS_ORIG_UNIT"], specPlot[, "ORIG_UNIT"])
    if (!is.na(specPlot[, "QUANTITY"])) {
      yPlotLab = paste(specPlot[, "QUANTITY"], ou, sep = " ")
    } else {
      yPlotLab = ou
    }
  }
  ## X_LAB
  if (specPlot[, "PLOT_OBJECTIVE"] %in% c("top_20_bar", "bot_20_bar", "top_bot_bar",
                                          "top_dot", "bot_dot", "top_bot_dot")) {
    xPlotLab = yPlotLab
    yPlotLab = NULL
  } else {
    xPlotLab = NULL
  }
  ## scaling
  scaling = 1/ifelse (!is.na(specPlot[, "QUANTITY"]) & specPlot[, "QUANTITY"] != "", translateUnit(specPlot[, "QUANTITY"]), 1)
  ## size
  size = specPlot[, "OBJECT_LAYOUT"]
  ## type of plot
  plotType = specPlot[, "PLOT_OBJECTIVE"]
  ## width
  plotWidth = switch(size, SQUARE = 3.23, RECTANGULAR = 7,
                     VERTICAL = 3.23, SMALL = 6.5*0.7)
  ## height
  plotHeight = switch(size, SQUARE = 4,RECTANGULAR = 4,
                      VERTICAL = 8, SMALL = 3.3*0.7)
  ## group
  group = ifelse(!is.na(specPlot[, "GROUP"]), specPlot[, "GROUP"], NA)

  ## part
  plotPart = specPlot[, "PART"]
  ## return
  pltInfo = list(plotName = plotName, plotType = plotType, group = group,
                 xAxis = xAxis, yAxis = yAxis, plotArea = area, 
                 yPlotLab = yPlotLab, xPlotLab = xPlotLab, scaling = scaling,
                 plotWidth = plotWidth, plotHeight = plotHeight, 
                 plotYears = yrs, plotPart = plotPart, legendLabels = legendLabels)
}
