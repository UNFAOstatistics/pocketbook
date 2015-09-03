create_map_here <- function(manualPalette=FALSE,manual_palette=c("#a6611a","#dfc27d","#f5f5f5","#80cdc1","#018571")){
  
  if (!(manualPalette)) {
    colPart = plot_colors(part = syb_part, 12)
    mapColFun = colorRampPalette(c("white", colPart[["Main"]][1]))
    
    if ("No Data" %in% unique(cat_data$value_cat)) {
      nCol = length(levels(cat_data$value_cat)) -1
      tmpCol = mapColFun(nCol)[2]
      mapColFun = colorRampPalette(c(tmpCol, colPart[["Main"]][1]))
      mapColors = mapColFun(nCol)
      mapColors <- c("grey70", mapColors)
    } 
    if (!("No Data" %in% unique(cat_data$value_cat))){
      nCol = length(levels(cat_data$value_cat))
      tmpCol = mapColFun(nCol)[2]
      mapColFun = colorRampPalette(c(tmpCol, colPart[["Main"]][1]))
      mapColors = mapColFun(nCol)
    }
  }
  if (manualPalette) {
    #if ("No Data" %in% unique(cat_data$value_cat)) {
    #  mapColors <- c(manual_palette,"grey70")
    #} 
    #if (!("No Data" %in% unique(cat_data$value_cat))){
      mapColors <- manual_palette
#    }
  }
  
  
  
  
  # Create the plot
  p <- ggplot(data=map.plot, aes(long,lat,group=group))
  #  ---- grid below the countries ------------------------
  p <- p + geom_path(data = gr_rob, aes(long, lat, group = group, fill = NULL), 
                     linetype = "solid", color = col.main2, alpha=.1)
  # Grey for the non-data regions
  p <- p + geom_polygon(aes(fill = value_cat), colour = NA)
  p <- p + geom_polygon(fill=NA,colour = alpha("white", 1/2), size=.4, guide = FALSE)
  p <- p + theme(legend.position = c(0.05,0.05), 
                 legend.justification=c(0,0),
                 legend.key.size=unit(6,'mm'),
                 legend.direction = "vertical",
                 legend.background=element_rect(colour=NA, fill=alpha("white", 1/3)),
                 #legend.background=element_rect(colour=NA, fill=NA),
                 legend.text=element_text(size=12), 
                 legend.title=element_text(size=12), 
                 title=element_text(size=16), 
                 panel.background = element_blank(), 
                 plot.background = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.grid.major = element_blank(),
                 axis.text = element_blank(), 
                 axis.title = element_blank(), 
                 axis.ticks = element_blank())
  if (region_to_report == "REU") p <- p + coord_cartesian(xlim=c(-4290114,13198767,ylim=c(1965387,8184223)))
  if (region_to_report == "RNE") p <- p + coord_cartesian(xlim=c(-4078415,1783566,ylim=c(29258,6247852)))
  if (region_to_report == "LAC") p <- p + coord_cartesian(xlim=c(-14078415,-1078415,ylim=c(29258,0)))
  p <- p + guides(fill = guide_legend(title = map_unit, family="PT Sans",
                                      title.position = "top", 
                                      title.hjust=0))
  p <- p + scale_fill_manual(values=mapColors,labels = comma) # no data to think about
  p <- p + guides(colour=FALSE)
  p
}