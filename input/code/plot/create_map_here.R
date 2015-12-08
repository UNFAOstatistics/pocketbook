create_map_here <- function(manualPalette=FALSE,manual_palette=c("#a6611a","#dfc27d","#f5f5f5","#80cdc1","#018571")){
  
  if (!(manualPalette)) {
    colPart = plot_colors(part = syb_part, 6)
    mapColFun = colorRampPalette(c("white", colPart[["Main"]][1]))
    
    if ("No Data" %in% unique(cat_data$value_cat)) {
      nCol = length(levels(cat_data$value_cat)) -1
      tmpCol = mapColFun(nCol)[2]
      mapColFun = colorRampPalette(c(tmpCol, colPart[["Main"]][1]))
      mapColors = mapColFun(nCol)
      mapColors <- c("grey50", mapColors)
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
  
  grat_robin <- spTransform(graticule, CRS("+proj=robin"))  # reproject graticule
  gr_rob <- fortify(grat_robin)

    # crop the grid
  if (region_to_report %in% "REU") {
    gr_rob <- gr_rob[gr_rob$lat >= 3065387 & gr_rob$lat <= 8184223,]
    gr_rob <- gr_rob[gr_rob$long >= -3290114 & gr_rob$long <= 13198767,]
    
    map.plot <- map.plot[map.plot$lat >= 3065387 & map.plot$lat <= 8184223,]
    map.plot <- map.plot[map.plot$long >= -3290114 & map.plot$long < 13198767,]
  }

  if (region_to_report %in% "RAF") {
    gr_rob <- gr_rob[gr_rob$lat >= min(map.plot$lat) & gr_rob$lat <= max(map.plot$lat),]
    gr_rob <- gr_rob[gr_rob$long >= -1690114 & gr_rob$long < 4798767,]
    
    map.plot <- map.plot[map.plot$lat >= min(map.plot$lat) & map.plot$lat <= max(map.plot$lat),]
    map.plot <- map.plot[map.plot$long >= -1690114 & map.plot$long < 4798767,]
  }

  if (region_to_report %in% c("RAP")) { # these are values from RAF data as we want to show the outline of RAF map here
    gr_rob <- gr_rob[gr_rob$lat >= min(map.plot$lat) & gr_rob$lat <= max(map.plot$lat),]
    gr_rob <- gr_rob[gr_rob$long >= 4290114 & gr_rob$long < 18198767,]
    
    map.plot <- map.plot[map.plot$lat >= min(map.plot$lat) & map.plot$lat <= max(map.plot$lat),]
    map.plot <- map.plot[map.plot$long >= 3290114 & map.plot$long < 18198767,]
  }
  if (region_to_report %in% c("RNE")) { # these are values from RAF data as we want to show the outline of RAF map here
    gr_rob <- gr_rob[gr_rob$lat >= -3716253 & gr_rob$lat <= 3988108,]
    gr_rob <- gr_rob[gr_rob$long >= -1608244 & gr_rob$long <= 5955556,]

    map.plot <- map.plot[map.plot$lat >= min(map.plot$lat) & map.plot$lat <= max(map.plot$lat),]
    map.plot <- map.plot[map.plot$long >= -1608244 & map.plot$long < 5955556,]
    
  }
  

  # Create the plot
  p <- ggplot(data=map.plot, aes(long,lat,group=group))
  #  ---- grid below the countries ------------------------
  p <- p + geom_path(data = gr_rob, aes(long, lat, group = group, fill = NULL), 
                     # linetype = "solid", color = col.main2, alpha=.1)
                     linetype = "solid", color = "grey80", size = 0.5)
  # #  ---- whole region in grey  ------------------------
  if (region_to_report == "RNE") p <- p + geom_polygon(data=map.df[which(map.df[["RAF"]]),], fill = "grey95", colour = alpha("white", 1/2))
  #  ---- grid below the countries ------------------------
  p <- p + geom_polygon(aes(fill = value_cat), colour = NA)
  p <- p + geom_polygon(fill=NA,colour = alpha("white", 1/2), size=.4, guide = FALSE)
  if (region_to_report %in% c("COF","GLO")){
    p <- p + theme(legend.position = c(0.00,0.05), 
                   legend.justification=c(0,0),
                   legend.key.height=unit(8,'mm'),
                   legend.key.width=unit(5,'mm'),
                   legend.direction = "vertical",
                   legend.background=element_rect(colour=NA, fill=alpha("white", 2/3)),
                   #legend.background=element_rect(colour=NA, fill=NA),
                   legend.text=element_text(size=15), 
                   legend.title=element_text(size=14), 
                   title=element_text(size=17), 
                   panel.background = element_blank(), 
                   plot.background = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.grid.major = element_blank(),
                   axis.text = element_blank(), 
                   axis.title = element_blank(), 
                   axis.ticks = element_blank())
  }
  
  if (region_to_report %in% "RAP"){
    p <- p + theme(legend.position = c(0.05,0.05), 
                   legend.justification=c(0,0),
                   legend.key.height=unit(8,'mm'),
                   legend.key.width=unit(4.5,'mm'),
                   legend.direction = "vertical",
                   legend.background=element_rect(colour=NA, fill=alpha("white", 2/3)),
                   #legend.background=element_rect(colour=NA, fill=NA),
                   legend.text=element_text(size=15), 
                   legend.title=element_text(size=14), 
                   title=element_text(size=16), 
                   panel.background = element_blank(), 
                   plot.background = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.grid.major = element_blank(),
                   axis.text = element_blank(), 
                   axis.title = element_blank(), 
                   axis.ticks = element_blank())
  }
  
  
  if (region_to_report %in% c("REU")){
    p <- p + theme(legend.position = c(0.00,0.1), 
                   legend.justification=c(0,0),
                   legend.key.height=unit(8,'mm'),
                   legend.key.width=unit(4,'mm'),
                   legend.direction = "vertical",
                   legend.background=element_rect(colour=NA, fill=alpha("white", 2/3)),
                   legend.text=element_text(size=16), 
                   legend.title=element_text(size=15), 
                   title=element_text(size=17), 
                   panel.background = element_blank(), 
                   plot.background = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.grid.major = element_blank(),
                   axis.text = element_blank(), 
                   axis.title = element_blank(), 
                   axis.ticks = element_blank())
  }
  
  if (region_to_report %in% "RNE"){
    p <- p + theme(legend.position = c(0.03,0.05), 
                   legend.justification=c(0,0),
                   legend.key.height=unit(7,'mm'),
                   legend.key.width=unit(4,'mm'),
                   legend.direction = "vertical",
                   legend.background=element_rect(colour=NA, fill=alpha("white", 2/3)),
                   legend.text=element_text(size=14), 
                   legend.title=element_text(size=13), 
                   title=element_text(size=16), 
                   panel.background = element_blank(), 
                   plot.background = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.grid.major = element_blank(),
                   axis.text = element_blank(), 
                   axis.title = element_blank(), 
                   axis.ticks = element_blank())
  }
  
  if (region_to_report %in% "RAF"){
    p <- p + theme(legend.position = c(0.05,0.05), 
                   legend.justification=c(0,0),
                   legend.key.height=unit(8,'mm'),
                   legend.key.width=unit(4.5,'mm'),
                   legend.direction = "vertical",
                   legend.background=element_rect(colour=NA, fill=alpha("white", 2/3)),
                   legend.text=element_text(size=15), 
                   legend.title=element_text(size=14), 
                   title=element_text(size=16), 
                   panel.background = element_blank(), 
                   plot.background = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.grid.major = element_blank(),
                   axis.text = element_blank(), 
                   axis.title = element_blank(), 
                   axis.ticks = element_blank())
  }

  p <- p + guides(fill = guide_legend(title = map_unit, family="PT Sans",
                                      title.position = "top", 
                                      title.hjust=0))
  
    p <- p + scale_fill_manual(values=mapColors) # no data to think about
  p <- p + guides(colour=FALSE)
  p
}