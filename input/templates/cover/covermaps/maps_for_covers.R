#' # Regional maps for pocketbook cover
#' 
#' Markus Kainu
#'
#' Last updated: **`r Sys.time()`**

#+ setup, include = F
knitr::opts_chunk$set(list(echo=TRUE,
                           eval=TRUE,
                           cache=FALSE,
                           warning=FALSE,
                           message=FALSE))
library(knitr)
opts_chunk$set(fig.width = 6, fig.height = 5)

regional15_web <- TRUE
source("~/faosync/pocketbooks/pocketbook/run.R")
# libraries

#' Plot the maps
#+ plot


shape <- fao_world
shape <- spTransform(fao_world, CRS("+proj=robin"))
# Fortify the shape
shape$id <- rownames(shape@data)
map.points <- fortify(shape, region = "id")
map.df <- merge(map.points, shape, by = "id")
map.df$FAOST_CODE[map.df$FAOST_CODE %in% 41] <- 351
map.df <- map.df[-22:-72]
map.df <- left_join(map.df,region_key)
the_whole   <- map.df # for mapping the grey areas


for (region_to_report in c("RAP","RAF","REU","RNE")){
  
  df <- subgrouping(region_to_report = region_to_report, gather=FALSE)
  
  # merge data with the region info
  overview.df <- merge(map.df,df[c("FAOST_CODE","subgroup")],by="FAOST_CODE")
  overview.df <- arrange(overview.df, order)
  #overview.df <- merge(overview.df,FAOcountryProfile,by="FAOST_CODE")
  
  # define map unit
  map_unit <- ""
  
  # graticule
  grat_robin <- spTransform(graticule, CRS("+proj=robin"))  # reproject graticule
  # gr_rob <- fortify(grat_robin)
  
  # overview.df$group <- ifelse(!is.na(overview.df$subgroup), TRUE, FALSE)
  
  overview.df <- arrange(overview.df, order)
  # Create the plot
  p <- ggplot(data=overview.df, aes(long,lat,group=group))
  #  ---- grid below the countries ------------------------
  # p <- p + geom_path(data = gr_rob, aes(long, lat, group = group, fill = NULL),
  #                    linetype = "solid", color = "grey80", alpha=.5)
  # Grey for the non-data regions
  p <- p + geom_polygon(data=the_whole, fill="grey70", color=alpha("white", 1/2))
  p <- p + geom_polygon(aes(fill = subgroup), colour = alpha("white", 1/2), show.legend = FALSE)
  # p <- p + geom_polygon(data=the_whole, fill=NA, colour = alpha("white", 1/2), size=.4, show.legend = FALSE)
  # p <- p + geom_polygon(data=the_whole, fill=NA, colour = alpha("white", 1/2), size=.4, show.legend = FALSE)
  p <- p + scale_fill_manual(values=rep("Orange", 12))
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
  p <- p + guides(fill = guide_legend(title = map_unit, family="PT Sans",
                                      title.position = "top",
                                      title.hjust=0))
  # p <- p + coord_map("ortho", orientation=c(55, 37, 0))
  p <- p + guides(colour=FALSE)
  print(p)
  ggsave(paste0("covermap_",region_to_report,".pdf"), p, 
         width = 11.7, height = 8.27, useDingbats=FALSE)
  # ggsave()  
}

#' ## vector pdf's
#' 
#' - [Asia & Pacific](covermap_RAP.pdf)
#' - [Africa](covermap_RAF.pdf)
#' - [Europe & Central Asia](covermap_REU.pdf)
#' - [North Africa & Middle East](covermap_RNE.pdf)