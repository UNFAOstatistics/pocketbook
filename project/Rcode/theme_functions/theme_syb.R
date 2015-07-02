##' Load the pre-defined ggplot theme
##'
##' @export
##'


theme_syb = function(part){
    if(missing(part))
        part = 1

    main_col = plot_colors(part = part)[["Main"]][1]

    theme_update(
        ## Texts
        axis.text.x = element_text(family = "", face = "plain", colour = "black",
                                   size = 8, hjust = 1, vjust = 1, angle = 0,
                                   lineheight = 0.7),
        axis.text.y = element_text(family = "", face = "plain", colour = "black",
                                   size = 8, hjust = 1, vjust = 1, angle = 0,
                                   lineheight = 0.7),

        ## Label ticks
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(family = "", face = "plain", colour = "black",
                                    size = 8, hjust = 0.5, vjust = 0.2, angle = 90,
                                    lineheight = 0.7),

        ## Legend
        legend.background = element_blank(),
        legend.key = element_rect(fill = NA, colour = NA),
        legend.text = element_text(size = 8),
        legend.title = element_blank(),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.box = NULL,

        ## panel and background
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_line(colour = main_col, size = 0.5),
        panel.grid.minor = element_blank(),
        plot.background = element_blank(),
        plot.title =  element_blank()
        )
}
