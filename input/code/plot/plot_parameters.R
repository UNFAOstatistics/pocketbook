top_right_plot_height <- 2.5
top_right_plot_width  <- 3

left_plot_height  <- 6
left_plot_width  <- 3.2

right_plot_height <- 6
right_plot_width <- 3.2

bottom_plot_height <- 3
bottom_plot_width  <- 6

# defaults
map.fig.width   <- NULL
map.fig.height  <- NULL
map.out.height  <- NULL
map.out.width   <- NULL
map.out.extra   <- NULL


# RAF
if (region_to_report == "RAF") map.fig.width  <- 7
if (region_to_report == "RAF") map.fig.height  <- 9
if (region_to_report == "RAF") map.out.height  <- "1.35\\columnwidth"
if (region_to_report == "RAF") map.out.extra  <- 'angle=0'

# RAP
if (region_to_report == "RAP") map.fig.width  <- 7
if (region_to_report == "RAP") map.fig.height  <- 9
if (region_to_report == "RAP") map.out.width  <- "1.05\\columnwidth"
if (region_to_report == "RAP") map.out.extra  <- 'angle=0'

# REU
if (region_to_report == "REU") map.fig.width  <- 13
if (region_to_report == "REU") map.fig.height  <- 7
if (region_to_report == "REU") map.out.width  <- "2.0\\columnwidth"
if (region_to_report == "REU") map.out.extra  <- 'angle=90'

# RNE
if (region_to_report == "RNE") map.fig.width  <- 6
if (region_to_report == "RNE") map.fig.height  <- 8
if (region_to_report == "RNE") map.out.height  <- "1.4\\columnwidth"
if (region_to_report == "RNE") map.out.extra  <- 'angle=0'

# LAC
if (region_to_report == "LAC") map.fig.width  <- 8
if (region_to_report == "LAC") map.fig.height  <- 12
if (region_to_report == "LAC") map.out.width  <- "1.2\\columnwidth"
if (region_to_report == "LAC") map.out.extra  <- 'angle=0'


# GLO
if (region_to_report == "GLO") map.fig.width  <- 12
if (region_to_report == "GLO") map.fig.height  <- 8
if (region_to_report == "GLO") map.out.width  <- "1.62\\columnwidth"
if (region_to_report == "GLO") map.out.extra  <- 'angle=90'

# COF
if (region_to_report == "COF") map.fig.width  <- 12
if (region_to_report == "COF") map.fig.height  <- 8
if (region_to_report == "COF") map.out.width  <- "1.7\\columnwidth"
if (region_to_report == "COF") map.out.extra  <- 'angle=90'
