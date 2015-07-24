##' Pre-defined color for the statistical year book
##'
##' @param part Which color pallete to use
##' @export
##'
##'

plot_colors = function(part = 1, n = 5){
    col_list = list()
    col_list[[1]] = switch(part,
           rgb(17, 104, 132, alpha = c(1, 0.2, 0) * 255, maxColorValue = 255),
           rgb(255, 92, 100, alpha = c(1, 0.2, 0) * 255, maxColorValue = 255),
           rgb(130, 40, 46,  alpha = c(1, 0.2, 0) * 255, maxColorValue = 255),
           rgb(53, 118, 53,  alpha = c(1, 0.2, 0) * 255, maxColorValue = 255),
           rgb(111, 78, 55,  alpha = c(1, 0.2, 0) * 255, maxColorValue = 255), # Coffee
           rgb(166, 123, 91, alpha = c(1, 0.2, 0) * 255, maxColorValue = 255), # Cafe au Lait
           rgb(75, 54, 33,   alpha = c(1, 0.2, 0) * 255, maxColorValue = 255) # Coffee
           )
    col_list[[2]] = switch(n,
            rgb(68,119,170, maxColorValue = 255),
            rgb(c(68, 204),
                c(119, 102),
                c(170, 119),
                maxColorValue = 255),
            rgb(c(68, 221, 204),
                c(119, 204, 102),
                c(170, 119, 119),
                maxColorValue = 255),
            rgb(c(68, 17, 221, 204),
                c(119, 119, 204, 102),
                c(170, 51, 119, 119),
                maxColorValue = 255),
            rgb(c(51, 136, 17, 221, 204),
                c(34, 204, 119, 204, 102),
                c(136, 238, 51, 119, 119),
                maxColorValue = 255),
            rgb(c(51, 136, 17, 221, 204, 170),
                c(34, 204, 119, 204, 102, 68),
                c(136, 238, 51, 119, 119, 153),
                maxColorValue = 255),
            rgb(c(51, 136, 68, 17, 221, 204, 170),
                c(34, 204, 170, 119, 204, 102, 68),
                c(136, 238, 153, 51, 119, 119, 153),
                maxColorValue = 255),
            rgb(c(51, 136, 68, 17, 153, 221, 204, 170),
                c(34, 204, 170, 119, 153, 204, 102, 68),
                c(136, 238, 153, 51, 51, 119, 119, 153),
                maxColorValue = 255),
            rgb(c(51, 136, 68, 17, 153, 221, 204, 136, 170),
                c(34, 204, 170, 119, 153, 204, 102, 34, 68),
                c(136, 238, 153, 51, 51, 119, 119, 85, 153),
                maxColorValue = 255),
            rgb(c(51, 136, 68, 17, 153, 221, 102, 204, 136, 170),
                c(34, 204, 170, 119, 153, 204, 17, 102, 34, 68),
                c(136, 238, 153, 51, 51, 119, 0, 119, 85, 153),
                maxColorValue = 255),
            rgb(c(51, 102, 136, 68, 17, 153, 221, 102, 204, 136, 170),
                c(34, 153, 204, 170, 119, 153, 204, 17, 102, 34, 68),
                c(136, 204, 238, 153, 51, 51, 119, 0, 119, 85, 153),
                maxColorValue = 255),
            rgb(c(51, 102, 136, 68, 17, 153, 221, 102, 204, 170, 136, 170),
                c(34, 153, 204, 170, 119, 153, 204, 17, 102, 68, 34, 68),
                c(136, 204, 238, 153, 51, 51, 119, 0, 119, 102, 85, 153),
                maxColorValue = 255)
            )
    names(col_list) = c("Main", "Sub")
    col_list
}


