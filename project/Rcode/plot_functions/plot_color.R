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
           rgb(130, 40, 46, alpha = c(1, 0.2, 0) * 255, maxColorValue = 255),
           rgb(53, 118, 53, alpha = c(1, 0.2, 0) * 255, maxColorValue = 255)
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






## plot_colors = function (part = "1"){
##   col.main1 = switch(part,
##                       "1" = rgb(17,104,132, maxColorValue = 255),
##                       "2" = rgb(255,92,100, maxColorValue = 255),
##                       "3" = rgb(130,40,46, maxColorValue = 255),
##                       "4" = rgb(53,118,53, maxColorValue = 255))
##   col.main2 = switch(part,
##                       "1" = rgb(17,104,132, maxColorValue = 255,
##                       alpha = 0.2*255),
##                       "2" = rgb(255,92,100, maxColorValue = 255,
##                       alpha = 0.2*255),
##                       "3" = rgb(130,40,46, maxColorValue = 255,
##                       alpha = 0.2*255),
##                       "4" = rgb(53,118,53, maxColorValue = 255,
##                       alpha = 0.2*255))
##   col.main3 = switch(part,
##                       "1" = rgb(17,104,132, maxColorValue = 255, alpha = 0),
##                       "2" = rgb(255,92,100, maxColorValue = 255, alpha = 0),
##                       "3" = rgb(130,40,46, maxColorValue = 255, alpha = 0),
##                       "4" = rgb(53,118,53, maxColorValue = 255, alpha = 0))
##   col1 <- rgb(68,119,170, maxColorValue = 255)
##   col2 <- rgb(136,204,238, maxColorValue = 255)
##   col3 <- rgb(68,170,153, maxColorValue = 255)
##   col4 <- rgb(17,119,51, maxColorValue = 255)
##   col5 <- rgb(153,153,51, maxColorValue = 255)
##   col6 <- rgb(221,204,119, maxColorValue = 255)
##   col7 <- rgb(102,17,0, maxColorValue = 255)
##   col8 <- rgb(204,102,119, maxColorValue = 255)
##   col9 <- rgb(136,34,85, maxColorValue = 255)
##   col10 <- rgb(170,68,153, maxColorValue = 255)
##   assign("col.main1", col.main1, envir = .GlobalEnv)
##   assign("col.main2", col.main2, envir = .GlobalEnv)
##   assign("col.main3", col.main3, envir = .GlobalEnv)
##   assign("col1",col1, envir = .GlobalEnv)
##   assign("col2",col2, envir = .GlobalEnv)
##   assign("col3",col3, envir = .GlobalEnv)
##   assign("col4",col4, envir = .GlobalEnv)
##   assign("col5",col5, envir = .GlobalEnv)
##   assign("col6",col6, envir = .GlobalEnv)
##   assign("col7",col7, envir = .GlobalEnv)
##   assign("col8",col8, envir = .GlobalEnv)
##   assign("col9",col9, envir = .GlobalEnv)
##   assign("col10",col10, envir = .GlobalEnv)
## }
