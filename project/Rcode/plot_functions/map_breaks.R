##' Function to create the discrete interval
##'
##' This only a wrapper function to round the numbers in the
##' middle. The classIntervals function is highly recommended. We only
##' wrote this because for reporting purposes.
##'
##' @export

map_breaks = function(value, n, style = "jenks"){
    brks = classIntervals(value, n = n, style = style)$brks
    midBrks = brks[-c(1, n + 1)]
    midBrks = signif(midBrks, ifelse(midBrks >= 1000, 3, 2))
    brks[-c(1, n + 1)] = midBrks
    brks
}
