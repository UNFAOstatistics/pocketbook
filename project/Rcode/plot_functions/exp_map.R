##' Exports maps
##'
##' Function to export maps in pdf
##'
##' @param output Folder path where we want to save the map.
##' @param mapName Name of the map.
##' @param width Width of the map.
##' @param height Height of the map.

exp_map = function(outputPath, mapName, width = 7.5, height = 4, ...) {
  pdf(file = paste(outputPath, mapName , ".pdf", sep = ""), family = "PT Sans",
      width = width, height = height, title = NULL,
      pointsize = 10, colormodel = "cmyk", pagecentre = FALSE)
  print(eval(parse(text = mapName)))
  dev.off()
  cat("Map", mapName, "created\n")
}
