##' Create captions
##'
##' A function for creating captions
##'
##' @param dissemination The dissemination file.
##' @param objectName The OBJECT_NAME in the dissemination file.
##' @param output The output path.
##' @param manual Optional. Specify the caption.
##' @return The caption for the specified object.
##' @export

captions = function(dissemination = diss.df, objectName, output, manual = NULL) {
  
  object = subset(dissemination, OBJECT_NAME == objectName)
  fileOut = paste(output, "Caption_", objectName, ".tex", sep = "")
  
  if(file.exists(fileOut)) file.remove(fileOut)
  file.create(fileOut)
  
  if (!is.null(manual)) {
    caption = manual
  } else {
    if (is.na(object[, "OBJECT_DESCRIPTION"])) {
      caption = "To be written"
    } else {
      if (is.na(object[, "YEAR_START"]) | is.na(object[, "YEAR_END"])) {
        print("Year is missing. Impossible to create the caption.")
        caption = "To be written"
      } else {
        if (object[, "YEAR_START"] == object[, "YEAR_END"]) {
          year = object[, "YEAR_START"]
        } else {
          if (object[, "PLOT_OBJECTIVE"] %in% c("reg_uni_bar", "top_dot", "bot_dot", "top_bot_dot") &
                object[, "GROUP"] == "Year" & is.na(object[, "INTERVAL"])) {
            year = paste(object[, "YEAR_START"], object[, "YEAR_END"], sep = " and ")
          } else {
            year = paste(object[, "YEAR_START"], object[, "YEAR_END"], sep = " to ")
          }
        }
        if (object[, "OBJECT_TYPE"] == "CHART") {
          if (!is.na(object[, "INTERVAL"])) {
            caption = paste(object[, "OBJECT_DESCRIPTION"], 
                            " (", year, "*)", sep = "")
          } else {
            caption = paste(object[, "OBJECT_DESCRIPTION"], 
                            " (", year, ")", sep = "")
          }
        } else {
          ou = ifelse(!is.na(object[, "DISS_ORIG_UNIT"]) & object[, "DISS_ORIG_UNIT"] != "", object[, "DISS_ORIG_UNIT"], object[, "ORIG_UNIT"])
          if (!is.na(object[, "QUANTITY"]) & object[, "QUANTITY"] != "") {
            unit = paste(object[, "QUANTITY"], ou, sep = " ")
          } else {
            unit = ou
          }
          if (!is.na(object[, "INTERVAL"])) {
            caption = paste(object[, "OBJECT_DESCRIPTION"], 
                            " (", unit, ", ", year, "*)", sep = "")
          } else {
            caption = paste(object[, "OBJECT_DESCRIPTION"], 
                            " (", unit, ", ", year, ")", sep = "")
          }
        }
      }
    }
  }
  
#   cat("\\caption{", sanitizeToLatex(as.character(caption, type = "text")), 
#       "\\label{", objectName, "}}", file = fileOut, append = TRUE, sep = "")
  cat("\\caption{", sanitizeToLatex(as.character(caption, type = "text")), "}",
      file = fileOut, append = TRUE, sep = "")
}