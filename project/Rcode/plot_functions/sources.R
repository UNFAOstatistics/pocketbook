##' Create sources
##'
##' A function for creating sources
##'
##' @param dissemination The dissemination file.
##' @param metadata The metadata file.
##' @param objectName The OBJECT_NAME in the dissemination file.
##' @param manual Optional. Specify the source.
##' @param output The output path.
##' @return The source for the specified object.
##' @export

sources = function(dissemination = diss.df, metadata = meta.lst$FULL, 
                   construction = con.df, objectName, output, manual = NULL,
                   printAPI = FALSE, APIbase, APIbook, APIyears, APIvariables) {
  
  object = subset(dissemination, OBJECT_NAME == objectName)  
  fileOut = paste(output, "Source_", objectName, ".tex", sep = "")
  
  if(file.exists(fileOut)) file.remove(fileOut)
  file.create(fileOut)
  
  variables = object[, paste("DATA_KEY", 1:10, sep = "")]
  variables = variables[!is.na(variables)]
  
  sourceVect = NULL
  if (!is.null(manual)) {
    sourceVect = manual
  } else {
    for (i in variables) {
      objmeta = subset(metadata, STS_ID == i)
      if (is.na(objmeta[, "SOURCE"])) {
        stop("Please, specify the source.")
      } else {
        conVar = subset(construction, 
                        subset = STS_ID == i & 
                          CONSTRUCTION_TYPE == "share")
        if (nrow(conVar) == 1) {
          if (is.na(conVar[, "STS_ID_CONSTR2"])) {
            sourceVect[length(sourceVect)+1] = 
              subset(metadata, 
                     subset = STS_ID == conVar[, "STS_ID_CONSTR1"])[, "SOURCE"]
          } else {
            sourceVect[length(sourceVect)+1] = 
              subset(metadata, 
                     subset = STS_ID == conVar[, "STS_ID_CONSTR1"])[, "SOURCE"]            
            sourceVect[length(sourceVect)+1] = 
              subset(metadata, 
                     subset = STS_ID == conVar[, "STS_ID_CONSTR2"])[, "SOURCE"]
          }
        } else {
          sourceVect[length(sourceVect)+1] = objmeta[, "SOURCE"]
        }
      }
    }
  }
  
  # API ---------------------------------------------------------------------
  
  if (missing(APIbase)) {
    if (missing(APIbook)) {
      APIbase = "http://ldvapp07.fao.org:8032/faosyb/rest/get/2013/others/"
    } else {
      switch(APIbook,
             REU = {APIbase = "http://ldvapp07.fao.org:8032/faosybreu/rest/get/2013/others/"},
             RAF = {APIbase = "http://ldvapp07.fao.org:8032/faosybraf/rest/get/2013/others/"},
             RNE = {APIbase = "http://ldvapp07.fao.org:8032/faosybrne/rest/get/2013/others/"},
             LAC = {APIbase = "http://ldvapp07.fao.org:8032/faosyblac/rest/get/2013/others/"},
             RAP = {APIbase = "http://ldvapp07.fao.org:8032/faosybrap/rest/get/2013/others/"})
    }
  }
  
  if (missing(APIyears)) {
    API = paste(APIbase, "1990-2015/", sep = "")
  } else {
    API = paste(APIbase, APIyears, "/", sep = "")
  }
  
  if (missing(APIvariables)) {
    API = paste(API, paste(unique(variables), collapse = ","), sep = "")
  } else {
    API = paste(API, APIvariables, sep = "")
  }
  
  API = paste("\\ifprint\\else\\href{", API, 
              "}{\\includegraphics[width=0.3cm]{./Covers/csv_ICON.pdf}}\\fi", sep = "")
  
  # Output ------------------------------------------------------------------
  
  if (length(sourceVect) != 0) {
    if ("FAO, Statistics Division" %in% unique(sourceVect) &
        "FAO, Statistics Division (FAOSTAT)" %in% unique(sourceVect)) {
      sourceVect = subset(sourceVect, 
                          subset = sourceVect != "FAO, Statistics Division")
    }
    objsource = paste(unique(sourceVect), collapse = " and ")
    cat("\\footnotebar\n", file = fileOut, append = TRUE)
    if (length(objsource) == 1) {
      if (printAPI) {
        cat(paste("\\textit{Source}: ", 
                  sanitizeToLatex(objsource, type = "text"), ". ", API,
                  "\n\n", sep = ""),  file = fileOut, append = TRUE)
      } else {
        cat(paste("\\textit{Source}: ", 
                  sanitizeToLatex(objsource, type = "text"), ". ",
                  "\n\n", sep = ""),  file = fileOut, append = TRUE)
      }
    } else {
      if (printAPI) {
        cat(paste("\\textit{Sources}: ", 
                  sanitizeToLatex(objsource, type = "text"), ". ", API,
                  "\n\n", sep = ""),  file = fileOut, append = TRUE)
      } else {
        cat(paste("\\textit{Sources}: ", 
                  sanitizeToLatex(objsource, type = "text"), ". ",
                  "\n\n", sep = ""),  file = fileOut, append = TRUE)
      }
    }
  } else {
    cat("\\footnotebar\n", file = fileOut, append = TRUE)
    cat(paste("\\textit{Source}: ", ". ",
              "\n\n", sep = ""),  file = fileOut, append = TRUE)
  }
  
  if (!is.na(object[, "OBJECT_NOTE"])) {
    cat(sanitizeToLatex(object[, "OBJECT_NOTE"], type = "text"),  
        file = fileOut, append = TRUE)
  }
  
}