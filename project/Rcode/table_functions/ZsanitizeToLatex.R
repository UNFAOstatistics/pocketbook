##' Sanitize the expression for Latex code
##'
##' The function sanitize an expression for the Latex code
##'
##' @param str A string to be sanitized
##' @param html  
##' @param type Specify whether the expression is for text or table
##' @export

sanitizeToLatex <- function(str, html=FALSE, type=c("text","table")) {

      type <- match.arg(type)

      result <- as.character(str)
      
      result <- gsub("\\\\-","TEX.BACKSLASH",result)
      result <- gsub("\\\\","SANITIZE.BACKSLASH",result)
      result <- gsub("$","\\$",result,fixed=TRUE)
      result <- gsub(">","$>$",result,fixed=TRUE)
      result <- gsub("<","$<$",result,fixed=TRUE)
      result <- gsub("|","$|$",result,fixed=TRUE)
      result <- gsub("{","\\{",result,fixed=TRUE)
      result <- gsub("}","\\}",result,fixed=TRUE)
      result <- gsub("%","\\%",result,fixed=TRUE)
      result <- gsub("&","\\&",result,fixed=TRUE)
      result <- gsub("_","\\_",result,fixed=TRUE)
      ## result <- gsub("_", "\\textsubscript", result, fixed = TRUE)
      result <- gsub("#","\\#",result,fixed=TRUE)
      result <- gsub("^", ifelse(type == "table", "\\verb|^|",
                                 "\\textsuperscript "), result,fixed = TRUE)
      result <- gsub("~","\\~{}",result,fixed=TRUE)
      result <- gsub("Ã´","\\^{o}",result,fixed=TRUE)
      result <- gsub("?","\\^{o}",result,fixed=TRUE)
      result <- gsub("Ã¢","\\^{a}",result,fixed=TRUE)
      result <- gsub("Ã¨","\\`{e}",result,fixed=TRUE)
      result <- gsub("?","\\`{e}",result,fixed=TRUE)
      result <- gsub("Ã©","\\'{e}",result,fixed=TRUE)
      result <- gsub("?","\\'{e}",result,fixed=TRUE)
      result <- gsub("?","\\'{o}",result,fixed=TRUE)
      result <- gsub("?","\\`{o}",result,fixed=TRUE)
      result <- gsub("?","\\'{i}",result,fixed=TRUE)
      result <- gsub("?","\\`{i}",result,fixed=TRUE)
      result <- gsub("?","\\'{I}",result,fixed=TRUE)
      result <- gsub("?","\\`{I}",result,fixed=TRUE)      
      result <- gsub("?","\\r{A}",result,fixed=TRUE)
      result <- gsub("?","\\c{c}",result,fixed=TRUE)
      result <- gsub("?","\\'{a}",result,fixed=TRUE)
      result <- gsub("?","\\`{a}",result,fixed=TRUE)
      result <- gsub("?","\\'{A}",result,fixed=TRUE)
      result <- gsub("?","\\`{A}",result,fixed=TRUE)
      result <- gsub("?","\\'{u}",result,fixed=TRUE)
      result <- gsub("?","\\`{u}",result,fixed=TRUE)
      result <- gsub("?","\\~{n}",result,fixed=TRUE)
      result <- gsub("SANITIZE.BACKSLASH","$\\backslash$",result,fixed=TRUE)
      result <- gsub("TEX.BACKSLASH","\\-",result,fixed=TRUE)
      if(html) {
        result <- gsub("( www.[0-9A-Za-z./\\-\\_]*)"," \\\\url{\\1}",result)
      	result <- gsub("(http://(www.)*[0-9A-Za-z./\\-\\_]*)","\\\\url{\\1}",result)
      	dotSlash<-grepl("\\url\\{.*\\.}",result)
      	result[dotSlash] <- gsub("\\.\\}","\\}\\.",result[dotSlash])
      }
      
      ## special expressions
      result <- gsub("km2", "km\\textsuperscript{2}", result, fixed = TRUE)
      result <- gsub("m3", "m\\textsuperscript{3}", result, fixed = TRUE)
      result <- gsub("CO2", "CO\\textsubscript{2}", result, fixed = TRUE)
      
      
      return(result)
    }
# 
# sanitizeIf <-function(x,...) {
#   if(grepl(TeXExpr, x)) {
#     return(as.character(x))
#   } else {
#     return(sanitize2(x,...))
#   }
# }
# 
# TeXExpr <-"\\$.*[_\\].*\\$|\\{.*\\}|\\[.*[_\\].*\\$|\\\\[.*\\\\]|\\url|~"