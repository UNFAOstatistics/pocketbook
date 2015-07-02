
SYBxtable = function(output, tableName, tableTitle, data, subHeaders, subHeaders2, digitsx = NULL, 
                     years, units, tableBreaks = NULL, api = NULL, top50, groupEmph){
  
  require(xtable)
  ## output file
  outputFile = paste(tablesOutput, tableName, ".tex", sep = "")
  
  ############################################################################
  ## main table
  ############################################################################
  ## defines the main structure of the table using the package xtable
  
  xT = xtable(data)
  
  ############################################################################
  ## caption
  ############################################################################
  ## defines the caption of the table using the package xtable
  
  caption(xT) = paste(tableTitle, "\\label{", tableName,"}", sep = "")
  
  ############################################################################
  ## digits
  ############################################################################
  ## defines the digits of each single column of the table using the package
  ## xtable. the first two values in the input vector for the function digits
  ## have to be equal to zero because the first one corresponds to the "digit
  ## of the row names" and the second one to the "digits of the countries column"
  
  if (is.null(digitsx)) {
    ## ...by default equal to one
    digitsx = rep(1, ncol(data))
    ## ...but if the variable is integer, then set equal to zero
    ## strange code to check whether the values are integers
    ## NOTE (Filippo): changed
    #   checkInteger <- sapply(x[!x[, 1] %in% c(boldGroups,"China", "EU", 
    #                                           "European Union"), -1], 
    #                          function(x) isTRUE(all.equal(na.omit(x/round(x)), rep(1, length(na.omit(x/round(x)))), check.attributes = FALSE)))
    #   digitsx[c(TRUE, checkInteger)] <- 0
    checkInteger = sapply(data, is.integer)
    checkInteger[1] = TRUE
    digitsx[checkInteger] = 0
    ## ...and if thousands, millions, billions, trillions, and quadrillions,
    ## then equal to zero as well
    qntCol = grep("thousand|million|billion|trillion|quadrillion|years", units)
    if(length(qntCol) != 0) {
      digitsx[qntCol] = 0
    }
    digits(xT) = c(0, digitsx)
  } else {
    digits(xT) = c(0, digitsx)
  }
  
  ############################################################################
  ## alignement
  ############################################################################
  ## defines the alignement of each single column using the package xtable
  
  ## for each column finding the width of the max integer
  alignWidth = sapply(data[, -1], function(x) ceiling(log10(max(c(10, abs(x)), 
                                                                na.rm = TRUE))))
  if(any(alignWidth == Inf)){
    warning("Some column might contain Inf values. Breaks currently")
    alignWidth = sapply(data[, -1], function(x) ceiling(log10(max(c(10, abs(x[x != Inf])), 
                                                                  na.rm = TRUE))))
  }
  
  align(xT) = c("r", ## rownames align
                "p{3cm}<{\\raggedright}", ## countries align
                paste("d{", alignWidth, ".", digitsx[-1], "}", sep = "")) ## values align
  
  ############################################################################
  ## colnames
  ############################################################################
  ## define the latex code to be used to format the header
  
  ## api & web links
  if (!is.null(api)) {
    api = paste(api, tableName, sep = "")
    ifWeb = paste("\\ifprint\\else \\href{", api, 
                  "}{\\includegraphics[width=0.3cm]{./Covers/csv_ICON.pdf}}\\fi\n", 
                  sep = "")
    myCols = paste("\\rowcolor{@tableheadcolor}",  ifWeb, "&\n", sep = "")
    mySubs = "\\rowcolor{@tableheadcolor} &\n"
  } else {
    myCols = "\\rowcolor{@tableheadcolor} &\n"
    mySubs = "\\rowcolor{@tableheadcolor} &\n"
  }
  mySubs2 = "\\rowcolor{@tableheadcolor} &\n"
  #################################
  ## columns enviros in the header
  #################################
  ## decides the structure of the first two line in the header
  
  cols = colnames(data)
  
  ## starting from 3 because 1 is the rownames and 2 is the country col
  count = 1
  countSubs = 1
  countSubs2 = 1 
  subs = subHeaders
  subs2 = subHeaders2
  
  for (i in 3:length(cols)) {
    if (cols[i] == cols[i-1]) {
      count = count + 1
      close = FALSE
      if (subs[i] == subs [i-1]) {
        countSubs = countSubs + 1
        closeSubs = FALSE
        if (subs2[i] == subs2 [i-1]) {
          countSubs2 = countSubs2 + 1
          closeSubs2 = FALSE
        } else {
          closeSubs2 = TRUE
        }
      } else {
        closeSubs = TRUE
        closeSubs2 = TRUE
      }
    } else {
      close = TRUE
      closeSubs = TRUE
      closeSubs2 = TRUE
    }
    
    ## instructions for last one
    if (i == length(cols)) {
      close = TRUE
      closeSubs = TRUE
      closeSubs2 = TRUE
      closeInterm = TRUE
      closeFinal = TRUE
    } else {
      if (subs[i] == subs[i + 1]) {
        closeInterm = FALSE
      } else {
        closeInterm = TRUE
      }
      closeFinal = FALSE
    }
    
    ## breaking line loop: close the multicolumn after one or n 
    ## HEADER
    if (close) {
      ## break in the first line
      if (!closeFinal) {
        ## ...but it is not the final break
        myCols = paste(myCols, 
                       "\\multicolumn{", count, "}{",
                       if (count == 1) "C{1}" else "H",
                       "}{\\color{white}", 
                       sanitizeToLatex(str = cols[i-1], type = "table"), 
                       "} &\n", sep = "")
      } else {
        ## ...and it is the final break
        if (cols[i] == cols[i-1]) {
          ## last and second last are the same, just one command for both
          myCols = paste(myCols, 
                         "\\multicolumn{", count, "}{",
                         if (count == 1) "C{1}" else "H",
                         "}{\\color{white}", 
                         sanitizeToLatex (str = cols[i-1], type = "table"), 
                         "} \\\\[-0.1ex]\n", sep = "")
        } else {
          ## last and second last different, one command each
          myCols = paste(myCols, 
                         "\\multicolumn{", count, "}{",
                         if (count == 1) "C{1}" else "H",
                         "}{\\color{white}", 
                         sanitizeToLatex (str = cols[i-1], type = "table"), 
                         "} &\n", sep = "")
          count = 1
          myCols = paste(myCols, 
                         "\\multicolumn{", count, "}{",
                         if (count == 1) "C{1}" else "H",
                         "}{\\color{white}", 
                         sanitizeToLatex (str = cols[i], type = "table"), 
                         "}  \\\\[-0.1ex]\n" , sep = "")
        }
      }
      count = 1
    } 
    
    ## SUB-HEADER
    if(closeSubs){
      ## break in the second line
      if(!closeFinal){
        ## ...but it is not the final break
        mySubs = paste(mySubs, 
                       "\\multicolumn{", countSubs, "}{",
                       if (countSubs == 1) "C{1}" else "H",
                       "}{\\color{white}", 
                       sanitizeToLatex(str = subs[i-1], type = "table"),
                       "} &\n", sep = "")
      } else {
        ## ...and it is the final break
        if (subs[i] == subs[i-1] & cols[i] == cols[i-1]) {
          ## last and second last are the same for both first and second line, 
          ## just one command for both
          mySubs = paste(mySubs, 
                         "\\multicolumn{", countSubs, "}{",
                         if (countSubs == 1) "C{1}" else "H",
                         "}{\\color{white}", 
                         sanitizeToLatex (str = subs[i-1], type = "table"), 
                         "} \\\\[-0.1ex]\n", sep = "")
        } else { 
          ## last and second last different at least in one between first 
          ## and second line, one command each     
          mySubs = paste(mySubs, 
                         "\\multicolumn{", countSubs, "}{",
                         if (countSubs == 1) "C{1}" else "H",
                         "}{\\color{white}", 
                         sanitizeToLatex (str = subs[i-1], type = "table"), 
                         "} &\n", sep = "")
          countSubs = 1
          mySubs = paste(mySubs, 
                         "\\multicolumn{", countSubs, "}{",
                         if (countSubs == 1) "C{1}" else "H",
                         "}{\\color{white}", 
                         sanitizeToLatex (str = subs[i], type = "table"), 
                         "}  \\\\[-0.1ex]\n" , sep = "")
        }
      }
      countSubs = 1
    } 
    
    ## SUB-HEADER2
    if(closeSubs2){
      ## break in the second line
      if(!closeFinal){
        ## ...but it is not the final break
        mySubs2 = paste(mySubs2, 
                        "\\multicolumn{", countSubs2, "}{",
                        if (countSubs2 == 1) "C{1}" else "H",
                        "}{\\color{white}", 
                        sanitizeToLatex(str = subs2[i-1], type = "table"),
                        "} &\n", sep = "")
      } else {
        ## ...and it is the final break
        if (cols[i] == cols[i-1] & subs2[i] == subs2[i-1] & 
            subs[i] == subs[i-1]) {
          ## last and second last are the same for both first and second line, 
          ## just one command for both
          mySubs2 = paste(mySubs2, 
                          "\\multicolumn{", countSubs2, "}{",
                          if (countSubs2 == 1) "C{1}" else "H",
                          "}{\\color{white}", 
                          sanitizeToLatex (str = subs2[i-1], type = "table"), 
                          "} \\\\[-0.1ex]\n", sep = "")
        } else { 
          ## last and second last different at least in one between first 
          ## and second line, one command each     
          mySubs2 = paste(mySubs2, 
                          "\\multicolumn{", countSubs2, "}{",
                          if (countSubs2 == 1) "C{1}" else "H",
                          "}{\\color{white}", 
                          sanitizeToLatex (str = subs2[i-1], type = "table"), 
                          "} &\n", sep = "")
          countSubs2 = 1
          mySubs2 = paste(mySubs2, 
                          "\\multicolumn{", countSubs2, "}{",
                          if (countSubs2 == 1) "C{1}" else "H",
                          "}{\\color{white}", 
                          sanitizeToLatex (str = subs2[i], type = "table"), 
                          "}  \\\\[-0.1ex]\n" , sep = "")
        }

    }
      countSubs2 = 1
    }
  }
  
  #############################
  ## horizontal lines in header
  #############################
  ## decides when the horizontal lines in the header should be cut
  
  ## first command to be used for header and sub-header
  first = "\\hhline{%\n\t>{\\arrayrulecolor{@tableheadcolor}}-" ## % removed
  firstSub = "\\hhline{%\n\t>{\\arrayrulecolor{@tableheadcolor}}-" ## % removed
  
  for (i in 2:length(cols)) {
    if (cols[i] == cols[i-1]) {
      new <- "-" 
      if (subs[i] == subs[i-1]) {
        newSub <- "-" 
      } else {
        newSub <- "%\n\t>{\\arrayrulecolor{@tableheadcolor}}|>{\\arrayrulecolor{white}}-"
      }
    } else {
      new <-  "%\n\t>{\\arrayrulecolor{@tableheadcolor}}|>{\\arrayrulecolor{white}}-"
      newSub <-  "%\n\t>{\\arrayrulecolor{@tableheadcolor}}|>{\\arrayrulecolor{white}}-"
    }
    first <- paste(first, new, sep = "")
    firstSub <- paste(firstSub, newSub, sep = "")
  }
  
  hhlines <- paste(first, 
                   "%\n\t>{\\arrayrulecolor{@tableheadcolor}}|%\n}", 
                   sep = "")
  hhlinesSub <- paste(firstSub, 
                      "%\n\t>{\\arrayrulecolor{@tableheadcolor}}|%\n}", 
                      sep = "")
  
  ############################################################################
  ## units
  ############################################################################
  ## defines the latex code to be used in order to input the units in the 
  ## header
  
  units <- sanitizeToLatex(str = units, html = FALSE, type = "table")
  unitsFinal <- ""
  for (unitIndex in 1:length(units)) {
    if (unitIndex == 1) {
      unitsFinal <- paste(unitsFinal, 
                          "\\rowcolor{@tableheadcolor}", 
                          sep = "")
    } else if (nchar(units[unitIndex]) >= 9) {
      unitsFinal <- paste(unitsFinal, 
                          " &\n ",
                          "\\multicolumn{1}{C{1}}{\\color{white}", 
                          units[unitIndex], "}", sep = "")
    } else {
      unitsFinal <- paste(unitsFinal, 
                          " &\n ",                          
                          "\\multicolumn{1}{H}{\\color{white}", 
                          units[unitIndex], "}", sep = "")
    }
    
  }
  units <- paste(unitsFinal, "\\\\ [-0.1ex]", sep = "")
  
  #############################
  ## years in header
  #############################
  ## defines the structure of years in the header
  
  years <- sanitizeToLatex(str = years, html = FALSE, type = "table")
  yearsFinal <- ""
  for (yrIndex in 1:length(years)) {
    if (yrIndex == 1) {
      yearsFinal <- paste(yearsFinal, 
                          "\\rowcolor{@tableheadcolor}", 
                          sep = "")
    } else if (nchar(years[yrIndex]) == 0 ) {
      yearsFinal <- paste(yearsFinal, 
                          " &\n ",
                          "\\multicolumn{1}{H}{", 
                          years[yrIndex], "}", sep = "")        
    } else if (nchar(years[yrIndex]) >= 9) {
      yearsFinal <- paste(yearsFinal, 
                          " &\n ",
                          "\\multicolumn{1}{P{1.5cm}}{\\color{white}", 
                          years[yrIndex], "}", sep = "")
    } else {
      yearsFinal <- paste(yearsFinal, 
                          " &\n ",                          
                          "\\multicolumn{1}{H}{\\color{white}", 
                          years[yrIndex], "}", sep = "")
    }
    
  }
  years <- paste(yearsFinal, "\\\\ [-0.1ex]", sep = "")
  
  ############################################################################
  ### summarizing headers
  ############################################################################
  longTableHeaders <- paste("\n\\toprule", 
                            "\n%first line header names\n", myCols, 
                            "\n%first line header horizontal lines\n", hhlines,
                            "\n%second line header names\n", mySubs,
                            "\n%second line header horizontal lines\n", hhlinesSub,
#                             "\n%third line header names\n", sh2, 
                            "\n%third line header names\n", mySubs2,
                            "\n%units in header \n", units, 
                            "\n%years in header\n", years, 
                            "\n\\midrule", sep = "")
  
  header <- c(paste(longTableHeaders, "\n\\endfirsthead\n", sep = ""), 
              paste("\\caption[]{", tableTitle , " (continued)}\\\\\n ", 
                    longTableHeaders, "\n\\endhead\n  \\bottomrule\n\\endfoot\n",
                    sep = ""))
  
  ############################################################################
  ### top 50 option
  ############################################################################
  
  ### break pages:
  if (top50) {
    country1Pos = NULL
    country2Pos = NULL
    country3Pos = NULL
    add1 = NULL
    add2 = NULL
    add3 = NULL
  } else {
    if (!is.null(tableBreaks)) {
      country1Pos = which(data[, 1] == tableBreaks[1])
      country2Pos = which(data[, 1] == tableBreaks[2])                  
      country3Pos = which(data[, 1] == tableBreaks[3])
      add1 = "\\pagebreak\n  "
      add2 = "\\pagebreak\n  "
      add3 = "\\pagebreak\n  "
    } else {
      country1Pos = NULL
      country2Pos = NULL
      country3Pos = NULL
      add1 = NULL
      add2 = NULL
      add3 = NULL
    }
  }

  header = c(header, add1, add2, add3)
  posList = list(-1, 0)
  if(!is.null(country1Pos)) posList[[3]] = country1Pos
  if(!is.null(country2Pos)) posList[[length(posList)+1]] = country2Pos
  if(!is.null(country3Pos)) posList[[length(posList)+1]] = country3Pos
  
  adds = list(pos = posList, command = header)
  
  ############################################################################
  ### emphasize groups
  ############################################################################
  
  xT2 = xT
  xT2[, 1] = sanitizeToLatex(as.character(xT2[, 1]))
  inGroup = xT2[, 1] %in% groupEmph
  xT2[inGroup, 1] = apply(xT2[inGroup, ], 2, function(x) paste("\\tablemph{\\bf{", x, "}}", sep = ""))[, 1]
  textIndent = "   \\hspace{2pt}\\hangindent=4pt\\relax "
  xT2[!inGroup,1] = apply(xT2[!inGroup,], 2, function(x) paste(textIndent, x, sep = ""))[, 1]
    
  ############################################################################
  ### Final step: print everything
  ############################################################################
  
#   indexes = which(xT2 == 0.0, arr.ind = TRUE)
#   for (ind in 1:nrow(indexes)) {
#     xT2[indexes[ind,1], indexes[ind,2]] = "<0.1"
#   }
  print(x = xT2, 
        tabular.environment = 'longtable',
        floating = FALSE, 
        file = outputFile,
        caption.placement = "top",  
        hline.after = NULL, 
        add.to.row = adds, 
        include.rownames = FALSE, 
        include.colnames = FALSE,
        sanitize.text.function = I,
        sanitize.colnames.function = NULL, 
        sanitize.rownames.function = NULL,
        append = FALSE, 
        format.args = list(big.mark = "\\\\,"))
  cat("printed table: ", tableName, " in ", outputFile, "\n")
  
}