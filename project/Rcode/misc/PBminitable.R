PBminitable = function(output, MTname, table) {
  
  fileOut = paste(output, MTname, ".tex", sep = "")
  if(file.exists(fileOut)) file.remove(fileOut)
  file.create(fileOut)
  
  latexMinitable = matrix(as.character(table))
  cat("\\scriptsize\n", file = fileOut, append = TRUE)
  cat("\\begin{center}\n", file = fileOut, append = TRUE)
  cat("\\begin{tabular}{lrr}\n", file = fileOut, append = TRUE)
#   cat("\\begin{tabular}{p{1.4cm}p{0.7cm}p{0.7cm}}\n", file = fileOut, append = TRUE)
  cat("\\toprule\n", file = fileOut, append = TRUE)
  cat(paste(sanitizeToLatex(colnames(table[1])), 
            sanitizeToLatex(colnames(table[2])), 
            sanitizeToLatex(colnames(table[3])), sep = " & "), 
      file = fileOut, append = TRUE)
  cat("\\\\\n", file = fileOut, append = TRUE)
  cat("\\midrule\n", file = fileOut, append = TRUE)
  for (i in 1:nrow(table)) {
    cat(paste(sanitizeToLatex(table[i, 1]), 
              sanitizeToLatex(table[i, 2]), 
              sanitizeToLatex(table[i, 3]), sep = " & "), 
        file = fileOut, append = TRUE)
    cat("\\\\\n", file = fileOut, append = TRUE)
  }
  cat("\\toprule\n", file = fileOut, append = TRUE)
  cat("\\end{tabular}\n", file = fileOut, append = TRUE)
  cat("\\end{center}\n", file = fileOut, append = TRUE)
}
