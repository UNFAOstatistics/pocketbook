#' Function that create the latex spreads.


chart_spread <- function(title="title",
                       LeftTextCode="loremipsum",
                       RightTextCode="empty_minitable",
                       LeftChartCode="plot",
                       footnoteLeft = NULL,
                       footnoteRight  = NULL,
                       RightChartCode="plot",
                       BottomChartCode="plot",
                       MapCode="plot",
                       notes=""){
  
  if (exists("footnoteRight")) footnoteRightText <- paste0("\\footnotesize{",footnoteRight,"}  \n")
  if (!exists("footnoteRight")) footnoteRightText <- "\\vspace{-7pt} \n"
  
  if (exists("footnoteRight")) footnoteLeftText <- paste0("\\footnotesize{",footnoteLeft,"}  \n")
  if (!exists("footnoteRight")) footnoteLeftText <- "\\vspace{-7pt} \n"
  
  #cat(paste0("\\begin{ChartPage}{ \\Large{",title,"} } \n")) # This fixes the font size but messes the tOC font size
  cat(paste0("\\begin{ChartPage}{ ",title," } \n"))
  
  cat(paste0("\\LeftText{",LeftTextCode,"} \n"))
  

#   cat(paste0("\\RightText{\\IfFileExists{./Tables/",RightTextCode,".tex}  
# 	               {\\begin{table} \n
# 	               \\input{./Captions/Caption_",RightTextCode,".tex} \n
# 	               \\input{./Tables/",RightTextCode,".tex} \n
# 	               \\end{table}}} \n \n"))
#   
  cat(paste0("\\RightText{\\input{./Tables/",RightTextCode,".tex}} \n"))
  
   
	cat(paste0("\\LeftChart{\\begin{chart} \n 
	               \\input{./Captions/Caption_",LeftChartCode,".tex} \n",
	               "\\IfFileExists{./Plots/",LeftChartCode,".pdf}{\\includegraphics[width = 4.5cm, height = 7.5cm]{{./Plots/",LeftChartCode,"}.pdf}}{} \n",
	               "\\end{chart}} \n"))
	
	cat(paste0("\\RightChart{\\begin{chart} \n 
	               \\input{./Captions/Caption_",RightChartCode,".tex} \n",
	                footnoteRightText,
	               "\\IfFileExists{./Plots/",RightChartCode,".pdf}{\\includegraphics[width = 4.5cm, height = 7.5cm]{{./Plots/",RightChartCode,"}.pdf}}{} \n",
	               "\\end{chart}} \n"))

	
  cat(paste0("\\BottomChart{\\begin{chart} \n 
	               \\input{./Captions/Caption_",BottomChartCode,".tex} \n",
	           "\\IfFileExists{./Plots/",BottomChartCode,".pdf}{\\includegraphics[width = 8cm, height = 4cm]{{./Plots/",BottomChartCode,"}.pdf}}{} \n",
	           "\\end{chart}} \n"))

	
	cat("\\end{ChartPage}")
	
	cat("\\vspace{10 mm}")
  cat(paste0("\\textbf{",notes,"}"))
  
  cat(paste0("\\begin{figure} \n",
  "\\input{./Captions/Caption_",MapCode,".tex} \n", 
  "\\IfFileExists{./Maps/",MapCode,".pdf}{\\centering\\includegraphics[height = 1\\columnwidth, angle=90]{{./Maps/",MapCode,"}.pdf}\\par}{\\newpage\\thispagestyle{empty}\\mbox{}}{} \n",
  "\\end{figure}"
  ))
		
}


chart_spread2 <- function(title="title",
                         LeftTextCode="leftU",
                         RightTextCode="rightU",
                         LeftChartCode="plot",
                         footnoteLeft = NULL,
                         footnoteRight  = NULL,
                         RightChartCode="plot",
                         BottomChartCode="plot",
                         MapCode="plot",
                         notes=""){
  
  if (exists("footnoteRight")) footnoteRightText <- paste0("\\footnotesize{",footnoteRight,"}  \n")
  if (!exists("footnoteRight")) footnoteRightText <- "\\vspace{-7pt} \n"
  
  if (exists("footnoteRight")) footnoteLeftText <- paste0("\\footnotesize{",footnoteLeft,"}  \n")
  if (!exists("footnoteRight")) footnoteLeftText <- "\\vspace{-7pt} \n"
  
  #cat(paste0("\\begin{ChartPage}{ \\Large{",title,"} } \n")) # This fixes the font size but messes the tOC font size
  cat(paste0("\\begin{ChartPage}{ ",title," } \n"))
  
  cat(paste0("\\LeftText{",LeftTextCode,"} \n"))
  
  
#   cat(paste0("\\RightText{\\IfFileExists{./Tables/",RightTextCode,".tex}  
#              {\\begin{table} \n
#              \\input{./Captions/Caption_",RightTextCode,".tex} \n
#              \\input{./Tables/",RightTextCode,".tex} \n
#              \\end{table}}} \n \n"))
  
  # This is misleading as it is a chart instead we are calling for RightTextCode
  
  cat(paste0("\\RightText{\\begin{chart} \n 
             \\input{./Captions/Caption_",RightTextCode,".tex} \n",
             
             "\\IfFileExists{./Plots/",RightTextCode,".pdf}{\\includegraphics[width = 4cm, height = 3cm]{{./Plots/",RightTextCode,"}.pdf}}{} \n \n",
             footnoteLeftText,
             "\\end{chart}} \n"))
  
  
  
  cat(paste0("\\LeftChart{\\begin{chart} \n 
             \\input{./Captions/Caption_",LeftChartCode,".tex} \n",
             "\\IfFileExists{./Plots/",LeftChartCode,".pdf}{\\includegraphics[width = 4.5cm, height = 7.5cm]{{./Plots/",LeftChartCode,"}.pdf}}{} \n",
             "\\end{chart}} \n"))
  
  cat(paste0("\\RightChart{\\begin{chart} \n 
             \\input{./Captions/Caption_",RightChartCode,".tex} \n",
             footnoteRightText,
             "\\IfFileExists{./Plots/",RightChartCode,".pdf}{\\includegraphics[width = 4.5cm, height = 7.5cm]{{./Plots/",RightChartCode,"}.pdf}}{} \n",
             "\\end{chart}} \n"))
  
  
  cat(paste0("\\BottomChart{\\begin{chart} \n 
             \\input{./Captions/Caption_",BottomChartCode,".tex} \n",
             "\\IfFileExists{./Plots/",BottomChartCode,".pdf}{\\includegraphics[width = 8cm, height = 4cm]{{./Plots/",BottomChartCode,"}.pdf}}{} \n",
             "\\end{chart}} \n"))
  
  
  cat("\\end{ChartPage}")
  
  cat("\\vspace{10 mm}")
  cat(paste0("\\textbf{",notes,"}"))
  
  cat(paste0("\\begin{figure} \n",
             "\\input{./Captions/Caption_",MapCode,".tex} \n", 
             "\\IfFileExists{./Maps/",MapCode,".pdf}{\\centering\\includegraphics[height = 1\\columnwidth, angle=90]{{./Maps/",MapCode,"}.pdf}\\par}{\\newpage\\thispagestyle{empty}\\mbox{}}{} \n",
             "\\end{figure}"
  ))
  
  }