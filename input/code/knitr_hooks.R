library(knitr)

# pdf.options(useDingbats = TRUE)
pdf.options(encoding='Cyrillic')

knit_hooks$set(chunk = function(x, options) x) # do not wrap output in kframe

syb_pdf = function(file, width, height) {
  pdf(file, width = width, height = height, pointsize = 10, family = "PT Sans")
}


opts_chunk$set(list(echo=FALSE,
                    eval=TRUE,
                    cache=FALSE,
                    warning=FALSE,
                    message=FALSE,
                    cache.path="~/fao_cache/regional15/",
                     dev="syb_pdf",
                     fig.ext='pdf',
                    results='asis')
               )

# Why we have to short_text_ChartPage and short_text_ChartPage2 hooks etc......

knit_hooks$set(short_text_ChartPage = function(before, options, envir) {

  # challenge with hooks was that when eval=FALSE hook was still executed and produced error
  # Added the following line for each hook that stops the function IF there is FALSE in eval either
  # written directly or passed through the spread condition region gadget thing..

  if (options$short_text_ChartPage == FALSE) return()
  # Set fig size
  fig.width  =  3
  fig.height =  10
     if (!before) {
       return(paste0("\\begin{ChartPage}{ ",spread_title," } \n",
               "\\LeftText{",short_text,"} \n"))
     }
})

knit_hooks$set(short_text_ChartPage2 = function(before, options, envir) {

  # challenge with hooks was that when eval=FALSE hook was still executed and produced error
  # Added the following line for each hook that stops the function IF there is FALSE in eval either
  # written directly or passed through the spread condition region gadget thing..

  if (options$short_text_ChartPage2 == FALSE) return()
  # Set fig size
  fig.width  =  3
  fig.height =  10
     if (!before) {
       return(paste0("\\begin{ChartPage2}{ ",spread_title," } \n",
               "\\LeftText{",short_text,"} \n"))
     }
})


knit_hooks$set(top_right_plot = function(before, options, envir) {

     if (options$top_right_plot == FALSE) return()

  if (before) {
       return("\\RightText{\\begin{chart} \n")
     } else {

      # return(paste0("\\caption{\\renewcommand\\normalsize{\\fontsize{6.6}{7}}% \\normalsize ",caption_text,"}","\\end{chart}} \n"))
      return(paste0("\\caption{\\large{",caption_text,"}}","\\end{chart}} \n"))
    }
})

# \\caption{\\renewcommand\\normalsize{\\fontsize{6.6}{7}}%\n              \\normalsize test}\\end{chart}} \n

knit_hooks$set(top_right_minitable = function(before, options, envir) {

     if (options$top_right_minitable == FALSE) return()

  if (before) {
       return("\\RightText{ \n")
     } else {

      return(paste0("} \n"))
    }
})



knit_hooks$set(left_plot = function(before, options, envir) {

  if (options$left_plot == FALSE) return()
     if (before) {
       return("\\LeftChart{\\begin{chart} \n")
     } else {

      return(paste0("\\vspace{.6mm} \n","\\caption{\\large{",caption_text,"}}","\\end{chart}} \n"))
    }
})

knit_hooks$set(right_plot = function(before, options, envir) {

  if (options$right_plot == FALSE) return()
     if (before) {
       return("\\RightChart{\\begin{chart} \n")
     } else {

      return(paste0("\\vspace{.6mm} \n","\\caption{\\large{",caption_text,"}}","\\end{chart}} \n"))
    }
})

knit_hooks$set(bottom_plot = function(before, options, envir) {

  if (options$bottom_plot == FALSE) return()
     if (before) {
       return("\\BottomChart{\\begin{chart} \n")
     } else {

      return(paste0("\\caption{\\large{",caption_text,"}}","\\end{chart}} \n \\end{ChartPage}"))
      # return(paste0("\\end{chart}} \n \\end{ChartPage}"))
    }
})


knit_hooks$set(bottom_plot2 = function(before, options, envir) {

  if (options$bottom_plot2 == FALSE) return()
     if (before) {
       return("\\BottomChart{\\begin{chart} \n")
     } else {

      return(paste0("\\caption{\\large{",caption_text,"}}","\\end{chart}} \n \\end{ChartPage2}"))
      #return(paste0("\\end{chart}} \n \\end{ChartPage2}"))
    }
})



# knit_hooks$set(end_chunk = function(before, options, envir) {
#
#      if (!before) {
#        return("\\end{ChartPage}")
#     }
#
# })

knit_hooks$set(map_plot = function(before, options, envir) {

  if (options$map_plot == FALSE) return()
     if (before) {
       return("\\begin{figure} \n")
     } else {

      return(paste0("\\caption{\\large{",caption_text,"}} \n","\\end{figure}"))
    }

})

# remove all the kframe tags from all chunk outputs (as we are placing outputs in custom environments already!)
local({
  hook_chunk = knit_hooks$get('chunk')
  knit_hooks$set(chunk = function(x, options) {
    x = hook_chunk(x, options)
    #if (options$results == 'asis' && !options$echo && options$fig.num == 0) {
    if (options$results == 'asis') {
      # remove all kframe's
      gsub('\\\\(begin|end)\\{kframe\\}', '', x)
    } else x
  })
})
