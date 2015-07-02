##' Function to manipulate the data
##'
##' @export

plot_data = function(x, y, group, subset, type, data,
                     scale, nCnty = nCnty, env = .GlobalEnv){
  switch(type,
         "reg_uni_bar" = {
           data[, y] = data[, y] * scale
           new_data = subset(data, subset = eval(subset),
                             select = unique(c(x, y, group)))
           new_data[, group] = factor(new_data[, group])
         },
         "reg_uni_line" = {
           data[, y] = data[, y] * scale
           new_data = subset(data, subset = eval(subset),
                             select = unique(c(x, y, group)))
           new_data[, group] = factor(new_data[, group])
         },
         "multi_dodge_bar" = {
           data[, y] = data[, y] * scale
           new_data = melt(subset(data, subset = eval(subset)),
                           id.var = x, measure.var = y)
           assign("group", "variable", envir = env)
         },
         "multi_stack_bar" = {
           data[, y] = data[, y] * scale
           new_data = melt(subset(data, subset = eval(subset)),
                           id.var = x, measure.var = y)
           assign("group", "variable", envir = env)
         },
         "multi_line" = {
           data[, y] = data[, y] * scale
           new_data = melt(subset(data, subset = eval(subset)),
                           id.var = x, measure.var = y)
           assign("group", "variable", envir = env)
         },
         "multi_stack_line" = {
           data[, y] = data[, y] * scale
           new_data = melt(subset(data, subset = eval(subset)),
                           id.var = x, measure.var = y)
           assign("group", "variable", envir = env)
         },
         "scatter_plot" = {
           new_data = subset(data, subset = eval(subset),
                             select = unique(c(x, y, group)))
         },
         "top_20_bar" = {
           data[, x] = data[, x] * scale
           complete_data = na.omit(subset(data, subset = eval(subset),
                                          select = c(x, y)))
           new_data = head(arrange(complete_data,
                                   desc(complete_data[, x])), n = nCnty)
           new_data$level = rep("Countries with\nhighest values", nCnty)
           new_data[, y] = factor(new_data[, y],
                                  levels = rev(new_data[, y]))
           assign("group", "level", envir = env)
         },
         "bot_20_bar" = {
           data[, x] = data[, x] * scale
           complete_data = na.omit(subset(data, subset = eval(subset),
                                          select = c(x, y)))
           new_data = tail(arrange(complete_data,
                                   desc(complete_data[, x])), n = nCnty)
           new_data$level = rep("Countries with\nlowest values", nCnty)
           new_data[, y] = factor(new_data[, y],
                                  levels = rev(new_data[, y]))
           assign("group", "level", envir = env)
         },
         "top_bot_bar" = {
           data[, x] = data[, x] * scale
           complete_data = na.omit(subset(data, subset = eval(subset),
                                          select = c(x, y)))
           new_data =
             rbind(head(arrange(complete_data,
                                desc(complete_data[, x])), n = nCnty/2),
                   tail(arrange(complete_data,
                                desc(complete_data[, x])), n = nCnty/2))
           new_data$level = c(rep("Countries with\nhighest values",
                                  nCnty/2), rep("Countries with\nlowest values", nCnty/2))
           new_data[, y] = factor(new_data[, y],
                                  levels = rev(unique(new_data[, y])))
           assign("group", "level", envir = env)
         },
         "top_dot" = {
           data[, x] = data[, x] * scale
           if (!is.null(group)) {
             complete_data = na.omit(subset(data, subset = eval(subset),
                                            select = c(x, y, group)))
             top_maxYear = subset(complete_data, 
                                  subset = complete_data[, group] == 
                                    max(complete_data[, group]))
             top_maxYear = head(arrange(top_maxYear, 
                                        desc(top_maxYear[, x])), n = nCnty)
             new_data = subset(complete_data, subset = complete_data[, y] %in% 
                                 top_maxYear[, y])
             new_data[, y] = factor(new_data[, y],
                                    levels = rev(top_maxYear[, y]))
             new_data[, group] = factor(new_data[, group])
           } else {
             complete_data = na.omit(subset(data, subset = eval(subset),
                                            select = c(x, y, "Year", "Area")))
             top_maxYear = head(arrange(complete_data, 
                                        desc(complete_data[, x[1]])), n = nCnty)
             top_maxYear[, y] = factor(top_maxYear[, y],
                                       levels = rev(top_maxYear[, y]))
             new_data = melt(subset(top_maxYear, subset = eval(subset)),
                             id.var = c(y, "Year", "Area"))
             assign("group", "variable", envir = env)
           }
           if (group == "pooled") {
             complete_data = na.omit(subset(data, subset = eval(subset),
                                            select = c(x, y, group)))
             top_maxYear = subset(complete_data, 
                                  subset = complete_data[, group] == 
                                    max(complete_data[, group]))
             top_maxYear = head(arrange(top_maxYear, 
                                        desc(top_maxYear[, x])), n = nCnty)
             new_data = subset(complete_data, subset = complete_data[, y] %in% 
                                 top_maxYear[, y])
             new_data[, y] = factor(new_data[, y],
                                    levels = rev(top_maxYear[, y]))
             new_data[, group] = factor(new_data[, group])
           } 

         },
         "bot_dot" = {
           data[, x] = data[, x] * scale
           complete_data = na.omit(subset(data, subset = eval(subset),
                                          select = c(x, y, group)))
           bot_maxYear = subset(complete_data, 
                                subset = complete_data[, group] == 
                                  max(complete_data[, group]))
           bot_maxYear = tail(arrange(bot_maxYear, 
                                      desc(bot_maxYear[, x])), n = nCnty)
           new_data = subset(complete_data, subset = complete_data[, y] %in% 
                               bot_maxYear[, y])
           new_data[, y] = factor(new_data[, y],
                                  levels = rev(bot_maxYear[, y]))
           new_data[, group] = factor(new_data[, group])
         },
         "top_bot_dot" = {
           data[, x] = data[, x] * scale
           complete_data = na.omit(subset(data, subset = eval(subset),
                                          select = c(x, y, group)))
           new_data = subset(complete_data, 
                                   subset = complete_data[, group] == 
                                     max(complete_data[, group]))
           new_data = 
             rbind(head(arrange(new_data, 
                                desc(new_data[, x])), n = nCnty/2),
                   tail(arrange(new_data, 
                                desc(new_data[, x])), n = nCnty/2))
           new_data$level = c(rep("Countries with\nhighest values",
                                        nCnty/2), rep("Countries with\nlowest values", nCnty/2))     
#            new_data = subset(complete_data, subset = complete_data[, y] %in% 
#                                topbot_maxYear[, y])
           new_data[, y] = factor(new_data[, y],
                                  levels = rev(new_data[, y]))
#            new_data[, group] = factor(new_data[, group])
           assign("group", "level", envir = env)
         },
         # ---------------------------------------------------------
         # ---------------------------------------------------------
          # new types by Markus
           "multi_dot_sh" = {
           data[, y] = data[, y] * scale
           new_data = melt(subset(data, subset = eval(subset)),
                           id.var = x, measure.var = y)
           assign("group", "variable", envir = env)
         },
           "multi_dot_no" = {
           data[, y] = data[, y] * scale
           new_data = melt(subset(data, subset = eval(subset)),
                           id.var = x, measure.var = y)
           assign("group", "variable", envir = env)
         },
           "reg_uni_dot" = {
           data[, y] = data[, y] * scale
           new_data = subset(data, subset = eval(subset),
                             select = unique(c(x, y, group)))
           new_data[, group] = factor(new_data[, group])
         },
           "multi_dodge_dot" = {
           data[, y] = data[, y] * scale
           new_data = melt(subset(data, subset = eval(subset)),
                           id.var = x, measure.var = y)
           assign("group", "variable", envir = env)
         },

         "manual" = {
           new_data = data
         })
  new_data
}