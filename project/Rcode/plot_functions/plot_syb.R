##' A function for standardised SYB plots
##'
##' @param x
##' @param y
##' @param data
##' @param group
##' @param type
##' @param x_lab
##' @param y_lab
##' @param legend_lab
##' @export

plot_syb = function(x, y, group = NULL, type, subset = TRUE, data, scale = 1,
                    x_lab = NULL, y_lab = NULL, legend_lab = NULL,
                    col_pallete, nCnty = 20){
    subset = substitute(subset)
    env = environment()
    new_data = plot_data(x = x, y = y, scale = scale, subset = subset,
        group = group, type = type, data = data, env = env, nCnty = nCnty)
    plot_dictionary(x = x, y = y, group = group, type = type,
                    data = new_data, x_lab = x_lab, y_lab = y_lab,
                    legend_lab = legend_lab, col_pallete = col_pallete)
    # plot_dictionary2(x = x, y = y, group = group, type = type,
    # 				        scale=scale,subset=subset, env = env, nCnty = nCnty,
    #                 data = data, x_lab = x_lab, y_lab = y_lab,
    #                 legend_lab = legend_lab, col_pallete = col_pallete)
}
