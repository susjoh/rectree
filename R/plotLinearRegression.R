#' A function to plot a linear regression.
#'
#' @param x numeric. Continuous variable.
#' @param y numeric. Continuous variable
#' @param group character. Defines a grouping variable.
#' @param dataset data.frame. Default is rec_rate.
#' @import broom
#' @import dplyr
#' @import ggplot2
#' @export


plotLinearRegression <- function(x, y, group = NULL, dataset = rec_rate){

  if(!is.null(group)){

    if(length(unique(dataset[,group])) > 8){


      suppressMessages(print(eval(parse(text = paste0("ggplot(dataset, aes(", x, ", ", y, ")) +
    geom_point(alpha =0.8) +
    stat_smooth(method = \"lm\", se = F) +
    theme_bw() +
    facet_wrap(~", group, ", scales = \"free\")")))))

    } else {

      suppressMessages(print(eval(parse(text = paste0("ggplot(dataset, aes(", x, ", ", y, ", colour = ", group, ")) +
    geom_point(alpha =0.8) +
    stat_smooth(method = \"lm\", se = F) +
    scale_colour_brewer(palette = \"Set1\") +
    theme_bw() +
    facet_wrap(~", group, ") +
    theme(axis.text.x  = element_text (size = 12),
          axis.text.y  = element_text (size = 12),
          axis.title.y = element_text (size = 12, vjust = 2),
          axis.title.x = element_text (size = 12),
          legend.position = \"none\")")))))
    }
  } else {

    suppressMessages(print(eval(parse(text = paste0("ggplot(dataset, aes(", x, ", ", y, ")) +
    geom_point(alpha =0.8) +
    stat_smooth(method = \"lm\", se = F) +
    scale_colour_brewer(palette = \"Set1\") +
    theme_bw() +
    theme(axis.text.x  = element_text (size = 12),
          axis.text.y  = element_text (size = 12),
          axis.title.y = element_text (size = 12, vjust = 2),
          axis.title.x = element_text (size = 12),
          legend.position = \"none\")")))))

  }

}

