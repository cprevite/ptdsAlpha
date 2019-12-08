
#' @title Bubble graph
#' @description Data visualisation of two variables of the users choice
#' in a two dimensional plot.
#' A third dimension is represented by the size of the bubble
#' @param x_var First variables to select
#' @param y_var Second variables to select
#' @return A \code{plot} representing two variables in a bubble chart
#' @author Ana-Maria Casian
#' @examples
#' bubble_function ('gdp','productivity')
#' @import plotly tidyr
#' @export
bubble_function <- function(x_var, y_var) {

  #transform data
  data <- data %>%
    spread(variable, value)
  data <-  na.omit(data)
  data$year = as.integer(as.character(data$year))

  #create bubble plot with function plot_ly
  plot_bubble <- plot_ly(
    data,
    x = ~ data[[x_var]],
    y = ~ data[[y_var]],
    color = ~ data$country,
    frame = ~ data$year,
    text = ~ data$country,
    hoverinfo = "text",
    type = 'scatter',
    mode = 'markers'
  )

  return(plot_bubble)

}
