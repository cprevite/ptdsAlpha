
#' @title Bubble graph
#' @description Data visualisation of two variables of the users choice
#' in a two dimensional plot.
#' A third dimension is represented by the size of the bubble
#' @param x_var A \code{list} of variables to select
#' @param y_var A \code{list} of variables to select
#' @return A \code{plot} representing two variables in a bubble chart
#' @author Ana-Maria Casian
#' @examples
#' bubble_function ('gdp','productivity')
#' @export
bubble_function <- function(x_var, y_var) {

   data <- data %>%
    tidyr:: spread(variable, value)
  data <-  na.omit(data)
  data$year = as.integer(as.character(data$year))

  plot_bubble <- plotly:: plot_ly(
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

  print(plot_bubble)

}
