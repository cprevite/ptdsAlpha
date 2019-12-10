
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
bubble_function <-
  function(dataset = data,
           x_var = "GDP",
           y_var = "Productivity") {
    #transform data
    data <-  na.omit(dataset)
    data$Year = as.integer(as.character(data$Year))

    #font axis
    f <- list(family = "Courier New, monospace",
              size = 18,
              color = "#7f7f7f")
    x <- list(title = x_var,
              titlefont = f)
    y <- list(title = y_var,
              titlefont = f)

    #create bubble plot with function plot_ly
    plot_bubble <- plot_ly(
      data,
      x = ~ data[[x_var]],
      y = ~ data[[y_var]],
      color = ~ data$Country,
      frame = ~ data$Year,
      text = ~ data$Country,
      hoverinfo = "text",
      type = 'scatter',
      mode = 'markers'
    ) %>%
      layout(xaxis = x, yaxis = y, title = "Bubble chart")

    return(plot_bubble)

  }
