
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
           ctry = "Switzerland",
           eco_var = "GDP",
           pol_var = "CO2 Emissions") {
    #transform data
    data_new <-  na.omit(data)
    data_new$Year = as.integer(as.character(data_new$Year))

    data_new <- data_new %>%
      dplyr::filter(Country %in% ctry)


    #font axis
    f <- list(family = "Courier New, monospace",
              size = 18,
              color = "#7f7f7f")
    x <- list(title = eco_var,
              titlefont = f)
    y <- list(title = pol_var,
              titlefont = f)

    #create bubble plot with function plot_ly
    plot_bubble <- plot_ly(
      data,
      x = ~ data_new[[eco_var]],
      y = ~ data_new[[pol_var]],
      color = ~ data_new$Country,
      frame = ~ data_new$Year,
      text = ~ data_new$Country,
      hoverinfo = "text",
      type = 'scatter',
      mode = 'markers'
    ) %>%
      layout(xaxis = x, yaxis = y, title = "Bubble chart")

    return(plot_bubble)

  }
