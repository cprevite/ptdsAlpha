
#' @title Variables per Country through Year
#' @description Compute an linetype graph containing the desired variables per desired countries through year
#' @param ctry A \code{list} of countries to selectcoun
#' @param var A \code{list} of variables to select
#' @return A \code{plot} containing the line per variables and per countries
#' @author Claudio Previte
#' @examples
#' country_function(c('Italy', 'France', 'Spain'),'Productivity')
#' country_function('Italy', c('Productivity', 'GDP'))
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
