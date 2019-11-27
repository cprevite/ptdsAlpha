
#' @title Variables per Country through Year
#' @description Compute an linetype graph containing the desired variables per desired countries through year
#' @param ctry A \code{list} of countries to select
#' region of interest, i.e. [a, b].
#' @param var A \code{list} of variables to select
#' @return A \code{plot} containing the line per variables and per countries
#' @author Claudio Previte
#' @examples
#' country_function(c('Italy', 'France', 'Spain'),'Productivity')
#' country_function('Italy', c('Productivity', 'GDP'))
#' @export
country_function <- function(ctry, var){

  data.long.function <- data.long %>%
    filter(Variable %in% var & Country %in% ctry)

  plot_country <- data.long.function %>%
    ggplot2::ggplot(aes(x = data.long.function$Year,
               y = data.long.function$Value,
               color = data.long.function$Country,
               group = data.long.function$Country)) +
    labs(title = paste(var), subtitle = 'by selected countries' , x = 'Year', y = paste(var), color = 'Country') +
    scale_x_discrete(breaks=seq(1970,2020,5)) +
    scale_y_continuous(labels = function(x) format(x, big.mark = "'",
                                                   scientific = FALSE)) +
    geom_line()

  return(plot_country)
}
