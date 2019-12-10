
#' @title Variables per Country through Year
#' @description Compute a linetype graph containing the desired variables per
#' desired countries through year
#' @param ctry A \code{list} of countries to select
#' @param var A \code{list} of variables to select
#' @return A \code{plot} containing the line per variables and per countries
#' @author Claudio Previte
#' @examples
#' country_function(c('Italy', 'France', 'Spain'),'Productivity')
#' country_function('Italy', c('Productivity', 'GDP'))
#' @import dplyr ggplot2
#' @export
country_function <- function(ctry, var){

  # filter variables
  data.long.function <- data %>%
    dplyr::filter(variable %in% var & country %in% ctry)

# plot value of selected variables for selected countries on x axis (year)
  plot_country <- data.long.function %>%
    ggplot2::ggplot(ggplot2::aes(x = data.long.function$year,
               y = data.long.function$value,
               color = data.long.function$country,
               group = data.long.function$country)) +
    ggplot2::labs(title = paste(var), subtitle = 'by selected countries' ,
                  x = 'Year', y = paste(var), color = 'Country') +
    ggplot2::scale_x_discrete(breaks=seq(1970,2020,5)) +
    ggplot2::scale_y_continuous(labels = function(x) format(x, big.mark = "'",
                                                   scientific = FALSE)) +
    ggplot2::geom_line()+
    ggplot2::theme_bw()

  print(plot_country)
}

