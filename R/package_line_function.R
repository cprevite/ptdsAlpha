
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
#' @import dplyr
#' @export
country_function <-
  function(dataset = data,
           ctry = 'Switzerland',
           var = 'GDP') {

    #transform data to long format
    dataGather <- gather(data, key = "Variable", value = "Value", c(3:19))

    # filter variables
    dataCountry_function <- dataGather %>%
      dplyr::filter(Variable %in% var & Country %in% ctry)

    # plot value of selected variables for selected countries on x axis (year)
    plot_country <- dataCountry_function %>%
      ggplot2::ggplot(
        ggplot2::aes(
          x = dataCountry_function$Year,
          y = dataCountry_function$Value,
          color = dataCountry_function$Country,
          group = dataCountry_function$Country
        )
      ) +
      ggplot2::labs(
        title = paste(var),
        subtitle = 'by selected countries' ,
        x = 'Year',
        y = paste(var),
        color = 'Country'
      ) +
      ggplot2::scale_x_discrete(breaks = seq(1970, 2020, 5)) +
      ggplot2::scale_y_continuous(
        labels = function(x)
          format(x, big.mark = "'",
                 scientific = FALSE)
      ) +
      ggplot2::geom_line() +
      ggplot2::theme_bw()

    print(plot_country)
  }
