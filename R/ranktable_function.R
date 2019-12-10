#' @title RankTable by Years, Countries and Variables
#' @description Compute a table with the value of the selected variables for the
#' selected countries and years, and the rank
#' @param yrs A \code{list} of Years to select
#' @param ctry A \code{list} of Countries to select
#' @param var A \code{list} of Variables to select
#' @return A \code{table} containing the desired information and the rank for
#' each countries
#' @author Claudio Previte
#' @example
#' ranktable_function(c(2001, 1992), c('Albania', 'Andorra'), c('GDP', 'Productivity'))
#' @import dplyr kableExtra
#' @export
ranktable_function <-
  function(dataset = data,
           yrs = '2000',
           ctry = 'Switzerland',
           var = 'GDP') {

    #transform data to long format
    dataGather <- gather(data, key = "Variable", value = "Value", c(3:19))

    # create rank variable and total rank
    dataRanktable <- dataGather %>%
      dplyr::group_by(Year, Variable) %>%
      dplyr::mutate(Rank = rank(-round(Value), na.last = 'keep'),
                    Total = round(max(Rank, na.rm = T))) %>%
      dplyr::group_by(Country) %>%
      dplyr::filter(Country %in% ctry &
                      Variable %in% var & Year %in% yrs) %>%
      kableExtra::kable(col.names = c("Country",
                                      "Year",
                                      "Variable",
                                      "Value",
                                      "Rank",
                                      'Total')) %>%
      kableExtra::kable_styling(bootstrap_options = c("striped", "hover"))


    return(dataRanktable)
  }
