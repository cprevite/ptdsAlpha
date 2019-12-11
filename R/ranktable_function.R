#' @title RankTable by Years, Countries and Variables
#' @description Compute a table with the value of the selected variables for the
#' selected countries and years, and the rank
#' @param yrs A \code{list} of Years to select
#' @param ctry A \code{list} of Countries to select
#' @param var A \code{list} of Variables to select
#' @return A \code{table} containing the desired information and the rank for
#' each countries
#' @author Claudio Previte
#' @examples
#' ranktable_function(
#'          yrs = c('2001', '1992'),
#'          ctry = c('Albania', 'Andorra'),
#'          var  = c('GDP', 'Productivity'))
#' @import dplyr
#' @export
ranktable_function <-
  function(dataset = data,
           yrs = c('1990','2000'),
           ctry = c('France','Switzerland'),
           var = c('CO2 Emissions','GDP')) {

    #transform data to long format
    dataGather <- tidyr::gather(data,
                                key = "Variable",
                                value = "Value",
                                c(3:19))

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
