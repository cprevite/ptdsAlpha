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
ranktable_function <- function(yrs, ctry, var){

  # create rank variable and total rank
  data.ranktable.function <- data.long %>%
    dplyr::group_by(year, Variable) %>%
    dplyr::mutate(Rank = rank(-round(Value), na.last = 'keep'), Total = round(max(Rank, na.rm = T))) %>%
    dplyr::group_by(country) %>%
    dplyr::filter(country %in% ctry & Variable %in% var & year %in% yrs) %>%
    dplyr::mutate(Rank = ifelse(Rank > 10,
                         cell_spec(Rank, color = "red", bold = T),
                         cell_spec(Rank, color = "green", italic = T))) %>%
    kableExtra::kable(col.names = c("Country",
                        "Year",
                        "Variable(s)",
                        "Value",
                        "Rank",
                        'Total')) %>%
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover"))


  return(data.ranktable.function)
}
