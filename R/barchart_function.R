#' @title Barchart of the Economic and Pollution variable for the European countries in a certain year
#' @description Produces a barchart with the chosen Economic and Pollution variable in order to view the standing of the
#' European countries in a chosen year.
#' @param pol_var A \code{list} of Pollution variables to select
#' @param eco_var A \code{list} of Economic variables to select
#' @param yrs A \code{list} of all the years to be chosen
#' @return A \code{plot} containing a barchart
#' @author Redwan Hasan
#' @examples
#' barchart_function('Greenhouse_Gas_Emissions,'Productivity', 1988)
#' barchart_function('CO2_Emissions_from_Transport', 'GDP', 2017)
#' @export

barchart_function <- function(pol_var, eco_var, yrs) {
  data(world)
  
  world_eu <- world %>% filter(continent == "Europe")
  world_eu <- left_join(world_eu[,c(1, 2)], data, by = c("name_long" = "country"))
  eu_graph <- world_eu %>% filter(year %in% yrs) %>% 
    dplyr::select(iso_a2, year, pol_var, eco_var)
  eu_graph$geom <- NULL
  
  bargraphs <- ggplot(eu_graph) + geom_bar(aes_string(x = "iso_a2", 
                                                      weight = pol_var, 
                                                      fill = eco_var)) + 
    xlab("Country code") + 
    ylab(pol_var) +
    labs(title = paste0(pol_var, " comparison against ", eco_var),
         subtitle = paste0(yrs))
  return(bargraphs)
}