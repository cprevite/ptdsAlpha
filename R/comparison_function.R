#' @title Interactive map of the European region showing the change betweem Economic and Pollution variables over time
#' @description Produces an interactive map of the European Union region over four different years to show the interaction
#' and change between the Economic and Pollution variables
#' @param pol_var A \code{list} of Pollution variables to select
#' @param eco_var A \code{list} of Economic variables to select
#' @return A \code{plot} containing four interactive maps of Europe based on the variables chosen
#' @author Redwan Hasan
#' @examples
#' comparison_function('Greenhouse_Gas_Emissions,'Productivity')
#' comparison_function('CO2_Emissions_from_Transport', 'GDP')
#' @import dplyr
#' @export

comparison_function <- function(dataset, pol_var="CO2_Emissions_from_Transport", eco_var="GDP") {

  #Here I call the database 'world' from the library spData to be used later for mapping countries


  #Extracting and filtering data in order to map it
  world_eu <- spData::world %>% filter(continent == "Europe")
  world_eu <-
    left_join(world_eu[,c(1, 2)], data, by = c("name_long" = "Country"))
  world_eu_4yrs <-
    world_eu %>% filter(Year %in% c(2000, 2005, 2010, 2015))

  #Using the tmap function along with the previous variables, plotting the map of europe for 4 different years
  map_eu <- tmap::tm_shape(world_eu_4yrs) +
    tmap::tm_polygons() +
    tmap::tm_shape(world_eu_4yrs) +
    tmap::tm_fill(col = pol_var) +
    tmap::tm_symbols(col = "black",
               border.col = "white",
               size = eco_var) +
    tmap::tm_facets(by = "year",
              nrow = 2,
              free.coords = FALSE)

  tmap::tmap_mode("view")
  return(map_eu)
}
