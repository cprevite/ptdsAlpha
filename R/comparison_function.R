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
#' @import dplyr maptools ggmap hrbrthemes here tidyverse DataExplorer maps tmap mapview leaflet ggplot2 viridis readxl
#' shiny sf raster spData magick gganimate reshape2 mapproj
#' @export

comparison_function <- function(pol_var, eco_var) {
  
  data(world)
  
  world_eu <- world %>% filter(continent == "Europe")
  world_eu <-
    left_join(world_eu[,c(1, 2)], data, by = c("name_long" = "country"))
  world_eu_4yrs <-
    world_eu %>% filter(year %in% c(2000, 2005, 2010, 2015))
  
  map_eu <- tm_shape(world_eu_4yrs) +
    tm_polygons() +
    tm_shape(world_eu_4yrs) +
    tm_fill(col = pol_var) +
    tm_symbols(col = "black",
               border.col = "white",
               size = eco_var) +
    tm_facets(by = "year",
              nrow = 2,
              free.coords = FALSE)
  
  tmap_mode("view")
  return(map_eu)
}