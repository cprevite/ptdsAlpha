#' @title Animation of the European indicators over time
#' @description Produces a GIF about one variable and shows an EU chloropleth map with colors changing accordingly to the data evolution
#' @param pol_var A \code{list} of any variable to select
#' @return An \code{animation} containing the map of Europe based on the variable chosen
#' @author Jeremy Choppe
#' @examples
#' comparison_function('Greenhouse_Gas_Emissions')
#' comparison_function('CO2_Emissions_from_Transport')
#' @export

gif_function <- function(var) {
  world1 <- map_data("world")
  world_eu <- world %>% filter(continent == "Europe")
  world_eu <- world_eu[, c(1, 2)]
  world_eu <-
    left_join(world_eu, data, by = c("name_long" = "country"))
  world_eu <- world_eu[-1,]

  gif <- tm_shape(world_eu) + tm_fill(col = var) + tm_polygons()  +
    tm_facets(along = "year", free.coords = FALSE)

  tmap_animation(
    gif,
    filename = "tmap.gif",
    delay = 30,
    width = 1000,
    height = 900
  )

}
