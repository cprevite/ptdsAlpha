#' @title Evolution of European indicators through time
#' @description Displays a GIF showing the evolution of one variable through time over an EU chloropleth map
#' @param var A \code{list} of any variable to select from the dataset
#' @return An \code{animation} containing the map of Europe based on the variable chosen
#' @author Jeremy Choppe
#' @examples
#' gif_function('Greenhouse_Gas_Emissions')
#' @import dplyr maptools ggmap hrbrthemes here tidyverse DataExplorer maps tmap mapview leaflet ggplot2 viridis readxl
#' shiny sf raster spData magick gganimate reshape2 mapproj
#' @export

gif_function <- function(var) {

  #selecting the desired world area, which is relevant to the dataset
  world1 <- map_data("world")
  world_eu <- world %>% filter(continent == "Europe")
  world_eu <- world_eu[, c(1, 2)]
  world_eu <- left_join(world_eu, data, by = c("name_long" = "country"))
  world_eu <- world_eu[-1,]

  #setting the gif features with some tmap options and along years
  gif <- tm_shape(world_eu) + tm_fill(col = var) + tm_polygons()  +
    tm_facets(along = "year", free.coords = FALSE)

  #defining the dimensions of the gif and where to save the file created
  tmap_animation(
    gif,
    filename = "tmap.gif",
    delay = 30,
    width = 1000,
    height = 900
  )

}
