#' @title Evolution of European indicators through time
#' @description Displays a GIF showing the evolution of one variable through
#' time over an EU chloropleth map
#' @param var A \code{list} of any variable to select from the dataset
#' @return An \code{animation} containing the map of Europe based on the
#' variable chosen
#' @author Jeremy Choppe
#' @examples
#' gif_function(data,'Greenhouse_Gas_Emissions')
#' @import dplyr ggplot2
#' @export

gif_function <- function(dataset=data, var='Greenhouse_Gas_Emissions') {

  #selecting the desired world area, which is relevant to the dataset

  world_eu <- spData::world %>% filter(continent == "Europe")
  world_eu <- world_eu[, c(1, 2)]
  world_eu <- left_join(world_eu, dataset, by = c("name_long" = "Country"))
  world_eu <- world_eu[-1,]

  #setting the gif features with some tmap options and along years
  gif <-
    tmap::tm_shape(world_eu) + tmap::tm_fill(col = var) + tmap::tm_polygons()  +
    tmap::tm_facets(along = "Year", free.coords = FALSE)

  #defining the dimensions of the gif and where to save the file created
  tmap::tmap_animation(
    gif,
    filename = "tmap.gif",
    delay = 30,
    width = 1000,
    height = 900
  )

 return("tmap.gif")

}
