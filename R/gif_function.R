#' @title Evolution of European indicators through time
#' @description Displays a GIF showing the evolution of one variable through
#' time over an EU chloropleth map
#' @param var A \code{list} of any variable to select from the dataset
#' @return An \code{animation} containing the map of Europe based on the
#' variable chosen
#' @author Team Alpha
#' @examples
#' gif_function('Greenhouse_Gas_Emissions')
#' @import dplyr
#' @export

gif_function <- function(dataset = data, var = 'CO2 Emissions') {
  #Call data 'world' from the 'spData' package
  world <- spData::world

  #Extracting and filtering data in order to map it
  world_eu <- world %>% filter(continent == "Europe")
  world_eu <- world_eu[, c(1, 2)]
  world_eu <-
    left_join(world_eu, dataset, by = c("name_long" = "Country"))
  world_eu <- world_eu[-1,]

  #setting the gif features with some tmap options and along years
  gif <-
    tmap::tm_shape(world_eu) +
    tmap::tm_fill(col = var) +
    tmap::tm_polygons()  +
    tmap::tm_facets(along = "Year", free.coords = FALSE)


  #defining the dimensions of the gif and saving it under 'var_tmap.gif'
  tmap::tmap_animation(
    gif,
    filename = ("./gif_folder/tmap.gif"),
    delay = 30,
    width = 1000,
    height = 900
  )

  return(tmap.gif)

}
