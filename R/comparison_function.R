#' @title Interactive map of the European region showing the change between
#' Economic and Pollution variables over time
#' @description Produces two interactive maps of the European region over
#' two different years to show the interaction
#' and change between the Economic and Pollution variables
#' @param yrs A \code{list} of years to select
#' @param pol_var A \code{list} of Pollution variables to select
#' @param eco_var A \code{list} of Economic variables to select
#' @return  \code{Plots} containing interactive maps of Europe based
#' on the variables chosen
#' @author Team Alpha
#' @examples
#' comparison_function(yrs = 2000,
#' pol_var = "CO2 Emissions from Transport", eco_var = "GDP")
#' @import dplyr
#' @export

comparison_function <-
  function(dataset = data,
           yrs = 2000,
           pol_var = "CO2 Emissions from Transport",
           eco_var = "GDP") {

    #Call data 'world' from the 'spData' package
    world <- spData::world

    #Extracting and filtering data in order to map it
    world_eu <- world %>% filter(continent == "Europe")

    #Rename Russian Federation and Macedonia in Russia and North Macedonia
    world_eu$name_long <- world_eu$name_long %>%
      gsub(pattern = 'Russian Federation', replacement = 'Russia')
    world_eu$name_long <- world_eu$name_long %>%
      gsub(pattern = 'Macedonia', replacement = 'North Macedonia')

  #Continue extraction
  world_eu <-
    left_join(world_eu[,c(1, 2)], dataset, by = c("name_long" = "Country"))
  world_eu_4yrs <-
    world_eu %>% filter(Year %in% yrs)

  #Using the tmap function along with the previous variables,
  #plotting the map of europe for 4 different years
  map_eu <- tmap::tm_shape(world_eu_4yrs) +
    tmap::tm_polygons() +
    tmap::tm_shape(world_eu_4yrs) +
    tmap::tm_fill(col = pol_var) +
    tmap::tm_symbols(col = "black",
               border.col = "white",
               size = eco_var) +
    tmap::tm_facets(by = "Year",
              nrow = 1,
              free.coords = FALSE) +
    tmap::tm_layout(title = yrs)


  return(tmap::tmap_leaflet(map_eu))
}
