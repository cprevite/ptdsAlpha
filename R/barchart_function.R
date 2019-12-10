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
#' @import dplyr maptools ggmap hrbrthemes here tidyverse DataExplorer maps tmap mapview leaflet ggplot2 viridis readxl
#' shiny sf raster spData magick gganimate reshape2 mapproj
#' @export
barchart_function <- function(dataset = data,
                              pol_var = 'CO2_Emissions',
                              eco_var = 'Productivity',
                              yrs = 2000) {
  browser()
  #Here I call the database 'world' from the library spData to be used later for mapping countries
  world <- spData::world

  #Extracting and filtering data in order to map it
  world_eu <- world %>% filter(continent == "Europe")
  world_eu <- left_join(world_eu[,c(1, 2)], dataset, by = c("name_long" = "Country"))

  eu_graphGather <- gather(world_eu, key = "Variable", value = "Value", c(4:20))


  eu_graph_function <- eu_graphGather %>%
    filter(Year %in% yrs) %>%
    dplyr::select(iso_a2, Year, Variable, Value)



  #Making the geom column as null else it creates problem with the barchart. Hence disabled it.
  eu_graph_function$geom <- NULL

  # eu_graph_function$Variable <- str_replace_all(eu_graph_function$Variable,"[:blank:]","_")

  #pasrsing problem->changing varname to underscored
  tmp_pol <- gsub(' ','_',pol_var)
  tmp_eco <- gsub(' ','_',eco_var)

  #Developing the barchart based on the variables and observations selected above.
  bargraphs <- ggplot(eu_graph_function) + geom_bar(aes(
    x = iso_a2,
    weight = tmp_pol,
    fill = tmp_eco
  )) +
    xlab("Country code") +
    ylab(tmp_pol) +
    labs(title = paste0(pol_var, " comparison against ", eco_var),
         subtitle = paste0(yrs))
  return(bargraphs)
}



weight = eu_graph_function$Value[eu_graph_function$Variable == pol_var] ,
fill = eu_graph_function$Value[eu_graph_function$Variable == eco_var]
