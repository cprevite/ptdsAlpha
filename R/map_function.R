#' @title Pollution & Economic Overview of European countries 
#' @description Produces a colorful graphical output of the European map in regards to the chosen Economic or Pollution variables
#' in a given year 
#' @param yrs A \code{list} of years to select
#' @param col_val A \code{list} of Economic and Pollution variables to select
#' @return A \code{plot} containing the map of Europe based on the variable chosen
#' @author Redwan Hasan
#' @examples
#' map_function(2008,'Productivity')
#' map_function(1982, 'Greenhouse_Gas_Emissions')
#' @import dplyr maptools ggmap hrbrthemes here tidyverse DataExplorer maps tmap mapview leaflet ggplot2 viridis readxl
#' shiny sf raster spData magick gganimate reshape2 mapproj
#' @export

map_function <- function(yrs, col_val) {
  
  data_map <- data %>% 
    group_by(country) %>% 
    filter(year == yrs) %>% 
    dplyr::select(col_val)
  
  world <- map_data("world")
  
  mapbig <- left_join(data_map, world, by = c("country" = "region"))
  
  world_map <- ggplot() + theme(
    panel.background = element_rect(fill = "slategray1", color = NA),
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )
  
  europe_map <-
    world_map + coord_fixed(xlim = c(-9, 42.5),
                            ylim = c(36, 70.1),
                            ratio = 1.5)
  
  europeview <- europe_map +
    geom_polygon(
      data = mapbig,
      aes_string(
        fill = col_val,
        x = "long",
        y = "lat",
        group = "group"
      ),
      color = "grey70"
    ) +
    labs(
      title = paste0(col_val),
      subtitle = paste0("for European countries in ", as.character(yrs)),
      caption = "Data: Euromonitor"
    )  +
    theme(text = element_text(size = 15),
          plot.title = element_text(face = "bold")) +
    scale_fill_viridis(
      option = "plasma",
      direction = -1,
      name = "",
      na.value = "grey80",
      guide = guide_colorbar(
        barheight = unit(70, units = "mm"),
        barwidth = unit(10, units = "mm")
      )
    )
  
  return(europeview)
  
}
