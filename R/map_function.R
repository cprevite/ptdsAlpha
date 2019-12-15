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
#' @import dplyr
#' @export

map_function <-
  function(dataset = data,
           yrs = '2000',
           col_val = 'GDP'
  ) {

  #Here the data variable is the one initially used when we loaded the excel
  #file and saved it to variable data
  data_map <- dataset %>%
    dplyr::group_by(Country) %>%
    dplyr::filter(Year == yrs) %>%
    dplyr::select(col_val)

  #We use the built-in database world from one of the libraries used to map the
  #countries in a plot
  world <- ggplot2::map_data('world')

  #Left-joining 2 databases from which data were obtained.
  mapbig <- dplyr::left_join(data_map, world, by = c("Country" = "region"))

  #Prepare the outline of the map
  world_map <- ggplot2::ggplot() + ggplot2::theme(
    panel.background = ggplot2::element_rect(fill = "slategray1", color = NA),
    panel.grid = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    axis.title.x = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank()
  )

  #Fixing the limits
  europe_map <-
    world_map + ggplot2::coord_fixed(xlim = c(-9, 42.5),
                            ylim = c(36, 70.1),
                            ratio = 1.5)

  #Putting out the Europe map using all the variables above
  europeview <- europe_map +
    ggplot2::geom_polygon(
      data = mapbig,
      ggplot2::aes_string(
        fill = col_val,
        x = "long",
        y = "lat",
        group = 'group'
      ),
      color = "grey70"
    ) +
    ggplot2::labs(
      title = paste0(col_val),
      subtitle = paste0("for European countries in ", as.character(yrs)),
      caption = "Data: Euromonitor"
    )  +
    ggplot2::theme(text = ggplot2::element_text(size = 15),
          plot.title = ggplot2::element_text(face = "bold")) +
    viridis::scale_fill_viridis(
      option = "plasma",
      direction = -1,
      name = "",
      na.value = "grey80",
      guide = ggplot2::guide_colorbar(
        barheight = ggplot2::unit(70, units = "mm"),
        barwidth = ggplot2::unit(10, units = "mm")
      )
    )

  return(europeview)

}



