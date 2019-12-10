#' @title Animated Top10 Countries per Variables
#' @description Compute an animated ranking with the 10 top countries per year
#' @param var A \code{list} of variables to select
#' @return A \code{plot} containing an animated ranking
#' @author Claudio Previte
#' @examples
#' animated_top10('Productivity')
#' @import dplyr ggplot2
#' @export
animated_top10 <- function(dataset = data, var = 'Productivity') {

  # transform data to long format
  dataGather <- gather(data, key="Variable", value="Value", c(3:19))

  # filter variable
  dataAnimated_top10 <- dataGather %>%
    dplyr::filter(Variable %in% var)

  # create Rank variable
  prod_formatted <- dataAnimated_top10 %>%
    dplyr::group_by(Year) %>%
    dplyr::mutate(
      Rank = rank(-Value) * 1,
      Value_rel = Value / Value[Rank == 1],
      Value_lbl = round(Value)
    ) %>%
    dplyr::group_by(Country) %>%
    dplyr::filter(Rank <= 10) %>%
    plotly::ungroup()

  # generate staticplot with countries top ten ranking
  staticplot <-
    ggplot2::ggplot(
      prod_formatted,
      aes(
        Rank,
        Value_rel,
        group = Country,
        fill = as.factor(Country),
        color = as.factor(Country)
      )
    ) +
    ggplot2::geom_tile(aes(
      y = Value / 2,
      height = Value,
      width = 0.9
    ), color = NA) +
    ggplot2::geom_text(aes(y = 0, label = paste(Country, " ")), vjust = 0.2, hjust = 1) +
    ggplot2::geom_text(aes(y = Value, label = Value_lbl, hjust = -0.2, vjust = 0)) +
    ggplot2::coord_flip(clip = "off", expand = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_reverse() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::theme(
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.position = "none",
      panel.background = element_blank(),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(size = .1, color = "grey"),
      panel.grid.minor.x = element_line(size = .1, color = "grey"),
      plot.title = element_text(
        size = 28,
        hjust = 0.5,
        face = "bold",
        colour = "grey",
        vjust = -1
      ),
      plot.subtitle = element_text(
        size = 18,
        hjust = 0.5,
        face = "italic",
        color = "grey"
      ),
      plot.caption = element_text(
        size = 25,
        hjust = 0.95,
        face = "bold",
        color = "grey"
      ),
      plot.background = element_blank(),
      plot.margin = margin(2, 2, 2, 4, "cm")
    )

  # animate the the staticplot
  anim <- staticplot +
    gganimate::transition_states(Year,
                      transition_length = 30,
                      state_length = 30) +
    gganimate::view_follow(fixed_x = TRUE)  +
    labs(
      title = paste(var, sep = '\n'),
      subtitle  =  "Top 10 Countries per Year",
      caption  = "{closest_state}"
    )


  return(anim)
}
