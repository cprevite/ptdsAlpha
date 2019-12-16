#' @title Animated Top10 Countries per Variables
#' @description Compute an animated ranking with the 10 top European countries
#' per year in regards to a chosen economic or pollution variable
#' @param var A variable to select
#' @return A \code{plot} containing an animated ranking
#' @author Team Alpha
#' @examples
#' animated_top10('Productivity')
#' @import dplyr
#' @export
animated_top10 <- function(dataset = data, var = 'Productivity') {
  # transform data to long format
  dataGather <- gather(data, key = "Variable", value = "Value", c(3:19))

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
      ggplot2::aes(
        Rank,
        Value_rel,
        group = Country,
        fill = as.factor(Country),
        color = as.factor(Country)
      )
    ) +
    ggplot2::geom_tile(ggplot2::aes(
      y = Value / 2,
      height = Value,
      width = 0.9
    ), color = NA) +
    ggplot2::geom_text(ggplot2::aes(y = 0, label = paste(Country, " ")),
                       vjust = 0.2,
                       hjust = 1) +
    ggplot2::geom_text(ggplot2::aes(
      y = Value,
      label = Value_lbl,
      hjust = -0.2,
      vjust = 0
    )) +
    ggplot2::coord_flip(clip = "off", expand = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_reverse() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::theme(
      axis.line = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      legend.position = "none",
      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(size = .1, color = "grey"),
      panel.grid.minor.x = ggplot2::element_line(size = .1, color = "grey"),
      plot.title = ggplot2::element_text(
        size = 28,
        hjust = 0.5,
        face = "bold",
        colour = "grey",
        vjust = -1
      ),
      plot.subtitle = ggplot2::element_text(
        size = 18,
        hjust = 0.5,
        face = "italic",
        color = "grey"
      ),
      plot.caption = ggplot2::element_text(
        size = 25,
        hjust = 0.95,
        face = "bold",
        color = "grey"
      ),
      plot.background = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(2, 2, 2, 4, "cm")
    )

  # animate the the staticplot
  anim <- staticplot +
    gganimate::transition_states(Year,
                                 transition_length = 30,
                                 state_length = 30) +
    gganimate::view_follow(fixed_x = TRUE)  +
    ggplot2::labs(
      title = paste(var, sep = '\n'),
      subtitle  =  "Top 10 Countries per Year",
      caption  = "{closest_state}"
    )


  return(anim)
}
