#' @title Barchart of the Economic and Pollution variable for the European countries in a certain year
#' @description Produces a barchart with the chosen Economic and Pollution variable in order to view the standing of the
#' European countries in a chosen year.
#' @param pol_var A \code{list} of Pollution variables to select
#' @param eco_var A \code{list} of Economic variables to select
#' @param yrs A \code{list} of all the years to be chosen
#' @return A \code{plot} containing a barchart
#' @author Redwan Hasan
#' @import dplyr
#' @export
barchart_function <- function(dataset = data,
                              pol_var = 'CO2 Emissions',
                              eco_var = 'Productivity in Primary Sector',
                              yrs = 2000) {

  #Here I call the database 'world' from the library spData to be used later for mapping countries
  world=spData::world

  #Extracting and filtering data in order to map it
  world_eu <- world %>% filter(continent == "Europe")

  #rename Russian Federation and Macedonia in Russia and North Macedonia
  world_eu$name_long <- world_eu$name_long %>%
    gsub(pattern = 'Russian Federation', replacement = 'Russia')
  world_eu$name_long <- world_eu$name_long %>%
    gsub(pattern = 'Macedonia', replacement = 'North Macedonia')

  #Continue extraction
  world_eu <- dplyr::left_join(world_eu[,c(1, 2)], data, by = c("name_long" = "Country"))
  eu_graph <- world_eu %>%
    filter(Year %in% yrs) %>%
    dplyr::select(iso_a2, Year, pol_var, eco_var)

  #Making the geom column as null else it creates problem with the barchart. Hence disabled it.
  eu_graph$geom <- NULL

  #Resolve parsing errors by changing whitespaces into underscores
  tmp_pol <- gsub(pattern = ' ',
                  replacement = '_',
                  x = pol_var)
  tmp_eco <- gsub(pattern = ' ',
                  replacement = '_',
                  x = eco_var)
  colnames(eu_graph)[which(colnames(eu_graph) == pol_var)] <- tmp_pol
  colnames(eu_graph)[which(colnames(eu_graph) == eco_var)] <- tmp_eco

  #Developing the barchart based on the variables and observations selected above.
  bargraphs <-
    ggplot2::ggplot(eu_graph) +
    ggplot2::geom_bar(ggplot2::aes_string(x = "iso_a2",
                                          weight = tmp_pol,
                                          fill = tmp_eco)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle=45, hjust=1),
          plot.caption = element_text(hjust = 0)) +
    ggplot2::xlab("Country code") +
    ggplot2::ylab(pol_var) +
    ggplot2::labs(title = yrs ,fill = '')

  return(bargraphs)

  }

