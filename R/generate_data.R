#' @export
generate_data <- function() {

  # data preparation
  data <- as.data.frame(
    readxl::read_excel(
      system.file("extdata", "Passport_Stats.xls", package = "ptdsAlpha")
    )
  )

  # delete useless column
  data <-
    data[, -which(names(data) %in% c("Unit", "Current Constant"))]

  # rename  column Geography
  data <- data %>%
    rename(country = Geography)

  data <- data %>%
    gather(c(3:44), key = year, value = values)

  data <- data %>%
    spread(Category, values)

  # colnames(data) <- colnames(data) %>%
  #   tolower() %>%
  #   gsub(pattern = " ", replacement = "_") # tolower variables names

  data <- as_tibble(data)

  data$country <- as.factor(data$country)

  data$year <- as.factor(data$year)

  data[, 3:19] <- sapply(data[, 3:19], as.numeric)
  # data <- gather(data, key="variable", value="value", c(3:19))

  #assign shorter names to columns to enhance graphic display
  newnames <- c(
    "Country",
    "Years",
    "CO2 Emissions from Fossil Fuels",
    "CO2 Emissions from Transport",
    "CO2 Emissions",
    "GDP",
    "Greenhouse Gas Emissions",
    "Greenhouse Gas Emissions from Agriculture",
    "Greenhouse Gas Emissions from Energy",
    "Greenhouse Gas Emissions from Industry",
    "Greenhouse Gas Emissions from Waste",
    "Productivity",
    "Productivity in Primary Sector",
    "Productivity in Construction",
    "Productivity in Private Tertiary Sector",
    "Productivity in Secondary Sectors",
    "Productivity in Public Tertiary Sector",
    "Productivity in Other Services",
    "Productivity per Hour Worked")

  colnames(data) <- newnames

  #reorder the dataframe so that :
  # - columns 3:8 --> pollution variables
  # - columns 9:19 --> socioeconomic variables

  data <- data[,c(1:5,7:11,6,12:19)] #reordering GDP
  data <- data[,c(1,2,5,3,4,6:19)] #reordering CO2 Emissions

  #footnote:
  #
  #this function is used to generate data from the xls file and the data can be
  #used as such in the ptdsAlpha ShinyApp. However, as the GDP data from the
  #orginal file weren't satisfying (expressed overall per country and in local
  #currency), another file is used in the app. It is similar to this one but
  #with manual modifications on the GDP (GDP per person in euros) and a reduced
  #time window (1980:2017)

  return(data)

}



