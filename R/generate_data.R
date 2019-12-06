#' @export
generate_data <- function() {

  # data preparation
  data <- as.data.frame(
    readxl::read_excel(
      system.file("extdata", "Passport_Stats.xls", package = "ptdsAlpha")
    )
  )

  # data <- data[,-c(3:4)] # delete useless column
  #
  # data <- data %>%
  #   rename(country = Geography) # rename one column
  #
  # data <- data %>%
  #   gather(c(3:44), key = year, value = values) # tidy data
  #
  # data <- data %>%
  #   spread(Category, values) # tidy data
  #
  # colnames(data) <- colnames(data) %>%
  #   tolower() %>%
  #   gsub(pattern = " ", replacement = "_") # tolower variables names
  #
  # # data$country <- tolower(data$country) # tolower country column
  #
  # data <- as_tibble(data)
  #
  # data$country <- as.factor(data$country)
  #
  # data$year <- as.factor(data$year)
  #
  # data[, 3:19] <- sapply(data[, 3:19], as.numeric)
  #
  #
  # # data.small <- data[, c(1, 2, 3, 9, 10 ,16)]
  #
  # data.long <- na.omit(gather(data, key="measure", value="value", c(3:19)))
  # data.long <- na.omit(data.long)
  #
  #
  # data$year= as.Date(as.numeric(paste(data$year, 1, 1, sep = "-"), "%Y%m%d"))
  # data$year= as.integer(as.character(data$year))
  #
  # data[c(3:19)] = scale(data[c(3:19)])
  #
  #

  return(data)

}

