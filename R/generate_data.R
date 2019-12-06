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

  colnames(data) <- colnames(data) %>%
    tolower() %>%
    gsub(pattern = " ", replacement = "_") # tolower variables names

  data <- as_tibble(data)

  data$country <- as.factor(data$country)

  data$year <- as.factor(data$year)

  data[, 3:19] <- sapply(data[, 3:19], as.numeric)
  data <- gather(data, key="variable", value="value", c(3:19))

  return(data)

}



