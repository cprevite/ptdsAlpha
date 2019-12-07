#' @title Forecast of a variable over a number of years
#' @description Given data, a country, a variable and a forecasting length,
#' produces a time series and a forecast of the variable over time, and an
#' interactive plot of the forecast
#' @param dataset a preprocessed dataset from which information is extracted
#' @param geo a country that is contained in the dataset
#' @param variable a variable that is contained in the dataset
#' @param fc_length a integer representing the number of years of the forecast
#'
#' @return A \code{list} containing the time series created, the time series of
#' the forecast and the dygraph \code{plot}
#' @import dplyr stringr dygraphs forecast tidyr
#' @export
#'
#' @examples
#' #store the function results in a variable
#' my_fc <- forecast_function(data, 'France', 'GDP', 10)
#' #plot the interactive dygraph plot
#' my_fc$plot
#'
#'
#' @author Pierre-Emmanuel Got
#'
forecast_function <- function(dataset,geo,variable,fc_length) {


  # pivoting the dataframe to fit the function
  data <- dataset %>%
    pivot_wider(
      names_from = variable,
      values_from = value
    )

  #---retrieving time serie from the country and variable specified---
  #-set the country-
  data <- data %>%
    filter(str_detect(country, paste0(geo)))

  #retrieve index of the desired variable:
  index <- which(colnames(data) %in% paste0(variable))

  #-creates the time series-
  data.ts <-
    ts(data[, index], frequency = 1, start = c(1980))

  #---graphical adjustments for the graph as a js function---
  #credits: https://towardsdatascience.com/how-to-create-better-interactive-forecast-plots-using-r-and-dygraph-29bdd7146066

  interval_value_formatter <- "function(num, opts, seriesName, g, row, col) {
  value = g.getValue(row, col);
  if(value[0] != value[2]) {
  lower = Dygraph.numberValueFormatter(value[0], opts);
  upper = Dygraph.numberValueFormatter(value[2], opts);
  return '[' + lower + ', ' + upper + ']';
  } else {
  return Dygraph.numberValueFormatter(num, opts);
  }
}"

  #---creating the graph with dygraphs---
  aarima <- auto.arima(data.ts, stepwise = FALSE)
  dfcast <- forecast(object = data.ts, model = aarima, h = fc_length)

  data.fc <- dfcast %>%
  {cbind(actuals=.$x, forecast_mean=.$mean,
         lower_95=.$lower[,"95%"], upper_95=.$upper[,"95%"],
         lower_80=.$lower[,"80%"], upper_80=.$upper[,"80%"])}

  # data.fc <-
  # data.ts %>%
  #   auto.arima(stepwise = FALSE) %>%
  #   forecast(
  #     h=fc_length
  #     ) %>%
  #   {cbind(actuals=.$x, forecast_mean=.$mean,
  #          lower_95=.$lower[,"95%"], upper_95=.$upper[,"95%"],
  #          lower_80=.$lower[,"80%"], upper_80=.$upper[,"80%"])}

  data.dg <-
    dygraph(
      data.fc,
      main = paste0("FC display of ",
                    variable,
                    " over ",
                    fc_length,
                    " years in ",
                    geo),
      ylab = paste0(variable)
    ) %>%
    dyAxis("y", valueFormatter = interval_value_formatter) %>%
    dySeries("actuals", color = "black") %>%
    dySeries("forecast_mean", color = "blue", label = "forecast") %>%
    dySeries(c("lower_80", "forecast_mean", "upper_80"),
             label = "80%",
             color = "blue") %>%
    dySeries(c("lower_95", "forecast_mean", "upper_95"),
             label = "95%",
             color = "blue") %>%
    dyLegend(labelsSeparateLines = TRUE) %>%
    dyRangeSelector() %>%
    dyOptions(digitsAfterDecimal = 1) %>%
    dyCSS(
      textConnection(
        ".dygraph-legend
        {background-color: rgba(255, 255, 255, 0.5) !important; }"
      )
    )

  #---returning the time series, the forecast and the graph in a list---
  vals <-
    list(data_series = data.ts,
         data_forecast = data.fc,
         data_plot = data.dg)
  return(vals)
  }

