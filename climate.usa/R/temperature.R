#' Estimate trend in temperature
#'
#' Estimate the trend of temperatures over time, in units of degrees Celsius per year.
#'
#' @param data contains the created dataset
#' @param data_column is date column in data
#' @param temp_column is temperature column in data
#'
#' @return a plot with
#' \itemize{
#' \item \code{type} curve of the temperature trend
#' }
#'@examples
#'temp_trend(dat, "LST_DATE", "T_DAILY_AVG", 2)


temp_trend <- function(data, date_column, temp_column) {

  data$YEAR <- year(data[[date_column]])
  yearly_data <- data %>%
    group_by(YEAR) %>%
    summarize(AVG_TEMP = mean(.data[[temp_column]], na.rm = TRUE), .groups = 'drop')

  model_formula <- as.formula(paste("AVG_TEMP ~ poly(YEAR, ", 2, ")"))
  model <- lm(model_formula, data = yearly_data)

  prediction_years <- data.frame(YEAR = seq(min(yearly_data$YEAR), max(yearly_data$YEAR), length.out = 200))
  prediction_years$PREDICTED_TEMP <- predict(model, newdata = prediction_years)

  ggplot(prediction_years, aes(x = YEAR, y = PREDICTED_TEMP)) +
    geom_line(color = "blue", linewidth = 1.5) +
    labs(title = "Yearly Temperature Trend Model (Polynomial Regression)",
         x = "Year", y = "Temperature (°C)") +
    theme_minimal()
}

temp_trend(dat, "LST_DATE", "T_DAILY_AVG")



