#'
#'
#'
#'
#'
#'


temp_trend <- function(data, date_column, temp_column, degree = 2) {

  data$YEAR <- year(data[[date_column]])
  yearly_data <- data %>%
    group_by(YEAR) %>%
    summarize(AVG_TEMP = mean(.data[[temp_column]], na.rm = TRUE), .groups = 'drop')

  model_formula <- as.formula(paste("AVG_TEMP ~ poly(YEAR, ", degree, ")"))
  model <- lm(model_formula, data = yearly_data)

  prediction_years <- data.frame(YEAR = seq(min(yearly_data$YEAR), max(yearly_data$YEAR), length.out = 200))
  prediction_years$PREDICTED_TEMP <- predict(model, newdata = prediction_years)

  ggplot(prediction_years, aes(x = YEAR, y = PREDICTED_TEMP)) +
    geom_line(color = "blue", size = 1.5) +
    labs(title = "Yearly Temperature Trend Model (Polynomial Regression)",
         x = "Year", y = "Temperature (°C)") +
    theme_minimal()
}

#temp_trend(dat, "LST_DATE", "T_DAILY_AVG", 2)



