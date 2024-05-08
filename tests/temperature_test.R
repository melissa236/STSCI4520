library(testthat)

test_data <- data.frame(
  LST_DATE = as.Date(c("2010-01-01", "2010-12-31", "2011-01-01", "2011-12-31", "2012-01-01")),
  T_DAILY_AVG = c(10, 12, 15, 14, 18)
)

test_that("temp_trend runs without errors", {
  expect_error(
    temp_trend(test_data, "LST_DATE", "T_DAILY_AVG"),
    regexp = NA  
  )
})

