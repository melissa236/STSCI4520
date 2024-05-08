library(testthat)
library(dplyr)
library(lubridate)

dat <- data.frame(
  STATION_NAME = c("Asheville 13 S", "Asheville 13 S", "Asheville 13 S"),
  LST_DATE = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")),
  T_DAILY_AVG = c(5, 6, 7)
)

test_that("yearly_cycle returns a dataframe with correct columns", {
  result <- yearly_cycle("Asheville 13 S", 2024)
  expect_true("day_of_year" %in% names(result))
  expect_true("T_DAILY_AVG" %in% names(result))
  expect_true("YEAR" %in% names(result))
})


test_that("yearly_cycle computes daily averages correctly", {
  dat <- rbind(dat, data.frame(
    STATION_NAME = "Asheville 13 S",
    LST_DATE = as.Date("2024-01-01"),
    T_DAILY_AVG = 15
  ))
  result <- yearly_cycle("Asheville 13 S", 2024)
  expected_avg <- mean(c(5, 15))
  computed_avg <- result %>% filter(day_of_year == 1) %>% pull(T_DAILY_AVG)
  expect_equal(computed_avg, expected_avg)
})
