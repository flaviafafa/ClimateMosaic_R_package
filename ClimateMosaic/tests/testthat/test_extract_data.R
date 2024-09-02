test_that("extract_data returns errors when inputs not valid", {

  df <- climate_df[, -c(2,3,5)]
  df_bad <- df
  df_bad[[2]] <- "non-numeric"

  expect_error(extract_data(df, 53878, var = "t_daily_avg"))
  expect_error(extract_data(df, "1234", var = "t_daily_avg"))
  expect_error(extract_data(df, "53878", var = "random"))
  expect_error(extract_data(df, "53878", var = "t_daily_avg", starting_date = "03-77"))
  expect_error(extract_data(df, "53878", var = "t_daily_avg", ending_date = "1110"))
  expect_error(extract_data(df_bad, "53878", var = "t_daily_avg"))

})

test_that("extract_data returns a data frame with the correct columns", {
  id <- "64758"
  expect_equal(
    colnames(extract_data(dat = climate_df[, -c(2,3,5)], id, var = "t_daily_min")),
    c("station_id", "longitude", "latitude", "lst_date", "t_daily_min")
  )
  expect_equal(
    ncol(extract_data(climate_df[,-c(2,3,5)], id, var = c("p_daily_calc", "t_daily_avg"))),
    6)
})

test_that("extract_data returns rows as desired",{
  station_id <- "53152"
  dat <- extract_data(dat = climate_df[,-c(2,3,5)],"53152", var = "t_daily_avg",
                                    starting_date = "2024-03-01", ending_date = "2024-03-31")
  expect_equal(length(unique(dat$station_id)), 1)
  expect_equal(length(unique(dat$longitude)), 1)
  expect_equal(length(unique(dat$latitude)), 1)
  expect_equal(class(dat$lst_date), "Date")
  expect_equal(sum(dat$lst_date <= as.Date("2024-03-31")), nrow(dat))
  expect_equal(sum(dat$lst_date >= as.Date("2024-03-01")), nrow(dat))
  expect_equal(class(dat$t_daily_avg), "numeric")
})
