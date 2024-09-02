test_that("yearly_cycle validates inputs as desired",{

  dat <- extract_data(climate_df[,-c(2,3,5)], "64758", "t_daily_avg")[, 4:5]
  expect_error(yearly_cycle(c(1,2)))
  expect_error(yearly_cycle(dat, M = 4.5))
  expect_error(yearly_cycle(dat, M = "4"))
  expect_error(yearly_cycle(dat, M = -1))

  dat_bad <- dat
  dat_bad$extra <- NA
  expect_error(yearly_cycle(dat_bad))

  dat_bad <- dat
  dat_bad$t_daily_avg <- "non_numeric"
  expect_error(yearly_cycle(dat_bad))

  dat_bad <- dat
  dat_bad$lst_date <- "77889900"
  expect_error(yearly_cycle(dat_bad))
})


test_that("yearly_cycle returns a data frame with the correct columns and rows", {
  station_id <- "64758"
  dat <- extract_data(climate_df[,-c(2,3,5)], station_id, "t_daily_avg")
  pred <- yearly_cycle(dat[,4:5], M = 2)
  expect_equal(names(pred), c("day_of_year", "t_expected"))
  expect_equal(nrow(pred), 365)

  pred <- yearly_cycle(dat[,4:5], M = 4)
  expect_equal(names(pred), c("day_of_year", "t_expected"))
  expect_equal(nrow(pred), 365)
})
