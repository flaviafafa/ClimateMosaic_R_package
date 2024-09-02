test_that("overall_trend validates inputs as desired",{

  dat <- extract_data(climate_df[, -c(2,3,5)], "64758", "t_daily_avg")[, 4:5]
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


test_that("overall_trend returns a list with the correct attributes", {
  dat <- extract_data(climate_df[,-c(2,3,5)], "03047", "t_daily_avg")[,4:5]
  stats <- overall_trend(dat)
  expect_equal(names(stats), c("trend", "se", "p_value"))
  expect_equal(class(stats$trend), "numeric")
  expect_equal(class(stats$se), "numeric")
  expect_equal(class(stats$p_value), "numeric")
})
