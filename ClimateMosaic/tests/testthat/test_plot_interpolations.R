test_that("plot_interpolations checks inputs as desired",{

  grid_points <- create_grid(1)
  df <- climate_df[climate_df$lst_date == as.Date("2020-01-01"), c("longitude",
                                                                   "latitude", "t_daily_max")]
  interpolations <- interpolate_data(df, grid_points)

  expect_error(plot_interpolations(1:10, ""))
  expect_error(plot_interpolations(interpolations[-1], ""))
  df_bad <- interpolations
  df_bad[[1]] <- "non-numeric"
  expect_error(plot_interpolations(df_bad, ""))
  expect_error(plot_interpolations(df, "", vibe = "no vibe"))
}
)


test_that("plot_interpolations does not produce error", {
  grid_points <- create_grid()
  df <- climate_df[climate_df$lst_date == as.Date("2020-01-01"), c("longitude",
                                                                   "latitude", "t_daily_max")]
  interpolations <- interpolate_data(df, grid_points)
  breaks <- seq(min(interpolations$t_daily_max, na.rm = T),
                         max(interpolations$t_daily_max, na.rm = T),
                         length.out = 3)
  labels <- breaks
  pdf(NULL)
  plot_interpolations(interpolations, title = "Test plot_interpolations",
                      breaks = breaks, labels = labels)
  dev.off()
  expect_equal("all good", "all good")
})
