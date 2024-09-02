test_that("interpolate_data validates inputs as desired", {
  grid_points <- create_grid(0.7)
  df <- climate_df[climate_df$lst_date == as.Date("2024-01-01"),
                              c("longitude", "latitude", "t_daily_max")]
  grid_bad <- grid_points
  grid_bad$extra <- NA
  expect_error(interpolate_data(1:10, grid_points))
  expect_error(interpolate_data(df, 1:10))
  expect_error(interpolate_data(df, grid_bad))
}
)

test_that("interpolate_data returns a data frame with the correct columns
          and number of rows", {
  grid_points <- create_grid(0.5)
  df <- climate_df[climate_df$lst_date == as.Date("2024-01-01"),
                   c("longitude", "latitude", "t_daily_max")]
  interpolations <- interpolate_data(df, grid_points)
  expect_equal(colnames(interpolations),c("longitude", "latitude", "t_daily_max"))
  expect_equal(nrow(interpolations), nrow(grid_points))
})
