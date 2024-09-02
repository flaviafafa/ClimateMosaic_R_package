test_that("create_grid validates inputs as desired", {

  expect_error(create_grid(width = 1, start_lat = 25))
  expect_error(create_grid(width = 1, end_lat = 50))
  expect_error(create_grid(width = 1, start_lon = -140))
  expect_error(create_grid(width = 1, end_lon = -65))
  expect_error(create_grid(width = "1"))
  expect_error(create_grid(width = 1, start_lat = 40, end_lat = 30))
  expect_error(create_grid(width = 1, start_lon = -70, end_lon = -120))
})


test_that("create_grid returns a data frame with the correct columns", {
  grid_points <- create_grid(width = 0.5, start_lat = 30,
                             end_lat = 40, start_lon = -100, end_lon = -80)
  expect_equal(names(grid_points), c("longitude", "latitude"))
  expect_equal(min(grid_points$longitude), -100)
  expect_equal(max(grid_points$longitude), -80)
  expect_equal(min(grid_points$latitude), 30)
  expect_equal(max(grid_points$latitude), 40)

  grid_points <- create_grid(width = 0.5, start_lat = 30,
                             end_lat = 40, start_lon = -100, end_lon = -80, elevation = T)
  expect_equal(names(grid_points), c("longitude", "latitude", "elevation"))
})
