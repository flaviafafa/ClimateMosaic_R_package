# before running this script, make sure your current working directory
# is ClimateMosaic and run build (toolbar at top) -> Install Package.

library(usethis)
library(devtools)
library(lubridate)
library(dplyr)
library(ggplot2)
library(maps)
library(mapdata)
library(sp)
library(fields)
library(maps)
# if any function has been changed, run this
devtools::load_all()
data("climate_df")

# if any test has been updated, run this
devtools::test()

# if any function documentation has been updated, run this
devtools::document()
?extract_data
?yearly_cycle
?create_grid
?overall_trend
?plot_interpolations
?interpolate_data

devtools::run_examples()

# make sure it returns errors when it should
devtools::load_all()

extract_data(12)
extract_data(12345)
extract_data("1234")
extract_data("53878", var = "random")
extract_data("53878", starting_date = "03-77")
extract_data("53878", ending_date = "1110")
temp <- extract_data(climate_df[,-c(2,3,5)], id = "53878",
                     var = "t_daily_avg", starting_date = "2024-03-01")


dat <- extract_data("64758", "t_daily_avg")[,4:5]
yearly_cycle(dat)
yearly_cycle(c(1,2))
yearly_cycle(dat, M = 4.5)
yearly_cycle(dat, M = "4")
yearly_cycle(dat, M = -1)

dat_bad <- dat
dat_bad$extra <- NA
yearly_cycle(dat_bad)

dat_bad <- dat
dat_bad$t_daily_avg <- "non_numeric"
yearly_cycle(dat_bad)

dat_bad <- dat
dat_bad$lst_date <- "77889900"
yearly_cycle(dat_bad)

devtools::load_all()
create_grid(width = 1, start_lat = 25)
create_grid(width = 1, end_lat = 50)
create_grid(width = 1, start_lon = -120)
create_grid(width = 1, end_lon = -65)
create_grid(width = "1")
create_grid(width = 1, start_lat = 40, end_lat = 30)
create_grid(width = 1, start_lon = -70, end_lon = -120)

grid_points <- create_grid(0.7)
df <- climate_df[climate_df$lst_date == as.Date("2024-01-01"), c("longitude",
                            "latitude", "t_daily_max")]
grid_bad <- grid_points
grid_bad$extra <- NA
interpolate_data(1:10, grid_points)
interpolate_data(df, 1:10)
interpolate_data(df, grid_bad)

grid_points <- create_grid(1)
df <- climate_df[climate_df$lst_date == as.Date("2020-01-01"), c("longitude",
                                                                 "latitude", "t_daily_max")]
interpolations <- interpolate_data(df, grid_points)
plot_interpolations(1:10, "")
plot_interpolations(interpolations[-1], "")
df_bad <- interpolations
df_bad[[1]] <- "non-numeric"
plot_interpolations(df_bad, "")
plot_interpolations(df, "", vibe = "no vibe")

devtools::load_all()
grid_points <- create_grid(1)
df <- climate_df[climate_df$lst_date == as.Date("2023-12-31"), c("longitude",
                           "latitude", "t_daily_min")]
interpolations <- interpolate_data(df, grid_points)

plot_interpolations(interpolations, title = "random", vibe = "split_zero", breaks = 0:5, labels = 0:5)
plot_interpolations(interpolations, title = "random", vibe = "neutral", breaks =0:5 , labels = 0:5)
plot_interpolations(interpolations, title = "random", vibe = "cold")
plot_interpolations(interpolations, title = "random", vibe = "hot", breaks = 0:5, labels = 0:5)

use_data(climate_df, overwrite = TRUE)
use_data(station_df, overwrite = TRUE)


df <- extract_data(climate_df[,-c(2,3,5)], id = "94078", var = c("t_daily_avg", "t_daily_min"),
         starting_date = "2024/03/01", ending_date = "2024/03/31")
climate_df[,-c(2,3,5)]

df_bad <- climate_df[, -c(2,3,5)]
df_bad[[2]] <- "non-numeric"
extract_data(df_bad, id = "53878", var = "t_daily_avg")

df <- climate_df[, -c(2,3,5)]
extract_data(df, id = "53878", var = "random")
