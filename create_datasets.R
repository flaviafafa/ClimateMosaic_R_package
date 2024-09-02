# Assume this script is in the same directory as the data folder.
base_path <- "CRND0103-202404080750"

library(dplyr)
library(lubridate)
library(ggplot2)
library(sp)
library(tools)
library(usethis)

years <- 2000:2024

climate_df <- data.frame()
station_df <- data.frame()

# loop over year folders and txt files to retrieve data
for (year in years) {
    folder_path <- file.path(base_path, as.character(year))
    txt_files <- list.files(path = folder_path, pattern = "\\.txt$", full.names = TRUE)
    for (file in txt_files) {
        # extract station info
        station_info <- tail(strsplit(file, "-")[[1]], 1)
        station_info <- substr(station_info, 1, nchar(station_info) - 4)
        state <- substr(station_info, 1, 2)
        station_name <- substr(station_info, 4, nchar(station_info))
        # extract temperature data
        file_data <- cbind(read.table(file)[,1:11], state, station_name)
        climate_df <- rbind(climate_df, file_data)
        station_data <- data.frame(
            station_id = c(file_data[1, 1]),
            longitude = file_data[1,4],
            latitude = file_data[1,5],
            state = state,
            station_name = station_name
      )
         station_df <- rbind(station_df,  station_data)
  }
}

colnames(climate_df) <- c("station_id", "lst_date", "crx_vn", "longitude",
                          "latitude", "t_daily_max", "t_daily_min", "t_daily_mean",
                          "t_daily_avg", "p_daily_calc", "solarad_daily", "state",
                          "station_name")

# In station_df, we found two stations with the same id but different names:
# Huslia_27_W and Huslia_27_E. Their latitudes are the same, and longitudes
# are different by 0.01. After investigating the data, we found that
# Huslia_27_W only has one row, which is for 2023/08/31. Everything for Huslia_27_E
# looks normal, and the data for 2023/08/31 at Huslia_27_E is exactly the same
# as that at Huslia_27_W except for the longitude. So we decided to remove
# Huslia_27_W from station_df and its data from climate_df (approved by Joe).

# The latitude of Huslia_27_W is 65.66 and the longitude is -155.47.

station_df <- distinct(station_df, station_id, .keep_all = TRUE)
climate_df <- filter(climate_df, !((latitude == 65.66) & (longitude == -155.47)))
reorder <- c("station_id", "state", "station_name", colnames(climate_df)[2:11])
climate_df <- climate_df[,reorder, drop = FALSE]


# add variable contiguous to station_df, indicating whether the station is within
# the contiguous US
us_map <- subset(map_data("usa"),
                 !region %in% c("alaska", "hawaii", "puerto rico"))

unique_regions <- unique(us_map$region)
combined_long <- c()
combined_lat <- c()

for (region in unique_regions) {
  region_data <- us_map[us_map$region == region, ]
  combined_long <- c(combined_long, region_data$long, NA)
  combined_lat <- c(combined_lat, region_data$lat, NA)
}

inside <-
  sp::point.in.polygon(station_df$longitude,
                                       station_df$latitude,
                                       combined_long, combined_lat)
station_df$contiguous <- inside == 1


# clean missing values
climate_df[, 8:13][climate_df[, 8:13] == -9999] <- NA
climate_df[, 5][climate_df[,5] == -9] <- NA


# convert to date
climate_df$lst_date <- ymd(climate_df$lst_date)


# convert station id to five-char string according to the documentation
climate_df$station_id <- sprintf("%05d", climate_df$station_id)
station_df$station_id <- sprintf("%05d", station_df$station_id)

head(climate_df)
head(station_df)


# save data sets as .rda files for compression purposes
setwd("ClimateMosaic/data")
use_data(station_df)
use_data(climate_df)

tools::resaveRdaFiles("climate_df.rda", compress = "xz")
tools::resaveRdaFiles("station_df.rda", compress = "xz")
