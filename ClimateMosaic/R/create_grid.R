#' Creates a grid of points within the contiguous US.
#'
#' Starting and ending latitudes and longitudes and/or resolution of the grid can be
#' controlled. Returns a data frame of longitudes, latitudes, and elevation (if
#' desired) at the grid points within the contiguous USA (all states/regions excluding
#'  Alaska, Hawaii, and Puerto Rico).
#'
#' @details
#' The US map might be slightly different across sources. The map used here comes from
#' the ggplot2 package.
#'
#' @param width (optional) Distance in decimal degrees between two neighboring points
#' on the grid. Default is 0.5.
#' @param start_lat (optional) Starting latitude in decimal degrees, ranging 25.1299
#' to 49.3832. Should be smaller than end_lat if both specified. Default is NULL,
#' indicating 25.1299 (southernmost point of contiguous US).
#' @param end_lat (optional) Ending latitude in decimal degrees, range 25.1299 to
#' 49.3832. Should be larger than start_lat if both specified. Default is NULL,
#'  indicating 49.3832 (northernmost point of contiguous US).
#' @param start_lon (optional) Starting longitude in decimal degrees, range -124.6813
#' to -67.0074. Should be smaller than end_lon if both specified. Default is NULL,
#' indicating -124.6813 (westernmost point of contiguous US).
#' @param end_lon (optional) Ending longitude in decimal degrees, range -124.6813 to
#' -67.0074. Should be larger than start_lon if both specified. Default is NULL,
#'  indicating -67.0074 (easternmost point of contiguous US).
#' @param elevation (optional) Boolean indicating whether to include the elevation
#   at the grid points in the output dataframe. Default is FALSE.
#' @return a data frame with the following columns:
#' \itemize{
#'     \item \code{latitude} Latitude in decimal degrees of the grid point.
#'     \item \code{longitude} Longitude in decimal degrees of the grid point.
#'     \item \code{elevation} Elevation in meters at the grid point (will only be
#'     provided if user specifies elevation = TRUE).
#' }
#'
#' @examples
#' grid <- create_grid(width = 0.7)
#' head(grid)
#' @export
create_grid <- function(width = 0.5, start_lat = NULL,
                        end_lat = NULL, start_lon = NULL, end_lon = NULL, elevation = FALSE){

  # get US map
  usa_map <- subset(ggplot2::map_data("usa"),
                    !region %in% c("alaska", "hawaii", "puerto rico"))

  # validate inputs
  if (!inherits(width, "numeric")) {
    stop("width should be a decimal.")
  }
  if (is.null(start_lon)) {
    start_lon <- min(usa_map$long)
  } else if (start_lon > max(usa_map$long) || start_lon < min(usa_map$long)) {
    stop(paste0("start_lon must be in [", min(usa_map$long), ",", max(usa_map$long),
                "]."))
  }
  if (is.null(end_lon)) {
    end_lon <- max(usa_map$long)
  } else if (end_lon > max(usa_map$long) || end_lon < min(usa_map$long)){
    stop(paste0("end_lon must be in [", min(usa_map$long), ",", max(usa_map$long),
                "]."))
  }
  if (start_lon > end_lon){
    stop("end_lon must be larger than start_lon")
  }

  if (is.null(start_lat)) {
    start_lat <- min(usa_map$lat)
  } else if (start_lat > max(usa_map$lat) || start_lat < min(usa_map$lat)) {
    stop(paste0("start_lat must be in [", min(usa_map$lat), ",", max(usa_map$lat),
                "]."))
  }

  if (is.null(end_lat)) {
    end_lat <- max(usa_map$lat)
  } else if (end_lat > max(usa_map$lat) || end_lat < min(usa_map$lat)){
    stop(paste0("end_lat must be in [", min(usa_map$lat), ",", max(usa_map$lat),
                "]."))
  }
  if (start_lat > end_lat){
    stop("end_lat must be larger than start_lat")
  }

  # generate grid points
  longitudes <- seq(start_lon, end_lon, by = width)
  latitudes <- seq(start_lat, end_lat, by = width)
  grid_points <- expand.grid(longitude = longitudes, latitude = latitudes)

  unique_regions <- unique(usa_map$region)
  combined_long <- c()
  combined_lat <- c()

  for (region in unique_regions) {
    region_data <- usa_map[usa_map$region == region, ]
    combined_long <- c(combined_long, region_data$long, NA)
    combined_lat <- c(combined_lat, region_data$lat, NA)
  }

  # get points within contiguous USA
  inside <-
    sp::point.in.polygon(grid_points$longitude,
                         grid_points$latitude,
                         combined_long,
                         combined_lat)
  inside_points <- grid_points[inside == 1,]

  if (elevation){
    locs <- data.frame(x = inside_points$longitude, y = inside_points$latitude)
    elevation_df <- elevatr::get_elev_point(locations = locs,
                                prj = "+proj=longlat +datum=WGS84", src = "aws")
    inside_points$elevation <- elevation_df$elevation
  }

  return(inside_points)
}
