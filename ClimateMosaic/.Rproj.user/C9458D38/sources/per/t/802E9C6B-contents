#' Plots the gridded interpolations on the contiguous US map.
#'
#' Plots the grid points, defined by longitudes and latitudes, and the interpolations
#' at the points in the contiguous US (i.e., excluding Alaska, Hawaii, Puerto Rico).
#'
#' @details
#' The vibe argument determines the color theme:
#' \itemize{
#'        \item \code{"neutral"} rainbow color palette
#'        \item \code{"cold"} cold color palette
#'        \item \code{"hot"} warm color palette
#'        \item \code{"split_zero"} white for values around zero, red for positive values,
#'                                          and blue for negative values.
#' }
#'
#' @param interpolations a dataframe of three columns: longitude and latitude of the
#' grid point (in decimal degrees) and interpolation at the point.
#' @param title main title for the resulting plot.
#' @param vibe (optional) color theme of the plot. Should be one of 'neutral', 'cold',
#' 'hot', 'split_zero'. Default is 'neutral'. See Details.
#' @param breaks (optional) legend breaks for the plot. Should be specified along with
#' labels with the same length.
#' @param labels (optional) legend labels for the plot. Should be specified along with
#' breaks with the same length.
#' @param caption (optional) caption for the plot. Default is NULL.
#'
#' @importFrom rlang sym
#'
#' @examples
#' # generate grid
#' data("climate_df")
#' grid_points <- create_grid(0.7)
#' # get minimum temperature on 2023-12-31 by station
#' df <- climate_df[climate_df$lst_date == as.Date("2023-12-31"),
#'                             c("longitude", "latitude", "t_daily_min")]
#' # interpolate temperature to grid points
#' interpolations <- interpolate_data(df, grid_points)
#' # to see coldness around US
#' plot_interpolations(interpolations,
#' title = "Interpolated Min. Temperature on 2023-12-31", vibe = "cold")
#' # to see temperature above or below zero
#' plot_interpolations(interpolations,
#' title = "Interpolated Min. Temperature on 2023-12-31", vibe = "split_zero")
#'
#' # get maximum temperature on 2023-07-31 by station
#' df <- climate_df[climate_df$lst_date == as.Date("2023-07-31"),
#'                             c("longitude", "latitude", "t_daily_max")]
#' # interpolate temperature to grid points
#' interpolations <- interpolate_data(df, grid_points)
#' plot_interpolations(interpolations,
#' title = "Interpolated Max. Temperature on 2023-07-31", vibe = "hot")
#'
#' @export
plot_interpolations <- function(interpolations, title, vibe = "neutral",
                                                       breaks = NULL, labels = NULL, caption = NULL) {

  # validate inputs
  if (!inherits(interpolations, "data.frame")) {
    stop("'interpolations must be a data frame.")
  }
  if (ncol(interpolations) != 3){
    stop("interpolations must have three columns: longitude, latitude, and data.")
  }
  interpolations <- stats::na.omit(interpolations)
  if (!all(sapply(interpolations, is.numeric))){
    stop("All columns of 'interpolations' should be numeric.")
  }
  if (! vibe %in% c("neutral", "hot", "cold", "split_zero")){
    stop("vibe should be one of 'neutral', 'hot, 'cold', 'split_zero'.")
  }

  colnames(interpolations) <- c("longitude", "latitude", "dat")

  # get US map
  us_map <- ggplot2::map_data("state")
  us_map <- us_map[!us_map[[5]] %in% c("alaska", "hawaii", "puerto rico"), ]
  colnames(us_map) <- c("long", "lat", "group", "order", "region", "subregion")

  # deal with non-standard evaluation in ggplot2
  longitude <- sym("longitude")
  latitude <- sym("latitude")
  dat <- sym("dat")
  long <- sym("long")
  lat <- sym("lat")
  group <- sym("group")

  # basic styles
  gg <- ggplot2::ggplot() +
    ggplot2::geom_raster(data = interpolations,
                                          ggplot2::aes(x =!!longitude, y = !!latitude, fill= !!dat),
                                          interpolate = FALSE) +
    ggplot2::theme_minimal() +
    ggplot2::geom_polygon(data = us_map, ggplot2::aes(x = !!long, y = !!lat, group = !!group),
                                           fill = NA, color = "black") +
    ggplot2::labs(fill = NULL, title = title, x = "Longitude", y = "Latitude", caption = caption) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 10), size = 12),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 10), size = 12),
      plot.title = ggplot2::element_text(margin = ggplot2::margin(b = 10), size = 14)
    ) +
    ggplot2::coord_quickmap() # aspect ratio adjustment

  # fill in colors according to vibe
  colors_vector <- c("#00008F", "#000099", "#0000A3", "#0000AE", "#0000B8", "#0000C2",
                     "#0000CC", "#0000D6", "#0000E0", "#0000EA", "#0000F6", "#0000FF",
                     "#0009FF", "#0015FF", "#001FFF", "#0029FF", "#0033FF", "#003DFF",
                     "#0047FF", "#0051FF", "#005CFF", "#0066FF", "#0070FF", "#007AFF",
                     "#0084FF", "#008EFF", "#0098FF", "#00A2FF", "#00ACFF", "#00B6FF",
                     "#00C0FF", "#00CBFF", "#00D5FF", "#00DFFF", "#00E9FF", "#00F4FF",
                     "#00FEFF", "#08FFF7", "#13FFEC", "#1DFFE2", "#27FFD8", "#31FFCE",
                     "#3CFFC3", "#46FFB9", "#50FFAF", "#5AFFA5", "#64FF9B", "#6FFF90",
                     "#79FF87", "#83FF7D", "#8CFF73", "#96FF69", "#A0FF5F", "#ABFF54",
                     "#B5FF4A", "#BFFF40", "#C9FF36", "#D3FF2C", "#DEFF21", "#E7FF18",
                     "#F2FF0D", "#FDFF02", "#FFF900", "#FFED00", "#FFE300", "#FFD900",
                     "#FFCF00", "#FFC500", "#FFBB00", "#FFB000", "#FFA600", "#FF9C00",
                     "#FF9200", "#FF8800", "#FF7F00", "#FF7400", "#FF6A00", "#FF6000",
                     "#FF5600", "#FF4C00", "#FF4100", "#FF3700", "#FF2D00", "#FF2300",
                     "#FF1900", "#FF0E00", "#FF0300", "#FB0000", "#EF0000", "#E50000",
                     "#DB0000", "#D00000", "#C60000", "#BC0000", "#B20000", "#A80000",
                     "#9E0000", "#930000", "#890000", "#800000")

  if (vibe == "hot"){
    # warm color palette
    colors <- colors_vector[60:100]
  } else if (vibe == "cold"){
    # cold color palette
    colors <- rev(colors_vector[1:40])
  } else if (vibe == "neutral"){
    # rainbow color palette
    colors <- colors_vector
  }
  if (vibe != "split_zero"){
    if (!is.null(breaks) && !is.null(labels)){
      gg <- gg + ggplot2::scale_fill_gradientn(colors = colors, na.value = "transparent",
                                                                            breaks = breaks, labels = labels)
    } else{
      gg <- gg + ggplot2::scale_fill_gradientn(colors = colors, na.value = "transparent")
    }
  } else {
    if (!is.null(breaks) && !is.null(labels)){
      gg <- gg + ggplot2::scale_fill_gradient2(low = "#0369a1",
                                                                            high = "#b91c1c",
                                                                            mid = "white",
                                                                            midpoint = 0,
                                                                            breaks = breaks, labels = labels,
                                                                            na.value = "transparent")
    } else{
      gg <- gg + ggplot2::scale_fill_gradient2(low = "#0369a1",
                                                                            high = "#b91c1c",
                                                                            mid = "white",
                                                                            midpoint = 0,
                                                                            na.value = "transparent")
    }
  }

  print(gg)

}

