#' Interpolates data to the given grid.
#'
#' Interpolates climate data at the U.S. weather stations to the given grid points.
#' Latitudes and longitudes are the primary covariates and location variables. Other
#' covariates such as elevation can be provided. The package GpGp is used (see Details).
#'
#' @details
#' The functions fit_model and predictions from the package GpGp are used for
#' interpolating the data. For fit_model, "matern_sphere" is used as the argument
#' for covfun_name. See documentation:
#' \url{https://cran.r-project.org/web/packages/GpGp/GpGp.pdf}.
#'
#' @param df a dataframe containing training data. It has columns: longitudes and
#' latitudes of the stations in decimal degrees, respectively, other covariates if
#' any, observed values to be interpolated (must be the last column).
#' @param grid_points a dataframe containing the longitudes and latitudes in decimal
#' degrees (first two columns) and other covariates (if specified in df) of the grid
#' points to be interpolated to.
#'
#' @return a dataframe of three columns: longitude and latitude of the grid point, in
#'  decimal degrees, and interpolation at the point.
#'
#' @examples
#' # generate grid
#' grid_points <- create_grid(0.7)
#' # get minimum temperature on 2023-12-31 by station
#' df <- climate_df[climate_df$lst_date == as.Date("2023-12-31"), c("longitude",
#'                            "latitude", "t_daily_min")]
#' # interpolate temperature to grid points
#' interpolations <- interpolate_data(df, grid_points)
#' head(interpolations)
#' @export
#'
interpolate_data <- function(df, grid_points) {
  if (!inherits(df, "data.frame")) {
    stop("df must be a data frame.")
  }
  if (!inherits(grid_points, "data.frame")) {
    stop("grid_points must be a data frame.")
  }
  if (ncol(df) != ncol(grid_points) + 1){
    stop("df should have exactly one more column than grid_points")
  }

  # write formula
  covariates <- names(df)[-ncol(df)]
  formula <- stats::as.formula(paste("~", paste(covariates, collapse = " + ")))

  # fit the model
  grid_points <- as.matrix(grid_points)
  design_matrix <- stats::model.matrix(formula, data = df)
  fit <- GpGp::fit_model(y = df[[ncol(df)]],
                         locs = df[, 1:2],
                         X = design_matrix,
                         covfun_name = "matern_sphere",
                         silent = TRUE)

  # construct design matrix for prediction
  pred_X <- matrix(0, nrow = nrow(grid_points), ncol = ncol(design_matrix))
  colnames(pred_X) <- colnames(design_matrix)
  pred_X[,1] <- 1 # intercept
  pred_X[,2:ncol(pred_X)] <- as.matrix(grid_points)
  colnames(grid_points)[1:2] <- colnames(df)[1:2]

  # make predictions
  pred <- GpGp::predictions(fit = fit, locs_pred = grid_points[, 1:2],
                            X_pred = pred_X)
  interpolations <- as.data.frame(cbind(grid_points, pred = pred))
  colnames(interpolations)[ncol(interpolations)] <- colnames(df)[ncol(df)]
  return(interpolations)
}




