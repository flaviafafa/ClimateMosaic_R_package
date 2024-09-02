#' Extracts time series data for a weather station.
#'
#' Extracts time series data of the specified variables for a given weather station
#' from user-provided data. Starting date and ending date for the time series can be
#' specified. Only the data available will be returned.
#'
#' @param dat dataframe with the following columns in sequence: station id
#' (five-character string), date of observation (Date object), state longitude,
#' state latitude, temperature-related variables of your interest (can be one or more).
#' @param id five-character string representing a weather station you want to extract
#' data at.
#' @param  var vector of variable name(s) for the time series. Should be one or more
#'  from the column names of the given dat.
#' @param  starting_date (optional) Date object for the starting date of the time
#' series. Default is "2000-01-01".
#' @param  ending_date (optional) Date object for the ending date of the time series.
#'  Default is "2024-04-30".
#' @return a data frame with the following variables:
#' \itemize{
#'     \item \code{station_id} station id of the specified station.
#'     \item \code{longitude} station longitude.
#'     \item \code{latitude} station latitude.
#'     \item \code{lst_date} date of the observation as Date object.
#'     \item \code{var} time-series data for the specified variables.
#' }
#'
#' @examples
#' # extract daily average and minimum temperatures at station "94078" for March, 2024
#' df <- extract_data(climate_df[,-c(2,3,5)], id = "94078",
#'                                 var = c("t_daily_avg", "t_daily_min"),
#'                                 starting_date = "2024/03/01", ending_date = "2024/03/31")
#' head(df)
#' @export
extract_data <- function(dat, id, var, starting_date = "2000-01-01",
                                          ending_date = "2024-04-30") {

  # validate inputs and display error messages if necessary
  # this only checks for common errors in inputs
  if (!inherits(dat[[2]], "Date")){
    stop("The second column of dat should be Date objects.")
  }
  if (!inherits(id, "character") || nchar(id) != 5) {
    stop("station_id must be a 5-character string.")
  }
  if (!all(var %in% colnames(dat))) {
    stop("var contains invalid variable names.")
  }
  tryCatch({
    start_date <- as.Date(starting_date)
  }, error = function(e) {
    stop("Invalid date format for starting_date. Error: ",
         e$message)
  })
  tryCatch({
    end_date <- as.Date(ending_date)
  }, error = function(e) {
    stop("Invalid date format for ending_date. Error: ",
         e$message)
  })
  if (start_date > end_date) {
    stop("starting_date must be before ending_date.")
  }

  # extract time series data
  colnames(dat)[1:4] <- c("station_id", "lst_date", "longitude", "latitude")

  result <- dat[dat$station_id == id & dat$lst_date >= start_date &
                          dat$lst_date <= end_date,
                          c("station_id", "longitude", "latitude", "lst_date", var)]

  return(result)

}


