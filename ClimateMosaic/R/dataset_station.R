#' Weather stations in the U.S.
#'
#' Lists weather stations maintained by the National Centers for Environmental
#' Information (NCEI) with their ids, names, longitudes, latitudes, states, and
#' regions.
#'
#' @format a dataframe with 236 rows and 6 variables:
#' \describe{
#'        \item{station_id}{station identifier of the form 'XXXXX'}
#'        \item{longitude}{station longitude in decimal degrees}
#'        \item{latitude}{station latitude in decimal degrees}
#'        \item{state}{state of the station}
#'        \item{station_name}{station name (e.g. AZ_Tucson_11_W)}
#'        \item{contiguous}{whether the station is within the contiguous US (i.e., all
#'                                        states and regions except for Alaska, Hawaii, and Puerto
#'                                        Rico)}
#' }
#' @source National Centers for Environmental Information (NCEI):
#'  \url{https://www.ncei.noaa.gov/pub/data/uscrn/products/daily01/}
"station_df"
