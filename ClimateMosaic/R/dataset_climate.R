#' Daily temperature data at weather stations
#'
#' Provides daily average, mean, minimum, maximum temperatures, total precipitation,
#' and total solar energy level at weather stations in the U.S. Also includes
#' information about the stations. Data is provided by the National Centers for
#' Environmental Information (NCEI) and covers 2000-11-14 to 2024-04-07 at the time
#' of creation.
#'
#' @format a dataframe with 113,4351 rows and 13 variables:
#' \describe{
#'        \item{station_id}{station identifier of the form 'XXXXX'}
#'        \item{state}{state of the station}
#'        \item{station_name}{station name (e.g. AZ_Tucson_11_W)}
#'        \item{lst_date}{Local Standard Time (LST) date of the observation}
#'        \item{crx_vn}{version number of the station data logger program that was in
#'                                  effect at the time of the observation}
#'        \item{longitude}{station longitude in decimal degrees}
#'        \item{latitude}{station latitude in decimal degrees}
#'        \item{t_daily_max}{maximum air temperature in degrees Celsius in the day}
#'        \item{t_daily_min}{minimum air temperature in degrees Celsius in the day}
#'        \item{t_daily_mean}{mean air temperature in degrees Celsius for the day,
#'                                            calculated by (t_daily_max + t_daily_min) / 2}
#'        \item{t_daily_avg}{average air temperture in degrees Celsius for the day}
#'        \item{p_daily_calc}{total amount of precipitation in mm}
#'        \item{solarad_daily}{total solar energy in MJ/meter^2}
#' }
#' @source See \url{https://www.ncei.noaa.gov/pub/data/uscrn/products/daily01/}
#' for data and documentation.
"climate_df"
