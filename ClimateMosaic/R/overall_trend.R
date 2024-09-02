#' Estimates the overall trend of temperature for a weather station.
#'
#' Fits a linear model of a temperature-related variable (daily average, minimum,
#' etc.) versus year and harmonics on day of year using user-provided data. It returns
#' the slope coefficient for year along with the standard error and p-value for the
#' estimated coefficient. See "Details" for more about the model and assumptions.
#'
#' @details The model is:
#' \deqn{h(t, y) = A_0 + Cy+ \sum_{k=1}^M \left[ A_k \cos(\omega_k t) +
#'  B_k \sin(\omega_k t)\right],} where \eqn{t} is the day of year from 1 to
#'  \eqn{ N = 365}, \eqn{\omega_k = \frac{2\pi k}{N} } and \eqn{M} is the number of
#'  harmonics used; \eqn{y} is the year, and \eqn{C} is the slope coefficient for year.
#'  Typically \eqn{M = 2} is the most suitable for daily temperature data though
#'  it depends. Smaller \eqn{M} might be prone to under-fitting, and larger \eqn{M}
#'  might result in over-fitting.
#'  Assumptions for the model is:
#'  \itemize{
#'        \item \code{Linearity}: The relationship between year and the response
#'                                                is linear when holding day of year constant. Also,
#'                                                there is an annual cycle within each year that can be
#'                                                captured by harmonics on day of year.
#'        \item \code{Independence}: Observations should be independent of each other.
#'        \item \code{Homoscedasticity}:  The variance of the errors (residuals) should
#'        be constant across all levels of the predictor variables.
#'        \item \code{Normality of Errors}: The errors (residuals) should be normally
#'         distributed.
#'}
#'
#' @param dat a dataframe of two columns: the first is the date of the observation as
#' Date objects; the second is the observation for the temperature-related variable
#' to be modeled.
#' @param M (optional) an positive integer indicating the number of harmonics on day
#' of year to be used in the model. Default is 2.
#' @return a list of:
#' \itemize{
#'     \item \code{trend} estimated slope coefficient for the year variable.
#'     \item \code{se} standard deviation of the estimated coefficient for year.
#'     \item \code{p_value} p value for the estimated coefficient for year.
#' }
#'
#' @examples
#' t_dat <- extract_data(climate_df[,-c(2,3,5)], id = "53150", var = "t_daily_avg")
#' stats <- overall_trend(t_dat[,4:5])
#' print(stats$trend)
#' print(stats$rse)
#' print(stats$p_value)
#' @export
overall_trend <- function(dat, M = 2){

  # validate inputs
  if (!inherits(dat, "data.frame")) {
    stop("dat must be a data frame.")
  }
  if (ncol(dat) != 2){
    stop("dat must have two columns: date and observation value.")
  }
  if (!(is.numeric(M) && (M == floor(M)))){
    stop("M must be an integer.")
  }
  if (M <= 0){
    stop("M should be positive")
  }
  tryCatch({
    dates <- as.Date(dat[[1]])
  }, error = function(e) {
    stop("Invalid date format in the first column of dat. Error: ",
         e$message)
  })
  if (!inherits(dat[[2]], "numeric")) {
    stop("Second column of dat should be numeric")
  }

  # scale day of number for leap years to decimals from 1 to 365
  dat$day_of_year <- as.numeric(format(dates, "%j"))
  leap <- lubridate::leap_year(lubridate::year(dates))
  leap_seq <- seq(1, 365, length.out = 366)
  dat[leap, ]$day_of_year <- leap_seq[dat[leap, ]$day_of_year]
  dat$year <- lubridate::year(dat$lst_date)

  # fit linear model of year + harmonics on day of year
  formula <- paste0(colnames(dat)[2], "~")
  for (i in 1:M){
    m <- i*2
    formula <- paste0(formula, "cos(", m, "*pi*day_of_year/365)+sin(", m,
                                  "*pi*day_of_year/365)+")
  }
  formula <- paste0(formula, "year")
  lm.fit <- stats::lm(formula, data = dat)

  # get coefficient, SE, and p value
  trend <- stats::coef(lm.fit)["year"]
  se <- summary(lm.fit)$coefficients["year", "Std. Error"]
  p_value <- summary(lm.fit)$coefficients["year", "Pr(>|t|)"]

  return(list(trend = trend, se = se, p_value = p_value))
}
