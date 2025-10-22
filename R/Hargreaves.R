#' Hargreaves' PET estimation
#'
#' Estimating daily PET based on min/max temperature
#'
#' @param tmin Daily minimum temperature
#' @param tmax Daily maximum temperature
#' @param JD Julian day
#' @param lat Latitude in degree decimal (ex: 56.1)
#'
#' @return Daily PET in mm/day
#' @export
#'
#' @examples Hargreaves_Day(15,25,as.Date("2011-11-30"),54.0)
#'
Hargreaves_Day <- function (tmin, tmax, JD, lat) {
  n <- length(tmin)
  ET0 <- tmin * NA
  T<- (tmin + tmax)/2
  Tr <- tmax - tmin
  Tr <- ifelse(Tr < 0, 0, Tr)
  Ra<-Extraterrestrial_Solar(JD,lat) #[MJ/m2/day]
  Ra <- ifelse(Ra < 0, 0, Ra)
  ET0 <- 0.0023 * 0.408 * Ra * (T + 17.8) * Tr^0.5
  ET0 <- ifelse(ET0 < 0, 0, ET0)
  return(ET0)
}
