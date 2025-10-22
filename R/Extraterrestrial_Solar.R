#' Extraterrestrial solar radiation
#'
#' Calculating potential solar radiation in [MJ/m2/day] given day of the year and latitude
#'
#' @param JD Julian day
#' @param lat Latitude in degrees
#'
#' @return Extraterrestrial solar radiation [MJ/m2/day]
#' @export
#'
#' @examples
Extraterrestrial_Solar<-function(JD,lat) {
  Gsc<-0.0820  # solar constant in MJ/m2/min
  delta <- 0.409 * sin(0.0172 * JD - 1.39)
  dr <- 1 + 0.033 * cos(0.0172 * JD)
  latr <- lat/57.2957795
  sset <- -tan(latr) * tan(delta)
  omegas <- sset * 0
  omegas[sset>={-1} & sset<=1] <- acos(sset[sset>={-1} & sset<=1])
  omegas[sset<{-1}] <- max(omegas)
  Ra <- (24*60*Gsc/pi) * dr * (omegas * sin(latr) * sin(delta) + cos(latr) * cos(delta) * sin(omegas))
  Ra <- ifelse(Ra < 0, 0, Ra) #Extraterrestrial Radiation
}
