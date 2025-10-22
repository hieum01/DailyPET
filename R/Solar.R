#' Solar radiation estimation
#'
#' Estimating daily solar radiation in KJ/m2/day or W/m2
#'
#' @param lat Latitude in degrees
#' @param Jday Julian date
#' @param Tx Daily maximum temperature in degree C
#' @param Tn Daily minimum temperature in degree C
#' @param albedo Albedo (default=0.2)
#' @param forest Forest ratio
#' @param slope Slope derived from DEM
#' @param aspect Aspect derived from DEM
#' @param units Solar radiation unit (default=MJm2d)
#' @param latUnits Unit of latitude: 'degrees' or 'radians' (default=degrees)
#' @param printWarn logical (TRUE or FALSE)
#'
#' @return Solar radiation in units (MJ/m2/day or W/m2)
#' @export
#'
#' @examples
Solar <-  function (lat, Jday, Tx, Tn, albedo=0.2, forest=0, slope=0, aspect = 0, units="MJm2d") {
  # library(insol)

  latr <- lat*pi/180 # lat in radian

  if (units == "MJm2d") convert <- 1 else convert <- 0.0864  # can convert to W/m2
  return( signif((1 - albedo) * (1 - forest) * transmissivity(Tx, Tn) *
                   Extraterrestrial_Solar(Jday,lat) * slopefactor(latr, Jday, slope, aspect) / convert , 5 ))
}
