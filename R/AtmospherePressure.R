#' Atmosphere pressure
#'
#' Calculating atmosphere pressure in kPa
#'
#' @param Tair Temperature in degree Celcius
#' @param Rh Relative humidity [ratio]
#' @param Elev Elevation in meter
#'
#' @return Atmp.pressure in kPa
#' @export
#'
#' @examples
AP.kPa <-function(Tair,Rh,Elev){
    library(bigleaf)
    SVP<-SVP.ClaCla(Tair) # saturation water vapor at diurnal temperatures in hectopascal (hPa)
    WVP=WVP.RH.T(Rh,Tair)
    VPD= (SVP-WVP)/10 #Vapor pressure deficit (kPa) 1kPa=10hPa
    Atmp.pressure<-pressure.from.elevation(Elev,Tair,VPD) #kPa

    return(Atmp.pressure)
  }
