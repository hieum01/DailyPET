#' Net outgoing longwave radiation
#'
#' @param JD Julian day
#' @param Ele Elevation in meter
#' @param lat Latitude in decimal degrees
#' @param Tmin Daily minimum temperature in degree Celsius
#' @param Tmax Daily maximum temperature in degree Celsius
#' @param e Partial water vapor pressure in KPa
#' @param Rs Incoming solar (shortwave) radiation in MJ/m2/day or W/m2
#' @param Rs_unit Unit of Rs in character: 'Wm2' or 'MJm2day'
#'
#' @return Rnl [MJ/m2/day]
#' @export
#'
#' @examples
Net_Out_Longwave<- function (JD, Ele,lat,Tmin,Tmax,e,Rs,Rs_unit='MJm2day') {
  T0 <- 273.16
  Tmin.K<-Tmin+T0
  Tmax.K<-Tmax+T0

  Ra<-Extraterrestrial_Solar(JD,lat) # Extraterrestrial solar radiation [MJ/m2/day]

  Rso<-(0.75+(2*10^(10^-5))*Ele)*Ra  # Clear skay solar radiation [MJ/m2/day]

  if(Rs_unit!='MJm2day') {
    Rs<-Rs*0.0864  # converting W/m2 to MJ/m2/day
  }
  Sigma<- 4.903*10^-9  # Stefan-Boltzmann constant in MJ/K4/m2/day

  Rnl<-Sigma*((Tmax.K^4+Tmin.K^4)/2)*(0.34-0.14*sqrt(e))*((1.358*Rs/Rso)-0.35)

  Rnl <- ifelse(Rnl < 0, 0, Rnl)
  return(Rnl)
}
