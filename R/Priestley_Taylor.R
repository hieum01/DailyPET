#' Priestley-Taylor PET method
#'
#' Estimating daily PET based on using the Priestley-Taylor PET estimation method
#'
#' @param JD Julian day
#' @param lat Latitude in degree decimal (ex: 56.1)
#' @param Ele Elevation above mean sea level in meter
#' @param tmin Daily minimum temperature
#' @param tmax Daily maximum temperature
#' @param Relhum Relative humidity in ratio
#' @param Rs Mean daily solar radiation. If Rs=NULL, Rs is esimated in MJ/m2/day.If Rs_unit is not 'MJm2day' but 'W/m2', Rs_Unit should be "Wm2'
#' @param Rs_Unit Unit of Rs. If Rs_Unit='Wm2', it converts to MJ/m2/day. Default is Rs_Unit='MJm2day'.
#'
#' @return Daily PET in mm/day
#' @export
#'
#' @examples
#
PET_Priestley_Taylor<- function (JD,lat,Ele,tmin,tmax,Relhum,Rs=NULL,Rs_Unit='MJm2day') {
  Day.Length<-length(JD)
  Tave<- (tmin + tmax)/2

  #====================================================
  # Incoming solar radiation in MJ/m2/day
  #====================================================
  if(is.null(Rs)) {
    Rs<-Solar(lat, Jday=JD, tmax, tmin) # Estimated solar radiation in MJ/m2/day
  } else if (Rs_Unit!='MJm2day') {
    Rs<-Rs*0.0864 # converting W/m2 to MJ/m2/day
  }
  #====================================================
  # Slope of saturation vapor pressure curve
  #====================================================
  Delta<-4098*(0.6108*exp(17.27*Tave/(Tave+237.3)))/((Tave+273.3)^2)
  #====================================================
  # Atmosphere pressure in kPa
  #====================================================
  P=AP.kPa(Tave,Relhum,Ele)  # Atmp.Pressure in kPa
  #====================================================
  # Psychrometric constant
  #====================================================
  Gamma.pc<-0.000665*P
  #====================================================
  # Saturation and partial vapor pressure
  #====================================================
  Es.Tmin<-SVP.ClaCla(tmin)/10  #kPa
  Es.Tmax<-SVP.ClaCla(tmax)/10  #kPa
  Es<-(Es.Tmin+Es.Tmax)/2  # Mean saturation vapor pressure in kPa

  Ea<-Relhum*Es # Partial vapor pressure in kPa
  #====================================================
  # Net solar (shortwave) radiation
  #====================================================
  alpha<-0.23 #Albedo or canopy reflection coefficient
  Rns=(1-alpha)*Rs  # MJ/m2/day
  #====================================================
  # Net Outgoing longwave solar radiation
  #====================================================
  Rnl<-Net_Out_Longwave(JD,Ele,lat,tmin,tmax,Ea,Rs)
  #====================================================
  # Net radiation
  #====================================================
  Rn=Rns-Rnl
  if(Day.Length>2) {
    Rn[which(Rn<0)]<-0
  } else {
    if(Rn<0) Rn=0
  }

  #==============================================================================
  # PET calculation

  Alpha<-1.26 #Priestley-Taylor coefficient
  Lambda<-2.45 # latent heat of vapourization = 2.45 (MJ/kg)

  ET0 <-(Alpha*(Delta/(Delta+Gamma.pc))*Rn)/Lambda
  #==============================================================================
  ET0 <- ifelse(ET0 < 0, 0, ET0)
  return(ET0)
}
