#' Penman-Monteith
#'
#' Calculation of the the Penman-Monteith potential evapotraspiration (PET) estimation (FAO-56 method)
#'
#' @references
#' Zotarelli, L., Dukes, M.D., Romero, C.C., Migliaccio, K.W., 2010. Step by Step Calculation of the Penman-Monteith Evapotranspiration (FAO-56 Method). IFAS Extension, University of Florida. .
#' Allen, R.G., Pereira, L.S., Raes, D., Smith, M., 1998. Crop evapotranspiration-Guidelines for computing crop water requirements-FAO Irrigation and drainage paper 56. Fao, Rome 300, D05109.
#' @author Hyung Eum (hyung.eum@gov.ab.ca)
#'
#' @param JD Julian day
#' @param lat Latitude in decimal degrees
#' @param Ele Elevation in meter
#' @param tmin Daily minimum temperature in degree Celsius
#' @param tmax Daily maximum temperature in degree Celsius
#' @param wind Wind speed in m/sec
#' @param Relhum Relative humidity in ratio
#' @param Rs Mean daily solar radiation. If Rs=NULL, Rs is estimated in MJ/m2/day.If Rs_unit is not 'MJm2day' but 'W/m2', Rs_Unit should be "Wm2'
#' @param wind.height Height of wind speed measurement above the ground surface in meter (default=2)
#' @param Rs_Unit Unit of Rs. If Rs_Unit='Wm2', it converts to MJ/m2/day. Default is Rs_Unit='MJm2day'.
#'
#' @return
#' @export
#'
#' @examples
PET.PM.FAO<-function (JD,lat,Ele,tmin,tmax,wind,Relhum,Rs=NULL,wind.height=2,Rs_Unit='MJm2day') {
  Cn<-900  # Numerator constant for the refrence crop type (short crop=900 and Tall crop=1600)
  Cd<-0.34 # Denominator constant for the refrence crop type (short crop/daily=0.34 and Tall crop/daily=0.38)

  Day.Length<-length(JD)
  Tave<-(tmin+tmax)/2
  #JDay <- strptime(Date.Seq, format="%Y-%m-%d")$yday+1 #Julian day
  #====================================================
  # Incoming solar radiation in MJ/m2/day
  #====================================================
  if(is.null(Rs)) {
    Rs<-Solar(lat, Jday=JD, tmax, tmin) # Estimated solar radiation in MJ/m2/day
    #Rs<-Solar.daily*0.0864  # converting W/m2 to MJ/m2/day
  } else if (Rs_Unit!='MJm2day') {
    Rs<-Rs*0.0864 # converting W/m2 to MJ/m2/day
  }
  #====================================================
  # wind speed
  #====================================================
  if(wind.height!=2) {
    u2=4.87*wind/(log(67.8*wind.height-5.42))
  } else {
    u2=wind
  }
  #====================================================
  # Slope of saturation vapor pressure curve
  #====================================================
  Delta<-4098*(0.6108*exp(17.27*Tave/(Tave+237.3)))/((Tave+273.3)^2)

  #====================================================
  # Atmosphere pressure
  #====================================================
  P=AP.kPa(Tave,Relhum,Ele)  # Atmp.Pressure in kPa

  #====================================================
  # Psychrometric constant in kPa/degree C
  #====================================================
  Gamma.pc<-0.000665*P
  #====================================================
  # Saturation and partial vapor pressure
  #====================================================
  Es.Tmin<-SVP.ClaCla(tmin)/10 #kPa
  Es.Tmax<-SVP.ClaCla(tmax)/10 #kPa
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
  Rnl<-Net_Out_Longwave(JD, Ele,lat,tmin,tmax,Ea,Rs)
  #====================================================
  # Net radiation
  #====================================================
  Rn=Rns-Rnl
  if(Day.Length>2) {
    Rn[which(Rn<0)]<-0
  } else {
    if(Rn<0) Rn=0
  }
  #====================================================
  # Delta Term (DT)
  #====================================================
  DT=Delta/(Delta+Gamma.pc*(1+Cd*u2))
  #====================================================
  # Psi Term (PT)
  #====================================================
  PT=Gamma.pc/(Delta+Gamma.pc*(1+Cd*u2))
  #====================================================
  # Temperature Term (TT)
  #====================================================
  TT=(Cn/(Tave+273))*u2
  #====================================================
  # ET radiation term
  #====================================================
  ET.rad<-0.408*Rn*DT
  #====================================================
  # ET wind term
  #====================================================
  ET.wind<-PT*TT*(Es-Ea)
  #====================================================
  # Daily PET: mm/day
  #====================================================
  ET0=ET.rad+ET.wind
  return(ET0)
}
