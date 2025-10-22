#' Simplified Priestley-Taylor PET method
#'
#' Estimating daily PET based on min/max temperature using the simplified Priestley-Taylor PET estimation method
#' Solar radiation and net radiation are calculated by an empirical equation suggested by Alberta Climate Information Service.
#'
#' @references
#' Alberta Climate Information Service (ACIS). 2014. Estimating solar radiation using daily maximum and minimum temperature. Alberta Agriculture and Forestry.
#' Edmonton, AB. https://www.acis.alberta.ca/acis/docs/Estimating-solar-radiation-using-daily-max-and-min-temperatures-data-y2014_m06_d13.pdf
#'
#' @param JD Julian day
#' @param lat Latitude in degree decimal (ex: 56.1)
#' @param Elev Elevation above mean sea level in meter
#' @param tmin Daily minimum temperature
#' @param tmax Daily maximum temperature
#'
#' @return Daily PET in mm/day
#' @export
#'
#' @examples
#' JD=Julian_Day(as.Date("2011-11-30"))
#' PET_PT_AF(JD=JD,lat=54.0,Elev=700,tmin=15,tmax=25)
#'
PET_PT_AF <- function (JD,lat,Elev=700,tmin,tmax) {
  n <- length(tmin)
  ET0 <- tmin * NA
  Tave<- (tmin + tmax)/2
  Tr <- tmax - tmin
  Tr <- ifelse(Tr < 0, 0, Tr)

  Ra <- Extraterrestrial_Solar(JD,lat) # Extraterrestrial Radiation in [MJ/m2/day]

  # Incoming shortwave solar radiation (MJ/m2/d)
  # Alberta Climate Information Service (ACIS). 2014. Estimating solar radiation using
  # daily maximum and minimum temperature. Alberta Agriculture and Forestry.
  # Edmonton, AB. https://agriculture.alberta.ca/acis/docs/estimating-solarradiation-
  # using-daily-max-and-min-temperatures-data-y2014-m06-d13.pdf
  # Accessed: June 2016.
  # adjusted coefficient k=0.16 ACIS(2014)
  Rs<-0.16*sqrt(Tr)*Ra

  # net radiation (MJ/m2/d)
  Rn<-0.63*Rs-3.456

  #==============================================================================
  # PET calculation

  # slope of the saturation vapour pressure vs. temperature curve (kPa/?C)
  Delta<-4098*(0.6108*exp(17.27*Tave/(Tave+237.3)))/(Tave+237.3)^2
  P<-101.3*((293-0.0065*Elev)/293)^5.26 # Atmospheric pressure [kPa]
  Alpha<-1.26 #Priestley-Taylor coefficient
  G<-0 # soil heat flux (MJ/m2/d)
  Gamma<- 0.665*10^(-3)*P # psychrometric constant (kPa/degree C)
  Lambda<-2.45 # latent heat of vapourization = 2.45 (MJ/kg)

  ET0 <-(Alpha*(Delta/(Delta+Gamma))*Rn)/Lambda
  #==============================================================================
  ET0 <- ifelse(ET0 < 0, 0, ET0)
  return(ET0)
}
