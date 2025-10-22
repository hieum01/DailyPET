#' Ordin' PET estimation
#'
#' Estimating daily PET based on min/max temperature
#'
#' More details on the the Ordin's method, please refer to Oudin, L., Hervieu, F., Michel, C., Perrin, C., Andr?assian, V.,
#' Anctil, F. & Loumagne, C. (2005) Which potential evapotranspiration input for a lumped rainfall-runoff model? Part 2:
#' Towards a simple and efficient potential evapotranspiration model for rainfall-runoff modelling.
#' J. Hydrol. 303(1-4), 290-306

#' @param tmin Daily minimum temperature
#' @param tmax Daily maximum temperature
#' @param JD Julian day
#' @param lat Latitude in degree decimal (ex: 56.1)
#'
#' @return Daily PET in mm/day
#' @export
#'
#' @examples Ordin(15,25,as.Date("2011-11-30"),54.0)
#'
Ordin <- function (tmin, tmax, JD, lat) {
  n <- length(tmin)
  ET0 <- tmin * NA
  Tave<- (tmin + tmax)/2
  Tr <- tmax - tmin
  Tr <- ifelse(Tr < 0, 0, Tr)
  Ra<-Extraterrestrial_Solar(JD,lat) #[MJ/m2/day]
  Ra <- ifelse(Ra < 0, 0, Ra)
  Lambda<-2.45 #(MJ/kg)
  D_water<-0.9997 # (kg/m3)

  ET0<-sapply(1:n, function(i) {
    if ((Tave[i]+5) > 0) {
      PET<-Ra[i]/(Lambda*D_water)*(Tave[i]+5)/100
    } else {
      PET<- 0
    }
    return(PET)
  })

  return(ET0)
}
