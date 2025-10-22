#' Modified Hargreaves' PET estimation
#'
#' Estimating daily PET with the modified Hargreaves' method with min/max temperature and monthly precipitation
#'
#' More details on the the modified Hargreaves' method, please refer to Droogers P., Allen, R., 2002,
#' Estimating reference evapotranspiration under inaccurate data conditions, Irrgation and Drainage System, 16, 33-45.
#'
#' @param tmin Daily minimum temperature
#' @param tmax Daily minimum temperature
#' @param times Date (ex: 1950-01-01) in Date format
#' @param lat Latitude in degree decimal (ex: 56.1)
#' @param pr Monthly precipitation [mm/month]
#' @param monthlyDate Year and month in a date format (YYYY-MM)
#' @param ...
#'
#' @return Daily PET in mm/day
#' @export
#'
#' @examples
Mod_Hargreave_Day <- function (tmin, tmax, times, lat,pr,monthlyDate,...) {
  n <- length(tmin)
  Yr.month <- format(times,"%Y-%m")
  monthlyDate2<-format(monthlyDate,"%Y-%m")
  #monthlyDate<-as.Date(format(monthlyDate,"%Y-%m"))
  pr.date<-cbind.data.frame(monthlyDate2,pr)
  colnames(pr.date)<-c("Date","monthly_pr")
  ET0 <- tmin * NA
  Tave<- (tmin + tmax)/2
  Tr <- tmax - tmin
  Tr <- ifelse(Tr < 0, 0, Tr)
  J <- as.numeric(format(times,"%j"))
  delta <- 0.409 * sin(0.0172 * J - 1.39)
  dr <- 1 + 0.033 * cos(0.0172 * J)
  latr <- lat/57.2957795
  sset <- -tan(latr) * tan(delta)
  omegas <- sset * 0
  omegas[sset>={-1} & sset<=1] <- acos(sset[sset>={-1} & sset<=1])
  omegas[sset<{-1}] <- max(omegas)
  Ra <- 37.6 * dr * (omegas * sin(latr) * sin(delta) + cos(latr) * cos(delta) * sin(omegas))
  Ra <- ifelse(Ra < 0, 0, Ra)
  for (i in 1:n) {
    pr.selected<-pr.date$monthly_pr[Yr.month[i]==pr.date$Date]
    ET0[i]<- 0.0013 * 0.408 * Ra[i] * (Tave[i] + 17) * ((Tr[i]-0.0123*pr.selected)^0.76)
    ET0[i] <- ifelse(ET0[i] < 0, 0, ET0[i])
  }

  return(ET0)
}
