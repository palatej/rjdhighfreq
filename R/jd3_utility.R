#' Title
#'
#' @param group 
#' @param contrasts 
#' @param period 
#' @param startYear 
#' @param startPeriod 
#' @param length 
#'
#' @return
#' @export
#'
#' @examples
jd3_calendar<-function(group, contrasts=TRUE, period, startYear, startPeriod=1, length=0){
 if (length == 0)
   length = period*20
 jdom<-tsdomain_r2jd(period, startYear, startPeriod, length)
 jm<-.jcall("demetra/calendar/r/GenericCalendars", "Ldemetra/math/matrices/MatrixType;", "td", jdom, as.integer(group), contrasts)
 return(matrix_jd2r(jm))
}