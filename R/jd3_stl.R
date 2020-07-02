#' @nclude jd3_rslts.R
#' @import rJava
NULL

#' Title
#'
#' @param y 
#' @param period 
#' @param multiplicative 
#' @param swindow 
#' @param twindow 
#' @param robust 
#'
#' @return
#' @export
#'
#' @examples
stl<-function(y, period, multiplicative=TRUE, swindow=7, twindow=0, robust=TRUE){
  jrslt<-.jcall("demetra/stl/r/StlDecomposition", "Ldemetra/stl/r/StlDecomposition$Results;", "process", as.numeric(y), as.integer(period), multiplicative, as.integer(swindow), as.integer(twindow), robust)
  decomposition<-list(
    y=as.numeric(y),
    t=proc_vector(jrslt, "t"),
    sa=proc_vector(jrslt, "sa"),
    s=proc_vector(jrslt, "s"),
    i=proc_vector(jrslt, "i")
  )
  parameters<-list(
    multiplicative=multiplicative, 
    swindow=swindow, 
    twindow=twindow, 
    robust=robust
  )

  return(structure(list(
    decomposition=decomposition,
    parameters=parameters),
    class="JDSTL"))
}

#' Title
#'
#' @param y 
#' @param window 
#' @param degree 
#' @param jump 
#'
#' @return
#' @export
#'
#' @examples
loess<-function(y, window, degree=1, jump=1){
  if (degree != 0 && degree != 1)
    stop("Unsupported degree")
  if (jump <1)
    stop("jump should be greater then 0")
  return (.jcall("demetra/r/StlDecomposition", "[D", "loess", as.numeric(y), as.integer(window), as.integer(degree), as.integer(jump)))
}

