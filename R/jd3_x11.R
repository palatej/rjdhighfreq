#' @include jd3_rslts.R
#' @import rJava
NULL


#' Title
#'
#' @param y 
#' @param period 
#' @param mul 
#' @param trend.horizon 
#' @param trend.degree 
#' @param trend.kernel 
#' @param trend.asymmetric 
#' @param seas.s0 
#' @param seas.s1 
#' @param extreme.lsig 
#' @param extreme.usig 
#'
#' @return
#' @export
#'
#' @examples
x11<-function(y, period, mul=TRUE, trend.horizon=6, trend.degree=2,
                  trend.kernel=c("Henderson", "BiWeight", "TriWeight", "TriCube", "Uniform", "Triangular", "Epanechnikov", "Trapezoidal"),
                  trend.asymmetric=c("CutAndNormalize", "Direct", "MMSRE"),
                  seas.s0=c("S3X3", "S3X1", "S3X5", "S3X9", "S3X15"),
                  seas.s1=c("S3X5", "S3X3", "S3X1", "S3X9", "S3X15"),
                  extreme.lsig=1.5, extreme.usig=2.5){
  seas0=match.arg(seas.s0)
  seas1=match.arg(seas.s1)
  tkernel=match.arg(trend.kernel)
  asym=match.arg(trend.asymmetric)
  jrslt<-.jcall("demetra/saexperimental/r/X11Decomposition", "Ldemetra/saexperimental/r/X11Decomposition$Results;", "process", as.numeric(y), period, mul
                , as.integer(trend.horizon), as.integer(trend.degree),
                tkernel, asym, seas0, seas1, extreme.lsig, extreme.usig)
  decomposition<-list(
    y=as.numeric(y),
    t=proc_vector(jrslt, "d12"),
    sa=proc_vector(jrslt, "d11"),
    s=proc_vector(jrslt, "d10"),
    i=proc_vector(jrslt, "d13")
  )
  parameters<-list(
    multiplicative=mul, 
    trend.horizon=trend.horizon, 
    trend.degree=trend.degree,
    trend.kernel=trend.kernel,
    trend.asymmetric=trend.asymmetric,
    extreme.lsig=extreme.lsig, 
    extreme.usig=extreme.usig
  )
  
  return(structure(list(
    decomposition=decomposition,
    parameters=parameters),
    class="JDX11"))
}

#' Title
#'
#' @param y 
#' @param length 
#' @param musgrave 
#' @param ic 
#'
#' @return
#' @export
#'
#' @examples
henderson<-function(y, length, musgrave=TRUE, ic=4.5){
  return (.jcall("demetra/saexperimental/r/X11Decomposition", "[D", "henderson", as.numeric(y), as.integer(length), musgrave, ic))
}

# See Proietti-Luati [2008] (Real time estimation in local polynomial regression...) for the terminology
#' Title
#'
#' @param y 
#' @param horizon 
#' @param degree 
#' @param kernel 
#' @param endpoints 
#' @param ic 
#'
#' @return
#' @export
#'
#' @examples
localpolynomials<-function(y, horizon, degree=3, kernel=c("Henderson", "Uniform", "Biweight", "Triweight", "Tricube", "Gaussian", "Triangular", "Parabolic"), endpoints=c("DAF", "CC", "LC", "QL", "CQ"), ic=4.5){
  d<-2/(sqrt(pi)*ic)
  kernel=match.arg(kernel)
  endpoints=match.arg(endpoints)
  return (.jcall("demetra/saexperimental/r/LocalPolynomialFilters", "[D", "filter", as.numeric(y), as.integer(horizon), as.integer(degree), kernel, endpoints, d))
}

