#' @include jd3_rslts.R
#' @import rJava
NULL


#' Title
#'
#' @param y 
#' @param period Period of the seasonal component
#' @param adjust True if an actual fractional airline model is used. False if the period is rounded to the nearest integer
#' @param sn Signal/noise decomposition. The signal is the seasonally adjusted series and the noise the seasonal component
#' @param stde True if standard deviations of the components must be computed. In some cases (memory limits), it is currently not possible to compute them
#' @param nbcasts Number of backcasts
#' @param nfcasts Number of forecasts
#'
#' @return
#' @export
#'
#' @examples
fractionalAirlineDecomposition<-function(y, period, adjust=T, sn=F, stde=F, nbcasts=0, nfcasts=0){
  checkmate::assertNumeric(y, null.ok = F)
  checkmate::assertNumeric(period, len = 1, null.ok = F)
  checkmate::assertLogical(adjust, len = 1, null.ok = F)
  checkmate::assertLogical(sn, len = 1, null.ok = F)
  jrslt<-.jcall("demetra/highfreq/r/FractionalAirlineProcessor", "Ldemetra/highfreq/FractionalAirlineDecomposition;", "decompose", as.numeric(y), 
                period, adjust, sn, stde, as.integer(nbcasts), as.integer(nfcasts))
  
  yc<-proc_vector(jrslt, "y")
  sa<-proc_vector(jrslt, "sa")
  s<-proc_vector(jrslt, "s")
  if (sn){
    if (stde){
      decomposition<-list(
        y=yc,
        sa=sa,
        s=s,
        s.stde=proc_vector(jrslt, "s_stde")
      )
    }else{
      decomposition<-list(
        y=yc,
        sa=sa,
        s=s
      )
    }
  }else{
    t<-proc_vector(jrslt, "t")
    i<-proc_vector(jrslt, "i")
    if (stde){
      decomposition<-list(
        y=yc,
        t=t,
        sa=sa,
        s=s,
        i=i,
        t.stde=proc_vector(jrslt, "t_stde"),
        s.stde=proc_vector(jrslt, "s_stde"),
        i.stde=proc_vector(jrslt, "i_stde")
      )
    }else{
      decomposition<-list(
        y=yc,
        t=t,
        sa=sa,
        s=s,
        i=i
      )
      
    }
  }
  estimation<-list(
    parameters=proc_vector(jrslt, "parameters"),
    score=proc_vector(jrslt, "score"),
    covariance=proc_matrix(jrslt, "pcov")
  )
  likelihood<-proc_likelihood(jrslt, "likelihood.")
  
  return(structure(list(
                   decomposition=decomposition,
                   estimation=estimation,
                   likelihood=likelihood),
         class="JDFractionalAirlineDecomposition"))
}


#' Title
#'
#' @param y 
#' @param periods 
#' @param x 
#' @param mean 
#' @param outliers 
#' @param criticalValue 
#'
#' @return
#' @export
#'
#' @examples
fractionalAirlineEstimation<-function(y, periods, x = NULL, mean = FALSE, outliers=NULL, criticalValue=6, precision=1e-12, approximateHessian=F){
  checkmate::assertNumeric(y, null.ok = F)
  checkmate::assertNumeric(criticalValue, len = 1, null.ok = F)
  checkmate::assertNumeric(precision, len = 1, null.ok = F)
  checkmate::assertLogical(mean, len = 1, null.ok = F)
  if (is.null(outliers))
    joutliers<-.jnull("[Ljava/lang/String;")
  else
    joutliers=.jarray(outliers, "java.lang.String")
  jrslt<-.jcall("demetra/highfreq/r/FractionalAirlineProcessor", "Ldemetra/highfreq/FractionalAirlineEstimation;", "estimate", as.numeric(y), matrix_r2jd(x), mean, .jarray(periods), joutliers
                , criticalValue, precision, approximateHessian)
  model<-list(
    y=as.numeric(y),
    variables=proc_vector(jrslt, "variables"),
    X=proc_matrix(jrslt, "regressors"),
    b=proc_vector(jrslt, "b"),
    bcov=proc_matrix(jrslt, "bvar"),
    linearized=proc_vector(jrslt, "lin")
  )
  estimation<-list(
    parameters=proc_vector(jrslt, "parameters"),
    score=proc_vector(jrslt, "score"),
    covariance=proc_matrix(jrslt, "pcov")
  )
  likelihood<-proc_likelihood(jrslt, "likelihood.")
  
  return(structure(list(
    model=model,
    estimation=estimation,
    likelihood=likelihood),
    class="JDFractionalAirlineEstimation"))
}


