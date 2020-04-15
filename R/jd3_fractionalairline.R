#' @include jd3_procresults.R
#' @import rJava
NULL


#' Title
#'
#' @param y 
#' @param period 
#' @param adjust 
#' @param sn 
#'
#' @return
#' @export
#'
#' @examples
fractionalAirlineDecomposition<-function(y, period, adjust=T, sn=F){
  checkmate::assertNumeric(y, null.ok = F)
  checkmate::assertNumeric(period, len = 1, null.ok = F)
  checkmate::assertLogical(adjust, len = 1, null.ok = F)
  checkmate::assertLogical(sn, len = 1, null.ok = F)
  jrslt<-.jcall("demetra/highfreq/r/FractionalAirlineProcessor", "Ldemetra/highfreq/FractionalAirlineDecomposition;", "decompose", as.numeric(y), period, adjust, sn)
  
  decomposition<-list(
    y=as.numeric(y),
    t=proc_vector(jrslt, "t"),
    sa=proc_vector(jrslt, "sa"),
    s=proc_vector(jrslt, "s"),
    i=proc_vector(jrslt, "i")
  )
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
fractionalAirlineEstimation<-function(y, periods, x = NULL, mean = FALSE, outliers=NULL, criticalValue=6, precision=1e-12){
  checkmate::assertNumeric(y, null.ok = F)
  checkmate::assertNumeric(criticalValue, len = 1, null.ok = F)
  checkmate::assertNumeric(precision, len = 1, null.ok = F)
  checkmate::assertLogical(mean, len = 1, null.ok = F)
  if (is.null(outliers))
    joutliers<-.jnull("[Ljava/lang/String;")
  else
    joutliers=.jarray(outliers, "java.lang.String")
  jrslt<-.jcall("demetra/highfreq/r/FractionalAirlineProcessor", "Ldemetra/highfreq/FractionalAirlineEstimation;", "estimate", as.numeric(y), matrix_r2jd(x), mean, .jarray(periods), joutliers
                , criticalValue, precision)
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


