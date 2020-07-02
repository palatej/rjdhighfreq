#' @include jd3_rslts.R jd3_ts.R
#' @import rJava
NULL

setClass(
  Class="JD3_Holidays",
  representation = representation(internal = "jobjRef" )
)

setGeneric(name="add", def = function( object, item, ...){standardGeneric("add")})

setMethod("add", signature = c(object="JD3_Holidays"), function(object, item, offset=0, weight=1, gregorian=TRUE){
  if ( is.jnull(object@internal) ){
    return (FALSE)
  }else{
    if (is.character(item)){
      ok<-.jcall(object@internal, "Z", "add", item, as.integer(offset), weight, !gregorian)
       return (ok)
    }else if (is.numeric(item)){
      if (length(item)!= 2)
        return (FALSE)
      ok<-.jcall(object@internal, "Z", "addFixedDay", as.integer(item[1]), as.integer(item[2]),
             weight, !gregorian)
      return (ok)
    }else
      return (FALSE)
  }
})

jd3_holidays<-function(){
  jrslt<-.jnew("demetra/calendar/r/Holidays")
  new (Class = "JD3_Holidays", internal = jrslt)
}



