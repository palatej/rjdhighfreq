#' @nclude jd3_rslts.R
#' @import rJava
NULL

#' Title
#'
#' @param object 
#' @param id 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
setGeneric(name="saDecomposition", def = function( object, id, ... ){standardGeneric("saDecomposition")})

#' Title
#'
#' @param object 
#' @param id 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
setGeneric(name="result", def = function( object, id, ... ){standardGeneric("result")})

#' Title
#'
#' @param object 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
setGeneric(name="dictionary", def = function( object, ... ){standardGeneric("dictionary")})

setClass(
  Class="JD3_Object",
  representation = representation(internal = "jobjRef" )
)

setClass(
  Class="JD3_ProcResults",
  contains = "JD3_Object"
)

setMethod("dictionary", "JD3_ProcResults", function(object){
  if (is.null(object@internal)){
    NULL
  }else{
    proc_dictionary(.jclass(object@internal))
  }
  
})

setMethod("result", signature = c(object="JD3_ProcResults", id="character"), function(object, id){
  if (is.null(object@internal)){
    NULL
  }else{
    proc_data(object@internal, id)}
})

