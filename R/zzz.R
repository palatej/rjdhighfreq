#' @import rJava
NULL

jd_clobj <- NULL

.onLoad <- function(libname, pkgname) {
  # For debugging: to see if Jars are effectively loaded
  # options(java.parameters = "-verbose:class")

  # TODO : devtools will look only in RJDemetra3\java for JAR files so copied them there too
  result <- .jpackage(pkgname, lib.loc=libname)
  if (!result) stop("Loading java packages failed")

  # what's your java  version?  Need > 1.5.0.
  jversion <- .jcall('java.lang.System','S','getProperty','java.version')
  if (jversion < "1.8.0") {
    stop(paste("Your java version is ", jversion,
               ".  Need 1.8.0 or higher.", sep=""))
  } else {
    cat("Java requirements fullfilled, found version ",jversion,"\n")
  }

  # often used while calling java
  jd_clobj <<- .jcall("java/lang/Class", "Ljava/lang/Class;", "forName", "java.lang.Object")
}
