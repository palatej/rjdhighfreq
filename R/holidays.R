#' @include jd3_holidays.R
#' @import rJava
NULL


# Defined in same way as ... in holidays function
predefinedCalendars <- list(
  CommonEU = {
    list(
      c(12,31),
      list("NewYear",offset=0:1),
      list("Christmas",offset=-1:1),
      "Easter","EasterMonday",
      "MayDay", "Ascension", "WhitMonday", "Assumption", "AllSaintsDay")
  },
  France=list("CommonEU", c(7,14), c(11,11),c(5,8)),
  Germany=list("CommonEU", c(10,3)),
  Belgium=list("CommonEU", c(7,21),c(11,11),c(5,8))
)

#'
#' List the prespecified holidays in JDemetra
#'
#'
#'
#' Note : should request it to JDemetra API, is currently hardcoded, copied from enum demetra.timeseries.calendars.DayEvent
#' @export
listPrespecifiedHolidays <- function() {
   c("NewYear","Christmas","ShroveMonday", "ShroveTuesday","AshWednesday","MaundyThursday","GoodFriday","Easter",
     "EasterMonday","MayDay","Ascension","Pentecost","CorpusChristi","WhitMonday","Assumption","AllSaintsDay",
     "Halloween", "Armistice")
}



#'
#' List some predefined calendars in this package.
#'
#' @export
listPredefinedCalendars <- function(onlyNames=T) {
  checkmate::assertLogical(onlyNames, len = 1, null.ok = F)

  if (onlyNames==F) return(predefinedCalendars)
  return(names(predefinedCalendars))
}


#'
#' Constructor for a holidays calendar
#'
#' @param ... a serie of holiday items. One of :
#' * a prespecified calender, see [listPredefinedCalendars()]
#' * a prespecified holiday, see [listPrespecifiedHolidays()]
#' * an integer vector of length 2 (month, day) indication a specific day
#' * a named list containing as first item one of the above possibilities (excluding a calender) with in addition optional the following named elementd :
#'    * _offset_ : to indicate multiple days around specified day
#'    * _weight_ : optional a weight
#'    * _gregorian_  : a logical
#'
#' @return [Holidays] a named list containing the calendar created in JDemetra. In addition it contains some labels and the used arguments to construct it.
#'
#'@examples
#'\dontrun{
#'   # Just one day on 12th june
#'   Holidays(c(6,12))
#'   # Just one day being a prespecified day called _Christmas°
#'   Holidays("Christmas")
#'   # combining both
#'   Holidays(c(6,12), "Christmas")
#'   # combining both but also the day before and after it included
#'   Holidays(list(c(6,12), offset=-1:1),
#'            list("Christmas", offset=-1:1))
#'   # Reuse default calendar already specified for Belgium
#'   Holidays("Belgium")
#'   # Reuse default calendar already specified for Belgium but add some more days
#'   Holidays("Belgium", c(6, 12))
#'}
#' @export
Holidays <- function(...) {
  jhol <- jd3_holidays()
  # TODO : Argument 'type' of holidaysMatrix could be moved to holidays calendar.
  # TODO : can infer this too
  allowedHolidayArgs <- c("offset","weight","gregorian")

  prespecifiedHolidays <- listPrespecifiedHolidays()
  prespecifiedCalendars <- listPredefinedCalendars()

  # return label and as side effect call JDemetra
  addToCalendar <- function(x) {
    # check item and add to JD3 Holidays
    if (is.character(x)) {
      # a name
      if (length(x)==1) {
        # could be a predefined calendar or a prespecified data in JDemetra
        if (x %in% prespecifiedHolidays) {
          add(jhol, x)
          return(x)
        }
        if (x %in% prespecifiedCalendars) {
          cal <- predefinedCalendars[[x]]
          return(lapply(cal, FUN = addToCalendar))
        }
        stop(x, " is not a predefined calendar or holiday name")
      } else {
        stop("provide a vector of calendar/holiday names directly as atomic elements in the list and not as nested vector")
      }
    }

    if (is.numeric(x)) {
      # too often not explicit specified as integer (truncate without warning)
      x <- as.integer(x)
      if (length(x)==2) {
        add(jhol, x)
        return(paste0(x, collapse = "/"))
      }
      stop("an integer vector must contain exactly 2 integers being the month and the day")
    }

    if (is.list(x)) {
      # first argument should be a numeric vector of length 2 or a prespecified holiday
      # remaining should be the arguments of JD3_Holidays add method with expection that offset could be non-atomic
      if (length(x)==0) {
        warning("Empty list element provided, is ignored")
        return(NULL)
      }
      item <- x[[1]]
      label <- NULL
      if (is.character(item)) {
        if ((item %in% prespecifiedHolidays)==F) stop(item, " is not a prespecified holiday")
        label <- item
      } else {
        label <- paste0(item,collapse = "/")
      }
      if (length(x)>1) {
        # remaining arguments must be named
        xargs <- x[-1]
        xargsn <- names(xargs)
        if (is.null(xargsn)) stop("when a list is provided as element, all its components should be named with exception of the first")
        if (all(xargsn %in% allowedHolidayArgs)==F) stop("Invalid names for list element provided : ", setdiff(xargsn, allowedHolidayArgs))
        # multiple offsets are allowed so iterate
        offsets <- xargs$offset
        if (is.null(offsets)) {
          offsets <- 0
        } else {
          xargs$offset <- NULL
        }
        return(lapply(offsets, FUN=function(offset) {
          do.call("add", args = c(list(jhol, item, offset=offset),xargs))
          sep <- if (offset>0) "+" else ""
          return(paste0(label,sep, offset))
        }))
      }
    }
    stop("Not expected to arrive here")
  }

  # Complement the calendar with each provided element
  labels <- lapply(list(...), FUN=addToCalendar)
  # remove NULLs and make flat
  labels <- unlist(labels, recursive = T)

  return(structure(list(ptr=jhol,
                        labels= labels,
                        args=list(...)),
                   class="Holidays"))
}



#'
#' Constructor for a holiday matrix in JDemetra, with time as rows and holidays as columns. A flag indicates if it is an holiday or not.
#'
#'@param a holidays calender specification constructed by [Holidays()]
#'@param startingDate a [Date] or a character to coerce to a Date being the first day as from which the matrix is made
#'@param length [integer] the number of days to include
#'@param type [character] specific treatment of sundays, days around working days ... See JDemetra.
#'
#'@return JDMatrix which is a named list containing the regression matrix created in JDemetra. In addition it contains the used arguments to construct it.
#'
#'@export
HolidaysMatrix<-function(holidays, startingDate, length, type=c("Default", "SkipSundays", "NextWorkingDay", "PreviousWorkingDay")){
  if (is.character(startingDate)) startingDate <- as.Date(startingDate)

  # TODO : is it limited to dates??
  checkmate::assertDate(startingDate, len = 1, null.ok = F)
  checkmate::assertInteger(length, len = 1, null.ok = F)
  checkmate::assertClass(holidays, classes = "Holidays", null.ok = F)
  type <- match.arg(type)

  jm <- .jcall(holidays$ptr@internal, "Ldemetra/math/matrices/MatrixType;", "holidays",
               format(startingDate, "%Y-%m-%d"),
               as.integer(length),
               type)

  return(structure(list(
                      ptr=matrix_jd2r(jm),
                      spec=list(holidays=holidays, startingDate=startingDate, type=type)),
                  class="JDMatrix"))
}





