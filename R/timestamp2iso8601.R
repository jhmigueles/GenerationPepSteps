#' timestamp2iso8601
#'
#' @param x Character vector with timestamps
#' @param tz Character, timezone in which the data were collected as in http://en.wikipedia.org/wiki/Zone.tab
#' @param time_format Character string giving a date-time format as used by \link[base]{strptime}.
#'
#' @return
#' @export
#'
#' @examples
timestamp2iso8601 = function(x,tz = "", time_format = c()){
  # timeformats to try ----
  if (is.null(time_format)) {
    tryFormats = c("%Y-%m-%d %H:%M:%OS", "%Y/%m/%d %H:%M:%OS",
                   "%Y-%m-%d %H:%M:%S", "%Y/%m/%d %H:%M:%S",
                   "%Y-%m-%d %H:%M", "%Y/%m/%d %H:%M",

                   "%m/%d/%Y %H:%M:%OS", "%m-%d-%Y %H:%M:%OS",
                   "%m/%d/%Y %H:%M:%S", "%m-%d-%Y %H:%M:%S",
                   "%m/%d/%Y %H:%M", "%m-%d-%Y %H:%M",

                   "%d/%m/%Y %H:%M:%OS", "%d-%m-%Y %H:%M:%OS",
                   "%d/%m/%Y %H:%M:%S", "%d-%m-%Y %H:%M:%S",
                   "%d/%m/%Y %H:%M", "%d-%m-%Y %H:%M")
  }
  # format conversion to iso 8601 ----
  if (is.null(time_format)) {
    POStime = as.POSIXlt(as.numeric(as.POSIXlt(x,tz, tryFormats = tryFormats)), origin = "1970-01-01", tz)
  } else if(!is.null(time_format)) {
    POStime = as.POSIXlt(as.numeric(as.POSIXlt(x,tz, format = time_format)), origin = "1970-01-01", tz)
  }
  POStimeISO = strftime(POStime,format = "%Y-%m-%dT%H:%M:%S%z")
  return(POStimeISO)
}
