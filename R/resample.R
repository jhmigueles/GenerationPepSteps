#' resample
#'
#' @param data Data frame including time stamps and step metrics to be resampled
#' @param timestamp_col Character or numeric indicating the column where the timestamp is stored
#' @param timestamp_format Character string giving a date-time format as used by \link[base]{strptime}.
#' @param id_col Character or numeric indicating the column where the ID is stored. Leave blank if the ID is not stored
#' @param mon Character indicating what monitor was used. Options are Fibion, Scriin, ActiGraph, Other.
#'
#' @return Data frame resampled
#' @export
#'
#' @examples
resample = function(data = c(), timestamp_col = c(), timestamp_format = c(),
                    id_col = c(), mon = c()){
  require(lubridate)
  ts = data[, timestamp_col]
  if (timestamp_format != "iso") {
    ts_iso = timestamp2iso8601(ts, tz = "Europe/Stockholm", time_format = timestamp_format)
  } else {
    ts_iso = ts
    if (mon == "FIBION") ts_iso = gsub(" ", "", ts_iso)
  }

  ts_posix = suppressMessages(lubridate::as_datetime(ts_iso, tz = "Europe/Stockholm"))


  # fill day 0?
  d0_first_ts = c(lubridate::hour(ts_posix[1]), lubridate::minute(ts_posix[1]), lubridate::second(ts_posix[1]))
  d0_ts0 = ts_posix[1] - d0_first_ts[3] - (60*d0_first_ts[2]) - (60*60*d0_first_ts[1])

  # fill last day?
  last = length(ts)
  d1_last_ts = c(lubridate::hour(ts_posix[last]), lubridate::minute(ts_posix[last]), lubridate::second(ts_posix[last]))

  if (any(d1_last_ts != c(23, 59, 0))) {
    d1_ts1 = ts_posix[last] + (1440*60 - d1_last_ts[3] - (60*d1_last_ts[2]) - (60*60*d1_last_ts[1]) - 60)
  } else if (any(d1_last_ts == c(23, 59, 0))) {
    d1_ts1 = d1_last_ts
  }

  # resample
  ts_posix_resampled = seq.POSIXt(from = d0_ts0, to = d1_ts1, by = "min")
  ts_iso_resampled = timestamp2iso8601(ts_posix_resampled, tz = "Europe/Stockholm")

  # new data frame
  newData = as.data.frame(matrix(data = 0, ncol = ncol(data), nrow = length(ts_iso_resampled)))
  if (is.character(timestamp_col)) colnames(newData) = colnames(data)
  newData[,timestamp_col] = ts_iso_resampled
  if (length(id_col) > 0) newData[,id_col] = data[1, id_col]
  if (mon == "FIBION") {
    newData[, 1] = timestamp2iso8601(ts_posix_resampled, tz = "utc")
    newData[, 3] = seq(from = data[1,3], by = 6000, length.out = nrow(newData))
    for (i in 4:ncol(newData)) {
      newData[, i] = c(rep(data[, i], each = 15) / 15, rep(0, times = nrow(newData) - (nrow(data)*15)))
    }
  } else if (mon == "Scriin") {
    measured = which(ts_iso_resampled %in% ts_iso)
    newData[measured,] = data
  } else {
    stop("Resampling is only available for FIBION and Scriin devices at the moment.")
  }


  # return
  return(newData)
}
