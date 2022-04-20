#' detect_midnights
#'
#' @param t Vector of timestamps in which look for midnights
#'
#' @return
#' @export
#'
#' @examples
detect_midnights = function(t = c()) {
  mnightsi = grep("00:00:00", t, fixed = T)
  # IF ONLY 1 DAY...
  if (length(mnightsi) < 2) {
    day = rep(1, times = length(t))
    return(list(midnightsi = mnightsi, day = day))
  }
  # ELSE
  day = rep(NA, times = length(t))
  d = 1
  #Loop through the rest of the days
  for (j in 1:length(mnightsi)) {
    if (j < length(mnightsi)) {
      if (j == 1 & mnightsi[j] > 1) {
        day[1:mnightsi[j]] = d
      } else if (j == 1 & mnightsi[j] == 1) {
        next()
      } else {
        day[mnightsi[j - 1]:mnightsi[j]] = d
      }
      d = d + 1
    } else {
      day[mnightsi[j - 1]:mnightsi[j]] = d; d = d + 1
      day[mnightsi[j]:length(day)] = d
    }
  }
  return(list(midnightsi = mnightsi, day = day))
}
