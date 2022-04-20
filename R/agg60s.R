#' agg60s
#'
#' @param data Data frame including time stamps and step metrics to be aggregated
#' @param epoch Epoch length in which the original data are sampled
#'
#' @return New data frame aggregated per 60s
#' @export
agg60s = function(data = c(), epoch = c()) {
  require(zoo)
  rolling_window = 60 / epoch

  # New data frame
  newData = as.data.frame(matrix(NA, nrow = nrow(data)/rolling_window, ncol = ncol(data)))
  colnames(newData) = colnames(data)
  data$epoch_i = rep(1:nrow(newData), each = rolling_window)

  # loop through columns
  for (i in 1:ncol(newData)) {
    if (is.numeric(data[, i])) newData[, i] = aggregate(data[, i] ~ data$epoch_i, FUN = sum)[, 2]
    if (!is.numeric(data[, i])) {
      keep = which(c(1, diff(data$epoch_i)) == 1)
      newData[, i] = data[keep, i]
    }
  }
  # return
  return(newData)
}
