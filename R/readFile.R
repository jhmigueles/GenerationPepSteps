#' readFile
#'
#' @param file Character with the full path to the file to be read
#' @param mon Character indicating the monitor used, options are FIBION, Scriin, ActiGraph, Other
#'
#' @return Data frame with the accelerometer data information
#' @export
readFile = function(file = c(), mon = c()) {
  if (mon == "FIBION" | mon == "ActiGraph") acc = utils::read.csv(file)
  if (mon == "Scriin") acc = tryCatch(utils::read.delim(file, sep = "|", header = F),
                                      error = function(cond){return = NA})
  if (mon == "Other") stop("To be developed")
  return(acc)
}
