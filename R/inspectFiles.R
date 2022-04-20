#' inspectFiles
#'
#' @return List with datadir, outputdir, metadatadir, files, monitor, and format
#' @export
#'
#' @examples
inspectFiles = function(){
  # ask user what device is being analized (FIBION or Scriin)
  mon = menu(choices = c("FIBION", "Scriin", "ActiGraph", "Other"),
             title = "\nWhat monitor have you used to record steps?")
  if (mon == 1) {
    mon = "FIBION"
    format = "csv"
  } else if (mon == 2) {
    mon = "Scriin"
    format = "txt"
  } else if (mon == 3) {
    mon = "ActiGraph"
    format = menu(choices = c("agd", "csv"),
                  title = "\nWhat is the format of your files?")
    if (format == 1) format = "agd" else if (format == 2) format = "csv"
  } else if (mon == 4) {
    stop("Function to analyze data from other monitors is under development")
  }

  # Select datadir
  cat(paste("Please, select the directory containing the", mon, format, "files...\n"))
  cat(paste0(rep('_', options()$width), collapse = ''))
  datadir = utils::choose.dir(default = getwd(),
                              caption = paste("Select the directory containing the", mon, format, "files"))
  files = dir(datadir, pattern = paste0(".", format))
  nFiles = length(files)
  cat(paste("\nThere are", nFiles, mon, "files in this folder...\n"))
  print(files)
  iscorrect = menu(choices = c("Yes", "No"),
                   title = "\nDo you want to process these files?")
  if (iscorrect == 2) stop("Data processing stopped by the user")

  # select outputdir
  cat(paste("Please, select the directory where the new datasets should be stored\n"))
  cat(paste0(rep('_', options()$width), collapse = ''))
  outputdir = utils::choose.dir(default = getwd(),
                              caption = "Select the output directory")

  # save directories
  metadatadir = file.path(outputdir, "metadata/")
  if (!dir.exists(metadatadir)) dir.create(metadatadir)
  save(datadir, outputdir, metadatadir,files, mon, format,
       file = file.path(outputdir, "log_directories.RData"))

  invisible(list(datadir, outputdir, metadatadir,files, mon, format))
}
