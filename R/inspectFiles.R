#' inspectFiles
#'
#' @return List with datadir, outputdir, metadatadir, files, monitor, and format
#' @export
inspectFiles = function(){
  # ask user what device is being analized (FIBION or Scriin)
  mon = menu(choices = c("FIBION", "Scriin", "ActiGraph", "Other"),
             title = "\nWhat monitor have you used to record steps?")
  if (mon == 1) {
    id_col = c()
    mon = "FIBION"
    format = "csv"
    timestamp_col = "local"
    timestamp_format = "iso"
    epoch = 900
    steps = "activity.steps.count"
    steps_sporadic = "activity.steps2.count"
    sit2stand = "activity.sit2stand.count"
    resample = T
  } else if (mon == 2) {
    id_col = 1
    mon = "Scriin"
    format = "txt"
    timestamp_col = 2
    timestamp_format = "%Y-%m-%d %H:%M:%S%z"
    epoch = 60
    steps = 3
    steps_sporadic = c()
    sit2stand = c()
    resample = T
  } else if (mon == 3) {
    id_col = c()
    mon = "ActiGraph"
    format = menu(choices = c("agd", "csv"),
                  title = "\nWhat is the format of your files?")
    if (format == 1) format = "agd" else if (format == 2) format = "csv"
    if (format == "agd") stop("agd files cannot be read at the moment")
    timestamp_col = "timestamp"
    timestamp_format = c()
    epoch = "epochlengthinseconds"
    steps = "steps"
    steps_sporadic = c()
    sit2stand = c()
    resample = F
  } else if (mon == 4) {
    stop("Function to analyze data from other monitors is under development")
  }

  # Select datadir
  cat(paste("Please, select the directory containing the", mon, format, "files...\n"))
  cat(paste0(rep('_', options()$width), collapse = ''))
  datadir = utils::choose.dir(default = getwd(),
                              caption = paste("Select the directory containing the", mon, format, "files"))
  files = dir(datadir, pattern = paste0(".", format))
  files_full = dir(datadir, pattern = paste0(".", format), full.names = TRUE)
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

  if (!dir.exists(file.path(outputdir, mon))) dir.create(file.path(outputdir, mon))
  outputdir = file.path(outputdir, mon)

  # save directories
  metadatadir = file.path(outputdir, "metadata/")
  if (!dir.exists(metadatadir)) dir.create(metadatadir)
  save(datadir, outputdir, metadatadir,files, mon, format,
       file = file.path(metadatadir, "metadata.RData"))

  invisible(list(datadir = datadir, outputdir = outputdir, metadatadir = metadatadir,
                 files = files_full, mon = mon, format = format, id_col = id_col,
                 timestamp_col = timestamp_col, timestamp_format = timestamp_format,
                 epoch = epoch, steps = steps, steps_sporadic = steps_sporadic,
                 sit2stand = sit2stand, resample = resample))
}
