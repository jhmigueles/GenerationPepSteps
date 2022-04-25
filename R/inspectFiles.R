#' inspectFiles
#'
#' @return List with datadir, outputdir, metadatadir, files, monitor, and format
#' @export
inspectFiles = function(){

  # Select datadir FIBION
  cat(paste("Please, select the directory containing the FIBION files...\n"))
  cat(paste0(rep('_', options()$width), collapse = ''))
  datadir_fibion = utils::choose.dir(default = getwd(),
                              caption = paste("Where are your FIBION files?"))
  files_fibion = dir(datadir_fibion, pattern = ".csv")
  files_fibion_full = dir(datadir_fibion, pattern = ".csv", full.names = TRUE)
  nFiles_fibion = length(files_fibion)
  cat(paste("\nThere are", nFiles_fibion, "FIBION files in this folder...\n"))
  print(files_fibion)
  iscorrect = menu(choices = c("Yes", "No"),
                   title = "\nDo you want to process these files?")
  if (iscorrect == 2) stop("Data processing stopped by the user")

  # Select datadir Scriin
  cat(paste("Please, select the directory containing the Scriin files...\n"))
  cat(paste0(rep('_', options()$width), collapse = ''))
  datadir_scriin = utils::choose.dir(default = getwd(),
                              caption = paste("Where are your Scriin files?"))
  files_scriin = dir(datadir_scriin, pattern = ".txt")
  files_scriin_full = dir(datadir_scriin, pattern = ".txt", full.names = TRUE)
  nFiles_scriin = length(files_scriin)
  cat(paste("\nThere are", nFiles_scriin, "Scriin files in this folder...\n"))
  print(files_scriin)
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
  save(datadir_fibion, datadir_scriin, files_fibion, files_scriin,
       outputdir, metadatadir,
       file = file.path(metadatadir, "metadata.RData"))

  invisible(list(datadir_fibion = datadir_fibion, datadir_scriin = datadir_scriin,
                 outputdir = outputdir, metadatadir = metadatadir,
                 files_fibion = files_fibion, files_fibion_full = files_fibion_full,
                 files_scriin = files_scriin, files_scriin_full = files_scriin_full))
}
