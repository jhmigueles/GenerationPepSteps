#' step.metrics
#'
#' @param interactive should the progran run interactively?
#'
#' @return data frames with aggregated data
#' @export
step.metrics = function(interactive = T){

  if (isFALSE(interactive)) {
    datadir_fibion = "C:/Users/jaihid/OneDrive - Karolinska Institutet/accelerometry/storage/FIBION_Maria Lundgren/FIBION/"
    datadir_scriin = "C:/Users/jaihid/OneDrive - Karolinska Institutet/accelerometry/storage/FIBION_Maria Lundgren/SCRIIN/"
    outputdir = "C:/Users/jaihid/OneDrive - Karolinska Institutet/accelerometry/processing/Generation Pep/"
    metadatadir = "C:/Users/jaihid/OneDrive - Karolinska Institutet/Desktop/rm/metadata/"
    files_fibion = dir(datadir_fibion)
    files_fibion_full = dir(datadir_fibion, full.names = T)
    files_scriin = dir(datadir_scriin)
    files_scriin_full = dir(datadir_scriin, full.names = T)
  } else {
    # get directories and files to process ----
    metadata = inspectFiles()
    list2env(metadata, envir = environment())
  }

  # Get daily values (daySummary datasets)
  get_daily(datadir_scriin, datadir_fibion, outputdir)

  # Merge daily output
  merge_daily(datadir_scriin, datadir_fibion, outputdir)

  #Calculate means per week plain and weighted
  get_weekly(outputdir)

  cat("\n\nFINISHED!")

}

