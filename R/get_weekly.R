#' get_weekly
#'
#' @param outputdir Directory with output files
#'
#' @return
#' @export
get_weekly = function(outputdir) {
  cat("\n\n")
  cat(paste0(rep('_', options()$width), collapse = ''))
  cat(paste0("\n\nCalculating means per week...\n"))

  for (mon in c("Scriin", "FIBION")) {
    files = dir(file.path(outputdir, mon, "/daySummary"), full.names = T)
    pb = txtProgressBar(1, length(files), style = 3)

    names.out.2 = c("id", "start_date", "valid_days","valid_days_WD", "valid_days_WE",
                    "th_MOD","th_VIG",
                    "stepsperday_pla","stepsperday_wei",
                    "SporadicStepsperday_pla","SporadicStepsperday_wei",
                    "sit2standperday_pla","sit2standperday_wei",
                    "CAD_pk60_spm_pla","CAD_pk60_spm_wei", "CAD_N0s_pk60_spm_pla", "CAD_N0s_pk60_spm_wei",
                    "CAD_pk30_spm_pla","CAD_pk30_spm_wei", "CAD_N0s_pk30_spm_pla", "CAD_N0s_pk30_spm_wei",
                    "CAD_pk1_spm_pla","CAD_pk1_spm_wei",
                    "band_CAD_0_min_pla", "band_CAD_0_min_wei","band_CAD_1-19_min_pla","band_CAD_1-19_min_wei",
                    "band_CAD_20-39_min_pla","band_CAD_20-39_min_wei","band_CAD_40-59_min_pla","band_CAD_40-59_min_wei",
                    "band_CAD_60-79_min_pla","band_CAD_60-79_min_wei","band_CAD_80-99_min_pla","band_CAD_80-99_min_wei",
                    "band_CAD_100-119_min_pla","band_CAD_100-119_min_wei","band_CAD_120+_min_pla","band_CAD_120+_min_wei",
                    "MPA_min_pla","MPA_min_wei","VPA_min_pla","VPA_min_wei","MVPA_min_pla","MVPA_min_wei")

    output = data.frame(matrix(NA, nrow = length(files), ncol = length(names.out.2)))
    colnames(output) = names.out.2
    #Loop through files to calculate mean variables
    for (i in 1:length(files)) {
      D = read.csv(files[i])
      fi = 1                                                  #fi is the column of the new output data frame
      output[i,fi] = D[1,1]; fi = fi + 1
      output[i,fi] = D[1,"date"]; fi = fi + 1
      output[i,fi] = nrow(D); fi = fi + 1
      output[i,fi] = sum(D$wday_num < 6); fi = fi + 1
      output[i,fi] = sum(D$wday_num >= 6); fi = fi + 1
      output[i,fi:(fi + 1)] = c(D$th_MOD[1], D$th_VIG[1]); fi = fi + 2

      m = which(colnames(D) == "stepsperday")
      for (mi in m:ncol(D)) {
        output[i,fi] = mean(D[,mi]); fi = fi + 1
        output[i,fi] = ((mean(D[which(D$wday_num < 6), mi]) * 5) + (mean(D[which(D$wday_num >= 6), mi]) * 2)) / 7; fi = fi + 1
      }
      setTxtProgressBar(pb, i)
    }
    write.csv(output, file = paste0(file.path(outputdir, mon), "/personSummary.csv"), row.names = FALSE)
  }
}
