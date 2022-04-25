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

  # daysummary
  daysum = read.csv(paste0(outputdir, "/Merged/daysummary.csv"))

  # filters
  # daysummary (>= 10h/d)
  f10h = ifelse(daysum$dur_recorded_min >= 10*60, 1, 0)
  f8h = ifelse(daysum$dur_recorded_min >= 8*60, 1, 0)
  f6h = ifelse(daysum$dur_recorded_min >= 6*60, 1, 0)

  # Longest gap without steps (>= 5h/d)
  f_nosteps_5h = ifelse(daysum$longest_gap_without_steps_min < 5*60, 1, 0)
  f_nosteps_3h = ifelse(daysum$longest_gap_without_steps_min < 3*60, 1, 0)

  # We agree on using at least 6 hours recorded with gaps smaller than 5 hours without steps
  filter = ifelse(daysum$dur_recorded_min >= 6*60 & daysum$longest_gap_without_steps_min < 5*60, 1, 0)

  # filter
  daysum_f = daysum[which(filter == 1),]


  # Averages
  names.out.2 = c("id", "mon_scriin", "mon_fibion", "th_MOD","th_VIG",
                  "valid_days","valid_days_WD", "valid_days_WE",
                  "dur.recorded_min_pla", "dur.recorded_min_wei",
                  "longest_gap_without_steps_min_pla", "longest_gap_without_steps_min_wei",
                  "stepsperday.scriin_pla","stepsperday.scriin_wei",
                  "stepsperday.fibion_pla","stepsperday.fibion_wei",
                  "SporadicStepsperday.fibion_pla","SporadicStepsperday.fibion_wei",
                  "sit2standperday.fibion_pla","sit2standperday.fibion_wei",
                  "CAD_pk60_scriin_spm_pla","CAD_pk60_scriin_spm_wei", "CAD_N0s_pk60_scriin_spm_pla", "CAD_N0s_pk60_scriin_spm_wei",
                  "CAD_pk60_fibion_spm_pla","CAD_pk60_fibion_spm_wei", "CAD_N0s_pk60_fibion_spm_pla", "CAD_N0s_pk60_fibion_spm_wei",
                  "CAD_pk30_scriin_spm_pla","CAD_pk30_scriin_spm_wei", "CAD_N0s_pk30_scriin_spm_pla", "CAD_N0s_pk30_scriin_spm_wei",
                  "CAD_pk30_fibion_spm_pla","CAD_pk30_fibion_spm_wei", "CAD_N0s_pk30_fibion_spm_pla", "CAD_N0s_pk30_fibion_spm_wei",
                  "CAD_pk1_scriin_spm_pla","CAD_pk1_scriin_spm_wei",
                  "CAD_pk1_fibion_spm_pla","CAD_pk1_fibion_spm_wei",
                  "band_CAD_0_scriin_min_pla", "band_CAD_0_scriin_min_wei","band_CAD_1-19_scriin_min_pla","band_CAD_1-19_scriin_min_wei",
                  "band_CAD_20-39_scriin_min_pla","band_CAD_20-39_scriin_min_wei","band_CAD_40-59_scriin_min_pla","band_CAD_40-59_scriin_min_wei",
                  "band_CAD_60-79_scriin_min_pla","band_CAD_60-79_scriin_min_wei","band_CAD_80-99_scriin_min_pla","band_CAD_80-99_scriin_min_wei",
                  "band_CAD_100-119_scriin_min_pla","band_CAD_100-119_scriin_min_wei","band_CAD_120+_scriin_min_pla","band_CAD_120+_scriin_min_wei",
                  "band_CAD_0_fibion_min_pla", "band_CAD_0_fibion_min_wei","band_CAD_1-19_fibion_min_pla","band_CAD_1-19_fibion_min_wei",
                  "band_CAD_20-39_fibion_min_pla","band_CAD_20-39_fibion_min_wei","band_CAD_40-59_fibion_min_pla","band_CAD_40-59_fibion_min_wei",
                  "band_CAD_60-79_fibion_min_pla","band_CAD_60-79_fibion_min_wei","band_CAD_80-99_fibion_min_pla","band_CAD_80-99_fibion_min_wei",
                  "band_CAD_100-119_fibion_min_pla","band_CAD_100-119_fibion_min_wei","band_CAD_120+_fibion_min_pla","band_CAD_120+_fibion_min_wei",
                  "MPA_scriin_min_pla","MPA_scriin_min_wei","VPA_scriin_min_pla","VPA_scriin_min_wei","MVPA_scriin_min_pla","MVPA_scriin_min_wei",
                  "MPA_fibion_min_pla","MPA_fibion_min_wei","VPA_fibion_min_pla","VPA_fibion_min_wei","MVPA_fibion_min_pla","MVPA_fibion_min_wei")

  participants = unique(daysum_f$id)

  output = data.frame(matrix(NA, nrow = length(participants), ncol = length(names.out.2)))
  colnames(output) = names.out.2
  #Loop through participants
  for (i in 1:length(participants)) {
    D = daysum_f[which(daysum_f$id == participants[i]),]
    fi = 1                                                  #fi is the column of the new output data frame
    output[i,fi] = D[1,1]; fi = fi + 1
    output[i,fi] = D[1,"mon_scriin"]; fi = fi + 1
    output[i,fi] = D[1,"mon_fibion"]; fi = fi + 1
    output[i,fi:(fi + 1)] = c(D$th_MOD[1], D$th_VIG[1]); fi = fi + 2
    output[i,fi] = nrow(D); fi = fi + 1
    output[i,fi] = sum(D$wday_num < 6); fi = fi + 1
    output[i,fi] = sum(D$wday_num >= 6); fi = fi + 1

    m = grep("window_first.last_min", colnames(D))[1]
    for (mi in m:ncol(D)) {
      output[i,fi] = mean(D[,mi]); fi = fi + 1
      output[i,fi] = ((mean(D[which(D$wday_num < 6), mi]) * 5) + (mean(D[which(D$wday_num >= 6), mi]) * 2)) / 7; fi = fi + 1
    }
  }
  write.csv(output, file = paste0(file.path(outputdir), "/Merged/personsummary_record6h_gap5h.csv"), row.names = FALSE)
}
