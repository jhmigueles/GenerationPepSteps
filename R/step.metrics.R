#' step.metrics
#'
#' @return data frames with aggregated data
#' @export
step.metrics = function(){

  # get directories and files to process ----
  metadata = inspectFiles()
  list2env(metadata, envir = environment())

  # get thresholds ---
  th.MOD = menu(c("100 steps per minute", "other"), title = "\n\nWhat cadence threshold do you want to use to classify Moderate Intensity PA?")
  if (th.MOD == 1) th.MOD = 100
  if (th.MOD == 2) th.MOD = as.numeric(readline("\n\nPlease, write the threshold to use (in steps per minute):"))

  th.VIG = menu(c("130 steps per minute", "other"), title = "\n\nWhat cadence threshold do you want to use to classify Vigorous Intensity PA?")
  if (th.VIG == 1) th.VIG = 130
  if (th.VIG == 2) th.VIG = as.numeric(readline("\n\nPlease, write the threshold to use (in steps per minute):"))


  cat("\n\nCalculating features per day...\n")
  pb = txtProgressBar(1, length(files), style = 3)

  #Loop through the files ----
  for (i in 1:length(files)) {
    # read file
    acc = readFile(file = files[i], mon = mon)
    if (length(acc) == 1) next()

    # aggregate per 60s epochs
    if(epoch < 60) acc = agg60s(acc, epoch = epoch)

    # resample if needed
    if (isTRUE(resample)) acc = GenerationPepSteps::resample(acc, timestamp_col, timestamp_format, id_col = id_col, mon = mon)

    # timestamp (if resampled, it is already in iso8601)
    t = acc[, timestamp_col]
    if (isFALSE(resample) & format != "iso") t = timestamp2iso8601(t, time_format = timestamp_format, tz = "Europe/Stockholm")

    # detect midnights
    midnights = detect_midnights(t)
    list2env(midnights, envir = environment())

    #Loop through days to calculate variables
    date = date_num = wday_num = day.min = stepsperday = SporadicStepsperday = sit2standperday = NA
    first_epoch_with_steps = last_epoch_with_steps = longest_gap = NA
    pk60min = N0s_pk60min = N0s_pk30min = pk30min = pk1min = NA
    band_0 = band_1_19 = band_20_39 = band_40_59 = band_60_79 = band_80_99 = band_100_119 = band_120_higher = NA
    MPA = VPA = MVPA = NA
    for (di in 1:length(unique(day))) {
      #Date
      date[di] = strsplit(t[which(day == di)[1]], "T")[[1]][1]
      wday_num[di] = suppressMessages(format(lubridate::as_datetime(t[which(day == di)[1]]), "%u"))
      #Duration of the day (number of minutes recorded)
      day.min[di] = length(which(day == di))

      # first and last epoch with steps in the day
      tmp = data.frame(time = t[which(day == di)], steps = acc[day == di, steps])
      if (length(which(tmp$steps != 0)) > 30) {
        tmp_without0 = tmp[-which(tmp$steps == 0),]
        first_epoch_with_steps[di] = tmp_without0$time[1]
        last_epoch_with_steps[di] = tmp_without0$time[nrow(tmp_without0)]
        tmp_firstlast = tmp[which(tmp$time == first_epoch_with_steps[di]):which(tmp$time == last_epoch_with_steps[di]),]
        longest_gap[di] = max(rle(tmp_firstlast$steps)$lengths[which(rle(tmp_firstlast$steps)$values == 0)])
        rm(tmp, tmp_without0, tmp_firstlast)
      } else {
        first_epoch_with_steps[di] = NA
        last_epoch_with_steps[di] = NA
        longest_gap[di] = 1440
        rm(tmp)
      }

      #Steps/day
      if(!is.null(steps_sporadic)) acc[, steps] = acc[, steps] + acc[, steps_sporadic]
      stepsperday[di] = sum(acc[which(day == di), steps])

      #Sporadic steps and sit2stand per day
      if (!is.null(steps_sporadic)) SporadicStepsperday[di] = sum(acc[which(day == di), steps_sporadic])
      if (!is.null(sit2stand)) sit2standperday[di] = sum(acc[which(day == di), sit2stand])

      #Peaks cadence
      pk60min[di] = mean(sort(acc[which(day == di), steps], decreasing = TRUE)[1:60])
      N0s_pk60min[di] = length(which(sort(acc[which(day == di), steps], decreasing = TRUE)[1:60] == 0))
      pk30min[di] = mean(sort(acc[which(day == di), steps], decreasing = TRUE)[1:30])
      N0s_pk30min[di] = length(which(sort(acc[which(day == di), steps], decreasing = TRUE)[1:30] == 0))
      pk1min[di] = max(acc[which(day == di), steps])

      #Cadence band levels
      band_0[di] = length(which(acc[which(day == di), steps] == 0))
      band_1_19[di] = length(which(acc[which(day == di), steps] < 20 & acc[which(day == di), steps] > 0))
      band_20_39[di] = length(which(acc[which(day == di), steps] < 40 & acc[which(day == di), steps] >= 20))
      band_40_59[di] = length(which(acc[which(day == di), steps] < 60 & acc[which(day == di), steps] >= 40))
      band_60_79[di] = length(which(acc[which(day == di), steps] < 80 & acc[which(day == di), steps] >= 60))
      band_80_99[di] = length(which(acc[which(day == di), steps] < 100 & acc[which(day == di), steps] >= 80))
      band_100_119[di] = length(which(acc[which(day == di), steps] < 120 & acc[which(day == di), steps] >= 100))
      band_120_higher[di] = length(which(acc[which(day == di), steps] >= 120))

      #MPA, VPA, MVPA
      MPA[di] = length(which(acc[which(day == di), steps] < th.VIG & acc[which(day == di), steps] >= th.MOD))
      VPA[di] = length(which(acc[which(day == di), steps] >= th.VIG))
      MVPA[di] = length(which(acc[which(day == di), steps] >= th.MOD))
    }
    ##OUTPUT PER DAY
    names.out = c("id","date","wday_num","dur_day_min","th_MOD","th_VIG",
                  "first_epoch_with_steps", "last_epoch_with_steps", "longest_gap_without_steps",
                  "stepsperday", "SporadicStepsperday", "sit2standperday",
                  "CAD_pk60_spm","CAD_N0s_pk60_spm","CAD_pk30_spm", "CAD_N0s_pk30_spm","CAD_pk1_spm",
                  "band_CAD_0_min","band_CAD_1-19_min","band_CAD_20-39_min","band_CAD_40-59_min",
                  "band_CAD_60-79_min","band_CAD_80-99_min","band_CAD_100-119_min","band_CAD_120+_min",
                  "MPA_min","VPA_min","MVPA_min")

    daily.out = data.frame(matrix(NA, length(unique(day[which(is.na(day) == FALSE)])), length(names.out)))
    colnames(daily.out) = names.out

    fi = 1
    daily.out[, fi] = basename(files[i]); fi = fi + 1
    daily.out[, fi:(fi + 1)] = cbind(date, as.numeric(wday_num)); fi = fi + 2
    daily.out[, fi] = day.min; fi = fi + 1
    daily.out[, fi:(fi + 1)] = cbind(rep(th.MOD, times = nrow(daily.out)), rep(th.VIG, times = nrow(daily.out))); fi = fi + 2
    daily.out[, fi] = first_epoch_with_steps; fi = fi + 1
    daily.out[, fi] = last_epoch_with_steps; fi = fi + 1
    daily.out[, fi] = longest_gap; fi = fi + 1
    daily.out[, fi:ncol(daily.out)] = cbind(stepsperday, SporadicStepsperday, sit2standperday,
                                            pk60min, N0s_pk60min, pk30min, N0s_pk30min, pk1min,
                                            band_0, band_1_19, band_20_39, band_40_59, band_60_79,
                                            band_80_99, band_100_119, band_120_higher,
                                            MPA, VPA, MVPA)
    # Create output directory
    if(dir.exists(outputdir) == FALSE) {
      dir.create(outputdir)
    }
    if(dir.exists(paste0(outputdir,"/daySummary")) == FALSE) {
      dir.create(paste0(outputdir,"/daySummary/"))
    }
    write.csv(daily.out, file = paste0(outputdir,"/daySummary/", basename(files[i]), "_DaySum", ".csv"), row.names = F)
    setTxtProgressBar(pb, i)
  }

  ################################################################################################################################
  #Calculate means per week plain and weighted

  cat("\n\n")
  cat(paste0(rep('_', options()$width), collapse = ''))
  cat("\n\nCalculating means per week...\n")

  files = dir(paste0(outputdir, "/daySummary"), full.names = T)
  pb = txtProgressBar(1, length(files), style = 3)

  names.out.2 = c("id","start_date", "valid_days","valid_days_WD", "valid_days_WE",
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
    D = read.csv(paste0(outputdir,"/daySummary/", basename(files[i])))
    fi = 1                                                  #fi is the column of the new output data frame
    output[i,fi] = files[i]; fi = fi + 1
    output[i,fi] = D[1,"date"]; fi = fi + 1
    output[i,fi] = nrow(D); fi = fi + 1
    output[i,fi] = sum(D$wday_num < 6); fi = fi + 1
    output[i,fi] = sum(D$wday_num >= 6); fi = fi + 1
    output[i,fi:(fi + 1)] = c(th.MOD, th.VIG); fi = fi + 2

    for (mi in 10:ncol(daily.out)) {
      output[i,fi] = mean(D[,mi]); fi = fi + 1
      output[i,fi] = ((mean(D[which(D$wday_num < 6), mi]) * 5) + (mean(D[which(D$wday_num >= 6), mi]) * 2)) / 7; fi = fi + 1
    }
    setTxtProgressBar(pb, i)
  }

  write.csv(output, file = paste0(outputdir,"/personSummary.csv"), row.names = FALSE)
}
