#' merge_daily
#'
#' @param datadir_scriin Directory where the scriin files are stored
#' @param datadir_fibion Directory where the fibion files are stored
#' @param outputdir Directory to store the outpu
#'
#' @return
#' @export
merge_daily = function(datadir_scriin, datadir_fibion, outputdir, save_timeseries = TRUE) {

  # list of files
  files_scriin = dir(datadir_scriin, pattern = ".txt")
  files_scriin_full = dir(datadir_scriin, pattern = ".txt", full.names = TRUE)
  files_fibion = dir(datadir_fibion, pattern = ".csv")
  files_fibion_full = dir(datadir_fibion, pattern = ".csv", full.names = TRUE)

  # progress bar
  cat(paste0("\n\nMerging Scriin and Fibion daily information...\n"))
  pb = txtProgressBar(1, length(files_scriin), style = 3)

  #Loop through the files ----
  for (i in 1:length(files_scriin)) {

    # 5 - steps columns in scriin and fibion
    steps_sc = 3
    steps_fb = "activity.steps.count"
    steps_sporadic = "activity.steps2.count" # only fibion
    sit2stand = "activity.sit2stand.count"   # only fibion

    # read Scriin file
    scriin = readFile(file = files_scriin_full[i], mon = "Scriin")
    if (length(scriin) == 1) {setTxtProgressBar(pb, i); next()}

    # find file fibion
    ids = GenerationPepSteps:::ids
    id_scriin = as.numeric(substr(scriin[1,1], 8, 14))
    id_fibion = ids[which(ids[,1] == id_scriin), 2]
    id_fibion = paste0(substr(id_fibion,1,2),"-",substr(id_fibion,3,4),".",substr(id_fibion,5,6))
    fibion2read = grep(id_fibion, files_fibion)

    # read FIBION file
    if (length(fibion2read) == 1) fibion = readFile(file = files_fibion_full[fibion2read], mon = "FIBION")
    if (length(fibion2read) == 0) {setTxtProgressBar(pb, i); next()}

    # fibion steps - steps sporadic
    fibion[, steps_fb] = fibion[, steps_fb] + fibion[, steps_sporadic]

    # define thresholds
    age = ids[which(ids[,1] == id_scriin), 4]
    if (length(age) == 0) {setTxtProgressBar(pb, i); next()}
    if (age %in% 0:8) {th.MOD = 125; th.VIG = 155}
    if (age %in% 9:11) {th.MOD = 115; th.VIG = 130}
    if (age %in% 12:14) {th.MOD = 110; th.VIG = 125}
    if (age %in% 15:17) {th.MOD = 105; th.VIG = 125}

    # if any of the monitors is missing, the processing is already skipped at this point

    # 1 - resample
    scriin = GenerationPepSteps::resample(scriin, timestamp_col = 2, timestamp_format = "%Y-%m-%d %H:%M:%S%z",
                                          id_col = 1, mon = "Scriin")
    fibion = GenerationPepSteps::resample(fibion, timestamp_col = "local", timestamp_format = "iso",
                                          id_col = c(), mon = "FIBION")

    # 2 - timestamp to merge
    t_scriin = scriin[, 2]
    t_fibion = fibion[, "local"]
    t = lubridate::intersect(t_scriin, t_fibion)
    if (length(t) == 0) {setTxtProgressBar(pb, i); next()}

    # 3 - keep only time with registries in 2
    scriin = scriin[which(t_scriin %in% t),]
    fibion = fibion[which(t_fibion %in% t),]

    # 4 - detect midnights
    midnights = detect_midnights(t)
    mnightsi = midnights$midnightsi
    day = midnights$day

    # 5 - Loop through days to calculate variables
    date = date_num = wday_num = day.min = recorded.min = stepsperday_scriin = stepsperday_fibion = NA
    SporadicStepsperday_fibion = sit2standperday_fibion = NA
    first_epoch_with_steps = last_epoch_with_steps = window_firslast = longest_gap = NA
    pk60min_scriin = N0s_pk60min_scriin = pk60min_fibion = N0s_pk60min_fibion = NA
    pk30min_scriin = N0s_pk30min_scriin = pk30min_fibion = N0s_pk30min_fibion = NA
    pk1min_scriin = pk1min_fibion = NA
    band_0_scriin = band_0_fibion = band_1_19_scriin = band_1_19_fibion = NA
    band_20_39_scriin = band_20_39_fibion = band_40_59_scriin = band_40_59_fibion = NA
    band_60_79_scriin = band_60_79_fibion = band_80_99_scriin = band_80_99_fibion = NA
    band_100_119_scriin = band_100_119_fibion = band_120_higher_scriin = band_120_higher_fibion = NA
    MPA_scriin = VPA_scriin = MVPA_scriin = MPA_fibion = VPA_fibion = MVPA_fibion = NA

    for (di in 1:length(unique(day))) {
      #Date
      date[di] = strsplit(t[which(day == di)[1]], "T")[[1]][1]
      wday_num[di] = suppressMessages(format(lubridate::as_datetime(t[which(day == di)[1]]), "%u"))

      # first and last epoch with steps in the day based on Scriin monitor
      tmp = data.frame(time = t[which(day == di)], steps = scriin[day == di, steps_sc])
      if (length(which(tmp$steps != 0)) > 30) {
        tmp_without0 = tmp[-which(tmp$steps == 0),]
        first_epoch_with_steps[di] = tmp_without0$time[1]
        last_epoch_with_steps[di] = tmp_without0$time[nrow(tmp_without0)]
        tmp_firstlast = tmp[which(tmp$time == first_epoch_with_steps[di]):which(tmp$time == last_epoch_with_steps[di]),]
        window_firslast[di] = nrow(tmp_firstlast)
        if (min(tmp_firstlast$steps) > 0) {
          longest_gap[di] = 0
        } else {
          longest_gap[di] = max(rle(tmp_firstlast$steps)$lengths[which(rle(tmp_firstlast$steps)$values == 0)])[1]
        }
      } else {
        first_epoch_with_steps[di] = NA
        last_epoch_with_steps[di] = NA
        window_firslast[di] = 0
        longest_gap[di] = 1440
        rm(tmp)
      }

      # keep only time between first and last epoch with steps in Scriin every day
      if (window_firslast[di] > 0) {
        scriin_day = scriin[which(scriin[, 2] %in% tmp_firstlast[,1]),]
        fibion_day = fibion[which(fibion[, "local"] %in% tmp_firstlast[,1]),]
        if (nrow(scriin_day) != nrow(fibion_day)) stop(paste(files_scriin[i], di, "time recorded do not match!"))
        rm(tmp, tmp_without0, tmp_firstlast)
      }

      #Duration of the day (number of minutes recorded)
      day.min[di] = length(which(day == di))
      recorded.min[di] = nrow(scriin_day)

      #Steps/day
      stepsperday_scriin[di] = sum(scriin_day[, steps_sc])
      stepsperday_fibion[di] = sum(fibion_day[, steps_fb])

      #Sporadic steps and sit2stand per day
      SporadicStepsperday_fibion[di] = sum(fibion_day[, steps_sporadic])
      sit2standperday_fibion[di] = sum(fibion_day[, sit2stand])

      #Peaks cadence
      pk60min_scriin[di] = mean(sort(scriin_day[, steps_sc], decreasing = TRUE)[1:60])
      N0s_pk60min_scriin[di] = length(which(sort(scriin_day[, steps_sc], decreasing = TRUE)[1:60] == 0))
      pk30min_scriin[di] = mean(sort(scriin_day[, steps_sc], decreasing = TRUE)[1:30])
      N0s_pk30min_scriin[di] = length(which(sort(scriin_day[, steps_sc], decreasing = TRUE)[1:30] == 0))
      pk1min_scriin[di] = max(scriin_day[, steps_sc])

      pk60min_fibion[di] = mean(sort(fibion_day[, steps_fb], decreasing = TRUE)[1:60])
      N0s_pk60min_fibion[di] = length(which(sort(fibion_day[, steps_fb], decreasing = TRUE)[1:60] == 0))
      pk30min_fibion[di] = mean(sort(fibion_day[, steps_fb], decreasing = TRUE)[1:30])
      N0s_pk30min_fibion[di] = length(which(sort(fibion_day[, steps_fb], decreasing = TRUE)[1:30] == 0))
      pk1min_fibion[di] = max(fibion_day[, steps_fb])

      #Cadence band levels
      band_0_scriin[di] = length(which(scriin_day[, steps_sc] == 0))
      band_1_19_scriin[di] = length(which(scriin_day[, steps_sc] < 20 & scriin_day[, steps_sc] > 0))
      band_20_39_scriin[di] = length(which(scriin_day[, steps_sc] < 40 & scriin_day[, steps_sc] >= 20))
      band_40_59_scriin[di] = length(which(scriin_day[, steps_sc] < 60 & scriin_day[, steps_sc] >= 40))
      band_60_79_scriin[di] = length(which(scriin_day[, steps_sc] < 80 & scriin_day[, steps_sc] >= 60))
      band_80_99_scriin[di] = length(which(scriin_day[, steps_sc] < 100 & scriin_day[, steps_sc] >= 80))
      band_100_119_scriin[di] = length(which(scriin_day[, steps_sc] < 120 & scriin_day[, steps_sc] >= 100))
      band_120_higher_scriin[di] = length(which(scriin_day[, steps_sc] >= 120))

      band_0_fibion[di] = length(which(fibion_day[, steps_fb] == 0))
      band_1_19_fibion[di] = length(which(fibion_day[, steps_fb] < 20 & fibion_day[, steps_fb] > 0))
      band_20_39_fibion[di] = length(which(fibion_day[, steps_fb] < 40 & fibion_day[, steps_fb] >= 20))
      band_40_59_fibion[di] = length(which(fibion_day[, steps_fb] < 60 & fibion_day[, steps_fb] >= 40))
      band_60_79_fibion[di] = length(which(fibion_day[, steps_fb] < 80 & fibion_day[, steps_fb] >= 60))
      band_80_99_fibion[di] = length(which(fibion_day[, steps_fb] < 100 & fibion_day[, steps_fb] >= 80))
      band_100_119_fibion[di] = length(which(fibion_day[, steps_fb] < 120 & fibion_day[, steps_fb] >= 100))
      band_120_higher_fibion[di] = length(which(fibion_day[, steps_fb] >= 120))

      #MPA, VPA, MVPA
      MPA_scriin[di] = length(which(scriin_day[, steps_sc] < th.VIG & scriin_day[, steps_sc] >= th.MOD))
      VPA_scriin[di] = length(which(scriin_day[, steps_sc] >= th.VIG))
      MVPA_scriin[di] = length(which(scriin_day[, steps_sc] >= th.MOD))

      MPA_fibion[di] = length(which(fibion_day[, steps_fb] < th.VIG & fibion_day[, steps_fb] >= th.MOD))
      VPA_fibion[di] = length(which(fibion_day[, steps_fb] >= th.VIG))
      MVPA_fibion[di] = length(which(fibion_day[, steps_fb] >= th.MOD))

      # Time series
      if (isTRUE(save_timeseries)) get_timeseries(id = i, day = di, scriin_day, fibion_day, outputdir,
                                                  from = 9, to = 18)
    }
    ##OUTPUT PER DAY
    names.out = c("id","mon_scriin", "mon_fibion",
                  "date","wday_num","dur_day_min", "dur_recorded_min", "th_MOD","th_VIG",
                  "first_epoch_with_steps", "last_epoch_with_steps", "window_first.last_min", "longest_gap_without_steps_min",
                  "stepsperday_scriin", "stepsperday_fibion",
                  "SporadicStepsperday_fibion", "sit2standperday_fibion",
                  "CAD_pk60_spm_scriin","CAD_N0s_pk60_spm_scriin", "CAD_pk60_spm_fibion","CAD_N0s_pk60_spm_fibion",
                  "CAD_pk30_spm_scriin", "CAD_N0s_pk30_spm_scriin", "CAD_pk30_spm_fibion", "CAD_N0s_pk30_spm_fibion",
                  "CAD_pk1_spm_scriin", "CAD_pk1_spm_fibion",
                  "band_CAD_0_min_scriin","band_CAD_1-19_min_scriin","band_CAD_20-39_min_scriin","band_CAD_40-59_min_scriin",
                  "band_CAD_60-79_min_scriin","band_CAD_80-99_min_scriin","band_CAD_100-119_min_scriin","band_CAD_120+_min_scriin",
                  "band_CAD_0_min_fibion","band_CAD_1-19_min_fibion","band_CAD_20-39_min_fibion","band_CAD_40-59_min_fibion",
                  "band_CAD_60-79_min_fibion","band_CAD_80-99_min_fibion","band_CAD_100-119_min_fibion","band_CAD_120+_min_fibion",
                  "MPA_min_scriin","VPA_min_scriin","MVPA_min_scriin",
                  "MPA_min_fibion","VPA_min_fibion","MVPA_min_fibion")

    daily.out = data.frame(matrix(NA, length(unique(day[which(is.na(day) == FALSE)])), length(names.out)))
    colnames(daily.out) = names.out

    fi = 1
    daily.out[, fi] = i; fi = fi + 1
    daily.out[, fi] = substr(files_scriin[i], 11, 14); fi = fi + 1
    daily.out[, fi] = gsub(".csv","",files_fibion[fibion2read]); fi = fi + 1
    daily.out[, fi:(fi + 1)] = cbind(date, as.numeric(wday_num)); fi = fi + 2
    daily.out[, fi] = day.min; fi = fi + 1
    daily.out[, fi] = recorded.min; fi = fi + 1
    daily.out[, fi:(fi + 1)] = cbind(rep(th.MOD, times = nrow(daily.out)), rep(th.VIG, times = nrow(daily.out))); fi = fi + 2
    daily.out[, fi] = first_epoch_with_steps; fi = fi + 1
    daily.out[, fi] = last_epoch_with_steps; fi = fi + 1
    daily.out[, fi] = window_firslast; fi = fi + 1
    daily.out[, fi] = longest_gap; fi = fi + 1
    daily.out[, fi:ncol(daily.out)] = cbind(stepsperday_scriin, stepsperday_fibion,
                                            SporadicStepsperday_fibion, sit2standperday_fibion,
                                            pk60min_scriin, N0s_pk60min_scriin,
                                            pk60min_fibion, N0s_pk60min_fibion,
                                            pk30min_scriin, N0s_pk30min_scriin,
                                            pk30min_fibion, N0s_pk30min_fibion,
                                            pk1min_scriin, pk1min_fibion,
                                            band_0_scriin, band_1_19_scriin, band_20_39_scriin,
                                            band_40_59_scriin, band_60_79_scriin, band_80_99_scriin,
                                            band_100_119_scriin, band_120_higher_scriin,
                                            band_0_fibion, band_1_19_fibion, band_20_39_fibion,
                                            band_40_59_fibion, band_60_79_fibion, band_80_99_fibion,
                                            band_100_119_fibion, band_120_higher_fibion,
                                            MPA_scriin, VPA_scriin, MVPA_scriin,
                                            MPA_fibion, VPA_fibion, MVPA_fibion)
    # Create output files
    if (!dir.exists(file.path(outputdir, "Merged/"))) dir.create(file.path(outputdir, "Merged/"))
    if (!dir.exists(file.path(outputdir, "Merged/", "/daySummary"))) dir.create(file.path(outputdir, "Merged/", "/daySummary"))

    if (!("daily_all" %in% ls())) {
      daily_all = daily.out
    } else if ("daily_all" %in% ls()) {
      daily_all = rbind(daily_all, daily.out)
    }

    write.csv(daily.out, file = paste0(file.path(outputdir, "Merged/", "/daySummary/"), "/",i, "_daysum", ".csv"), row.names = F)

    setTxtProgressBar(pb, i)

  }
  write.csv(daily_all, file = paste0(outputdir, "/Merged/daysummary.csv"), row.names = F)
}

