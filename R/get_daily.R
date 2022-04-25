#' get_daily
#'
#' @param datadir_scriin Directory where the scriin files are stored
#' @param datadir_fibion Directory where the fibion files are stored
#' @param outputdir Directory to store the outpu
#'
#' @return
#' @export
get_daily = function(datadir_scriin, datadir_fibion, outputdir) {

  # list of files
  files_scriin = dir(datadir_scriin, pattern = ".txt")
  files_scriin_full = dir(datadir_scriin, pattern = ".txt", full.names = TRUE)
  files_fibion = dir(datadir_fibion, pattern = ".csv")
  files_fibion_full = dir(datadir_fibion, pattern = ".csv", full.names = TRUE)

  # progress bar
  cat(paste0("\n\nCalculating features per day...\n"))
  pb = txtProgressBar(1, length(files_scriin), style = 3)

  #Loop through the files ----
  for (i in 1:length(files_scriin)) {

    # read Scriin file
    scriin = readFile(file = files_scriin_full[i], mon = "Scriin")
    if (length(scriin) == 1) scriin = data.frame(id = files_scriin[i])  # FIND ID to process the fibion file

    # find file fibion
    ids = GenerationPepSteps:::ids
    id_scriin = as.numeric(substr(scriin[1,1], 8, 14))
    file_fib = ids[which(ids[,1] == id_scriin), 2]
    file_fib = paste0(substr(file_fib,1,2),"-",substr(file_fib,3,4),".",substr(file_fib,5,6))
    fibion2read = grep(file_fib, files_fibion)

    # read FIBION file
    if (length(fibion2read) == 1) fibion = readFile(file = files_fibion_full[fibion2read], mon = "FIBION")

    # define thresholds
    age = ids[which(ids[,1] == id_scriin), 4]
    if (length(age) == 0) next()
    if (age %in% 0:8) {th.MOD = 125; th.VIG = 155}
    if (age %in% 9:11) {th.MOD = 115; th.VIG = 130}
    if (age %in% 12:14) {th.MOD = 110; th.VIG = 125}
    if (age %in% 15:17) {th.MOD = 105; th.VIG = 125}

    for (mon in c("Scriin", "FIBION")) {
      # if there is no fibion file, skip
      if (mon == "Scriin" & ncol(scriin) == 1) next()
      if (mon == "FIBION" & length(fibion2read) == 0) next()
      if (mon == "Scriin") mon_sn = id_scriin else mon_sn = file_fib
      if (mon == "Scriin") sit2stand = c() else sit2stand = "activity.sit2stand.count"
      if (mon == "Scriin") acc = scriin else acc = fibion
      if (mon == "Scriin") files = files_scriin else files = files_fibion
      if (mon == "Scriin") timestamp_col = 2 else timestamp_col = "local"
      if (mon == "Scriin") timestamp_format = "%Y-%m-%d %H:%M:%S%z" else timestamp_format = "iso"
      if (mon == "Scriin") id_col = 1 else id_col = c()
      if (mon == "Scriin") steps = 3 else steps = "activity.steps.count"
      if (mon == "Scriin") steps_sporadic = c() else steps_sporadic = "activity.steps2.count"
      if (mon == "Scriin") sit2stand = c() else sit2stand = "activity.sit2stand.count"

      # resample files into 1 min epochs
      acc = GenerationPepSteps::resample(acc, timestamp_col, timestamp_format, id_col, mon)

      # timestamp
      t = acc[, timestamp_col]

      # detect midnights
      midnights = detect_midnights(t)
      mnightsi = midnights$midnightsi
      day = midnights$day

      #Loop through days to calculate variables
      date = date_num = wday_num = day.min = stepsperday = SporadicStepsperday = sit2standperday = NA
      first_epoch_with_steps = last_epoch_with_steps = window_firslast = longest_gap = NA
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
          window_firslast[di] = nrow(tmp_firstlast)
          if (min(tmp_firstlast$steps) > 0) {
            longest_gap[di] = 0
          } else {
            longest_gap[di] = max(rle(tmp_firstlast$steps)$lengths[which(rle(tmp_firstlast$steps)$values == 0)])[1]
          }
          rm(tmp, tmp_without0, tmp_firstlast)
        } else {
          first_epoch_with_steps[di] = NA
          last_epoch_with_steps[di] = NA
          window_firslast[di] = 0
          longest_gap[di] = 1440
          rm(tmp)
        }

        #Steps/day
        if (!is.null(steps_sporadic)) acc[, steps] = acc[, steps] + acc[, steps_sporadic]
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
      names.out = c("id","mon_sn","date","wday_num","dur_day_min","th_MOD","th_VIG",
                    "first_epoch_with_steps", "last_epoch_with_steps", "window_first.last_min", "longest_gap_without_steps_min",
                    "stepsperday", "SporadicStepsperday", "sit2standperday",
                    "CAD_pk60_spm","CAD_N0s_pk60_spm","CAD_pk30_spm", "CAD_N0s_pk30_spm","CAD_pk1_spm",
                    "band_CAD_0_min","band_CAD_1-19_min","band_CAD_20-39_min","band_CAD_40-59_min",
                    "band_CAD_60-79_min","band_CAD_80-99_min","band_CAD_100-119_min","band_CAD_120+_min",
                    "MPA_min","VPA_min","MVPA_min")

      daily.out = data.frame(matrix(NA, length(unique(day[which(is.na(day) == FALSE)])), length(names.out)))
      colnames(daily.out) = names.out

      fi = 1
      daily.out[, fi] = i; fi = fi + 1
      daily.out[, fi] = mon_sn; fi = fi + 1
      daily.out[, fi:(fi + 1)] = cbind(date, as.numeric(wday_num)); fi = fi + 2
      daily.out[, fi] = day.min; fi = fi + 1
      daily.out[, fi:(fi + 1)] = cbind(rep(th.MOD, times = nrow(daily.out)), rep(th.VIG, times = nrow(daily.out))); fi = fi + 2
      daily.out[, fi] = first_epoch_with_steps; fi = fi + 1
      daily.out[, fi] = last_epoch_with_steps; fi = fi + 1
      daily.out[, fi] = window_firslast; fi = fi + 1
      daily.out[, fi] = longest_gap; fi = fi + 1
      daily.out[, fi:ncol(daily.out)] = cbind(stepsperday, SporadicStepsperday, sit2standperday,
                                              pk60min, N0s_pk60min, pk30min, N0s_pk30min, pk1min,
                                              band_0, band_1_19, band_20_39, band_40_59, band_60_79,
                                              band_80_99, band_100_119, band_120_higher,
                                              MPA, VPA, MVPA)
      # Create output files
      if (!dir.exists(file.path(outputdir, mon))) dir.create(file.path(outputdir, mon))
      if (!dir.exists(file.path(outputdir, mon, "/daySummary"))) dir.create(file.path(outputdir, mon, "/daySummary/"))

      if (mon == "Scriin") {
        if (!("scriin_daily_all" %in% ls())) scriin_daily_all = daily.out
        if ("scriin_daily_all" %in% ls()) scriin_daily_all = rbind(scriin_daily_all, daily.out)
      } else if (mon == "FIBION") {
        if (!("fibion_daily_all" %in% ls())) fibion_daily_all = daily.out
        if ("fibion_daily_all" %in% ls()) fibion_daily_all = rbind(scriin_daily_all, daily.out)
      }
      # save 1 file per subject
      write.csv(daily.out, file = paste0(file.path(outputdir, mon, "/daySummary/"), "/",i,"_",mon_sn, "_daysum_", mon, ".csv"), row.names = F)
    }
    setTxtProgressBar(pb, i)
  }
  write.csv(scriin_daily_all, file = paste0(outputdir, "/Scriin/scriin_daysummary.csv"), row.names = F)
  write.csv(fibion_daily_all, file = paste0(outputdir, "/FIBION/fibion_daysummary.csv"), row.names = F)
}
