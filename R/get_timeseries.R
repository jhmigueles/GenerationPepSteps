#' get_timeseries
#'
#' @param outputdir Directory with output files
#' @param from Numeric, from what time (hours, accept decimals)
#' @param to Numeric, to what time (hours, accept decimals)
#' @param id id of participant
#' @param day day being analyzed
#' @param scriin_day data from scriin
#' @param fibion_day data from fibion
#'
#' @return
#' @export
get_timeseries = function(id, day, scriin_day, fibion_day, outputdir,
                          from = 9, to = 18) {


  # create storing dir ------
  storingdir = file.path(outputdir, "/Merged/time series/")
  if (!dir.exists(storingdir)) dir.create(storingdir)

  # Identify days with data from range0 to range1 ----
  # from (in decimal)
  tmp1 = as.character(scriin_day[1, 2]) # timestamps are the same for scriin and fibion
  tmp2 = unlist(strsplit(tmp1, "T"))[2]
  t0_char = unlist(strsplit(tmp2, "+", fixed = T))[1]
  tmp4 = as.numeric(unlist(strsplit(t0_char, ":", fixed = T)))
  t0 = tmp4[1] + (tmp4[2]/60) + (tmp4[3]/3600)

  date = unlist(strsplit(tmp1, "T"))[1]

  # to (in decimal)
  tmp1 = as.character(scriin_day[nrow(scriin_day), 2]) # timestamps are the same for scriin and fibion
  tmp2 = unlist(strsplit(tmp1, "T"))[2]
  t1_char = unlist(strsplit(tmp2, "+", fixed = T))[1]
  tmp4 = as.numeric(unlist(strsplit(t1_char, ":", fixed = T)))
  t1 = tmp4[1] + (tmp4[2]/60) + (tmp4[3]/3600)

  # day to include?
  included = (t0 <= from & t1 >= to)

  if (included) {

    # from what row
    from_h = as.character(floor(from))
    from_h = ifelse(nchar(from_h) == 1, paste0("0", from_h), from_h)
    from_min = as.character(floor((from - as.numeric(from_h)) * 60))
    from_min = ifelse(nchar(from_min) == 1, paste0("0", from_min), from_min)
    from_sec = as.character(floor((from - as.numeric(from_h) - as.numeric(from_min)/60) * 3600))
    from_sec = ifelse(nchar(from_sec) == 1, paste0("0", from_sec), from_sec)

    from_char = paste(from_h, from_min, from_sec, sep = ":")
    row0 = grep(from_char, scriin_day[,2])

    # to what row
    to_h = as.character(floor(to))
    to_h = ifelse(nchar(to_h) == 1, paste0("0", to_h), to_h)
    to_min = as.character(floor((to - as.numeric(to_h)) * 60))
    to_min = ifelse(nchar(to_min) == 1, paste0("0", to_min), to_min)
    to_sec = as.character(floor((to - as.numeric(to_h) - as.numeric(to_min)/60) * 3600))
    to_sec = ifelse(nchar(to_sec) == 1, paste0("0", to_sec), to_sec)

    to_char = paste(to_h, to_min, to_sec, sep = ":")
    row1 = grep(to_char, scriin_day[,2])

    scriin_day = scriin_day[row0:row1,]
    fibion_day = fibion_day[row0:row1,]

    # save datasets
    dir_data = file.path(storingdir, paste0("/data/"))
    if (!dir.exists(dir_data)) dir.create(dir_data)
    dir_i = file.path(dir_data, paste0("/id_", id))
    if (!dir.exists(dir_i)) dir.create(dir_i)
    write.csv(scriin_day, file = paste0(dir_i, "/scriin_day", day, ".csv"), row.names = F)
    write.csv(fibion_day, file = paste0(dir_i, "/fibion_day", day, ".csv"), row.names = F)

    # plot
    plot_dir = paste0(storingdir, "/plots/")
    if (!dir.exists(plot_dir)) dir.create(plot_dir)
    png(paste0(plot_dir, "/id",id, "_day", day, ".png"),
        width = 6, height = 4, res = 600, units = "in")
    par(las = 1)
    plot(scriin_day[, 3], type = "l", lwd = 3, col = "#003f5c",
         ylab = "Steps (nr.)", xlab = "Time (HH:MM)", xaxt = "n",
         main = paste0("ID: ", id, "; date: ", date))
    axis(side = 1,at = seq(1, nrow(scriin_day), by = 90),
          labels = c("9:00", "10:30", "12:00", "13:30", "15:00",
                     "16:30", "18:00"), tick = F, line = -0.8)
    lines(fibion_day$activity.steps.count, lwd = 3, col = "#bc5090")
    lines(fibion_day$activity.steps.count - fibion_day$activity.steps2.count,
          lwd = 1.5, col = "#ff6361")
    legend("topright", lwd = 3, col = c("#003f5c","#bc5090", "#ff6361"),
           legend = c("Scriin", "Fibion", "Fibion2"), ncol = 3, bty = "n")
    dev.off()
  }
}
