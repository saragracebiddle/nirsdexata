#' Plot the On/Off Kinetics of the provided data set
#'
#' @param rawdata rawdata object
#' @param type type of data to plot. Default setting will facet the plot
#' by type.
#'
#' @return ggplot
#' @export
#'
ook_plot <- function(rawdata, type = NULL){
  # TODO stop if not object of class rawdata

  info <- rawdata[["info"]]
  data <- rawdata[["data"]]

  # create lookup table
  # for creating new variable col_type
  # from col_name using rawdata$info$cols
  lookup <- rawdata$info$cols$col_type |>
    stringr::str_to_upper()
  names(lookup) <- rawdata$info$cols$col_name

  # crop by bounds
  # and remove rows with samp_num in rawdata$info$bounds$remove
  # add in time and zeroed time columns
  # and melt to longer format
  plotdata<- data[-info$bounds$removed, setdiff(c(info$cols$col_name, "samp_num"), info$bads)] |>
    dplyr::mutate(Time = samp_num / info$sfreq,
                  ZeroedTime = Time - info$bounds$meas_start) |>
    dplyr::filter(ZeroedTime >= 0 & Time <= info$bounds$meas_end) |>
    tidyr::pivot_longer(cols = setdiff(info$cols$col_name, info$bads),
                        names_to = "col_name",
                        values_to = "value")|>
    dplyr::mutate(col_type = unname(lookup[plotdata$col_name]))

  # if type is not null
  # filter by types listed in type
  if(!is.null(type)){
    if(length(type) == 1){type <- c(type)}
    plotdata <- plotdata |>
      dplyr::filter(col_type %in% type)
  }

  # use lookup table to create variable col_type from col_name
  plotdata["col_type" = unname(lookup[plotdata$col_name])]

  # create text labels to display on plot

  labels = data.frame(x = seq(60, 900, by = 120),
                      text = c("Rest", "Warm Up", "Work", "Rest", "Work", "Rest", "Work", "Rest"))

  # create title from information in rawdata$info
  title <- paste(info$subj_info$subj_id, info$meas_date, "On/Off Kinetics")

  # create plot
  plt <- ggplot(plotdata,
                aes(x = ZeroedTime, y = value))+
    facet_grid(col_type~., scales = "free_y")+
    geom_line(aes(color = col_name))+
    theme_bw()+
    scale_x_continuous(breaks = seq(0, 960, 120),
                       minor_breaks = seq(0,960,60))+
    labs(x = "Time (sec)",
         y = "",
         title = title,
         color = "")

  plt
}

#' Manipulate data frame for use in plotting functions
#'
#' @param rawdata rawdata object
#' @param type type of data to display
#'
#' internal function for use in ook_plot and plot_rawdata
prepare_for_ook_plot <- function(rawdata, type = NULL){
  prepare_shiny_plot(rawdata$data,
                     rawdata$info$sfreq,
                     rawdata$info$bounds$meas_start,
                     rawdata$info$bounds$meas_end,
                     rawdata$info$bads,
                     rawdata$info$bounds$removed,
                     rawdata$info$cols$col_type,
                     rawdata$info$cols$col_name,
                     type)

}

#' Wrapper around ggplot functions to plot on/off kinetics
#'
#' @param data data frame to use
#' @param x limits
#' @param y limits
ggook <- function(data, x, y){

  cols = c("ZeroedTime", "value", "col_name", "col_type")

  if(length(intersect(cols, colnames(data))) != length(cols)){
    stop("Provided data.frame is not compatible with this function.")
  }
  major_breaks = seq(0,960, 120)
  minor_breaks = seq(0,960,60)

  #TODO make manual color scale work
 plt <-  ggplot(data,
         aes(x = ZeroedTime, y = value))+
    geom_line(aes(color = col_name))+
    theme_bw()+
    coord_cartesian(xlim = x, ylim = y, expand = FALSE)+
    labs(x = "Time (sec)",
         y = "",
         color = "")

 if(is.null(x) & is.null(y)){
   return(plt + scale_x_continuous(breaks = seq(0,960,120)))
 }

 bk <- major_breaks[which(dplyr::between(major_breaks, x[1], x[2]))]
 vlines <- lapply(bk, function(x) geom_vline(xintercept = x))

 plt <- lapply(vlines, function(x) plt + x)

  plt
}

#' manual color scale for ggplot
#'
#' @param col_names character vector of unique column names in the dataset
#'
#' @return named vector of colors to use with each col_name
manual_color <- function(col_names){

  manual_color_opts <- c(
    "S1_D1_759" = "firebrick2",
    "S1_D1_854" = "royalblue1",
    "S2_D1_759" = "red2",
    "S2_D1_854" = "blue",
    "S3_D1_761" = "red4",
    "S3_D1_856" = "royalblue4",
    "TSI" = "springgreen2",
    "TSIFF" = "springgreen4"
  )

  manual_color_opts[col_names]
}

#' Plot Regressions on top of actual values
#'
#' @param models analyzed object
#'
#' @return ggplot object
#' @export
plot_regressions <- function(models){

  models$data |>
    dplyr::filter(Section != "BegRest" & Section != "WarmUp") |>
    ggplot(aes(x = SectionZeroedTime))+
      facet_grid(col_type ~ Section, scales = "free_y")+
      geom_line(aes(y = value), color = "grey23", linewidth = 1.25, alpha = 0.5)+
      geom_line(aes(y = predicted), color = "red", linewidth = 1.25, na.rm = TRUE)+
      labs(x = "Time Since Start of Section (seconds)",
         y = "")+
      scale_x_continuous(breaks =c(0,30,60,90,120), limits = c(0,120))+
      scale_color_manual(labels = c("Actual","Regression"),
                         values = c("grey23", "red"))+
      theme_light()

}


