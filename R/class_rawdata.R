#' Rawdata Class Constructor
#'
#' @param data data frame
#' @param info measurement metadata
#'
#' @return data frame of raw data
#' @export
new_rawdata <- function(data = data.frame(),
                        info = info(),
                        ...,
                        class = character()
                        ){
  stopifnot(is.data.frame(data))
  #TODO create is.info function


  #validate_rawdata(data, info)

  first_samp = data[["samp_num"]][[1]]

  last_samp = data[["samp_num"]][[length(data[["samp_num"]])]]
  meas_start = first_samp / info[["sfreq"]]
  meas_end = last_samp / info[["sfreq"]]

  info[["bounds"]] = list("first_samp" = first_samp,
                          "last_samp" = last_samp,
                          "meas_start" = meas_start,
                          "meas_end" = meas_end,
                          "removed" = c())

  rawdata <- list("data" = data,
                  "info" = info)

  structure(rawdata,
            ...,
            class = c(class, "rawdata", class(list())))
}

#' Rawdata Class Validator
#'
#' @param data data frame
#' @param info measurement metadata
#'
#' @return data
#' @export
validate_rawdata <- function(data, info){

# \\TODO rawdata validation



  data
}

#' Rawdata Class Helper
#'
#' @param data data frame of raw data
#' @param col_names character vector of column names
#' @param col_types character vector of column types
#' @param device_info list of device information
#' @param subj_info list of subject information
#' @param meas_date date of measurement
#' @param meas_start time to use as the start of the test
#' @param sfreq sample frequency
#' @param samps name of column of sample numbers
#' @param bads list of columns that were of bad quality and should not be used in analysis
#'
#'
#' @return rawdata
#' @export
rawdata <- function(data = data.frame(),
                    col_names = character(),
                    col_types = "misc",
                    device_info = NULL,
                    subj_info = NULL,
                    meas_date= NULL,
                    meas_start = NULL,
                    sfreq = NULL,
                    samps = "samp_num",
                    bads = NULL
                    ){
  data <- data |> setDT()

  info <- info(col_names = col_names,
               col_types = col_types,
               device_info = device_info,
               subj_info = subj_info,
               meas_date = meas_date,
               meas_start = meas_start,
               sfreq = sfreq,
               samps = samps,
               bads = bads)

  new_rawdata(data, info)
}

#' @export
print.rawdata <- function(x, ...){

  info <- x[["info"]]
  data <- x[["data"]]


  info
  head(data)
  invisible(x)
}

#' @export
plot.rawdata <- function(x, ...){

  data <- x[["data"]]
  info <- x[["info"]]

  plt <- ggplot2::ggplot(data,
                         ggplot2::aes(x = samp_num))

}

#' @export
adjust_start.rawdata <- function(x, adj_by){
  info = x[["info"]]

  info <- adjust_start(info, adj_by)

  x
}

#' @export
adjust_end.rawdata <- function(x, adj_by){

  info = x[["info"]]

  info <- adjust_end(info, adj_by)

  info
}




