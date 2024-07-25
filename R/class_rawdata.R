#' Rawdata Class Constructor
#'
#' @param data data frame
#' @param info measurement metadata
#' @param ... arguments to pass to children
#' @param class optional character of subclass
#'
#' @return data frame of raw data
#' @export
new_rawdata <- function(data = data.frame(),
                        info = info(),
                        ...,
                        class = character()
                        ){
  stopifnot(is.data.frame(data))
  stopifnot(is.info(info))


  #validate_rawdata(data, info)

  first_samp(info) <- data[["samp_num"]][[1]]

  last_samp(info) <-  data[["samp_num"]][[length(data[["samp_num"]])]]
  if(is.null(meas_start(info))){
    meas_start(info) <-  first_samp(info) / sfreq(info)
  }
  if(is.null(meas_end(info))){
    meas_end(info) <-  last_samp(info) / sfreq(info)
  }

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

  info <- create_info(col_names = col_names,
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


############# Accessors and Setters

#' @export
bads.rawdata <- function(x){
  x$info$bads
}

#' @export
`bads<-.rawdata` <- function(x, value){
  x$info$bads <- value
  x
}


#' @export
col_names.rawdata <- function(x){
  x$info$cols$col_name
}

#' @export
col_types.rawdata <- function(x){
  x$info$cols$col_type
}

#' @export
device_model.rawdata <- function(x){
  x$info$device_info$device_model
}

#' @export
device_site.rawdata <- function(x){
  x$info$device_info$device_site
}

#' @export
device_type.rawdata <- function(x){
  x$info$device_info$device_type
}

#' @export
first_samp.rawdata <- function(x){
  x$info$bounds$first_samp
}

#' @export
info.rawdata <- function(x){
  i <- x$info
  return(i)
}

#' @export
last_samp.rawdata <- function(x){
  x$info$bounds$last_samp
}

#' @export
meas_date.rawdata <- function(x){
  x$info$meas_date
}

#' @export
meas_end.rawdata <- function(x){
  x$info$bounds$meas_end
}

#' @export
`meas_end<-.rawdata` <- function(x, value){
  x$info$bounds$meas_end <- value
  x
}

#' @export
meas_id.rawdata <- function(x){
  x$info$meas_id
}

#' @export
`meas_id<-.rawdata` <- function(x, value){
  x$info$meas_id <- value
  x
}

#' @export
meas_start.rawdata <- function(x){
  x$info$bounds$meas_start
}

#' @export
`meas_start<-.rawdata` <- function(x, value){
  x$info$bounds$meas_start <- value
  x
}

#' @export
removed.rawdata <- function(x){
  x$info$bounds$removed
}

#' @export
`removed<-.rawdata` <- function(x, value){
  x$info$bounds$removed <- value
  x
}

#' @export
sfreq.rawdata <- function(x){
  x$info$sfreq
}

#' @export
subj_age.rawdata <- function(x){
  x$info$subj_info$subj_age
}

#' @export
subj_height.rawdata <- function(x){
  x$info$subj_info$subj_height
}

#' @export
subj_id.rawdata <- function(x){
  x$info$subj_info$subj_id
}

#' @export
`subj_id<-.rawdata` <- function(x, value){
  x$info$subj_info$subj_id <- value
  x
}

#' @export
subj_sex.rawdata <- function(x){
  x$info$subj_info$subj_sex
}

#' @export
subj_weight.rawdata <- function(x){
  x$info$subj_info$subj_weight
}

