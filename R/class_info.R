#' Info Class Constructor
#'
#' @param info list
#'
#' @return info list
#'
#'
#' @export
new_info <- function(info = list()){
  stopifnot(is.list(info))

  validate_info(info)

  inherit <- class(info)

  structure(info, class = c("info", inherit))
}

#' Info Class Validator
#'
#' @param info list
#'
#' @return list
#'
#' @export
validate_info <- function(info){

  # TODO
  # meas_start cannot be less than the first samp_num/sfreq


  # if subj_sex is provided,
  # check it is a valid input ("M" of"F")
  # if(!is.null(info$subj_info$subj_sex)){
  #   info$subj_info$subj_sex = match.arg(info$subj_info$subj_sex, c("M","F"))
  # }

  # if(!is.null(col_info)){
  #   col_names <- col_info$col_names
  #   col_types <- col_info$col_types
  # }
  # check col_info contains col_names and col_types
  # that col_types is of length 1 or the same length as col_names
  # that col_names and col_types are both character vectors
  # if(!is.character(col_names))

  info
}

#' Create an Info object
#'
#' @description
#' Create `info` object to record measurement metadata. This object behaves
#' like a list. It contains all metadata available for the measurement.
#' The only required input is `col_names`. Even if `col_names` is the only
#' user-supplied input, the `info` object will have empty slots for all possible
#' values.
#'
#' @details
#' Implementation is based on the info class for MNE python.
#'
#'  `col_names`is the only required argument. `col_types` defaults to "misc"
#' for all cols without other input. If both `col_names` and `col_types` are
#' provided, the length of `col_names` must match the length of `col_types` if
#' `col_types` is longer than 1. If `col_types` is of length 1, then all cols
#' will be that type.
#'
#'
#'
#' @param col_names A character vector of names of data columns
#' @param col_types  A character vector of data types. Defaults to "misc".
#' @param device_type Device type that collected the data.
#' @param device_model Device model name.
#' @param device_serial Device serial identifier.
#' @param device_site Device site.
#' @param device_info A list of device information.
#' @param subj_id subject identifier
#' @param subj_sex subject sex
#' @param subj_age subject age
#' @param subj_height subject height
#' @param subj_weight subject weight
#' @param subj_info subject information as a list
#' @param desc measurement description
#' @param meas_date datetime of the measurement
#' @param meas_id measurement id
#' @param meas_start time to use as the start of the testing
#' @param samps name of column containing sample numbers
#' @param sfreq sampling frequency in Hz
#' @param bads list of channels that have bad signal
#'
#' @return Info object containing all provided parameters. The Info object
#' inherits from list, so will behave as a list.
#'
#' @export
create_info <- function(col_names = character(),
                 col_types = "misc",
                 col_info = NULL,
                 device_type = NULL,
                 device_model = NULL,
                 device_serial = NULL,
                 device_site = NULL,
                 device_info = NULL,
                 subj_id = NULL,
                 subj_sex = NULL,
                 subj_age = NULL,
                 subj_height = NULL,
                 subj_weight = NULL,
                 subj_info = NULL,
                 desc = NULL,
                 meas_date = NULL,
                 meas_id = NULL,
                 meas_start = NULL,
                 samps = NULL,
                 sfreq = NULL,
                 bads = NULL){



  bounds = list("meas_start" = meas_start,
                "meas_end" = NULL,
                "first_samp" = NULL,
                "last_samp" = NULL,
                "removed" = c())

  cols <- data.frame(col_name = col_names,
                     col_type = col_types)

  # TODO add in col_info for extra information
  # maybe as a list column?

  if(is.null(device_info)){
    device_info = list("device_type" = device_type,
                      "device_model" = device_model,
                      "device_serial" = device_serial,
                      "device_site" = device_site)
  }

  if(is.null(subj_info)){
    subj_info = list("subj_id" = subj_id,
                     "subj_sex" = subj_sex,
                     "subj_age" = subj_age,
                     "subj_height" = subj_height,
                     "subj_weight" = subj_weight)
  }

  info = list("cols" = cols,
              "device_info" = device_info,
              "subj_info" = subj_info,
              "meas_date" = meas_date,
              "bounds" = bounds,
              "sfreq" = sfreq,
              "samps" = NULL,
              "bads" = bads,
              "desc" = desc
              )

  new_info(info)
}

#' @export
print.info <- function(x, ...){

  ncols = length(x[["cols"]][["col_name"]])

  meas_length = x[["bounds"]][["last_samp"]]

  meas_date = x[["meas_date"]]

  device_info = x[["device_info"]]
  subj_info = x[["subj_info"]]

  msg_nchan <- glue::glue("{device_info$device_type} {device_info$device_model}
                          {subj_info$subj_id}  {meas_date}
                          Measurement with {ncols} columns and {meas_length} samples.")

  # \\TODO create more informative print method for info object
  # needs to be able to ignore missing/NULL elements

  #device_type <- x[["device_info"]][["type"]]
  #device_model <- x[["device_info"]][["model"]]
  #device_serial <- x[["device_info"]][["serial"]]

  #device <- glue::glue("")

  print(msg_nchan)
  invisible(x)
}

#' @export
is.info <- function(x){
  if(length(intersect(c("info"), class(x))) == 1){
    return(TRUE)
  }
  return(FALSE)
}


####### Accessors and Setters

#' @export
bads.info <- function(x) x$bads

#' @export
`bads<-.info` <- function(x, value) {
  x$bads <- value
  x
}

#' @export
col_names.info <- function(x){
  x$cols$col_names
}

#' @export
col_types.info <- function(x){
  x$cols$col_types
}

#' @export
device_type.info <- function(x){
  x$device_info$device_type
}

#' @export
`device_type<-.info` <- function(x, value){
  x$device_info$device_type <- value
  x
}

#' @export
device_model.info <- function(x){
  x$device_info$device_model
}

#' @export
`device_model<-.info` <- function(x, value){
  x$device_info$device_model <- value
  x
}

#' @export
device_site.info <- function(x){
  x$device_info$device_site
}

#' @export
`device_site<-.info` <- function(x, value){
  x$device_info$device_site <- value
  x
}

#' @export
device_type.info <- function(x){
  x$device_info$device_type
}

#' @export
`device_type<-.info` <- function(x, value){
  x$device_info$device_type <- value
  x
}

#' @export
first_samp.info <- function(x) x$bounds$first_samp

#' @export
`first_samp<-.info` <- function(x,value) {
  x$bounds$first_samp <- value
  x
}

#' @export
last_samp.info <- function(x) x$bounds$last_samp

#' @export
`last_samp<-.info` <- function(x,value) {
  x$bounds$last_samp <- value
  x
}
#' @export
meas_date.info <- function(x){
  x$meas_date
}

#' @export
`meas_date<-.info` <- function(x, value){
  x$meas_date <- value
  x
}

#' @export
meas_end.info <- function(x){
  x$bounds$meas_end
}

#' @export
`meas_end<-.info` <- function(x, value){
  x$bounds$meas_end <- value
  x
}

#' @export
meas_id.info <- function(x){
  x$meas_id
}

#' @export
`meas_id<-.info` <- function(x, value){
  x$meas_id <- value
  x
}

#' @export
meas_start.info <- function(x){
  x$bounds$meas_start
}

#' @export
`meas_start<-.info` <- function(x, value){
  x$bounds$meas_start <- value
  x
}

#' @export
sfreq.info <- function(x){
  x$sfreq
}

#' @export
`sfreq<-.info` <- function(x, value){
  x$sfreq <- value
  x
}

#' @export
subj_id.info <- function(x){
  x$subj_info$subj_id
}

#' @export
`subj_id<-.info` <- function(x, value){
  x$subj_info$subj_id <- value
  x
}

#' @export
subj_sex.info <- function(x){
  x$subj_info$subj_sex
}

#' @export
`subj_sex<-.info` <- function(x, value){
  x$subj_info$subj_sex <- value
  x
}

#' @export
subj_age.info <- function(x){
  x$subj_info$subj_age
}

#' @export
`subj_age<-.info` <- function(x, value){
  x$subj_info$subj_age <- value
  x
}

#' @export
subj_height.info <- function(x){
  x$subj_info$subj_height
}

#' @export
`subj_height<-.info` <- function(x, value){
  x$subj_info$subj_height <- value
  x
}

#' @export
subj_weight.info <- function(x){
  x$subj_info$subj_weight
}

#' @export
`subj_weight<-.info` <- function(x, value){
  x$subj_info$subj_weight <- value
  x
}


# adjust_start <- function(x, adj_by){
#   UseMethod("adjust_start")
# }


# adjust_start.info <- function(x, adj_by){
#
#   start <- x[["bounds"]][["meas_start"]]
#
#   if((start + adj_by) < 0){
#     stop(
#       "Cannot adjust meas_start to before data begins.",
#       call. = FALSE
#     )
#   }
#
#   end <- x[["bounds"]][["meas_end"]]
#
#   if((start+adj_by) > end){
#     stop(
#       "Cannot adjust meas_start to after data ends.",
#       call. = FALSE
#     )
#   }
#
#   new = start + adj_by
#
#   x[["bounds"]][["meas_start"]] <- new
#
#   x
# }
#
#
# adjust_end.info <- function(x, adj_by){
#
#   start <- x[["bounds"]][["meas_start"]]
#   end <- x[["bounds"]][["meas_end"]]
#
#   if((end + adj_by) < start){
#     stop(
#       "Cannot adjust meas_end to before data begins.",
#       call. = FALSE
#     )
#   }
#
#   new = end + adj_by
#
#   sfreq = x[["sfreq"]]
#   end_samp = x[["bounds"]][["last_samp"]]
#
#   if((new)*sfreq > end_samp){
#     warning(
#       "Adjusted meas_end after data ends. Setting meas_end to time of last sample.",
#       call. = FALSE
#     )
#     new = end_samp / sfreq
#   }
#
#   x[["bounds"]][["meas_end"]] <- new
#
#   x
# }
