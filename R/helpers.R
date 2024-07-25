#' Label On/Off Kinetics Sections
#'
#' @param data data.frame
#' @param col_types character vector of col_types
#' @param col_names character vector of col_names
#'
#' @return data.frame
types_from_names <- function(data, col_types, col_names){

  #TODO if data frame doesnt have column for col_names,
  # this function cannot work
  #TODO rename col_names and col_types as signal_names and signal_types
  # TODO add parameter for name of column that holds signal_names
  # with default to "signal_name"

  lookup <- col_types |> stringr::str_to_upper()
  names(lookup) <- col_names

  data |>
    dplyr::mutate(col_type = unname(lookup[data$col_name]))
}



#' Remove bad columns from the dataset
#'
#' @param data data.frame of data
#' @param bads columns to remove
#'
#' @return good data
remove_bad <- function(data, bads){
  if(length(bads)==0){
    data
  }
    data |>
      dplyr::select(!dplyr::any_of(bads))
}

#' Remove rows from the data
#'
#' input data must be in long format
#' and values are replaced with NA
#' replacing with NA helps with ggplot geom_line functions
#'
#' @param data data.frame
#' @param remove vector of data to remove
#' @param col column to remove `remove` from
#'
#' @return data.frame
remove_rows <- function(data = data.frame(), remove, col = charact){

  stopifnot(is.data.frame(data))

  data <- data |> setDT() |> copy()

  if(is.null(remove) | length(remove) < 1){
    return(data)
  }
  data |>
    dplyr::rows_update(y = data.frame(
      samp_num = removed,
      value = NA),
      by = "samp_num"
    )
}



#' Create Time column
#'
#' @param data data.frame to use
#' @param sfreq sampling frequency
#' @param meas_start start of measurement
#' @param samp_num column containing sample numbers
#'
#' @return data.frame
adjust_times <- function(data, sfreq, meas_start, samp_num){

  stopifnot(is.data.frame(data))


  data |>
    dplyr::mutate(Time = samp_num / sfreq,
                  ZeroedTime = Time - meas_start)
}

#' Filter by selected column type/s
#'
#' @param data data.frame
#' @param type character vector of column type to include.
#'
#' @return data
select_types <- function(data, type = NULL){

  #TODO add in error if no col_type column
  #TODO figure out how to use the minus sign to remove types
  if(is.null(type)){
    data
  } else{
    if(length(type) == 1){type <- c(type)}
    data |>
      dplyr::filter(col_type %in% type)
  }
}





#' Average values across data type
#'
#' @param data data.frame
#'
#' @return data.frame
avg_type <- function(data){
  data |>
  dplyr::group_by(col_type, samp_num, ZeroedTime, Time) |>
    dplyr::summarise(value = mean(value), .groups = "keep") |>
    dplyr::ungroup()
}

#' Create on/off Kinetics sections
#'
#' @param data data.frame
#'
#' @return data.frame
create_sections <- function(data){
  data |>
    dplyr::mutate(Section = dplyr::case_when(
      ZeroedTime < 120 ~ "BegRest",
      ZeroedTime < 240 ~ "WarmUp",
      ZeroedTime < 360 ~ "Work1",
      ZeroedTime < 480 ~ "Rest1",
      ZeroedTime < 600 ~ "Work2",
      ZeroedTime < 720 ~ "Rest2",
      ZeroedTime < 840 ~ "Work3",
      ZeroedTime < 960 ~ "Rest3"
    )) |>
    dplyr::group_by(Section) |>
    dplyr::mutate(SectionZeroedTime = ZeroedTime - min(ZeroedTime)) |>
    dplyr::ungroup()
}

#' Prepare data for regressions
#'
#' @param rawdata rawdata object
#'
#' @return data.frame
prepare_for_modeling <- function(rawdata){
  rawdata$data |>
    remove_bad(bads = bads(rawdata)) |>
    adjust_times(sfreq = sfreq(rawdata),
                 meas_start = meas_start(rawdata),
                 samp_num = samp_num) |>
    crop(start = meas_start(rawdata),
         end = meas_end(rawdata),
         col = "Time") |>
    tidyr::pivot_longer(cols = setdiff(
      col_names(rawdata),
      bads(rawdata)
    ), names_to = "col_name",
    values_to = "value") |>
    remove_rows(remove = removed(rawdata)) |>
    types_from_names(col_types = col_types(rawdata),
                     col_names = col_names(rawdata)) |>
    avg_type()|>
    create_sections()
}
