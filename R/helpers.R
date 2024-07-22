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

#' Crop data by meas_start and meas_end
#'
#' @param data data.frame
#' @param meas_start start of testing in seconds. defaults to 0.
#' @param meas_end end of testing in seconds. defaults to NULL which does not
#' remove anything from the end of the data.frame
#'
#' @return data.frame
crop <- function(data, meas_start = 0, meas_end = NULL){
  #TODO if data does not have time columns to work with
  # TODO make meas_start and meas_end work with time in seconds and sample numbers?

  if(is.null(meas_end)){
    data |>
      dplyr::filter(ZeroedTime >= 0)
  } else{
    data |>
      dplyr::filter(ZeroedTime >= 0 & ZeroedTime <= meas_end)
  }

}

#' Remove bad columns from the dataset
#'
#' @param data data frame of data
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
#' @param data raw data
#' @param removed vector of sample numbers to remove
#'
#' @return data.frame
remove_rows <- function(data, removed){
  # TODO works with wide format as well as long format?
  if(is.null(removed) | length(removed) < 1){
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


#' Prepare data to pass into shiny plotting function
#'
#' @param data data.frame
#' @param sfreq sampling frequency
#' @param meas_start start of measurement
#' @param meas_end end of measurement
#' @param bads character vector of column names marked as "bad"
#' @param removed integer vector of sample numbers to remove from the dataset
#' @param col_types character vector of column types
#' @param col_names character vector of column names
#' @param type type of signal to display on the graph. Defaults to NULL and displays all types.
#'
#' @return plotdata
prepare_shiny_plot <- function(data,
                               sfreq,
                               meas_start,
                               meas_end,
                               bads,
                               removed,
                               col_types,
                               col_names,
                               type = NULL){

 data |>
    remove_bad(bads = bads) |>
    adjust_times(sfreq = sfreq, meas_start = meas_start, samp_num = samp_num) |>
    crop(meas_start = meas_start, meas_end = meas_end) |>
    tidyr::pivot_longer(cols = setdiff(col_names, bads),
                        names_to = "col_name",
                        values_to = "value")|>
    remove_rows(removed = removed)|>
    types_from_names(col_types = col_types,
                     col_names = col_names) |>
    select_types(type = type)

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
    remove_bad(bads = rawdata$info$bads) |>
    adjust_times(sfreq = rawdata$info$sfreq,
                 meas_start = rawdata$info$bounds$meas_start,
                 samp_num = samp_num) |>
    crop(meas_start = rawdata$info$bounds$meas_start,
         meas_end = rawdata$info$bounds$meas_end) |>
    tidyr::pivot_longer(cols = setdiff(
      rawdata$info$cols$col_name,
      rawdata$info$bads
    ), names_to = "col_name",
    values_to = "value") |>
    remove_rows(removed = rawdata$info$bounds$removed) |>
    types_from_names(col_types = rawdata$info$cols$col_type,
                     col_names = rawdata$info$cols$col_name) |>
    avg_type()|>
    create_sections()
}
