#' Create Time column
#'
#' @param dt data.frame to use
#' @param sfreq sampling frequency
#' @param meas_start start of measurement
#' @param samp_num column containing sample numbers
#'
#' @return data.frame
adjust_times <- function(dt, sfreq, meas_start, samp_num){

  stopifnot(is.data.frame(dt))
  stopifnot(is.double(sfreq))
  stopifnot(is.double(meas_start))
  stopifnot(is.character(samp_num))

  dt <- dt |>setDT() |>copy()
  dt[, Time := x/s, env =list(x = as.name(samp_num),
                              s = sfreq)][, ZeroedTime := Time - st,
                                          env = list(st = meas_start)]
}

#' Average values across data type
#'
#' @param dt data.frame
#' @param col character, name of column to calculate on
#' @param groups columns to group by
#'
#' @return data.frame
avg_type <- function(dt, col = character(), groups = character()){
  stopifnot(is.data.frame(dt))
  stopifnot(is.character(groups))

  dt <- dt |>setDT() |> copy()

  dt[, .(value = mean(co, na.rm = T)), by = g,
     env = list(co = as.name(col),
                g = as.call(c(quote(list), lapply(groups, as.name))))]
  # data |>
  # dplyr::group_by(col_type, samp_num, ZeroedTime, Time) |>
  #   dplyr::summarise(value = mean(value), .groups = "keep") |>
  #   dplyr::ungroup()
}

#' Create on/off Kinetics sections
#'
#' @param dt data.frame

#'
#' @return data.frame
create_sections <- function(dt){

  stopifnot(is.data.frame(dt))
  dt <- dt |> setDT() |> copy()

  sections <- c("BegRest", "WarmUp","Work1","Rest1","Work2","Rest2","Work3","Rest3")

  dt[, Section := fcase(
    ZeroedTime < 120, factor("BegRest", levels = sections),
    ZeroedTime < 240, factor("WarmUp", levels = sections),
    ZeroedTime < 360, factor("Work1", levels = sections),
    ZeroedTime < 480, factor("Rest1", levels = sections),
    ZeroedTime < 600, factor("Work2", levels = sections),
    ZeroedTime < 720, factor("Rest2", levels = sections),
    ZeroedTime < 840, factor("Work3", levels = sections),
    ZeroedTime >= 840, factor("Rest3", levels= sections),
    default = NA
  )][, SectionZeroedTime := ZeroedTime - min(ZeroedTime), by = Section]

  # data |>
  #   dplyr::mutate(Section = dplyr::case_when(
  #     ZeroedTime < 120 ~ "BegRest",
  #     ZeroedTime < 240 ~ "WarmUp",
  #     ZeroedTime < 360 ~ "Work1",
  #     ZeroedTime < 480 ~ "Rest1",
  #     ZeroedTime < 600 ~ "Work2",
  #     ZeroedTime < 720 ~ "Rest2",
  #     ZeroedTime < 840 ~ "Work3",
  #     ZeroedTime < 960 ~ "Rest3"
  #   )) |>
  #   dplyr::group_by(Section) |>
  #   dplyr::mutate(SectionZeroedTime = ZeroedTime - min(ZeroedTime)) |>
  #   dplyr::ungroup()
}

#' Remove rows from the data
#'
#' input data must be in long format
#' and values are replaced with NA
#' replacing with NA helps with ggplot geom_line functions
#'
#' @param dt data.frame
#' @param remove vector of data to remove
#' @param ref_col column name to find `remove`
#' @param value_col column to remove value from
#'
#' @return data.frame
remove_rows <- function(dt = data.frame(),
                        remove,
                        ref_col = character(),
                        value_col = character()){

  stopifnot(is.data.frame(dt))

  dt <- dt |> setDT() |> copy()

  if(is.null(remove) | length(remove) < 1){
    return(dt)
  }

  dt[col %in% remove,
     value_col := NA,
     env = list(col = as.name(ref_col),
                remove = remove,
                value_col = as.name(value_col))]
}

#' Filter by selected column type/s
#'
#' @param dt data.frame
#' @param type character vector of column type to include.
#'
#' @return data
select_types <- function(dt, type = NULL){

  #TODO add in error if no col_type column
  #TODO figure out how to use the minus sign to remove types
  if(is.null(type)){
    return(dt)
  } else{
    if(length(type) == 1){type <- c(type)}
    dt |>
      dplyr::filter(col_type %in% type)
  }
}

#' Label On/Off Kinetics Sections
#'
#' @param dt data.frame
#' @param col_types character vector of col_types
#' @param col_names character vector of col_names
#'
#' @return data.frame
types_from_names <- function(dt = data.frame(),
                             col_types = character(),
                             col_names = character()){

  stopifnot(is.data.frame(dt))
  stopifnot(is.character(col_types))
  stopifnot(is.character(col_names))

  dt <- dt |> setDT() |> copy()
  #TODO if data frame doesnt have column for col_names,
  # this function cannot work
  #TODO rename col_names and col_types as signal_names and signal_types
  # TODO add parameter for name of column that holds signal_names
  # with default to "signal_name"

  lookup <- col_types |> stringr::str_to_upper()
  names(lookup) <- col_names

  dt[, col_type := lookup[col_name]]
}





