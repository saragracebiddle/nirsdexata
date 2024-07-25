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
                 samp_num = "samp_num") |>
    crop(first = meas_start(rawdata),
         last = meas_end(rawdata),
         col = "Time") |>
    melt(id.vars = c("Time","samp_num", "ZeroedTime"),
         measure.vars = setdiff(col_names(rawdata), c("Time","samp_num", "ZeroedTime", bads(rawdata))),
         variable.name = "col_name",
         value.name = "value")|>
    remove_rows(remove = removed(rawdata),
                ref_col = "samp_num",
                value_col = "value") |>
    types_from_names(col_types = col_types(rawdata),
                     col_names = col_names(rawdata)) |>
    avg_type(col = "value", groups = c(
      "samp_num","Time","ZeroedTime","col_type"
    ))|>
    create_sections()
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
    adjust_times(sfreq = sfreq,
                 meas_start = meas_start,
                 samp_num = "samp_num") |>
    crop(first = meas_start, last = meas_end, col= "Time") |>
    melt(id.vars = c("Time","samp_num", "ZeroedTime"),
         measure.vars = setdiff(col_names, c("Time","samp_num","ZeroedTime", bads)),
         variable.name = "col_name",
         value.name = "value")|>
    remove_rows(remove = removed,
                ref_col = "samp_num",
                value_col = "value")|>
    types_from_names(col_types = col_types,
                     col_names = col_names) |>
    select_types(type = type)

}
