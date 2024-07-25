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
    crop(start = meas_start, end = meas_end, col= "Time") |>
    tidyr::pivot_longer(cols = setdiff(col_names, bads),
                        names_to = "col_name",
                        values_to = "value")|>
    remove_rows(remove = removed)|>
    types_from_names(col_types = col_types,
                     col_names = col_names) |>
    select_types(type = type)

}
