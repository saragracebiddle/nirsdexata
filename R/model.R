#' nls wrapped with possibly
#'
#'
#' @return function
#' @export
possibly_nls <- purrr::possibly(nls, otherwise = NA)


#' Self Starting Asymptotic Regression
#'
#' @param df data frame
#'
#' @return list of coefficients from regression if successful, NA if unable to run
#' @export
asym_model <- function(df){
  ## TODO use tidy evaluation and data masking to use input for x and y variables
  suppressWarnings(
    possibly_nls(value ~ SSasymp(SectionZeroedTime, Asym, R0, lrc), df)
  )
}


#' Run Regressions on On/Off Kinetics Data
#'
#' @param rawdata rawdata object
#'
#' @return dataframe
#' @export
run_models <- function(rawdata){
  nested <- group_nest_dt(Section, col_type) |>
    setkey(Section, col_type)

  nested[, Model := map(data, asym_model)]
}