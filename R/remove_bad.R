#' Remove bad columns from the dataset
#'
#' @param dt data.frame of data
#' @param bads columns to remove
#'
#' @return good data
remove_bad <- function(dt, bads){
  stopifnot(is.data.frame(dt))
  if(length(bads)==0 | is.null(bads)){
    return(dt)
  }
  stopifnot(is.character(bads))
  dt <- dt |> setDT()
  cols <- setdiff(colnames(dt), bads)
  dt[,
       .SD,
       .SDcols = cols]
}
