#' Nest data.table by Group
#'
#' @param dt data.table
#' @param ...  other arguments
#' @param .key key to group by
#'
#' @return nested data.table
#' @export
#'
#' @details
#'function created by Tyson Barrett
# List-columns in data.table: Nesting and unnesting data tables and vectors
# Abstract in PDF form online
group_nest_dt <- function(dt, ..., .key = "data"){
  stopifnot(data.table::is.data.table(dt))
  by <- substitute(list(...))
  dt <- dt[, list(list(.SD)), by = eval(by)]
  data.table::setnames(dt, old = "V1", new = .key)
  dt
}

#' Unnest data.table
#'
#' @param dt nested data.table
#' @param col column to unnest
#' @param id id to group by
#'
#' @return unnested data.table
#' @export
unnest_dt <- function(dt, col, id){
  stopifnot(data.table::is.data.table(dt))
  by <- substitute(id)
  col <- substitute(unlist(col, recursive = FALSE))
  dt[, eval(col), by = eval(by)]
}
