#' Remove rows from data
#'
#' @description
#' Delete rows from data. If `col` is provided, works like
#' `dplyr::filter` and filters values in column `col` that are
#' greater than `first` and less than `last`.
#' If `col` is not provided, works like subsetting based on row numbers.
#' Rows with row numbers less than `first` and greater than `last` are removed from
#' the dataset.
#'
#'
#' @param dt data.frame
#' @param first A numeric. If `col` is provided, rows with value less than
#' `first` in column `col` are removed.  If `col` is not provided, rows
#' with row number less than `first` are removed.
#' Defaults to NULL.
#' @param last A numeric. If `col` is provided, rows with value greater than
#' `last` in column `last` are removed.  If `col` is not provided, rows with
#' row number greater than `last` are removed.
#' Defaults to NULL.
#' @param col A character name of column to use to evaluate `first` and `end`.
#' Defaults to NULL.
#'
#' If `first`,`last`, and `col` are NULL, the original data will be returned
#' with a message that no rows were cropped.
#'
#' `first` and `last` are not inclusive. If `first` = 1, row 1 will not be removed.
#' To remove row 1, `first` must be 2.
#'
#' @return data.frame
crop <- function(dt, first, last, col){
  UseMethod("crop")
}

#'@export
crop.default <- function(dt, first = NULL, last = NULL, col = NULL){
  if(!is.data.frame(dt)){
    stop(
      "Provided `dt` must be a `data.frame` or inherit from `data.frame`.",
      call. = FALSE
    )
  }
  dt <- dt |> setDT() |> copy()

  if(is.null(first) & is.null(last)){
    message("NULL inputs provided. Returning `dt`.")
    return(dt)
  }
  if(is.null(col)){
    if(is.null(first)){
      first <- row.names(dt)[1]
    }
    if(is.null(last)){
      nm <- row.names(dt)
      last <- nm[length(nm)]
    }
    first <- as.integer(first)
    last <- as.integer(last)
    return(dt[s:e,
                env = list(s = first, e = last)])
  } else{
    col <- as.character(col)
    if(intersect(c(col), colnames(dt)) != col){
      stop(
        "Column `col` does not exist in `dt`."
      )
    }
    if(!is.numeric(dt[[col]])){
      stop(
        "Column `col` is not type numeric. Cannot crop using
        non-numeric values in `col`."
      )
    }

    if(!is.null(last)){
      last <- as.double(last)
      dt <- dt[c <= e,
                   env = list(c = as.name(col),
                              e = last)]
    }
    if(!is.null(first)){
      first <- as.double(first)
      dt <- dt[c >= s,
                   env = list(c = as.name(col),
                              s = first)]
    }
    return(dt)
  }
  stop(
    "I don't know how you did this, but your inputs must be wrong.
    Read documentation for `crop` and try again."
  )
}


# crop.rawdata <- function(data, start, end, col){
#
#   if(is.null(start)){
#     start <- meas_start(data)
#   }
#   if(is.null(end)){
#     end <- meas_end(data)
#   }
#   if(is.null(col)){
#     col <- "samp_num"
#   }
#
#   data(data)
#
# }
