#' Remove rows from data
#'
#' @description
#' Delete rows from data. If `col` is provided, works like
#' `dplyr::filter` and filters values in column `col` that are
#' greater than `start` and less than `end`.
#' If `col` is not provided, works like subsetting based on row numbers.
#' Rows with row numbers less than `start` and greater than `end` are removed from
#' the dataset.
#'
#'
#' @param data data.frame
#' @param start A numeric. If `col` is provided, rows with value less than
#' `start` in column `col` are removed.  If `col` is not provided, rows
#' with row number less than `start` are removed.
#' Defaults to NULL.
#' @param end A numeric. If `col` is provided, rows with value greater than
#' `end` in column `end` are removed.  If `col` is not provided, rows with
#' row number greater than `end` are removed.
#' Defaults to NULL.
#' @param col A character name of column to use to evaluate `start` and `end`.
#' Defaults to NULL.
#'
#' If `start`,`end`, and `col` are NULL, the original data will be returned
#' with a message that no rows were cropped.
#'
#' `start` and `end` are not inclusive. If `start` = 1, row 1 will not be removed.
#' To remove row 1, `start` must be 2.
#'
#' @return data.frame
crop <- function(data, start, end, col){
  UseMethod("crop")
}

#'@export
crop.default <- function(data, start = NULL, end = NULL, col = NULL){
  if(!is.data.frame(data)){
    stop(
      "Provided `data` must be a `data.frame` or inherit from `data.frame`.",
      call. = FALSE
    )
  }
  data <- data |> setDT() |> copy()

  if(is.null(start) & is.null(end)){
    message("NULL inputs provided. Returning `data`.")
    return(data)
  }
  if(is.null(col)){
    if(is.null(start)){
      start <- row.names(data)[1]
    }
    if(is.null(end)){
      nm <- row.names(data)
      end <- nm[length(nm)]
    }
    start <- as.integer(start)
    end <- as.integer(end)
    return(data[s:e,
                env = list(s = start, e = end)])
  } else{
    col <- as.character(col)
    if(intersect(c(col), colnames(data)) != col){
      stop(
        "Column `col` does not exist in `data`."
      )
    }
    if(!is.numeric(data[[col]])){
      stop(
        "Column `col` is not type numeric. Cannot crop using
        non-numeric values in `col`."
      )
    }

    if(!is.null(end)){
      end <- as.double(end)
      data <- data[c <= e,
                   env = list(c = as.name(col),
                              e = end)]
    }
    if(!is.null(start)){
      start <- as.double(start)
      data <- data[c >= s,
                   env = list(c = as.name(col),
                              s = start)]
    }
    return(data)
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
