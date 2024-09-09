#' NIRS Data Input
#'
#' @description
#' Reads a file of NIRS data and creates a data frame from it.
#'
#' @param file file to read
#' @param ... other parameters to pass to class methods
#'
#' @return rawdata
#' @export
#'
read_nirs <- function(file, ...){

  UseMethod("read_nirs")

}

#' @export
read_nirs.character <- function(file, ...){
  # throw file does not exist error if does not exist
  if(!file.exists(file)){
    error_file_dne(file)
  }

  class(file) <- extract_ext(file)

  callGeneric()
}

#' @export
read_nirs.txt <- function(file, ...,  delim = NULL){

  if(is.null(delim)){
    delim = '\t'
  }

  if(is.null(colnames)){
    colnames = 'short'
  }

  class(file) <- "character"

  s = tryCatch(scan(file,
                   n=1000,
                   sep=delim,
                   na.strings="",
                   what=character(),
                   quiet=T,
                   skipNul=T),
              error = function(cnd){
                rlang::abort("error_in_file_scan",
                             message= "Error when scanning file.")
              })

  numSamples <- get_metadata(s, type = "num_samples")

  hz <- get_metadata(s, type = "hz")
  ot <- get_metadata(s, type = "optode_template")
  legendinfo <- get_metadata(s, type = "legend")
  export_type = legendinfo[["export_type"]]
  legend = legendinfo[["cols"]]
  meas_date <- get_metadata(s, type = "meas_date")

  serialnum <- get_metadata(s, type = "serial")
  wavelengths <- get_metadata(s, type = "wavelengths")

  srows = scan(
    file,
    na.strings = "",
    what = character(),
    sep ="\n",
    skipNul = T,
    quiet = T,
    blank.lines.skip = F
  )

  row1 = stringr::str_which(
    srows,
    stringr::str_c(
      legend[!is.na(legend[1]),1], collapse = "\t"
    )
  ) +1

  problems = suppressWarnings(readr::parse_double(
    scan(
      file,
      na.strings = "",
      sep = "\t",
      what = character(),
      skip = row1,
      skipNul = T,
      quiet = T)
  )
  )

  out = data.frame(
    matrix(
      problems[!is.na(problems)],
      ncol = length(
        legend[!is.na(legend[1]),1][!stringr::str_detect(
          legend[!is.na(legend[1]),2],
          "Event"
        )]
      ),
      byrow = T)
  )

  collabels <- legend[!is.na(legend[1]),2][!stringr::str_detect(
    legend[!is.na(legend[1]),2], "Event"
  )]



  # \\TODO
  # instead of doing this explicitly, take from the column names
  # and other metadata
  if(export_type == "haemoglobin"){


        ch_name = c("S1_D1_759","S1_D1_854","S2_D1_759","S2_D1_854","S3_D1_761","S3_D1_856", "TSI", "TSIFF")
        ch_type = c(rep(c("hbo", "hbr"),3), "tsi", "misc")
        source_num = c(rep(c(1L,2L,3L), each = 2), NA_integer_, NA_integer_)
        det_num = c(rep(1L, times = 6), NA_integer_, NA_integer_)
        wavelength = c(rep(c(759L, 854L), each = 2), 761L, 856L, NA_integer_, NA_integer_)


  } else {

      ch_name = c("S1_D1_759","S1_D1_854","S2_D1_759","S2_D1_854","S3_D1_761","S3_D1_856")
      ch_type = rep("optical_density", times = 6)
      source_num = c(rep(c(1L,2L,3L), each = 2))
      det_num = c(rep(1L, times = 6))
      wavelength = c(rep(c(759L, 854L), each = 2), 761L, 856L)

  }

  sampcol <- "samp_num"


  colnames(out) <- c(sampcol, ch_name)


  if(ot == "PortaMon TSI"){
    bads = c("S3_D1_761", "S3_D1_856", "TSIFF")
  } else {
    bads = c("TSIFF")
  }

  info <- create_info(col_names = ch_name,
                      col_types = ch_type,
                      device_type = "NIRS",
                      device_model = "PortaMon",
                      device_serial = serialnum,
                      meas_date =meas_date,
                      sfreq = hz,
                      bads = bads)


  rawdata <- rawdata(
    data = out,
    col_names = ch_name,
    col_types = ch_type,
    device_info = list("device_type" = "NIRS",
                       "device_model" = "PortaMon",
                       "device_serial" = serialnum),
    meas_date = meas_date,
    sfreq = hz,
    bads = bads
  )

  rawdata
}

#' Retrieve example data sets
#'
#' @param path path to example data sets
#'
#' @return file path
#' @export
#'
#' @examples
#' read_nirs_example()
#' read_nirs_example("on_off_kinetics.txt")
#'
read_nirs_example <- function(path = NULL){
  if (is.null(path)) {
    dir(system.file("extdata", package = "nirsdexata"))
  } else {
    system.file("extdata", path, package = "nirsdexata", mustWork = TRUE)
  }
}

#' Get the file extension type
#'
#' @param file name of the file
#'
#' @return file extension type
#' * csv
#' * xlsx
#' * txt
#'
#' @export
extract_ext <- function(file){
  exttypes = c('csv','xlsx','txt')

  ext = stringr::str_extract(
    file,
    stringr::str_c(
      "(?<=\\.)", exttypes,"$", collapse = "|"
    )
  )

  if(is.na(ext)){
    rlang::abort("error_filetype",
                 message = "File Type {ext} not currently supported. Please input files as .txt, .csv, or .xlsx")
  }

  ext
}



