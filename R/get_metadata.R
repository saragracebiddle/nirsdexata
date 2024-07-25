#' Get metadata from scanned list
#'
#' @param s scanned list of data as characters
#' @param type type of metadata to look for
#'
#' @return value
#' @export
get_metadata <- function(s, type){

  # check for valid arguments
  if(!is.character(s)){
    rlang::abort(
      "error_bad_arg",
      message = "Supplied argument `s` is not a list of characters.")
  }
  if(!is.character(type)){
    rlang::abort(
      "error_bad_arg",
      message = "Supplied argument `type` is not a character.")
  }


  if(type == "legend"){

    # find index for the start of the legend in the list s
    # throw error if it can't be found
    legendstart = stringr::str_which(s, "Legend")
    if(length(legendstart) <1){
      error_file_metadata("legend")
    }


    if(length(stringr::str_which(s, "Trace")) > 0){
      legendtype = "haemoglobin"
    } else {
      legendtype = "optical_density"
    }

    # find index for the end of the legend in the list s
    # throw error if it can't be found
    legendend = stringr::str_which(s[(legendstart + 3):length(s)], "Event")
    if(length(legendend) < 1){
      error_file_metadata("legend")
    }

    # get the legend entries from the list
    # start at legendstart + 1 because the Trace is not a legend item
    # end at legendstart + legendend - 1
    p = s[(legendstart + 3):(legendstart + legendend)]


    # put into a 2 column data frame
    # column `X1` has column numbers
    # column `X2` has names of columns supplied by Oxysoft
    legend = list("export_type"= legendtype,
                  "cols" = data.frame(
                    matrix(
                      p[!is.na(p)],
                      ncol = 2,
                      byrow = T
                    )
                  ))


  } else if(type == "wavelengths"){
    #\\TODO
  } else {
    # data frame with metadata types and their respective
    # regular expressions to look for in 's' and how many
    # indexes further to find the actual values for the metadata
    mtdt = data.frame(
      types = c("num_samples",
                "hz",
                "optode_template",
                "meas_date",
                "serial"),
      txt = c("(Export|Selected) time span \\(sample numbers\\):?",
              "Export sample rate",
              "Optode-template",
              "Start of measurement",
              "Device id"),
      addi = c(2,1,1,1,1)
    )

    # check the type of metadata supplied actually is a supported type
    if(!(type %in% mtdt$types)){
      rlang::abort(
        "error_unsupp_metadata",
        message = "The supplied type of metadata is not supported.")
    }

    # regular expression to look for
    find <- mtdt[mtdt$types == type,]$txt

    # index where regular expression was found in list s
    i = which(stringr::str_detect(s,  find))

    # if regular expression is not found, throw error
    if(length(i) < 1){
      error_file_metadata(type)
    }

    # retrieve value from list s
    out <- s[i + mtdt[mtdt$types == type,]$addi]

    # return value as respective datatype
    if(type == "num_samples"){
      as.integer(out)
    } else if (type == "hz"){
      as.double(out)
    } else{
      out
    }
  }
}

#' Get participant ID from the file name
#'
#' @param file string
#'
#' @return string
#' @export
get_id_from_filename <- function(file){
  stringr::str_extract(
    file,
    "(?<=LIGHTSHIELD_?0{0,2})[1,2,3,4,5,6,7,8,9][:digit:]?"
  )
}

#' Make usable short labels for columns from the long names given by oxysoft
#'
#' @param longname character string
#'
#' @return shorter character string
#' @export
get_labels <- function(longname){

  if(stringr::str_detect(
    longname,
    "(S|s)ample (N|n)umber")
  ){
     "samp_num"
  } else if(stringr::str_detect(
    longname,
    "TSI Fit Factor"
  )){
    "TSIFF"
  } else if(stringr::str_detect(
    longname, "TSI")){
    "TSI"
  } else{
    stringr::str_to_lower(
      stringr::str_c(
        stringr::str_extract(
          longname, "Tx[\\d]"),
        stringr::str_extract(longname, "(O2|H)Hb")
      )
    )
  }
}

#' Get channel types from column names
#'
#' @param longname character string
#'
#' @return ch_type
#' @export
get_ch_type <- function(longname){
  if(stringr::str_detect(
    longname,
    "(S|s)ample (N|n)umber")
  ){
    "samp_num"
  } else if(stringr::str_detect(
    longname,
    "O2Hb"
  )){
    "hbo"
  } else if(stringr::str_detect(
    longname,
    "HHb"
  )) {
    "hbr"
  } else if(stringr::str_detect(
    longname,
    "TSI"
  )) {
    "tsi"
  } else {
    "misc"
  }
}


