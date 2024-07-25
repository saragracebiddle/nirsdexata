#' Acessor for `bads`
#'
#' @param x  object
#'
#' @return value if `bads` exists in the object, NULL otherwise
#' @export
bads <- function(x){
  UseMethod("bads")
}

#' @export
#' @method bads default
bads.default <- function(x){
  NULL
}

#' Set the value of `bads`
#'
#' @param x object
#' @param value new value
#'
#' @return modified object
#' @export
`bads<-` <- function(x, value){
  UseMethod("bad<-")
}

#' @export
#' @method bads<- default
`bads<-.default` <- function(x, value){
  NULL
}

#' Acessor for `col_names`
#'
#' @param x  object
#'
#' @return value if `col_names` exists in the object, NULL otherwise
#' @export
col_names <- function(x){
  UseMethod("col_names")
}

#' @export
#' @method col_names default
col_names.default <- function(x){
  NULL
}

# `col_names<-` <- function(x, value){
#   UseMethod("col_names<-")
# }


# `col_names<-.default` <- function(x, value){
#   NULL
# }

#' Acessor for `col_types`
#'
#' @param x  object
#'
#' @return value if `col_types` exists in the object, NULL otherwise
#' @export
col_types <- function(x){
  UseMethod("col_types")
}

#' @export
#' @method col_types default
col_types.default <- function(x){
  NULL
}


# `col_types<-` <- function(x, value){
#   UseMethod("col_types<-")
# }


# `col_types<-.default` <- function(x, value){
#   NULL
# }

#' Acessor for `device_type`
#'
#' @param x  object
#'
#' @return value if `device_type` exists in the object, NULL otherwise
#' @export
device_type <- function(x){
  UseMethod("device_type")
}

#' @export
#' @method device_type default
device_type.default <- function(x){
  NULL
}

#' Set the value of `device_type`
#'
#' @param x object
#' @param value new value
#'
#' @return modified object
#' @export
`device_type<-` <- function(x, value){
  UseMethod("device_type<-")
}

#' @export
#' @method device_type<- default
`device_type<-.default` <- function(x, value){
  NULL
}

#' Acessor for `device_model`
#'
#' @param x  object
#'
#' @return value if `device_model` exists in the object, NULL otherwise
#' @export
device_model <- function(x){
  UseMethod("device_model")
}

#' @export
#' @method meas_date default
device_model.default <- function(x){
  NULL
}

#' Set the value of `device_model`
#'
#' @param x object
#' @param value new value
#'
#' @return modified object
#' @export
`device_model<-` <- function(x, value){
  UseMethod("device_model<-")
}

#' @export
#' @method device_model<- default
`device_model<-.default` <- function(x, value){
  NULL
}

#' Acessor for `device_serial`
#'
#' @param x  object
#'
#' @return value if `device_serial` exists in the object, NULL otherwise
#' @export
device_serial <- function(x){
  UseMethod("device_serial")
}

#' @export
#' @method device_serial default
device_serial.default <- function(x){
  NULL
}

#' Acessor for `device_site`
#'
#' @param x  object
#'
#' @return value if `device_site` exists in the object, NULL otherwise
#' @export
device_site <- function(x){
  UseMethod("device_site")
}

#' @export
#' @method device_site default
device_site.default <- function(x){
  NULL
}

#' Set the value of `device_site`
#'
#' @param x object
#' @param value new value
#'
#' @return modified object
#' @export
`device_site<-` <- function(x, value) UseMethod("device_site<-")

#' @export
#' @method device_site<- default
`device_site<-.default` <- function(x, value) NULL


#' Accessor for `first_samp`
#'
#' @param x object
#'
#' @return value of `first_samp`
#' @export
first_samp <- function(x){
  UseMethod("first_samp")
}

#' @export
#' @method first_samp default
first_samp.default <- function(x) NULL

#' Setter for `first_samp`
#'
#' @param x object
#' @param value value to set as `first_samp`
#'
#' @return modified object
#' @export
`first_samp<-` <- function(x, value) UseMethod("first_samp<-")

#' @export
#' @method first_samp<- default
`first_samp<-.default` <- function(x, value) NULL

#' Accessor for `info`
#'
#' @param x object that contains `info`
#'
#' @return `info`
#' @export
info <- function(x) UseMethod("info")

#' @export
#' @method info default
info.default <- function(x) NULL

#' Accessor for `last_samp`
#'
#' @param x object
#'
#' @return value of `last_samp`
#' @export
last_samp <- function(x){
  UseMethod("last_samp")
}

#' @export
#' @method last_samp default
last_samp.default <- function(x) NULL

#' Setter for `last_samp`
#'
#' @param x object
#' @param value value to set as `last_samp`
#'
#' @return modified object
#' @export
`last_samp<-` <- function(x, value) UseMethod("last_samp<-")

#' @export
#' @method last_samp<- default
`last_samp<-.default` <- function(x, value) NULL

#' Acessor for `meas_date`
#'
#' @param x  object
#'
#' @return value if `meas_date` exists in the object, NULL otherwise
#' @export
meas_date <- function(x) UseMethod("meas_date")

#' @export
#' @method meas_date default
meas_date.default <- function(x) NULL

#' Set the value of `meas_date`
#'
#' @param x object
#' @param value new value
#'
#' @return modified object
#' @export
`meas_date<-` <- function(x, value) UseMethod("meas_end<-")

#' @export
#' @method meas_date<- default
`meas_date<-.default` <- function(x, value) NULL

#' Acessor for `meas_date`
#'
#' @param x  object
#'
#' @return value if `meas_date` exists in the object, NULL otherwise
#' @export
meas_date <- function(x) UseMethod("meas_date")

#' @export
#' @method meas_date default
meas_date.default <- function(x) NULL

#' Set the value of `meas_date`
#'
#' @param x object
#' @param value new value
#'
#' @return modified object
#' @export
`meas_date<-` <- function(x, value) UseMethod("meas_end<-")

#' @export
#' @method meas_date<- default
`meas_date<-.default` <- function(x, value) NULL

#' Acessor for `meas_end`
#'
#' @param x  object
#'
#' @return value if `meas_end` exists in the object, NULL otherwise
#' @export
meas_end <- function(x) UseMethod("meas_end")

#' @export
#' @method meas_end default
meas_end.default <- function(x) NULL

#' Set the value of `meas_end`
#'
#' @param x object
#' @param value new value
#'
#' @return modified object
#' @export
`meas_end<-` <- function(x, value) UseMethod("meas_end<-")

#' @export
#' @method meas_end<- default
`meas_end<-.default` <- function(x, value) NULL

#' Acessor for `meas_id`
#'
#' @param x  object
#'
#' @return value if `meas_id` exists in the object, NULL otherwise
#' @export
meas_id <- function(x) UseMethod("meas_id")

#' @export
#' @method meas_id default
meas_id.default <- function(x) NULL

#' Set the value of `meas_id`
#'
#' @param x object
#' @param value new value
#'
#' @return modified object
#' @export
`meas_id<-` <- function(x, value) UseMethod("meas_id<-")

#' @export
#' @method meas_id<- default
`meas_id<-.default` <- function(x, value) NULL

#' Acessor for `meas_start`
#'
#' @param x object
#'
#' @return value of `meas_start` if it exists, otherwise NULL
#' @export
meas_start <- function(x) UseMethod("meas_start")

#' @export
#' @method meas_start default
meas_start.default <- function(x) NULL

#' Setter for `meas_start`
#'
#' @param x object
#' @param value value to set as `meas_start`
#'
#' @return modified object if `meas_start` exists, otherwise NULL
#' @export
`meas_start<-` <- function(x, value) UseMethod("meas_start<-")

#' @export
#' @method meas_start<- default
`meas_start<-.default` <- function(x, value) NULL

#' Acessor for `removed`
#'
#' @param x  object
#'
#' @return value if `removed` exists in the object, NULL otherwise
#' @export
removed <- function(x) UseMethod("removed")

#' @export
#' @method removed default
removed.default <- function(x) NULL

#' Set the value of `removed`
#'
#' @param x object
#' @param value new value
#'
#' @return modified object
#' @export
`removed<-` <- function(x, value) UseMethod("sremoved<-")

#' @export
#' @method removed<- default
`removed<-.default` <- function(x, value) NULL

#' Acessor for `sfreq`
#'
#' @param x  object
#'
#' @return value if `sfreq` exists in the object, NULL otherwise
#' @export
sfreq <- function(x) UseMethod("sfreq")

#' @export
#' @method sfreq default
sfreq.default <- function(x) NULL

#' Set the value of `sfreq`
#'
#' @param x object
#' @param value new value
#'
#' @return modified object
#' @export
`sfreq<-` <- function(x, value) UseMethod("sfreq<-")

#' @export
#' @method sfreq<- default
`sfreq<-.default` <- function(x, value) NULL

#' Acessor for `bads`
#'
#' @param x  object
#'
#' @return value if `subj_age` exists in the object, NULL otherwise
#' @export
subj_age <- function(x) UseMethod("subj_age")

#' @export
#' @method meas_date default
subj_age.default <- function(x) NULL

#' Set the value of `subj_age`
#'
#' @param x object
#' @param value new value
#'
#' @return modified object
#' @export
`subj_age<-` <- function(x, value) UseMethod("subj_age<-")

#' @export
#' @method subj_age<- default
`subj_age<-.default` <- function(x, value) NULL

#' Acessor for `subj_height`
#'
#' @param x  object
#'
#' @return value if `subj_height` exists in the object, NULL otherwise
#' @export
subj_height <- function(x) UseMethod("subj_height")

#' @export
#' @method subj_height default
subj_height.default <- function(x) NULL

#' Set the value of `subj_height`
#'
#' @param x object
#' @param value new value
#'
#' @return modified object
#' @export
`subj_height<-` <- function(x, value) UseMethod("subj_height<-")

#' @export
#' @method subj_height<- default
`subj_height<-.default` <- function(x, value) NULL

#' Acessor for `subj_id`
#'
#' @param x  object
#'
#' @return value if `subj_id` exists in the object, NULL otherwise
#' @export
subj_id <- function(x) UseMethod("subj_id")

#' @export
#' @method subj_id default
subj_id.default <- function(x) NULL

#' Set the value of `subj_id`
#'
#' @param x object
#' @param value new value
#'
#' @return modified object
#' @export
`subj_id<-` <- function(x, value) UseMethod("subj_id<-")

#' @export
#' @method subj_id<- default
`subj_id<-.default` <- function(x, value) NULL

#' Acessor for `subj_sex`
#'
#' @param x  object
#'
#' @return value if `subj_sex` exists in the object, NULL otherwise
#' @export
subj_sex <- function(x) UseMethod("subj_sex")

#' @export
#' @method subj_sex default
subj_sex.default <- function(x) NULL

#' Set the value of `subj_sex`
#'
#' @param x object
#' @param value new value
#'
#' @return modified object
#' @export
`subj_sex<-` <- function(x, value) UseMethod("subj_sex<-")

#' @export
#' @method meas_date<- default
`subj_sex<-.default` <- function(x, value) NULL

#' Acessor for `subj_weight`
#'
#' @param x  object
#'
#' @return value if `subj_weight` exists in the object, NULL otherwise
#' @export
subj_weight <- function(x) UseMethod("subj_weight")

#' @export
#' @method subj_weight default
subj_weight.default <- function(x) NULL

#' Set the value of `subj_weight`
#'
#' @param x object
#' @param value new value
#'
#' @return modified object
#' @export
`subj_weight<-` <- function(x, value) UseMethod("subj_weight<-")

#' @export
#' @method subj_weight<- default
`subj_weight<-.default` <- function(x, value) NULL
