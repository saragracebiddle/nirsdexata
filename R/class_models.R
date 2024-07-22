#' Models Class Constructor
#'
#' @param models models
#'
#' @return model object
#' @export
new_models <- function(coefs,
                       predicted,
                       ...,
                       class = character()
                       ){

  #stopifnot(is.data.frame(models))


  models <- list(
    "coefs" = coefs,
    "predicted" = predicted
  )

  structure(models,
            ...,
            class = c(class, "asym_models", class(list())))

}

#' Models class helper
#'
#' @param rawdata rawdata object
#'
#' @return asym_models
#' @export
models <- function(rawdata){

  prep = prepare_for_modeling(rawdata) |>setDT()

  nested <- group_nest_dt(prep, Section, col_type)|>
    setkey(Section, col_type)

  asym_models = nested[, Model := map(data, asym_model)
                ]

  prep = asym_models[ is.na(Model) == FALSE,
                 `:=` (
                   Tidy = map(Model, tidy),
                   Augment = map(Model, augment)
                 ) , ]

  tidy <- unnest_dt(prep,
                    col = Tidy,
                    id = list(Section, col_type)) |>
    dcast(Section + col_type ~ term,
          value.var = 'estimate')|>
    merge(nested, all.y = TRUE)

  coefs <- tidy[,
                .(Section, col_type, Asym, R0, lrc)
  ][,
    Tau := (1/exp(lrc))]

  predicted <- unnest_dt(prep,
                         col=Augment,
                         id = list(Section, col_type))

  mods <- new_models(coefs,
             predicted)


}




