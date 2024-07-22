#' Analyzed Data Class Constructor
#'
#' @param data data object
#' @param info info object
#' @param coefs coefficients from models
#' @param ... other variables for children of class
#' @param class for children of class
#'
#' @return analyzed object
new_analyzed_data <- function(data,info, coefs, ..., class = character()){
  obj <- list(
    "data" = data,
    "info"= info,
    "coefs" = coefs
  )

  structure(
    obj,
    ...,
    class = c(class, "analyzed", class(list()))
  )
}

#' Analyzed Data helper
#'
#' @param rawdata rawdta object
#'
#' @return analyzed
#' @export
analysis <- function(rawdata){

  prep = prepare_for_modeling(rawdata) |> setDT()

  prep2 <- prep |>
    dplyr::filter(Section != "BegRest" & Section != "WarmUp")

  nested <- group_nest_dt(prep2, Section, col_type)|>
    setkey(Section, col_type)

  asym_models = nested[, Model := map(data, asym_model)
  ][ is.na(Model) == FALSE,
                      `:=` (
                        Tidy = map(Model, tidy),
                        Augment = map(Model, augment)
                      ) , ]

  steadystates <- steady_states(prep2)

  tidy <- unnest_dt(asym_models,
                    col = Tidy,
                    id = list(Section, col_type)) |>
    dcast(Section + col_type ~ term,
          value.var = 'estimate')|>
    merge(nested, all.y = TRUE)

  coefs <- tidy[,
                .(Section, col_type, Asym, R0, lrc)
  ][,
    Tau := (1/exp(lrc))] |>
    merge(y = steadystates)

  augment <- unnest_dt(asym_models, col = Augment,
                       id = list(Section, col_type)) |>
    dplyr::rename("predicted" = `.fitted`) |>
    dplyr::rename("residual" = `.resid`) |>
    dplyr::select(Section, col_type, SectionZeroedTime, predicted, residual)


  data <- merge(prep,
                augment,
                by = c("Section", "col_type", "SectionZeroedTime"),
                all.x = TRUE)


  # TODO get steady states and add those into the results
  new_analyzed_data(data,
                    rawdata$info,
                    coefs)

}

