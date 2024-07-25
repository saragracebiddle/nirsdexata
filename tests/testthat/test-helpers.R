test_that("avg_type", {
  rawdata <- read_nirs(read_nirs_example("on_off_kinetics.txt"))
  p <- rawdata$data |>
    remove_bad(bads = bads(rawdata)) |>
    adjust_times(sfreq = sfreq(rawdata),
                 meas_start = meas_start(rawdata),
                 samp_num = "samp_num") |>
    crop(first = meas_start(rawdata),
         last = meas_end(rawdata),
         col = "Time") |>
    melt(id.vars = c("Time","samp_num", "ZeroedTime"),
         measure.vars = setdiff(col_names(rawdata), c("Time","samp_num", "ZeroedTime", bads(rawdata))),
         variable.name = "col_name",
         value.name = "value")|>
    remove_rows(remove = removed(rawdata),
                ref_col = "samp_num",
                value_col = "value") |>
    types_from_names(col_types = col_types(rawdata),
                     col_names = col_names(rawdata))

  expect_equal(p[,
                 .(value = mean(value, na.rm = T)),
                 by = c("samp_num","Time","col_type","ZeroedTime")],
               avg_type(p, col = "value",
                        groups = c("samp_num","Time","col_type","ZeroedTime"))
  )


})

test_that("types_from_names works", {
  col_names = c("a","b",'c',"d")
  col_types = c("A","B","A","B")

  longdata <- data.frame(samp_num = seq(1,100),
                         col_name = rep(col_names, each = 25))

  test <- longdata |>
    dplyr::mutate(col_type = dplyr::case_match(
      col_name,
      c("a","c") ~ "A",
      c("b","d") ~"B"
    )) |>setDT()
  expect_equal(test, types_from_names(longdata, col_types, col_names))

  rawdata <- read_nirs(read_nirs_example("on_off_kinetics.txt"))
  p <- rawdata$data |>
    remove_bad(bads = bads(rawdata)) |>
    adjust_times(sfreq = sfreq(rawdata),
                 meas_start = meas_start(rawdata),
                 samp_num = "samp_num") |>
    crop(first = meas_start(rawdata),
         last = meas_end(rawdata),
         col = "Time")|>
    melt(id.vars = c("Time","samp_num", "ZeroedTime"),
         measure.vars = setdiff(col_names(rawdata), c("Time","samp_num","ZeroedTime", bads(rawdata))),
         variable.name = "col_name",
         value.name = "value") |>
    remove_rows(remove = removed(rawdata),
                ref_col = "samp_num",
                value_col = "value") |>
    types_from_names(col_types = col_types(rawdata),
                     col_names = col_names(rawdata))

  expect_setequal(colnames(p), c("samp_num","col_type","col_name","value","Time","ZeroedTime"))

  })



test_that("remove_rows works", {

  test <- data.frame(samp_num = 0:100,
                     value = 0:100)|>setDT()

  removed = c(3,4,5,6)

  test[(removed + 1), "value"] <- NA


  test2 <- data.frame(samp_num = 0:100,
                      value = 0:100)

  expect_dt <- test |>setDT() |>setindex("samp_num")


  expect_equal(expect_dt,
               remove_rows(test2, removed, ref_col = "samp_num",
                           value_col = "value"))

  removed = c()

  test <- data.frame(samp_num = 0:100,
                     value = 0:100) |>setDT()

  expect_equal(test, remove_rows(test, removed,ref_col = "samp_num",
                                 value_col = "value"))
})

test_that("adjust_times works", {

  data <- data.frame(samp_num = seq(0,100),
                     a = seq(0,100),
                     b = seq(0,100),
                     c = seq(0,100))

  sfreq = 10
  meas_start = 1.5


  test <- data |>
    dplyr::mutate(Time = samp_num /sfreq,
                  ZeroedTime = Time - meas_start) |>setDT()

  expect_equal(test, adjust_times(data, sfreq, meas_start, "samp_num"))
})

test_that("select_types works", {



  data <- data.frame(samp_num = seq(1,100),
                     col_name = rep(c("a","b","c","d"), each = 25),
                     col_type = rep(c("A","B","A","D"), each = 25))

  expect_equal(data, select_types(data))

  type = "A"

  expect_equal(data |>
                 dplyr::filter(col_type == "A"), select_types(data, type = type))

  type = c("A","B")

  expect_equal(data |>
                 dplyr::filter(col_type %in% c("A","B")), select_types(data, type = type))
})

test_that("prepare_shiny_plot works", {

    ex <- read_nirs(read_nirs_example("on_off_kinetics.txt"))

    expect_no_error(prepare_shiny_plot(ex$data,
                                       sfreq = ex$info$sfreq,
                                       meas_start = ex$info$bounds$meas_start,
                                       meas_end = ex$info$bounds$meas_end,
                                       bads = ex$info$bads,
                                       removed = ex$info$bounds$removed,
                                       col_types = ex$info$cols$col_type,
                                       col_names = ex$info$cols$col_name
    ))


})
