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
    ))
  expect_equal(test, types_from_names(longdata, col_types, col_names))
})

test_that("crop works", {

  data <- data.frame(samp_num = seq(0,100),
                     a = seq(0,100),
                     b = seq(0,100),
                     c = seq(0,100))

  sfreq = 10
  meas_start = 1.5

  test <- adjust_times(data, sfreq, meas_start, "samp_num")

  cropped <- test |>
    dplyr::filter(ZeroedTime >= 0)

  expect_equal(cropped, crop(test, meas_start=meas_start))

  meas_end = 5.3
  cropped <- test |>
    dplyr::filter(ZeroedTime >= 0 & ZeroedTime <= 5.3)
  expect_equal(cropped, crop(test, meas_start = meas_start, meas_end = meas_end))
})

test_that("remove_bad works", {

  data <- data.frame(samp_num = seq(0,100),
                     a = seq(0,100),
                     b = seq(0,100),
                     c = seq(0,100))

  bads <- c("c")
  removed <- c(3,4,5)

  test <- data |>
    dplyr::select(!dplyr::any_of(bads)) |>
    dplyr::rows_delete(y = data.frame(samp_num = removed))
  expect_equal(test, remove_bad(data, removed, bads))

  ex <- read_nirs(read_nirs_example("on_off_kinetics.txt"))

  expect_no_error(remove_bad(ex$data, removed = ex$info$bounds$removed, bads = ex$info$bads))

})

test_that("remove_bad works with null argument", {

  ex <- read_nirs(read_nirs_example("on_off_kinetics.txt"))

  expect_equal(ex$data, remove_bad(ex$data,
                                   bads = ex$info$bads))
})

test_that("remove_bad works with argument of length 1", {
  ex <- read_nirs(read_nirs_example("on_off_kinetics.txt"))

  ex$info$bads <- c("TSIFF")

  expect_equal(ex$data |>
                 dplyr::select(!TSIFF),
               remove_bad(ex$data, bads = ex$info$bads))

})

test_that("remove_rows works", {

  test <- data.frame(samp_num = 0:100,
                     value = 0:100)

  removed = c(3,4,5,6)

  test[(removed + 1), "value"] <- NA


  test2 <- data.frame(samp_num = 0:100,
                      value = 0:100)


  expect_equal(test,
               remove_rows(test2, removed))

  removed = c()

  test <- data.frame(samp_num = 0:100,
                     value = 0:100)

  expect_equal(test, remove_rows(test, removed))
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
                  ZeroedTime = Time - meas_start)

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
