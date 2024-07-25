test_that("remove_bad works", {

  data <- data.frame(samp_num = seq(0,100),
                     a = seq(0,100),
                     b = seq(0,100),
                     c = seq(0,100))

  bads <- c("c")
  removed <- c(3,4,5)

  test <- data |>
    dplyr::select(!dplyr::any_of(bads)) |>setDT()

  expect_equal(test, remove_bad(data, bads))

  ex <- read_nirs(read_nirs_example("on_off_kinetics.txt"))

  expect_no_error(remove_bad(ex$data, bads = ex$info$bads))

})

test_that("remove_bad works with null argument", {

  ex <- read_nirs(read_nirs_example("on_off_kinetics.txt"))

  expect_equal(ex$data, remove_bad(ex$data,
                                   bads = NULL))
})

test_that("remove_bad works with argument of length 1", {
  ex <- read_nirs(read_nirs_example("on_off_kinetics.txt"))

  ex$info$bads <- c("TSIFF")

  expect_equal(ex$data |>
                 dplyr::select(!TSIFF),
               remove_bad(ex$data, bads = ex$info$bads))

})
