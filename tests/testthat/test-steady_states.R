test_that("steady_states works", {
  data <- read_nirs(read_nirs_example("on_off_kinetics.txt"))
  prep = prepare_for_modeling(data) |>setDT()

  expect_no_condition(steady_states(prep))

})
