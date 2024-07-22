test_that("Throws error if file does not exist", {

  expect_error(
    read_nirs("thisdoesnotexist.txt"),
    class = "error_file_dne"
    )

})

test_that("read_nirs works with txt files", {

  expect_no_condition(
    read_nirs(read_nirs_example("on_off_kinetics.txt"))
  )

})
