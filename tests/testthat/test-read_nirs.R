test_that("Throws error if file does not exist", {

  expect_error(
    read_nirs("thisdoesnotexist.txt")
    )

})

test_that("read_nirs works with txt files", {

  expect_no_condition(
    read_nirs(read_nirs_example("on_off_kinetics.txt"))
  )

})

test_that("read_nirs makes correct columns", {
  ex2 <- read_nirs(read_nirs_example("on_off_kinetics_2.txt"))

  expect_equal(c("samp_num",
                 "S1_D1_759",
                 "S1_D1_854",
                 "S2_D1_759",
                 "S2_D1_854",
                 "S3_D1_761",
                 "S3_D1_856",
                 "TSI",
                 "TSIFF"), colnames(ex2$data))
})
