test_that("constructor works", {

  rawdata <- new_rawdata(
    data.frame(samp_num = c(1,2,3),
               S1_D1_759 = c(1,1,1),
               S1_D1_856 = c(2,2,2)),
    info(
      col_names = c("S1_D1_759", "S1_D1_856"),
      col_types = c("hbo", "hbr")
    )

  )

  expect_s3_class(rawdata, c("rawdata", "list"))
})

test_that("helper works",{

  rawdata <- rawdata(
    data.frame(samp_num = c(1,2,3),
               S1_D1_759 = c(1,1,1),
               S1_D1_856 = c(2,2,2)),
      col_names = c("S1_D1_759", "S1_D1_856"),
      col_types = c("hbo","hbr")
  )

  expect_s3_class(rawdata, c("rawdata", "list"))
})
