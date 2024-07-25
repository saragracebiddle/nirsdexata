test_that("constructor works", {

  basic_info <- list(
    "chs" = data.frame(
      ch_name = c("1", "2", "3"),
      ch_type = c("hbo","hbr", "misc"),
      source_num = c(1L,1L,1L),
      det_num = c(1L,2L,3L),
      wavelength = c(756L, 761L, 854L)
    )
  )

  info <- new_info(basic_info)

  expect_s3_class(info, c("info", "list"))
})

test_that("helper works", {

  basic_info <- list(
    "chs" = data.frame(
      ch_name = c("1", "2", "3"),
      ch_type = c("hbo","hbr", "misc"),
      source_num = c(1L,1L,1L),
      det_num = c(1L,2L,3L),
      wavelength = c(756L, 761L, 854L)
    )
  )

  info <- new_info(basic_info)

  expect_s3_class(info, c("info", "list"))
})

test_that("accessing elements of the object", {

  info <- create_info(col_names = c("1","2","3"),
               device_type = "PortaMon",
               device_model = "PortaMon",
               subj_id = "27")

  expect_match("27", subj_id(info))
  expect_match("PortaMon", device_model(info))

  meas_start(info) <- 4.0
  expect_equal(4.0,  meas_start(info))

})

# test_that("all elements of info are created", {
#   #TODO write the validator function for Info class
#
# })
