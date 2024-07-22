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



  expect_s3_class(info, c("info", "list"))
})

test_that("accessing elements of the object", {

  info <- info(col_names = c("1","2","3"),
               device_type = "PortaMon",
               device_model = "PortaMon",
               subj_id = "27")

  expect_type(info$subj_info, "list")

  expect_identical(info$subj_info, list("subj_id" = "27",
                                        "subj_sex" = NULL,
                                        "subj_age" = NULL,
                                        "subj_height" = NULL,
                                        "subj_weight" = NULL))

  expect_type(info$subj_info$subj_id, "character")



})

test_that("all elements of info are created", {
  #TODO write the validator function for Info class

})
