test_that("get legend onoff", { 
  file = read_nirs_example("on_off_kinetics.txt")
  delim = '\t'
  s = tryCatch(scan(file,
                    n=1000,
                    sep=delim,
                    na.strings="",
                    what=character(),
                    quiet=T,
                    skipNul=T),
               error = function(cnd){
                 rlang::abort("error_in_file_scan",
                              message= "Error when scanning file.")
               })
  legendinfo <- get_metadata(s, type = "legend")
  
  expect_length(legendinfo, 2)
  expect_length(legendinfo$cols$X1, 9)
  expect_contains(legendinfo$cols$X2, c("(Sample number)",
                                        "Rx1-Tx1 O2Hb (LaLonde_Test_FP_7_14_21)",
                                        "Rx1-Tx1 HHb (LaLonde_Test_FP_7_14_21)",
                                        "Rx1-Tx2 O2Hb (LaLonde_Test_FP_7_14_21)",
                                        "Rx1-Tx2 HHb (LaLonde_Test_FP_7_14_21)", 
                                        "Rx1-Tx3 O2Hb (LaLonde_Test_FP_7_14_21)",
                                        "Rx1-Tx3 HHb (LaLonde_Test_FP_7_14_21)",
                                        "Rx1-Tx1,Tx2,Tx3 TSI% (LaLonde_Test_FP_7_14_21)",
                                        "Rx1-Tx1,Tx2,Tx3 TSI Fit Factor (LaLonde_Test_FP_7_14_21)"))
})


test_that("get legend ltshld", {
  
  file = read_nirs_example("ltshld_onoff.txt")
  delim = '\t'
  s = tryCatch(scan(file,
                    n=1000,
                    sep=delim,
                    na.strings="",
                    what=character(),
                    quiet=T,
                    skipNul=T),
               error = function(cnd){
                 rlang::abort("error_in_file_scan",
                              message= "Error when scanning file.")
               })
  legendinfo <- get_metadata(s, type = "legend")
  
  expect_length(legendinfo, 2)
  expect_length(legendinfo$cols$X1, 9)
  expect_contains(legendinfo$cols$X2, c("(Sample number)",
                                        "Rx1-Tx1 O2Hb (Mov avg) (LIGHTSHIELD030_ONOFF_07_25_23_NEW)",
                                        "Rx1-Tx1 HHb (Mov avg) (LIGHTSHIELD030_ONOFF_07_25_23_NEW)",
                                        "Rx1-Tx2 O2Hb (Mov avg) (LIGHTSHIELD030_ONOFF_07_25_23_NEW)",
                                        "Rx1-Tx2 HHb (Mov avg) (LIGHTSHIELD030_ONOFF_07_25_23_NEW)", 
                                        "Rx1-Tx3 O2Hb (Mov avg) (LIGHTSHIELD030_ONOFF_07_25_23_NEW)",
                                        "Rx1-Tx3 HHb (Mov avg) (LIGHTSHIELD030_ONOFF_07_25_23_NEW)",
                                        "Rx1-Tx1,Tx2,Tx3 TSI% (Mov avg) (LIGHTSHIELD030_ONOFF_07_25_23_NEW)",
                                        "Rx1-Tx1,Tx2,Tx3 TSI Fit Factor (Mov avg) (LIGHTSHIELD030_ONOFF_07_25_23_NEW)"))
})

test_that("get_metadata handles error for dates", {
  
  file = read_nirs_example("on_off_kinetics.txt")
  delim = '\t'
  s = tryCatch(scan(file,
                    n=1000,
                    sep=delim,
                    na.strings="",
                    what=character(),
                    quiet=T,
                    skipNul=T),
               error = function(cnd){
                 rlang::abort("error_in_file_scan",
                              message= "Error when scanning file.")
               })
  meas_date <- get_metadata(s, type = "meas_date")
  expect_true(lubridate::is.POSIXt(meas_date))
  
  
  file = read_nirs_example("ltshld_onoff.txt")
  delim = '\t'
  s = tryCatch(scan(file,
                    n=1000,
                    sep=delim,
                    na.strings="",
                    what=character(),
                    quiet=T,
                    skipNul=T),
               error = function(cnd){
                 rlang::abort("error_in_file_scan",
                              message= "Error when scanning file.")
               })
  meas_date <- get_metadata(s, type = "meas_date")
  expect_type(meas_date, "character")
})
