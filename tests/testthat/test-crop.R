test_that("crop throws errors", {
  # throws error when data.frame not provided
  test <- data.frame(
    x = 0:100,
    y = 100:200
  )
  expect_error(crop(data = c(1,2,4)))

  expect_no_error(crop(data = data.frame(
    x = c(1,2,3)
  )))

  expect_no_error(crop(data = data.table(
    x = c(1,2,3)
  )))

  expect_error(crop(data = data.frame(
    x = c(1,2,3)
  ), start = 1, col = "y"))


  expect_no_error(crop(test, start = 5, end = 95))

  expect_no_error(crop(test, start = 5, end = 95, col = "x"))

  expect_error(crop(
    data.frame(
      a = c("A","B","C")
    ), start = 1, col = "a"
  ))
})

test_that("crop returns expected output.", {

  test <- data.table(
    x = 0:100,
    y = 100:200
  )

  # crop with no arguments returns the original data
  expect_equal(
    test, crop(test)
  )


  expect <- test[5:94]

  # crop with all argument varieties returns expected result
  expect_equal(length(expect), length(crop(test, start = 5, end = 94)))
  expect_equal(expect, crop(test, start = 5, end = 94))
  expect_equal(expect, crop(test, start = 104, end = 193, col = "y"))

  expect <- test[1:94]
  expect_equal(length(expect), length(crop(test, end = 94)))
  expect_equal(expect, crop(test, end = 193, col = "y"))

  expect <- test[5:101]
  expect_equal(length(expect), length(crop(test, start = 5)))
  expect_equal(expect, crop(test, start = 104, col = "y"))

})
