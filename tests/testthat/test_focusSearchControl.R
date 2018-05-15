context("FocusSearchControl")

test_that("Behaves as expected", {
  ctrl = makeFocusSearchControl(maxit = 7, restarts = 2, points = 99)
  expect_class(ctrl, "FocusSearchControl")
  expect_equal(ctrl$points, 99)
  expect_equal(ctrl$maxit, 7)
  expect_equal(ctrl$restarts, 2)
})

test_that("Throws errors for wrong inputs", {
  expect_error(makeFocusSearchControl(maxit = "7"), "Assertion on 'maxit' failed")
  expect_error(makeFocusSearchControl(show.info = TRUE), "unused argument")
})