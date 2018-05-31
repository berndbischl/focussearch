context("shrinkParSet")

test_that("Shrinking numeric param sets", {
  ps = makeParamSet(
    makeNumericParam("x1", lower = 0, upper = 10),
    makeNumericParam("x2", lower = 0, upper = 10)
  )
  # central x
  ps1 = shrinkParSet(ps, ps, data.frame("x1" = 5, "x2" = 3))
  expect_equal(ps1$pars$x1$lower, 5 - 10/4)
  expect_equal(ps1$pars$x1$upper, 5 + 10/4)
  expect_equal(ps1$pars$x2$lower, 3 - 10/4)
  expect_equal(ps1$pars$x2$upper, 3 + 10/4)
  
  # x to one side of the interval
  ps2 = shrinkParSet(ps, ps, data.frame("x1" = 0.4, "x2" = 9))
  expect_equal(ps2$pars$x1$lower, max(0, 0.4 - 10/4))
  expect_equal(ps2$pars$x1$upper, 0.4 + 10/4)
  expect_equal(ps2$pars$x2$lower, 9 - 10/4)
  expect_equal(ps2$pars$x2$upper, min(10, 9 + 10/4))
  
  # extreme x
  ps5 = shrinkParSet(ps, ps, data.frame("x1" = 0, "x2" = 10))
  expect_equal(ps5$pars$x1$lower, max(0, 0 - 10/4))
  expect_equal(ps5$pars$x1$upper, 0 + 10/4)
  expect_equal(ps5$pars$x2$lower, 10 - 10/4)
  expect_equal(ps5$pars$x2$upper, min(10, 10 + 10/4))
  
  # x out of bounds
  expect_error(shrinkParSet(ps, data.frame("x1" = -1, "x2" = 11), check.feasible = TRUE))
})


test_that("Shrinking mixed param sets", {
  ps = makeParamSet(
    makeNumericParam("x1", lower = 0, upper = 10),
    makeNumericParam("x2", lower = 0, upper = 10),
    makeDiscreteParam("x3", values = as.character(1:5)),
    makeLogicalParam("x4")
  )
  # x to one side of the interval
  ps3 = shrinkParSet(ps, ps, data.frame("x1" = 0.4, "x2" = 9, "x3" = "4", "x4" = TRUE))
  expect_equal(ps3$pars$x1$lower, max(0, 0.4 - 10/4))
  expect_equal(ps3$pars$x1$upper, 0.4 + 10/4)
  expect_equal(ps3$pars$x2$lower, 9 - 10/4)
  expect_equal(ps3$pars$x2$upper, min(10, 9 + 10/4))
  expect_true("4" %in% names(ps3$pars$x3$values))
  expect_true(names(ps3$pars$x4$values) == as.character("TRUE"))
})


test_that("Shrinking integer param sets", {
  ps = makeParamSet(makeIntegerParam("x1", lower = 0, upper = 10))
  # x to one side of the interval
  ps4 = shrinkParSet(ps, ps, data.frame("x1" = 7))
  expect_equal(ps4$pars$x1$lower, 4)
  expect_equal(ps4$pars$x1$upper, 10)
})

test_that("Shrinking numeric param sets with trafo", {
  set.seed(444)
  ps = makeParamSet(
    makeNumericParam("x1", lower = -10, upper = 10, trafo = function(x) 2^x),
    makeNumericParam("x2", lower = -5, upper = 5, trafo = function(x) exp(x))
  )
  # Some x
  ps1 = shrinkParSet(ps, ps, data.frame("x1" = .006, "x2" = 1))
  expect_true(ps1$pars$x1$upper - ps1$pars$x1$lower <= 10)
  expect_true(ps1$pars$x2$upper - ps1$pars$x2$lower <= 5)
  expect_true(ps1$pars$x1$upper - ps1$pars$x1$lower > 0)
  expect_true(ps1$pars$x2$upper - ps1$pars$x2$lower > 0)
  
  # Extreme x
  ps2 = shrinkParSet(ps, ps, data.frame("x1" = 2^-10, "x2" = exp(5)))
  expect_true(ps2$pars$x1$upper - ps2$pars$x1$lower <= 10)
  expect_true(ps2$pars$x2$upper - ps2$pars$x2$lower <= 5)
  expect_true(ps2$pars$x1$upper - ps2$pars$x1$lower > 0)
  expect_true(ps2$pars$x2$upper - ps2$pars$x2$lower > 0)
  
  # Infeasbile x (check that paramset is unchanged)
  ps3 = shrinkParSet(ps, ps, data.frame("x1" = 0, "x2" = exp(6)))
  expect_true(ps3$pars$x1$upper == 10)
  expect_true(ps3$pars$x1$lower == -10)
  expect_true(ps3$pars$x2$upper == 5)
  expect_true(ps3$pars$x2$lower == -5)
})