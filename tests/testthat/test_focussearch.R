context("general api")

test_that("General api works", {
  set.seed(123L)
  f = makeSphereFunction(2)
  f2 = function(x) apply(x, 1, f)
  ctrl = makeFocusSearchControl(maxit = 5, restarts = 1, points = 100)
  ps = makeParamSet(
    makeNumericParam("x1", lower = 0, upper = 10),
    makeNumericParam("x2", lower = 0, upper = 10)
  )
  z = focussearch(f2, ps, ctrl)
  expect_list(z, len = 2, types = c("double", "list"))
  expect_true(z$y < 0.1)
})


test_that("Mixed parameter spaces works", {
  set.seed(123L)
  # Artificial mixed function
  f = function(x) {
    y = makeSphereFunction(2)(as.numeric(x[1:2]))
    if (x["x3"] == "2") y = 2 * y
    return(y)
  }
  
  f2 = function(x) sapply(convertRowsToList(x, name.vector = TRUE), f)
  ctrl = makeFocusSearchControl(maxit = 3, restarts = 1, points = 100)
  ps = makeParamSet(
    makeNumericParam("x1", lower = 0, upper = 10),
    makeNumericParam("x2", lower = 0, upper = 10),
    makeDiscreteParam("x3", values = as.character(1:2))
  )
  z = focussearch(f2, ps, ctrl)
  expect_list(z, len = 2, types = c("double", "list"))
  expect_true(z$y < 0.1)
})

test_that("Hierarchical spaces work", {
  set.seed(123L)
  f = makeSphereFunction(3)
  f2 = function(x) apply(x, 1, f)
  ctrl = makeFocusSearchControl(maxit = 5, restarts = 1, points = 100)
  ps = makeParamSet(
    makeNumericParam("x1", lower = 0, upper = 10),
    makeNumericParam("x2", lower = 0, upper = 10, requires = quote(x1 < 7)),
    makeNumericParam("x3", lower = 0, upper = 10, requires = quote(x2 < 5))
  )
  z = focussearch(f2, ps, ctrl)
  expect_list(z, len = 2, types = c("double", "list"))
  expect_true(z$y < 0.1)
})

test_that("show.info works", {
  set.seed(123L)
  f = makeSphereFunction(2)
  f2 = function(x) apply(x, 1, f)
  ctrl = makeFocusSearchControl(maxit = 2, restarts = 1, points = 100)
  ps = makeParamSet(
    makeNumericParam("x1", lower = 0, upper = 10),
    makeNumericParam("x2", lower = 0, upper = 10)
  )
  expect_output(focussearch(f2, ps, ctrl, show.info = TRUE), regexp = "Multistart 1 of")
  expect_output(focussearch(f2, ps, ctrl, show.info = TRUE), regexp = "New best y:")
  expect_output(focussearch(f2, ps, ctrl, show.info = TRUE), regexp = "Type len Def")
})

test_that("Full random search works", {
  set.seed(123L)
  f = makeSphereFunction(2)
  f2 = function(x) apply(x, 1, f)
  ctrl = makeFocusSearchControl(maxit = 2, restarts = 1, points = 100, exploit = 0)
  ps = makeParamSet(
    makeNumericParam("x1", lower = 0, upper = 10),
    makeNumericParam("x2", lower = 0, upper = 10)
  )
  z = focussearch(f2, ps, ctrl)
  expect_list(z, len = 2, types = c("double", "list"))
  expect_true(z$y < 2)
})

test_that("Mixed exploit/explore search works", {
  set.seed(123L)
  f = makeSphereFunction(2)
  f2 = function(x) apply(x, 1, f)
  ctrl = makeFocusSearchControl(maxit = 2, restarts = 1, points = 100, exploit = 0.7)
  ps = makeParamSet(
    makeNumericParam("x1", lower = 0, upper = 10),
    makeNumericParam("x2", lower = 0, upper = 10)
  )
  z = focussearch(f2, ps, ctrl)
  expect_list(z, len = 2, types = c("double", "list"))
  expect_true(z$y < 1)
})