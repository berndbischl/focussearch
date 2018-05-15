context("doRandomSearch")

test_that("Random search in numeric spaces", {
  set.seed(123L)
  f = makeSphereFunction(2)
  fn = function(x) apply(x, 1, f)
  ctrl = makeFocusSearchControl(maxit = 5, restarts = 1, points = 100)
  ps = makeParamSet(
    makeNumericParam("x1", lower = 0, upper = 10),
    makeNumericParam("x2", lower = 0, upper = 10)
  )
  z = doRandomSearch(fn, ps, ctrl) 
  expect_list(z, len = 2, types = c("double", "list"), names = "named")
})

test_that("Random search in mixed spaces", {
  set.seed(123L)
  f = makeSwiler2014Function()
  fn = function(x) {sapply(convertRowsToList(x, name.vector = TRUE), f)}
  ctrl = makeFocusSearchControl(maxit = 5, restarts = 3, points = 100)
  ps = makeParamSet(
    makeDiscreteParam("x1", values = as.character(1:5)),
    makeNumericParam("x2", lower = 0, upper = 1),
    makeNumericParam("x3", lower = 0, upper = 1)
  )
  z = doRandomSearch(fn, ps, ctrl)
  expect_list(z, len = 2, types = c("double", "list"), names = "named")
})
