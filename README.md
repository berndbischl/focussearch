[![Build Status](https://travis-ci.org/berndbischl/focussearch.svg?branch=master)](https://travis-ci.org/berndbischl/focussearch)
[![Coverage Status](https://img.shields.io/codecov/c/github/berndbischl/focussearch/master.svg?maxAge=600)](https://codecov.io/github/berndbischl/focussearch?branch=master)

# focussearch

Global optimization of blackbox functions by iteratively shrinking the parameter space around good points

```r
# Search through a fully numeric param space
f = smoof::makeSphereFunction(2)
# Create a (vectorized) blackbox optimisation function
fn = function(x) apply(x, 1, f)
# Define focussearch parameters
ctrl = makeFocusSearchControl(maxit = 5, restarts = 3, points = 100)
# Define searchable parameter space
ps = makeParamSet(
  makeNumericParam("x1", lower = 0, upper = 10),
  makeNumericParam("x2", lower = 0, upper = 10)
  )
# Run focussearch
focussearch(fn, ps, ctrl)
```