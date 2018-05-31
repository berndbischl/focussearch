[![Build Status](https://travis-ci.org/berndbischl/focussearch.svg?branch=master)](https://travis-ci.org/berndbischl/focussearch)

# focussearch

Global optimization of blackbox functions by iteratively shrinking the parameter space around good points.
In each iteration `points` random points are drawn from the full param space, and the best point is selected
for shrinkage. 
Now supports trafo functions in the `ParamSet`.

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
