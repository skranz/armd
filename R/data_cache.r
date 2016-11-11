webCached = function(...) {
  library(R.cache)
  evalWithMemoization(...)
}