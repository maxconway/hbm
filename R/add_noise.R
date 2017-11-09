add.noise<- function(m, ...) {
  x = m
  v = x[upper.tri(x)]
  x[upper.tri(x)] = jitter(v, ...)
  v = diag(x)
  diag(x) = jitter(v, ...)
  x[x < 0] = 0
  x[lower.tri(x)] = t(x)[lower.tri(t(x))]
  return(x)
}