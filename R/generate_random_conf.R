generate.random.conf<-function(n, k = 3, perturb = NULL, 
                               scale = T, mean = 0, sd = 1)
{
  m = matrix(0, n, k)
  m[1, 1:k] = rnorm(k)
  for (i in 2:n)
  {
    for (j in 1:k)
    {
      m[i,j] = m[i-1, j] + rnorm(1, mean = mean, sd = sd)
    }
  }
  
  
  if (scale)
  {
    scalevec = rep(1, k)
    for (i in 1:k)
    {
      scalevec[i] = abs(m[1,i]-m[n,i])
    }
    m = scale(m, center = FALSE, scale = scalevec)
  }
  
  
  if (length(perturb) > 0)
  {
    if (max(perturb) > n || length(perturb) > n || min(perturb) < 1 )
    {
      stop("perturb must be a vector of size > 1 and <=n with integers in the range 1 to n (inclusive)")
    }
    perturb = sort(perturb)
    indices = sample(perturb, length(perturb), replace = FALSE)
    trials = 5
    i = 0
    while (sum(indices != perturb) > 0  && i < trials ) # edge case
    {
      indices = sample(perturb, length(perturb), replace = FALSE)
      i = i + 1
    }
    m[perturb, 1:k] = m[indices,1:k]
  }
  return(m)
}