communicability<-function(hm)
{
  n = nrow(hm)
  ns = max(hm)-1
  comm = matrix(0, n, n)
  for (s in 1:ns)
  {
    A = hm
    A[A > s] = 0
    comm = comm + as.matrix(expm(A))
  }
  comm = comm/ns
  return(comm)
}