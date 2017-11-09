hbm<-function(m, infl=2, ...)
{
  
  N = nrow(m)
  if (N != ncol(m))
  {
    stop("input matrix must be a square matrix")
  }
  hm = matrix(0, N, N)
  scale = 1
  previds = c()
  diff = 1
  scales = list()
  while(N > 1)
  {
    clusters = mcl(m, infl, ...)
    clustid = unique(clusters)
    assert_that(length(clustid)<nrow(hm), msg='Failed to find any clusters. Too many 0s in matrix?')
    ids = lapply(clustid, function(id) which(clusters == id))
    Ncurr = length(clustid)
    if (N - Ncurr == 0)
    {
      break
    }
    
    mtag = matrix(0, Ncurr, Ncurr)
    scales[[scale]] = ids
    for (i in 1:Ncurr)
    {
      for (j in i:Ncurr)
      {
        mtag[i,j] = mean(m[ids[[i]], ids[[j]]])
      }
    }
    mtag[lower.tri(mtag)] = t(mtag)[lower.tri(t(mtag))]
    scale = scale+1
    m = mtag
    N = Ncurr
  }

  
  if (length(scales) > 1)
  {
    for (scale in 2:length(scales))
    {
      currscale = scales[[scale]]
      prevscale = scales[[(scale-1)]]  
      for (i in 1:length(currscale))
      {
        indices = currscale[[i]]
        if (scale > 1)
        {
          indices = unlist(lapply(indices, function(x) prevscale[[x]]))
          currscale[[i]] = indices
        }
        scales[[scale]] = currscale
      }
    }
  }
  
  for (scale in rev(seq_along(scales)))
  {
    currscale = scales[[scale]]
    for (i in seq_along(currscale))
    {
      indices = currscale[[i]]
      hm[indices,indices] = scale
    }

  }
  hm[hm == 0] = length(scales)+1
  diag(hm) = 0
  return(list("hm"=hm, "scales"=scales))
}

