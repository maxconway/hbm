hierarchy<-function(hm)
{
  
  n = nrow(hm)
  pi = unlist(lapply(1:n, function(i)
  {
    x = hm[i,]
    if (i < 3)
    {
      x2 = x[(i+1):n]
      diff2 = x2[2:(length(x2)-1)]-x2[1:(length(x2)-2)]
      s2 = sum(diff2 < 0)
      s2
    }else if (i > n-2)
    {
      x1 = x[1:i]
      x1 = x1[x1 > 0]
      diff1 = x1[2:(length(x1)-1)]-x1[1:(length(x1)-2)]
      s1 = sum(diff1 > 0) 
      s1
      
    }else
    {
      x1 = x[1:i]
      x1 = x1[x1 > 0]
      diff1 = x1[2:(length(x1)-1)]-x1[1:(length(x1)-2)]
      s1 = sum(diff1 > 0)
      x2 = x[(i+1):n]
      diff2 = x2[2:(length(x2)-1)]-x2[1:(length(x2)-2)]
      s2 = sum(diff2 < 0) 
      s1+s2
      
    }
  }))
  
  return(-mean(pi))
  
}