hbm.features<-function(m, noise.factor, ncores = 1, ref = NULL, ...)
{
  if (length(ref) == 0)
  {
    ref = hbm(m, ...)$hm
  }
  n = nrow(ref)
  if (length( noise.factor) == 0)
  {
    stop("factor must be a non empty vector")
  }else if (sum( noise.factor <= 0) > 0)
  {
    stop("factor must be a positive numeric vector")
  }
  
  l = list()
  if (ncores > 1)
  {
    ff = NULL
    registerDoParallel(ncores)
    on.exit( stopImplicitCluster())
    l <- foreach(ff=noise.factor, .export = c("hbm", "add.noise", "mcl"), .packages = c("Matrix")) %dopar% {
      x = add.noise(m, factor=ff)
      hbm(x, 2, ...)$hm  }
  }else{
    l = lapply(noise.factor, function(f) {
      x = add.noise(m, factor=f)
      hbm(x, 2, ...)$hm})
  }  
  target = Reduce('+', l)
  
  target = target/(length(noise.factor))
  maxscale = floor(max(as.vector(target)))
  target[target > maxscale] = maxscale
  features = target
  features[which(ref-target != 0)] = NA
  
  return(list("noisy.hm" = target, "features" = features))
}