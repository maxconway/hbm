get.movements<-function(movement, hm, features = NULL)
{
  
  movement.summary = which(movement != 0, arr.ind = T)
  indices = which(movement != 0)
  type = movement[indices]
  scale = hm[indices]
  robust = c()
  if (length(features) > 0)
  {
    if (is.matrix(features))
    {
      robust = features[indices]
      robust[is.na(robust)] = 0
      robust[robust > 0] = 1
    }else if (is.list(features))
    {
      for (i in 1:length(features))
      {
        robust.i = features[indices]
        robust.i[is.na(robust.i)] = 0
        robust.i[robust.i > 0] = 1
        robust = cbind(robust, robust.i)
      }
      colnames(robust) = unlist(lapply(1:length(features), function(x) paste("robust", x, sep = "")))
    }
    
  }
  
  movement.summary = data.frame("from" = movement.summary[,1], "to" = movement.summary[,2], "type" = type, "scale" = scale) 
  if (length(robust) > 0)
  {
    movement.summary = cbind(movement.summary, "robust" = robust)
  }
  return(movement.summary)
  
}
