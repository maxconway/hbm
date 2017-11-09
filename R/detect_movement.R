detect.movement<-function(ref, target, ref.res, target.res, 
                          motion.prop.thresh = 0.75, siglevel = 0.05, verbose = FALSE)
{
  
  n = nrow(ref)
  if (sum(dim(ref) != dim(target)) > 0)
  {
    stop("reference and target matrices must be of the same dimension")
  }else if (ncol(ref) != n)
  {
    stop("input matrices must be square matrices")
  }
  
  movement = matrix(0, n, n)
  
  
  ## transform contact maps to probability matrices
  ref = sweep(ref,2,colSums(ref),`/`)
  ref[is.na(ref)] = 0
  diag(ref) = NA
  target = sweep(target,2,colSums(target),`/`)
  target[is.na(target)] = 0
  diag(target) = NA
  
  # extract scales and hierarchical matrix from the data structure
  ref.scales = ref.res$scales
  target.scales = target.res$scales
  ref.hm = ref.res$hm
  target.hm = target.res$hm
  
  maxscale = length(ref.scales) 
  
  # iterate through the scales and the clusters of the refernce
  # and look for changes, then map them to directed movement
  for (scale in 1:maxscale)
  {
    clusters = ref.scales[[scale]]
    for (i in 1:length(clusters))
    {
      sig.pack = FALSE
      sig.unfold = FALSE
      ids = clusters[[i]]
      
      # only check clusters with size > 1 that 
      # their maximal scale is the scale under consideration
      if (max(ref.hm[ids,ids]) == scale & length(ids) > 1) 
      {
        
        # check if the cluster has become significantly more packed or unfolded
        # this acchived by testing the relative within and
        #  outer connectivity in the ref and target
        notids = which(!(1:n %in% ids))
        ref.conn = unlist(lapply(ids, function(x) mean(ref[ids,x], na.rm = T)))/unlist(lapply(ids, function(x) mean(ref[notids,x], na.rm = T)))
        target.conn = unlist(lapply(ids, function(x) mean(target[ids,x], na.rm = T)))/unlist(lapply(ids, function(x) mean(target[notids,x], na.rm = T)))        
        answer = tryCatch(
          {
            pval.g = t.test(ref.conn, target.conn, alternative = "greater")$p.value
            
            if (!is.na(pval.g) && pval.g < siglevel)
              #target cluster is less compact than ref cluster
            {
              sig.unfold = TRUE
              if (verbose)
              {
                cat(paste("cluster", i, "at scale", scale, 
                          "is less compact in target (p-value <",pval.g,"\n",sep=" "))
              }
            }
            else 
            {
              pval.l = t.test(ref.conn, target.conn, alternative = "less")$p.value
              if (!is.na(pval.l) && pval.l < siglevel)
              {
                #target cluster is more compact than ref cluster
                sig.pack = TRUE
                if (verbose)
                {
                  cat(paste("cluster", i, "at scale", scale, 
                            "is more compact in target (p-value <",pval.l,"\n",sep=" "))
                }
                # we cannot detect orientation so we make it symmetric
                movement[ids, ids] = 1
                
              }
            }
          },error = function(w){}
        )
        
        # now inspect for local movements 
        c1 = ref.hm[ids,ids]
        c2 = target.hm[ids,ids]
        
        # look only within the scale of interest to avoid reps.
        c1[which(c1 != scale)] = 0
        c2[which(c1 == 0)] = 0
        
        # get the scales in the target cluster (and remove 0 that is always on the diagonal)
        unique.target.scales = sort(unique(as.vector(c2)))
        unique.target.scales = unique.target.scales[2:length(unique.target.scales)]
        
        # detect possible motion: indicated by multiple scales at the target cluster 
        min.scale = unique.target.scales[1]
        target.c2 = target.hm
        target.c2[ids,ids] = c2
        if (length(unique.target.scales) > 1)
        {
          prop.moved = unlist(lapply(ids, function(x) sum(target.c2[ids,x] > min.scale)/length(ids)))
          if (sum(prop.moved >= motion.prop.thresh) > 0) # possible movement
          {
            if (sig.pack) # movement because of significant within-cluster packing
            {
              clustered = ids[which(prop.moved < motion.prop.thresh)]
              moved = ids[which(prop.moved >= motion.prop.thresh)]
              movement[clustered, moved] = -1
              # we cannot be sure if any others have moved away additionally so we put 0.5
              for (region in moved)
              {
                attractor = which(target[,region] == max(target[,region], na.rm = T))[1]
                movement[region, attractor] = 0.5
              }
              
            }else 
            {
              moved = ids[which(prop.moved >= motion.prop.thresh)]
              for (region in moved)
              {
                attractor = which(target[,region] == max(target[,region], na.rm = T))[1]
                
                if (attractor %in% ids)
                {
                  if (sig.unfold) # this can happen becuase of movements away within the cluster
                  {
                    movement[region, attractor] = 1
                    others = ids[!(ids %in% c(region, attractor))]
                    vals = ref[region, others]
                    repulsor = others[which(vals == max(vals, na.rm = T))]
                    movement[region, repulsor] = -1
                    movement[region, others[others!=repulsor]] = -0.5
                  }
                  
                }else
                {
                  movement[region, attractor] = 1
                  movement[region, which(target.hm[attractor,] == 1)] = 0.5
                  others = ids
                  vals = ref[region, others]
                  repulsor = others[which(vals == max(vals, na.rm = T))]
                  movement[region, repulsor] = -1
                  movement[region, others[others!=repulsor]] = -0.5
                }
              }
            }
            
          }
        } # end if of local movements
        
      }# end big if
      
    }#end cluster for
  }#end scale for
  diag(movement) = 0
  return(movement)
}