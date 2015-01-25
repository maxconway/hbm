\name{detect.movement}
\alias{detect.movement}

\title{
Detect Movements Between Association Matrices using their HBMs
}
\description{
\code{detect.movement} takes a reference and a target association matrices, typically contact maps of chromosomes, and their hierarchical block matrices (computed with \code{\link{hbm}}) and detects movements between the reference and the target. A detected movement represents a disposition of node in a the reference, that gave rise to its new position in the target.  
}
\usage{
detect.movement(ref, target, ref.res, target.res, 
            motion.prop.thresh = 0.75, siglevel = 0.05, verbose = FALSE)
}
\arguments{
  \item{ref}{
the reference matrix. A numeric association matrix, typically a symmetric chromatin contact map. 
}
  \item{target}{
the target matrix to be compared to \code{ref}. A numeric association matrix, typically a symmetric chromatin contact map. 
}
  \item{ref.res}{
the result of calling \code{\link{hbm}} with \code{ref}.
}
  \item{target.res}{
the result of calling \code{\link{hbm}} with \code{target}.
}
  \item{motion.prop.thresh}{
numeric giving the threshold for detecting whether a region has moved based on the proportions of changed neighbors in its cluster. 
}
  \item{siglevel}{
 numeric giving the threshold for detecting a significant compactness/unfolding in a given cluster, set to 0.05 by default. 
}
  \item{verbose}{
boolean indicating whether to print intermediate results. 
}
}
\details{
\code{detect.movement} iterates through the scales of the reference matrix (using its HBM) and through the clusters at each scale and test for movements in the target. For each cluster it further tests for significant differences in association probabilities and for changes in the neighbors of each node.
}
\value{
\code{detect.movement} returns a numeric matrix, with the i,j-th entry taking one of the following values:\cr-1 if i has moved away from j,\cr 1 if i has moved towards j,\cr 0 if there was no movement, and\cr -0.5 and 0.5 for implicated away and towards movements,  respectively.
}
\references{
\code{hbm}'s website: \href{http://www.cl.cam.ac.uk/~ys388/hbm/}{http://www.cl.cam.ac.uk/~ys388/hbm/}
}
\author{
Yoli Shavit
}

\seealso{
\code{\link{get.movements}} to summarize the results of \code{detect.movement} \cr
\code{\link{hbm}} to learn how to build hierarchical block matrices\cr
\code{hbm}'s tutorials at \href{http://www.cl.cam.ac.uk/~ys388/hbm/}{http://www.cl.cam.ac.uk/~ys388/hbm/}
}
\examples{
set.seed(2)
n = 200 # chain size

# control configuration
conf = generate.random.conf(n, sd = 0.5, scale = FALSE)
# condition-1
conf.tr.1 = conf
conf.tr.1[99,] = conf.tr.1[77,]-0.5

# generate contact map-like matrices
control = exp(-1*as.matrix(dist(conf))) 
tr.1 = exp(-1*as.matrix(dist(conf.tr.1))) 

control.res = hbm(control, 2)
tr.1.res = hbm(tr.1, 2)
m1 = detect.movement(control, tr.1, control.res, tr.1.res)
resm = get.movements(m1, control.res$hm)
resm

# compare with configuration
par(mfrow = c(1,2))
cols = rep("black", n)
cols[unique(resm$from)] = "green"
plot(conf, xlab = "X", ylab = "Y", type = 'n', main = "Control")
text(conf[,1:2], labels = 1:n, cex = 0.75, col = cols)
cols = rep("black", n)
cols[unique(resm$from)] = "green"
cols[resm$to[which(resm$type == 0.5)]] = "pink"
cols[resm$to[which(resm$type == 1)]] = "red"
cols[resm$to[which(resm$type == -0.5)]] = "cyan"
cols[resm$to[which(resm$type == -1)]] = "blue"
plot(conf.tr.1, xlab = "X", ylab = "Y", type = 'n', main = "Condition-1", col = cols)
text(conf.tr.1[,1:2], labels = 1:n, cex = 0.75, col = cols)




}
