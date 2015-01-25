\name{hbm.features}
\alias{hbm.features}
\title{
Compute Robust Features in an HBM

}
\description{
\code{hbm.features} computes the main features of a hierarchical block matrix. 
}
\usage{
hbm.features(m, noise.factor, ncores = 1, ref = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{m}{
a numeric association matrix, typically a chromatin contact map.}

  \item{noise.factor}{
numeric vector giving the noise factor to add with \code{\link{add.noise}} at each iteration. The length of this vector will determine the number of iterations. 
}
  \item{ncores}{
integer giving the number of cores to register and use. If this is larger than one, iterations will be executed in parallel. 
}
  \item{ref}{
hierarchical block matrix computed with \code{\link{hbm}} from \code{m}. If set to \code{NULL} this matrix will be computed as part of the execution of \code{hbm.features}.
}
  \item{\dots}{
additional parameters for \code{\link{hbm}}.
}
}

\details{
\code{hbm.features} adds noise to the given association matrix and executes \code{\link{hbm}} to generate a hierarchical block matrix. Repeating this for multiple iterations (with the same or different noise factor values) gives a mean hierarchical block matrix that can be compared with the matrix computed from the non noisy association matrix.
}

\value{
\code{hbm.features} returns a list with the following objects:
\item{noisy.hm}{The average hierarchical block matrix. A numeric matrix whose i,j-th entry gives the mean scale at which i and j were found in the same cluster across \code{\link{hbm}} iterations.}
\item{features}{\code{noisy.hm} with entries set to \code{NA} when different from the entries in the original non-noisy hierarchical block matrix. }
}

\references{
\code{hbm}'s website: \href{http://www.cl.cam.ac.uk/~ys388/hbm/}{http://www.cl.cam.ac.uk/~ys388/hbm/}
}
\author{
Yoli Shavit
}


\seealso{
\code{\link{add.noise}} to see how noise is added to matrices\cr
\code{\link{hbm}} to learn how to build hierarchical block matrices\cr
\code{hbm}'s tutorials at \href{http://www.cl.cam.ac.uk/~ys388/hbm/}{http://www.cl.cam.ac.uk/~ys388/hbm/}
}

\examples{

set.seed(2)
n = 100 # chain size
# generate chain configuration (random walk/giant loop model)
conf = generate.random.conf(n, sd = 0.5, scale = FALSE)
# generate a contact map -like matrix using the model c ~ exp(-d)
control = exp(-1*as.matrix(dist(conf)))
noise = rep(10, 10)
res = hbm.features(control, noise, prune = TRUE, pruning.prob = 0.01)

m = res$features
image(t(m)[,nrow(m):1], axes = FALSE)
ats = seq(0,1,0.2)
lbls = as.character(n*ats)
axis(1, at= ats, labels = lbls, cex.axis = 0.8) 
ats = seq(1,0,-1*0.2)
lbls = as.character(n*seq(0,1,0.2))
axis(2, at= ats, labels = lbls, cex.axis = 0.8) 
 
             





}
