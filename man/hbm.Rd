\name{hbm}
\alias{hbm}
\title{
Build a Hierarchical Block Matrix (HBM)
}
\description{
\code{hbm} builds a hierarchical block matrix from an association matrix, typically a symmetric chromatin contact map, by iteratively aggregating clusters.
}
\usage{
hbm(m, infl=2, ...)
}
\arguments{
  \item{m}{
a numeric association matrix, typically a chromatin contact map. 
}
  \item{infl}{
numeric giving the inflation parameter for \code{\link{mcl}}, set to 2 by default.
}
  \item{\dots}{
additional parameters for \code{mcl}.
}
}
\details{
\code{hbm} iteratively applies Markov Clustering (by calling \code{mcl}). In the first iteration, clustering is applied on the input association matrix. The resulting clusters are used to generate a new association matrix to cluster, whose i,j-th entry gives the mean association between all the nodes in the i-th and j-th clusters found in the previous iteration. This is repeated until all clusters are aggregated to a single cluster or when clusters can no longer be aggregated together.
}
\value{
\code{hbm} returns a list with the following objects:
\item{hm}{The hierarchical block matrix, defined as:\cr\eqn{hm_{i,j} =} the minimal scale (iteration) at which i and j were clustered together, or 0 if i=j}
\item{scales}{a list of length \code{max(hm)-1} whose k-th entry gives the list of clusters found at k-th scale (iteration).}
}
\references{
\code{hbm}'s website: \href{http://www.cl.cam.ac.uk/~ys388/hbm/}{http://www.cl.cam.ac.uk/~ys388/hbm/}
}
\author{
Yoli Shavit
}

\seealso{
\code{\link{mcl}} for the implementation of Markov Clustering\cr
\code{\link{detect.movement}} to see how \code{hbm}'s results are used to detect movements\cr
\code{\link{communicability}} to see how \code{hbm}'s results are used to compute the communicability between different locations.\cr 
\code{\link{hierarchy}} to see how \code{hbm}'s results are used to compute the hierarchy of the association matrix.\cr
\code{hbm}'s tutorials at \href{http://www.cl.cam.ac.uk/~ys388/hbm/}{http://www.cl.cam.ac.uk/~ys388/hbm/}
}
\examples{

set.seed(2)
n = 200 # chain size
# generate chain configuration (random walk/giant loop model)
conf = generate.random.conf(n, sd = 0.5, scale = FALSE)
# generate a contact map like matrix using the model c ~ exp(-d)
control = exp(-1*as.matrix(dist(conf)))
res = hbm(control)
m = res$hm
image(t(m)[,nrow(m):1], axes = FALSE)
ats = seq(0,1,0.2)
lbls = as.character(n*ats)
axis(1, at= ats, labels = lbls, cex.axis = 0.8) 
ats = seq(1,0,-1*0.2)
lbls = as.character(n*seq(0,1,0.2))
axis(2, at= ats, labels = lbls, cex.axis = 0.8) 
            
res$scales


}
