\name{mcl}
\alias{mcl}

\title{
Markov Clustering 
}
\description{
\code{mcl} implements the Markov Clustering algorithm (van Dongen, 2000) with a fixed expansion parameter (=2).
}
\usage{
mcl(m, infl, iter = 1000, remove.self.loops = FALSE, prune = FALSE, 
     thresh = 1e-06, pruning.prob = 1e-06, use.sparse = NULL, verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{m}{
A numeric matrix, given as input to the Markov Clustering algorithm.
}
  \item{infl}{
numeric. The inflation parameter for the Markov Clustering algorithm. 
}
  \item{iter}{
integer giving the maximal number of iterations for the Markov Clustering algorithm, set to 1000 be default (in practice the algorithm is shown to converge after 10-100 iterations). 
}
  \item{remove.self.loops}{
boolean indicating whether to remove self loops (i.e. set diagonal entries to 0), set to FALSE by default.
}
  \item{prune}{
boolean indicating whether to prune small probabilities (i.e. set to 0) in the transition matrix, set to FALSE by default. 
}
  \item{thresh}{
a numeric giving the difference threshold below which the transition matrix is considered to have converged. 
}
  \item{pruning.prob}{
numeric giving the threshold below which pruning should be applied, when \code{prune} is \code{TRUE}. Set to \code{1e-06} be default.
}
  \item{use.sparse}{
a boolean indicating whether to use sparse matrices. By default this value is set to NULL, so that sparse matrices are used only if the transition matrix is sparse enough to justify this representation (50\% sparsity and above). When set to \code{TRUE} (\code{FALSE}), \code{use.sparse} will force (disable) the use of sparse matrices.
}
  \item{verbose}{
boolean indicating whether to print the number of iterations before convergence was achieved.
}
}
\details{
\code{mcl} is called from \code{\link{hbm}} to build a hierarchical block matrix from an association matrix, typically a chromatin contact map. 
}
\value{
\code{mcl} returns a vector whose i-th entry is the cluster identifier of the i-th node, and two nodes are in the same cluster iff they have the same cluster identifier. 
}
\references{
Stijn van Dongen. A cluster algorithm for graphs. Technical Report INS-R0010, National Research Institute for Mathematics and Computer Science in the Netherlands, Amsterdam, May 2000.\cr\cr
\code{hbm}'s website: \href{http://www.cl.cam.ac.uk/~ys388/hbm/}{http://www.cl.cam.ac.uk/~ys388/hbm/}

}
\author{
Yoli Shavit
}

\seealso{
\code{\link{hbm}} to learn how to build hierarchical block matrices\cr
\code{hbm}'s tutorials at \href{http://www.cl.cam.ac.uk/~ys388/hbm/}{http://www.cl.cam.ac.uk/~ys388/hbm/}
}
