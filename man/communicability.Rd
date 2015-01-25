\name{communicability}
\alias{communicability}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute Communicability between Nodes in an HBM
}
\description{
\code{communicability} computes the scale communicability between nodes (rows) in a hierarchical block matrix \code{hm}, generated with \code{\link{hbm}}.}  

\usage{
communicability(hm)
}

\arguments{
  \item{hm}{a hierarchical block matrix computed with \code{\link{hbm}}, with:\cr \eqn{hm_{i,j} =} the minimal scale (iteration) at which i and j were clustered together, or 0 if i=j
}
}
\details{
The communicability of an adjacency matrix \code{A} can be expressed as \eqn{e^A}  (Estrada et al., 2012), so that the i,j-th entry is a weighted sum of all paths from i to j, where shortest paths are assigned with larger weights. For a hierarchical block matrix, computed with \code{\link{hbm}}, each scale \code{s} defines its own adjacency matrix, where all entries with values larger than s are set to 0. The scale-communicability between 2 nodes i and j in this matrix is defined here as the mean communicability across scales (excluding the largest scale).
}
\value{
\code{communicability} returns a matrix of the same dimensions as \code{hm}, where the (i,j)-th entry gives the scale-communicability between i and j in \code{hm}.
}
\references{
Estrada, E., Hatano, N. and Benzi, M. The physics of communicability in complex networks. Physics Reports 514, 89-119 (2012).\cr\cr
hbm's website: \href{http://www.cl.cam.ac.uk/~ys388/hbm/}{http://www.cl.cam.ac.uk/~ys388/hbm/}

}
\author{
Yoli Shavit 
}

\seealso{
\code{\link{hbm}} to learn how to build hierarchical block matrices\cr
\code{hbm}'s tutorials at \href{http://www.cl.cam.ac.uk/~ys388/hbm/}{http://www.cl.cam.ac.uk/~ys388/hbm/}
}
\examples{
set.seed(2)
n = 100 # chain size
conf = generate.random.conf(n, scale = FALSE)

# compute the HBM
hm.control = hbm(exp(-1*as.matrix(dist(conf))), 2)$hm

# compute scale communicability
comm = communicability(hm.control)

# explore for position 50
plot(1:n, comm[50,], xlab = "Position", ylab = "Communicability", 
main = "Communicability for Position 50", pch=16)

# plot in original configuration
cols = rep("black", n)
cols[which(comm[50,] > 100)] = "blue"
plot(conf, xlab = "X", ylab = "Y", type = "n")
text(conf, labels = 1:n, col = cols)

}
