\name{hierarchy}
\alias{hierarchy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute Hierarchy of an HBM
}
\description{
\code{hierarchy} computes the hierarchy of a hierarchical block  matrix computed with \code{hbm}. 
}
\usage{
hierarchy(hm)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{hm}{a hierarchical block matrix computed with \code{\link{hbm}}, with:\cr\eqn{hm_{i,j} =} the minimal scale (iteration) at which i and j were clustered together, or 0 if i=j
}
}
\details{
In a hierarchical matrix, computed with \code{\link{hbm}}, the behavior around the diagonal reflects the hierarchy of the association matrix. Specifically, for a hierarchical fractal-like structure we expect a non-decreasing series in the upper triangle of the matrix and a non-increasing series in the lower triangle. \code{hierarchy} counts the number of deviations from this behavior for each node: number of negative successive differences up to the diagonal and number of positive successive changes after the diagonal, and returns the negation of the mean number of changes across nodes.
}
\value{
\code{hierarchy} returns a numeric value giving the hierarchy of the matrix.
}
\references{
\code{hbm}'s website:  \href{http://www.cl.cam.ac.uk/~ys388/hbm/}{http://www.cl.cam.ac.uk/~ys388/hbm/}
}
\author{
Yoli Shavit
}

\seealso{
\code{\link{generate.random.conf}} to see how to generate interesting chains\cr
\code{\link{hbm}} learn how to build hierarchical block matrices\cr
\code{hbm}'s tutorials at \href{http://www.cl.cam.ac.uk/~ys388/hbm/}{http://www.cl.cam.ac.uk/~ys388/hbm/}
}
\examples{
set.seed(2)

n = 100 # chain size
#generate configurations 
conf = generate.random.conf(n, sd = 0.5, scale = FALSE)
#perturb the chain
conf.perturb.all = generate.random.conf(n, perturb = 1:n, sd = 0.5, scale = FALSE)
# and again with less perturbration 
conf.perturb = generate.random.conf(n, perturb = 10:50, sd = 0.5, scale = FALSE)

# compute the HBMs
hm.control = hbm(exp(-1*as.matrix(dist(conf))), 2)$hm
hm.perturb.all = hbm(exp(-1*as.matrix(dist(conf.perturb.all))), 2)$hm
hm.perturb = hbm(exp(-1*as.matrix(dist(conf.perturb))), 2)$hm

h.control = hierarchy(hm.control)
h.perturb = hierarchy(hm.perturb)
h.perturb.all = hierarchy(hm.perturb.all)
h = c(h.control, h.perturb, h.perturb.all)

# plot
plot(1:3, h, pch = 19, cex = 2, axes = FALSE, ylab = "Chain Hierarchy", xlab = "Condition")
axis(1, at = 1:3, labels = c("Control", "Perturbed-Partial", "Perturbed-All"))
axis(2)


}
