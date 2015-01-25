\name{generate.random.conf}
\alias{generate.random.conf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate a Random Chain Configuration
}
\description{
\code{generate.random.conf} generates a random chain configuration following a random walk/giant loop model (Sachs et al., 1995).
}
\usage{
generate.random.conf(n, k = 3, perturb = NULL, scale = T, mean = 0, sd = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
integer giving the chain length (number of beads in the chain).
}

 \item{k}{
integer giving the space dimension, set to 3 by default. 
}
  \item{perturb}{
integer vector of nodes (indices) to perturb. This argument can be used to generate a configuration that deviates from the chain constraints of successive beads (nodes). Perturbation is achieved by sampling a new order for the beads to perturb and exchanging their coordinates in the original configuration accordingly. By default \code{perturb} is set to NULL indicating no perturbation should not be applied. 
}
 
  \item{scale}{
boolean indicating whether or not to scale the generated configuration, set to \code{TRUE} by default. 
}
  \item{mean}{
numeric giving the mean of differences distribution along each axis, set to 0 by default. 
}
  \item{sd}{
numeric giving the standard deviation of differences distribution along each axis, set to 1 by default.
}
}

\details{
\code{generate.random.conf} aims to generate a chromosome-like chain of n beads (nodes), in a k-D Euclidean space (k=3 by default) that follows a random walk/giant loop model (Sachs et al., 1995). This is achieved by sampling the differences between successive beads' coordinates from a normal distribution \eqn{N(\mu, \sigma)} (\eqn{\mu = 0}, \eqn{\sigma = 1}, by default), across each axis (see examples in Hu et al., 2013 and Shavit et al., 2014). The configuration is scaled by default so that the distance between the first and last beads is approximately one unit. \code{generate.random.conf} can also be used to generate configurations that deviate from the chain constraints by perturbing beads.
}

\value{
\code{generate.random.conf} returns a n x k matrix, giving the coordinates of n beads (nodes) in a k-d space. 
}
\references{
Sachs, R. K., van den Engh, G., Trask, B., Yokota, H. and Hearst, J. E. A random-walk/giant-loop model for interphase chromosomes. Proceedings of the National Academy of Sciences of the United States of America, 92, 2710-4 (1995).\cr\cr
Hu, M. et al. Bayesian Inference of Spatial Organizations of Chromosomes. PLoS Computational Biology. 9, e1002893 (2013).\cr\cr
Shavit, Y., Hamey, F. K. and Lio, P. FisHiCal: an R package for iterative FISH-based calibration of Hi-C data. Bioinformatics, 30, 3120-3122 (2014).\cr\cr
\code{hbm}'s website: \href{http://www.cl.cam.ac.uk/~ys388/hbm/}{http://www.cl.cam.ac.uk/~ys388/hbm/}
}
\author{
Yoli Shavit 
}

\seealso{
\code{\link{hbm}} to learn how to build a hierarchical block matrix from a contact map of a random configuration.\cr
\code{hbm}'s tutorials at \href{http://www.cl.cam.ac.uk/~ys388/hbm/}{http://www.cl.cam.ac.uk/~ys388/hbm/}
}
\examples{
set.seed(2)
n = 100 
conf = generate.random.conf(n, k = 2)
plot(conf, xlab = "x", ylab = "y")

conf = generate.random.conf(n, k = 2, scale = FALSE)
plot(conf, xlab = "x", ylab = "y")

}
