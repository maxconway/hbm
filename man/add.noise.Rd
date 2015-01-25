\name{add.noise}
\alias{add.noise}
\title{
Add Noise to a Symmetric Association Matrix 
}
\description{
\code{add.noise} adds noise to a symmetric association matrix, typically a chromatin contact map.}
\usage{
add.noise(m, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{m}{ a symmetric numeric association matrix, typically a chromatin contact map.
}
 \item{\dots}{ additional parameters for \code{\link{jitter}}.}
}

\value{
\code{add.noise} returns a matrix of the same dimension as \code{m} but with noise added (see additional parameters for setting noise amount in \code{\link{jitter}}).}

\references{
\code{hbm}'s website: \href{http://www.cl.cam.ac.uk/~ys388/hbm/}{http://www.cl.cam.ac.uk/~ys388/hbm/}
}

\author{
Yoli Shavit
}


\seealso{
\code{\link{hbm.features}} to see how \code{add.noise} is used to estimate feature robustness in hierarchical block matrices\cr
\code{\link{hbm}} to learn how to build hierarchical block matrices\cr
\code{hbm}'s tutorials at \href{http://www.cl.cam.ac.uk/~ys388/hbm/}{http://www.cl.cam.ac.uk/~ys388/hbm/} 
}

\examples{
set.seed(2)
n = 200 # chain size
conf = generate.random.conf(n, sd = 0.5, scale = FALSE)
# generate a contact map -like matrix using the model c ~ exp(-d)
control = exp(-1*as.matrix(dist(conf)))
# add noise 
control.noisy = add.noise(control, factor = 5)

}
