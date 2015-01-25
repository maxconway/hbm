\name{get.movements}
\alias{get.movements}

\title{
Summarize Detected Movements 
}
\description{
\code{get.movements} summarizes the results of \code{\link{detect.movement}}.
}
\usage{
get.movements(movement, hm, features = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{movement}{
numeric matrix computed with \code{\link{detect.movement}}.
}
\item{hm}{
a hierarchical block matrix computed with \code{\link{hbm}}, with:\cr\eqn{hm_{i,j} =} the minimal scale (iteration) at which i and j were clustered together, or 0 if i=j
}
\item{features}{one or more feature matrices computed with \code{\link{hbm.features}}, used to indicate if the movement is detected within a robust feature. Set to \code{NULL} by default. When more than one matrix is provided \code{features} should be a list of matrices. 
}
}

\value{
\code{get.movements} returns a data frame with the following columns:\cr
\code{from}: the moving node,\cr
\code{to}: the node that \code{from} has moved towards or away from,\cr
\code{type}: one of the following values: (-1.0, 0.5, 0.5, 1) where -1 indicates \code{from} moved away from  \code{to}, 1 indicates \code{from} moved towards \code{to}, and -0.5 and 0.5 indicate possible/implicated movements correspondingly,\cr
\code{scale}: the scale at which the movement was detected, and\cr
\code{robust} an optional column (when \code{features} is not \code{NULL}) with 1 for non-\code{NA} elements in the \code{features} matrix, and 0 otherwise. 
}

\author{
Yoli Shavit
}

\references{
hbm's website: \href{http://www.cl.cam.ac.uk/~ys388/hbm/}{http://www.cl.cam.ac.uk/~ys388/hbm/}
}

\seealso{
\code{\link{detect.movement}} to see how changes between chains are detected\cr
\code{\link{hbm}} to learn how to build hierarchical block matrices\cr
\code{hbm}'s tutorials at \href{http://www.cl.cam.ac.uk/~ys388/hbm/}{http://www.cl.cam.ac.uk/~ys388/hbm/}
}

