\name{toString.URI}
\alias{toString.URI}
\title{String representation of a URI object.}
\description{
  This converts a \code{URI}  object into a
  user-level string format that can be used
  when reading the document.
}
\usage{
toString.URI(x, ...)
}
\arguments{
  \item{x}{the object of class \code{URI}.}
  \item{\dots}{unused but present to match the generic function \code{toString}.}
}
\value{
 A single string.
}

\author{Duncan Temple Lang <duncan@wald.ucdavis.edu>}

\seealso{
 \code{\link{URI}}
}
\examples{
\dontrun{
 u = URI("http://www.omegahat.org/index.html")
 toString(u)
}
}
\keyword{IO}

