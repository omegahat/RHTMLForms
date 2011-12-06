\name{mergeURI}
\alias{mergeURI}
\title{Combine one URI with respect to a base URI}
\description{
  This function allows the caller to combine one URI specification
  relative to another base URI and expand it.
  This handles resolving relative links, etc. relative to the URI
  in which they are defined.
}
\usage{
mergeURI(u, url, reduce = FALSE)
}
\arguments{
  \item{u}{the (partial) URI of class \code{URI}.}
  \item{url}{the base URI (of class \code{URI}) relative to which the URI \code{u} will be resolved.}
  \item{reduce}{a logical value indicating whether to simplify the path
    specification to remove the . and .. directory elements.}
}
\value{
  An object of class \code{URI}.
  See \code{\link{URI}}.
}
\author{Duncan Temple Lang <duncan@wald.ucdavis.edu> }


\seealso{
 \code{\link{URI}}
}
\examples{
\dontrun{
  u = URI("http://www.omegahat.org/index.html")

  # Merge at the same top-level of the path
  mergeURI(URI("foo.html"), u)

  u = URI("http://www.omegahat.org/download/R/packages")
  mergeURI(URI("xml"), u)


  u = URI("http://www.omegahat.org/download/R/packages/index.html")
  mergeURI(URI("../xml/index.html"), u)

  # not a relative URI so remains unaltered.
  mergeURI(URI("http://www.ggobi.org/index.html"), u)
}
}
\keyword{IO}

