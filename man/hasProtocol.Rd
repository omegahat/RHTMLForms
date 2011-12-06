\name{hasProtocol}
\alias{hasProtocol}
\alias{isRelative}
\alias{hasHost}
\title{URI management utilities}
\description{
  These functions are utilities for merging URI objects.
  They test different characteristics of the given URI.
}
\usage{
hasProtocol(u)
isRelative(u)
hasHost(u)
}
\arguments{
  \item{u}{the URI object of interest.}
}
\value{
  Each returns a logical value indicating
  whether the URI has:
  \item{protocol}{a protocol of the form http, https, ftp, news}
  \item{relative path}{a relative, rather than fully qualified,  path,
    e.g. ../foo.html or Foo.html as opposed to /foo.html and http://www.omegahat.org/index.html.}
  \item{host}{a domain or hostname such as www.omegahat.org.}
}

\author{Duncan Temple Lang <duncan@wald.ucdavis.edu> }

\seealso{
\code{\link{mergeURI}}
}
\examples{

}
\keyword{}

