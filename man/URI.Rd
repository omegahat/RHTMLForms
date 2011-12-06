\name{URI}
\alias{URI}
%\alias{parseURI}
\title{Create URI object}
\description{
  This function is responsible for creating a URI object
  either directly from a string or from its constituent parts.
  \code{URI} allows one to use an existing URI and replace
  some or all of its components to create a new URI.
}
\usage{
URI(u, path = "/", protocol = "http", port = 80)
}
%parseURI(u, isFile = FALSE)
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{u}{either a template URI or a string giving the URI in typical
    user-level format, e.g. \code{http://www.omegahat.org/index.html}.}
  \item{path}{the specification of the file within the web site.
    For example, this is the string \code{/a/b/c} in
     \code{http://www.mysite.org/a/b/c}.}
   \item{protocol}{a string indicating the protocol
     to use to access the URI.
     This is typically \code{"http"}, \code{https},
     \code{"ftp"},  or \code{news}.
     Currently, only \code{http} and \code{ftp} are supported.
     Efforts to support \code{https} are in progress.
   }
   \item{port}{the port number given as an integer or a string.
     Http uses 80 by default and Ftp uses 21. However, this allows us to
     connect using other ports on which the Web server is listening.
   }
%   \item{isFile}{}
  }
\details{

}
\value{
  An object of class \code{URI}.
  This has 4 elements.
  \item{protocol}{}  
  \item{host}{}
  \item{path}{}
  \item{port}{}  
  The \code{URI} object is merely a character vector containing these
  elements.
  Some of these elements may be missing if not specified.
  And in other cases, they will be inserted with missing values
  (\code{NA}) to identify that they are to be overridden.
}

\author{Duncan Temple Lang <duncan@wald.ucdavis.edu> }

\note{
  We have chosed to use S3 classes for simplicity
  within the project we are working.
}

\seealso{\code{\link{mergeURI}} }
\examples{
\dontrun{
 u = URI("http://www.omegahat.org/index.html")

 URI(u, protocol = "ftp")
 URI(u, path = "/download/R/packages/index.html")
}
}
\keyword{IO}

