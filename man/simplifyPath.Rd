\name{simplifyPath}
\alias{simplifyPath}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Simplify folder/directory paths. }
\description{
  This is a basic function that removes relative
  navigation within a directory path.
  In other words, it attempts to resolve
  \code{..} and \code{.}.

  Note that the function does not currently
  do extensive checking that the result makes
  sense.   To truly do this and to take account of symbolic links, etc.,
  we need to check via the operating system.
  We also don't worry about drive names (e.g. \code{C:}, \code{F:}) on Windows systems.
}
\usage{
simplifyPath(path, sep = "/")
}
\arguments{
  \item{path}{the vector of path strings to simplify.}
  \item{sep}{the directory separator.
    Since this is used for URIs, we use / as the default.
    We could use \code{.Platform$file.sep}.}
}

\value{
 A character vector with as many elements.
}
\author{Duncan Temple Lang <duncan@wald.ucdavis.edu>}


\seealso{
 \code{\link{mergeURI}}  
 \code{\link[base]{dirname}}
}
\examples{
\dontrun{
simplifyPath("/ab/cdef/ghi")
simplifyPath("/ab/cdef/ghi/..")
simplifyPath("/ab/cdef/../ghi/..")

simplifyPath("./ab/cdef/ghi/..") # "ab/cdef"

simplifyPath("./ab/") # "ab"
simplifyPath("/xyz/./ab/") # "xyz/ab"
}
}
\concept{directory}
\concept{file system}
\keyword{IO}

