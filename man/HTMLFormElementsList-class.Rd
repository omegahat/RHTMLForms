\name{HTMLFormElementsList-class}
\Rdversion{1.1}
\docType{class}
\alias{HTMLFormElementsList-class}
\alias{[,HTMLFormElementsList-method}

\title{Class \code{"HTMLFormElementsList"}}
\description{Class for representing the inputs of an HTML form}
\section{Objects from the Class}{A virtual Class: No objects may be created from it.}
\section{Extends}{
Class \code{"\linkS4class{list}"}, directly.
Class \code{"\linkS4class{vector}"}, by class "list", distance 2.
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "HTMLFormElementsList")}:
      this treats the form as a list but preserves the class so that we
      can use the resulting subset of inputs/parameters as a
      \code{HTMLFormElementList} object also.
	 }
}
\author{
Duncan Temple Lang
}
\seealso{
  \code{\link{getHTMLFormDescription}}
}
\examples{
showClass("HTMLFormElementsList")
}
\keyword{classes}
