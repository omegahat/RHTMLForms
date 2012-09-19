\name{usesJS}
\alias{usesJS}
\alias{usesJavaScript}
\title{Query if HTML form uses JavaScript code}
\description{
  These two functions allow us to query if the
  HTML form uses JavaScript code in the action/method or
  input elements.  If so,  static analysis of the form
  is more complicated if not impossible and we need to
  be able to evaluate the JavaScript code to determine
  possible arguments, values and input elements.
  This uses heuristics to determine if JavaScript code is present
  by analyzing the attributes of the input elements and the form itself.
}

\usage{
usesJS(form)
}
\arguments{
  \item{form}{a description of the HTML form of interest,
    returned by \code{\link{getHTMLFormDescription}}.}
}
\value{
 A logical value indicating if the form uses JavaScript code (\code{TRUE}).
}
\author{
Duncan Temple Lang
}
\seealso{
  \code{\link{getHTMLFormDescription}}
  \code{\link{createFunction}}
}
%\examples{}
\keyword{programming}
\keyword{interface}
