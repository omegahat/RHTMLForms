\name{getArgFormElements}
\alias{getArgFormElements}
\title{Filter for eliminating form elements}
\description{
  This function is used when constructing the argument list when
  creating a function to represent the HTML form.
  It filters out the HTMLFormElement objects from the form
  description in which we have  no interest.
  This happens when we are creating the argument list
  and creating the data for the form submission.
  Basically, this function, by default,
  discards the user interface components used
  to submit or reset the form rather than
  set values for the submission.
}
\usage{
getArgFormElements(desc, targetClasses = c("HTMLSubmitElement", "HTMLResetElement", "HTMLImageElement", "HTMLHiddenElement"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{desc}{an \code{HTMLFormDescription} object or its elements.}
  \item{targetClasses}{the collection of class names that we want to omit.}
}
\value{
  The subset of the \code{HTMLFormDescription} elements
  that do not inherit from the specified \code{targetClasses}.
}
\author{Duncan Temple Lang <duncan@wald.ucdavis.edu>}

\examples{
}
\keyword{interface}


