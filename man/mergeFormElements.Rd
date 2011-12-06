\name{mergeFormElements}
\alias{mergeFormElements}
\title{Combine HTML form elements}
\description{
  This function combines the contents of an
  HTML form element with existing
  information for that named element.
  This integerates the possible options,
  the default/selected values to combine
  the different HTML elements into a
  single R object.
  The function finds the corresponding element
  in the existing set given by \code{elements}
  and adds the information in \code{el}
  with that element in \code{elements}.
}
\usage{
mergeFormElements(el, elements, name = el$name)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{el}{the \code{HTMLFormElement} to be integrated with the values
   already gathered from the form in \code{elements}}
  \item{elements}{a named list of previously collected \code{HTMLFormElements}}
  \item{name}{the identifier to determine which element in
    \code{elements} corresponds to \code{el}.
    It is very rare that one would specify this differently from the
    default value.
   }
}
\value{
  The original list \code{elements} with the new information
  merged from \code{el}.
  One typically assigns the return value from this function
  to the variable used as the input for \code{elements}.
  
}
\author{Duncan Temple Lang <duncan@wald.ucdavis.edu>}

\seealso{
  \code{link{htmlSelectElement}}
  \code{link{htmlTextAreaElement}}
  \code{link{htmlInputElement}}
}
\examples{
}
\keyword{interface}


