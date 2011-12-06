\name{htmlTextAreaElement}
\alias{htmlTextAreaElement}
\alias{htmlSelectElement}
\alias{htmlInputElement}
\title{Constructors for HTML form element descriptions}
\description{
  These are functions that are used to create
  descriptions of an HTML form element (e.g. INPUT,
  TEXTAREA, SELECT) from an \code{XMLNode} object.
  These do not represent just a single node in the
  HTML document but rather the idea is that one accumulates information
  across different, related HTML nodes with the same
  name (e.g. radio buttons with
  \code{<INPUT type="radio" name="bob">}) in the same
  R object (of class \code{HTMLFormElement}).
  This allows us to conveniently understand an entire
  interface component in the HTML form from a single
  R object.

  These functions are typically used when parsing an HTML
  document using \code{\link{getHTMLFormDescription}}
  and the \code{\link{multiFormElementHandlers}}
  handlers.  They are not really designed for use
  directly as all the information is encoded in the
  node.
  We may make these more general
  in the future so that one can specify the different
  elements via separate arguments.
}
\usage{
htmlTextAreaElement(x)
htmlSelectElement(x)
htmlInputElement(x)
}
\arguments{
  \item{x}{an \code{XMLNode} object providing information about the HTML
    element.}
}

\value{
  An object of the basic class \code{HTMLFormElement}
  and some extended class.
  A \code{HTMLFormElement} object has the following
  basic elements:
  \item{name}{a character string given the name attribute of the
    element}
  \item{value}{the value attribute from the XML node. This is either
   an attribute in the INPUT element or the text it contains (e.g in the
   case of an OPTION element.}
 \item{defaultValue}{a character vector containing the value of the
   default value
   for this HTML form element, if available.
   For example, if a radio button is selected or a checkbox checked,
   its value is stored here as an element.}
  
}
\author{Duncan Temple Lang <duncan@wald.ucdavis.edu>}

\seealso{
\code{\link{getHTMLFormDescription}}
\code{\link{multiFormElementHandlers}}
}
\examples{

}
\keyword{interface}
\concept{HTML}

