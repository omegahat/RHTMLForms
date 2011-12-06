\name{validateValue}
\alias{validateValue}
\title{Generic function for validating user input for an HTML form element}
\description{
  This generic function and its methods allow us to
  check whether a particular value is consistent with
  its target or intended use.
  We use this to validate values for HTML form elements
  that will be submitted as part of the form
  and check whether it is consistent with the set
  of permissible values defined/implied by the corresponding
  form element.
  For radio and checkbox elements of the form,
  the value must correspond to one of
  the values or labels for the associated elements.
  For a text field, the length of the string must be
  within the limits specified by the form element,
  (if any are available for the element).
  For a select element (i.e. a pull-down menu),
  the value must match one of the visible labels (as seen on the menu)
  or one of the corresponding values.
}
\usage{
validateValue(desc, value, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{desc}{a description of the target. In our cases, these are all
  \code{HTMLFormElement} objects}
\item{value}{the value to be validated relative to the
  specifications provided by \code{desc}.}
 \item{\dots}{additional parameters for methods.}
}

\value{
  A logical value with
  \code{TRUE} indicating that the value is okay,
  or \code{FALSE} meaning that it is invalid.
}
\references{}
\author{Duncan Temple Lang <duncan@wald.ucdavis.edu>}

\seealso{
  \code{\link{formElementHandlers}}
  \code{\link[XML]{htmlTreeParse}}
}
\examples{
  # Taken from the URI
  # http://www.speakeasy.org/~cgires/perl_form.cgi

 # This will raise an error.
 textField = list(name = "some_text", nodeAttributes = c(maxlength = "50"))
 class(textField) = c("HTMLTextAreaElement", "HTMLFormElement")

 validateValue(textField,  "Just a test")

  # Should fail and will throw an error.
 try(validateValue(textField,  c("Just a test", "bob")))

 try(validateValue(textField,  paste(rep("A", 60), collapse="")))

 formArgs =c("some_text" = "Duncan",
             "choice" = "Ho",
             "radbut" = "eep",
             "box" = "box1, box2")

}
\keyword{IO}

