\name{formElementHandlers}
\alias{formElementHandlers}
\alias{multiFormElementHandlers}
\title{Gather information from HTML form elements}
\description{
  These functions are used when  parsing HTML pages
  containing forms to gather a description
  of the individual forms. 
  The idea is to read the individual elements within an HTML form
  and provide their details in terms of
 \itemize{
  \item{1}{the element name}
  \item{2}{the default value}
  \item{3}{the set of possible values}
  \item{4}{whether it is visible, i.e. settable by the user or simply a
    hidden field}
  }
  With this information, we can automate the access to the HTML form
  via an S function that provides the same user specification
  of options but without the manual operation and without the
  management of the returned data, e.g. saving it as a file and
  bringing it into S.

  We ignore JavaScript-related operations.

  \code{multiFormElementHandlers} can handle multiple
  forms within a single page.
  \code{formElementHandlers} accumulates all the form elements
  into a single structure and does not observe multiple
  form boundaries. If you have an HTML form with potentially
  more than one form, use \code{multiFormElementHandlers}.
  This is hidden from most users via the function
  \code{\link{getHTMLFormDescription}}.
}
\usage{
formElementHandlers(url = NULL, checkDynamic = TRUE, dropButtons = TRUE)
multiFormElementHandlers(url = NULL, checkDynamic = TRUE, dropButtons = TRUE)
}
\arguments{
  \item{url}{the URL of the HTML page. This is not necessary for
   creating the description of the form elements as this is done
   via a call to \code{\link[XML]{htmlTreeParse}},
   but it is used to provide a fully self-describing
   description of the form.}
 \item{checkDynamic}{a logical value indicating whether to test whether
   the form has dynamic elements.  If this is \code{TRUE},
   when the description of the form is complete,
   we call \code{checkDynamicForm} which processes dynamic elements
   (i.e. those with an onChange attribute)
   by submitting the form with
   different values for that element in order to construct
   the collection of different
   possible values for all elements that are accepted/possible for the
   different possible inputs for the dynamic element(s).
 }
 \item{dropButtons}{a logical value indicating whether to omit 
  button elements in the form description.  These are typically
  Submit or Reset buttons that are not relevant for submitting the form request from R.}
}
\details{
  This uses the \code{\link[XML]{htmlTreeParse}}
  function in the XML parsing package to
  gather up and process the different HTML form elements
  in the HTML document.  It organizes the information into
  a more programmatically accessible structure.
}
\value{
  An object of class \code{HTMLFormDescription}.
  \item{inputs}{a list describing the different select elements.
    Eac element corresponds to a separate select element
    and is a named character vector.
    The values in the character vector are the text for the
    option elements and the names are the corresponding
    value attribute which is submitted if that option is selected.
    }
  \item{textareas}{the names of the TEXT or TEXTAREA elements.}
  \item{fixed}{}
  \item{form}{the attributes (a named character vector) giving the
    HTML attributes associated with the FORM element.
    These describe the action, the URI for submission, the encoding
    format, etc.} 
  \item{url}{this is supplied when the handlers are created
    and allows the complete information about the
    form(s) to be entirely self-describing, i.e. to resolve relative
    links, etc. for the POST actions.}
  \item{hidden}{a list containing character vectors of length 1 or more.
    Each element in the list corresponds to an HTML element of type
      "hidden" with a name. Such elements can have multiple values for
      the same name, i.e. the name="x" can be repeated and all these
      values must be sent as part of the form.
  }
  \item{inputdefaults}{}
  \item{textareadefaults}{}  
  \item{selectdefaults}{}
  ...
}
\references{}
\author{Duncan Temple Lang <duncan@wald.ucdavis.edu>}
\note{

  Currently, we organize the
  information from a form into a simple HTMLFormDescription
  object which is an S3-style class.
  This maintains the information about the form in separate
  fields and one must look across these fields to understand
  an individual element. For example, one would get its
  
}

\seealso{

}
\examples{

}
\keyword{IO}

