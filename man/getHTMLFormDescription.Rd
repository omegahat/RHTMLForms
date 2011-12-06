\name{getHTMLFormDescription}
\alias{getHTMLFormDescription}
\alias{getHTMLFormDescriptionViaHandlers}
\title{Construct descriptions of forms in an HTML document}
\description{
  This function is used to read and process an HTML document
  (either from the file system or via HTTP/FTP)
  and extract descriptions of the <FORM> elements
  it contains. Each form is collected into
  an object of class \code{HTMLFormDescription}
  that describes the method for submitting
  the form (POST or GET), the URL for the submission,
  and the elements within the form.
  These descriptions can be used
  to mimic the form via functions (or GUIs) within R and validate values
  being submitted to forms.
}
\usage{
getHTMLFormDescription(url, dropButtons = TRUE, ..., baseURL = docName(doc))
getHTMLFormDescriptionViaHandlers(url, ..., multi = TRUE, handlers)
}
\arguments{
  \item{url}{the URI for the HTML document. If this is a local file,
    there is no need for }
  \item{...}{arguments that are passed directly to \code{\link[XML]{htmlTreeParse}}}
  \item{dropButtons}{a logical value indicating whether to omit 
      button elements in the form description.  These are typically
      Submit or Reset buttons that are not relevant for submitting the form request from R.}
  \item{baseURL}{the URL of the HTML form. This is used to compute relative URLs for the code associated with the
    form.}
  \item{multi}{a logical value indicating whether to use the handlers
    that deal with multiple forms within the document
    or (\code{FALSE})  expect just a single FORM element.
    }
    \item{handlers}{a collection of functions that are passed
      to \code{\link[XML]{htmlTreeParse}} as handlers for processing
      the different HTML elements within the document.
      The default is to provide an object of class
      \code{HTMLFormParser} which  is expected to have a
      \code{values} function that is called after the processing
      is done for the entire document to retrieve the
      object describing the HTML form(s).
      If this is not present, the \code{handlers} object
      is returned and the caller is expected to be able
      to extract the relevant information.
      This allows the caller to provide their own handlers
      that offer different processing facilities.
      If \code{multi} is \code{TRUE} (the default)
      and \code{handlers} is not specified,
      we call \code{multiFormElementHandlers}
      to get the \code{handlers} object so as to be able
      to deal with multiple forms within the URI.
      If \code{multi} is \code{FALSE}, we call
      \code{formElementHandler} which expects just a single
      form and is marginally more efficient as a result.
      Either of these handler generator functions can be used
      directly to create the handlers and called with
      different arguments to control the
      target URI  and/or the check for dynamic
      forms, or simply to reuse the same instance across
      multiple form description queries.
    }
}
\value{
  If \code{handlers} is provided by the caller
  and is not an object of class \code{HTMLFormParser}
  with a \code{values} function element, then the
  \code{handlers} object is returned.
  The caller is supposed to know how to extract the information.

  Otherwise, if \code{handlers} is a \code{HTMLMultiFormParser}
  object, a list of
  \code{HTMLFormDescriptions} is returned.
  The names of the elements in the list are taken from
  the names of the individual forms, if available.
  If \code{multi} is not specified and there is only a single
  form in the document, just that description object is returned.
  This simplifies accessing the elements.
  If this is not desired, specify \code{multi = TRUE}
  explicitly to have a list returned.

  If \code{multi} is given the value \code{FALSE},
  the \code{HTMLFormParser} is used and a single
  object of class \code{HTMLFormDescription} is returned.

  In either case, if  the handlers are instructed to check
  for a dynamic form (\code{checkDynamic}), if
  any of the elements have a \code{onChange}
  attribute, the form is submitted with
  the different options for that element and
  a description of the possible values for all the elements
  that corresponding to these different settings is included
  in the result in the \code{dynamicElements} field.
  This allows us to handle simple dynamic forms whose possible
  element values (but not entire structure) change when
  one element's value is selected. See the species field
  in the wormbase form for a simple example.
}
\author{Duncan Temple Lang <duncan@wald.ucdavis.edu>}


\seealso{
  \code{\link{multiFormElementHandlers}}
  \code{\link{formElementHandlers}}
  \code{\link[XML]{htmlTreeParse}}
}
\examples{
if(require(RCurl) && require(XML)) {
   txt = getURLContent("http://www.google.com")
   doc = htmlParse(txt, asText = TRUE)
   f = getHTMLFormDescription(doc)
}

eq = getHTMLFormDescription("http://neic.usgs.gov/neis/epic/epic_global.html")
}
\keyword{interface}
\concept{XML}
\concept{parsing}


