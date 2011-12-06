\name{createArgList}
\alias{createArgList}
\title{Create text for argument list of new function from HTML form description}
\description{
  This function is where we take the form description and
  create the signature of the function that is being created
  to represent the form.
  This essential defines the formal arguments of the new function.
  It takes the user-visible form elements as the main
  arguments and provides default values for those that have them.
  The arguments for the function are arranged so that those
  with no default values are listed first and within this group,
  in the order they were located in the HTML document.
 
  The function then adds call-time arguments
  to specify the URI (\code{url}), arbitrary arguments (\dots) to pass on to the form submission code,
  the HTMLFormDescription (\code{.formDescription}),
  and an argument (\code{.opts}) to pass a list of options to control the form
  submission.
}
\usage{
createArgList(formDescription, url = character(),
                 defaultCurlOptions = getDefaultFormCurlOptions(formDescription, url),
                 reader = NULL, isPost = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formDescription}{the list of HTML form elements, i.e. an object of class
    \code{HTMLFormElementsList} which typically is the
    \code{elements} field of the value returned
    from \code{getHTMLFormDescription}.
  }
  \item{url}{the URL to use as the default URI to which the form is
    posted.}
  \item{defaultCurlOptions}{the default options for controlling the
    RCurl HTTP query.  These options are specified by the person
    creating the function as the default options for curl.  The caller
    can override these in a specific call to this function.
    This is a named list of values, with the names being those of the
    Curl options. The default value for this parameter is
    to return the referer URL which is the URL of the HTML form document.}
  \item{reader}{a function that is used either directly as the default
    function to process the result of the HTTP request,
    or a function generator that is called to create the HTTP request
    reader.
    This function can be specified to provide a default way to process
    the HTTP response from the form submission. It can, for example, be
    used to follow redirections or links to fetch the actual result and
    read the data into an R object rather than returning just the HTML
    or text of the Web server's response.}
  \item{isPost}{a logical value indicating whether the form being
    processed is a GET or POST. If it is a POST (i.e. \code{TRUE})
    then we add a style argument to the function that interfaces to this
    form.}
}
\value{
  A string that can be used as the definition of the signature
  for the function being created. It defines parameters
  corresponding to the different HTML form elements,
  along with their default values where possible and
  additional parameters such as \code{.reader},
  \code{.formDescription}, \code{.opts}, etc.
  
}
\references{}
\author{Duncan Temple Lang <duncan@wald.ucdavis.edu>}
\note{
  We could create an \code{alist} and return this as a language
  object. However, it is more complex to do this given that we
  don't know which elements have default values and which don't.
}


\seealso{
\code{\link{getHTMLFormDescription}}
 }
\examples{

}
\keyword{interface}


