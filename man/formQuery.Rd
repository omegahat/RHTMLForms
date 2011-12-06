\name{formQuery}
\alias{formQuery}
\title{Runtime form input validation and submission}
\description{
  This function provides the runtime  support for
  validating the user-specified values for the different
  form elements relative to the form element descriptions
  to verify that the values are legitimate.
  This validation is performed by \code{check}
  Having done this
  This checks that text values are within the length (number of characters)
  constraints, that values for radio, checkbox and option menu elements
  correspond to elements within the enumerated set within the form.
  And final
  Having validated the values, it submits the HTTP request that
  constitutes the form, as it would be submitted by a browser.
}
\usage{
formQuery(args, url, formDescription, ...,
          .extraArgs = character(), .opts = list(),
           .checkArgs = TRUE, .addSubmit = TRUE, style = "POST",
             curl = getCurlHandle())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{args}{the list of user-specifiable arguments to submit with the form.}
  \item{url}{the URI to which the form will be submitted.}
  \item{formDescription}{the \code{HTMLFormDescription} object
    typically constructed via a call to
    \code{\link{getHTMLFormDescription}}. The methods handle
    processing the top-level description into the different pieces and
    calling the actual method that does something with it.}
  \item{...}{arguments that are passed on to the form submission
    function, \code{\link[RCurl]{getForm}}  or
    \code{\link[RCurl]{postForm}}}
  \item{.extraArgs}{a characer vector or list of additional arguments
    that are added to the HTTP request when submitting the form.}
  \item{.opts}{arguments for the form submission that are passed in a
    different way than the \dots mechanism. For example, they may have
    been created earlier and reused in different calls.}
  \item{.checkArgs}{a logical value indicating whether to validate the
    arguments. On occassion, it is convenient to bypass this step and
    assert that values are legitimate.}
  \item{.addSubmit}{a logical value indicating whether the value for the
    submit button should be added to the form query.  Some forms require
    this to work properly, others do not but can tolerate the extra
    "information",
    and some applications will fail if it is present.  It is typically
    harmless to include it but it is worth trying omitting it if
    the function fails with correct inputs.
  }
  \item{style}{a value that, if the form is a POST, is passed onto
    \code{\link[RCurl]{postForm}} to control which type of POST is used,
      i.e. 'multipart/form-data' or
      'application/x-www-form-urlencoded'.}
    \item{curl}{an object used to make the HTTP request.
      This can be specified by the caller so that she
      can reuse a connection across multiple calls.
      This facilitates working with cookies from a login and reused in
      subsequent requests.}
}
\details{

}
\value{
  The result of submitting the form.
  Typically this is simply a character string containing
  the text returned by the
  HTTP server in response to the form submission.
  However, the caller can control how the information is returned
  using the \dots and \code{.opts} arguments to control
  the form submission.
}
\references{}
\author{Duncan Temple Lang <duncan@wald.ucdavis.edu>}

\seealso{
   \code{\link{checkFormArgs}}
}
\examples{

}
\keyword{interface}
\concept{HTML form}
\concept{HTTP}

