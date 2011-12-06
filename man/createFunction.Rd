\name{createFunction}
\alias{createFunction}
\alias{writeFunction}
\title{Generate R function from HTML form description}
\description{
 These functions generate a new R function from an
 \code{HTMLFormDescription}
 by creating an argument list corresponding to the
 user-accessible elements in the HTML form
 and a body that validates the arguments
 against the possible values in the form description
 and submits the query using HTTP.
 The submission currently (and for the forseeable future)
 use \code{RCurl}.
}
\usage{
createFunction(formDescription, url = character(), verbose = FALSE,
                formElements = NULL, addSubmit = TRUE, reader = NULL,
                 processURLArgs = formDescription$formAttributes["method"] == "POST")
writeFunction(formDescription, funcName, reader = NULL,
               url = character(), con = paste("/tmp/", funcName, ".R", sep = ""),
               insertFormDescription = TRUE, verbose = FALSE,
                formElements = NULL, addSubmit = TRUE,
                 processURLArgs = formDescription$formAttributes["method"] == "POST")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formDescription}{the \code{HTMLFormDescription} object
    that provides all the information about the HTML form.
    This is typically obtained from calling \code{\link{getHTMLFormDescription}}.
  }
  \item{url}{if the \code{HTMLFormDescription} does not contain the
    target URI, the caller can specify this and it is used
    as the default value for the \code{url} argument
    in the newly created function.
    The caller of that function can still override this
    to have the form be submitted to an alternative site.
    }
  \item{verbose}{a logical value indicating whether to write information
   about the activities of this function as they occur. This is a tool
   for debugging/understanding.}
  \item{funcName}{if the function is to be written to a file (rather
    than created directly in R), this is the name to which the function is
    assigned in the file.
  }
  \item{formElements}{the list describing each of the HTML form elements
    within this form.  These are also in the \code{formDescription}
    object and we take them from their if this argument is \code{NULL}.
      This is available to allow us to specify different elements.}  
  \item{con}{a connection object or the name of a file to which to write
    the function text. If the connection is open before it is passed to
    this function, it will remain open; otherwise it is closed.
    This allows us to cumulate several functions into a single file
    or to include additional text about the functions. 
  }
  \item{insertFormDescription}{a logical value indicating whether
    we should add code to the function to
    include the \code{HTMLFormDescription} object.
    This is used currently when writing the function to a file.
    It is not needed when the function is created directly
    in R as we insert the actual \code{HTMLFormDescription}
    object directly into the formal arguments of the new function.
  }
  \item{reader}{a function that is used as the default function for
    processing the result of the HTTP request and which can convert this
    to a meaningful format.
    It can, for example, be
    used to follow redirections or links to fetch the actual result and
    read the data into an R object rather than returning just the HTML
    or text of the Web server's response
    Currently, this is used as the value of the
    \code{handlers} parameter in a call to \code{htmlTreeParse}
    on the body of the HTTP response.
    If this function
    is derived from the class \code{HTMLParseHandlerGenerator},
    we first invoke it and use the result as the value of the
    \code{handlers} parameter in the call.
    This function is  provided by the person creating the
    function from the HTML form.
    The caller of the resulting function can specify their
    own function (or \code{NULL}) to replace this default
    via the \code{.reader} argument.
  }
  \item{addSubmit}{a logical value indicating whether the value for the
    submit button should be added to the form query.  Some forms require
    this to work properly, others do not but can tolerate the extra
    "information",
    and some applications will fail if it is present.  It is typically
    harmless to include it but it is worth trying omitting it if
    the function fails with correct inputs.}
  \item{processURLArgs}{a logical value that controls whether we
    take any form arguments within the URL and move them from the
    URL to actual arguments added to the call to
    \link[RCurl]{getForm} or \link[RCurl]{postForm}.
  }
}
\details{
  This writes the text of the function to a connection.
  \code{createFunction} supplies its own text connection
  so that the text is never written to a file
  and then it parses and evaluates the resulting
  text to yield a function object.
  It resets the environment of the function so that
  it uses the usual global environment.
}
\value{
  \code{createFunction} returns a function object.
  \code{writeFunction} returns the name of the connection
  to which the function was written.
}
\author{Duncan Temple Lang <duncan@wald.ucdavis.edu>}


\seealso{
  \code{\link{getHTMLFormDescription}}
  \code{\link{formQuery}}
  \code{\link{checkFormArgs}}
}
\examples{
if(require("RCurl") && require("XML")) {
  txt = getURLContent("http://www.google.com")
  doc = htmlParse(txt, asText = TRUE)
  f = getHTMLFormDescription(doc)
  g = createFunction(f[[1]], 'http://www.google.com')
  g("R XML")
}

  # providing our own reader function
library(XML)
readGoogleResults =
function(txt)
{
  doc = htmlParse(txt, asText = TRUE, error = function(...)NULL)
  nodes = getNodeSet(doc, "//a[@class='l']")
  structure(sapply(nodes, xmlGetAttr, "href"),
            names = sapply(nodes, xmlValue))
}

g("foo", .reader = I(readGoogleResults))

  # or we can specify our handler when creating the function
  # so that callers don't have to specify it.
g = createFunction(f[[1]], reader = I(readGoogleResults))


f = getHTMLFormDescription("http://www.usbr.gov/gp/agrimet/station_bftm_bigflatturner.cfm",
                            error = function(...){})

 # The first function is a general search facility for the Web site, not
 # a form to get data we want.

print(f[[2]])

g = createFunction(f[[2]])

g(date = "01Jan23", start_time = "05:45", end_time = "15:45", parameters = "BV,OB,PC,SQ")

g(station_code = "BFTM = Big Flat, Turner, MT", date = "01Jan23",
     start_time = "05:45", end_time = "15:45", parameters = "BV,OB,PC,SQ")

funs = lapply(f, createFunction)

funs[[2]]("TRFM = Teton River", date = "01Jan23", start_time = "00:00", end_time = "23:45", parameters = "BV,OB,PC,SQ")

funs[[3]](water_year = "2001")

funs[[4]](year = "2001")

 # Problems with this one
\dontrun{
funs[[5]](year = "2004")
}
}
\keyword{interface}
\concept{meta-programming}
\concept{compilation}

