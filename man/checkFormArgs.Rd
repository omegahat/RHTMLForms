\name{checkFormArgs}
\alias{checkFormArgs}
\title{Validates form arguments relative to allowable values}
\description{
  This function process the user-specified arguments
  in a call to an HTML form and verifies that they are
  acceptable values based on the description of the form
  elements that were gathered when the form was processed.
  It checks values that correspond to a text field are
  within the length constraints.
  It ensures that values corresponding to a collection
  of radio buttons, or checkboxes or option menu (select tag)
  are indeed part of the permissible set of values.
  It also verifies that values are available for
  all required elements.
}
\usage{
checkFormArgs(desc, args, all = TRUE, ehandler = errorHandler())
}
\arguments{
  \item{desc}{an object of class \code{HTMLFormElementsList}
    that is a collection of the \code{HTMLFormElement} objects describing each
    of the form objects. This should not include the Reset or Submit elements.}
  \item{args}{the named list of arguments provided by the user in the
    call. The names should correspond to the names of the elements in
    \code{desc}.
    The values are compared to the accetpable values using
    the generic function \code{\link{validateValue}}.
    }
  \item{all}{a logical value indicating whether, if \code{TRUE}, to process all the
    values regardless if errors are encountered and to report
    all the errors at the end. Alternatively, if \code{FALSE}
    we terminate the validation with an error when the first
    error is encountered.
  }
  \item{ehandler}{a function that is called, if \code{all} is
    \code{TRUE},
      each time there is an invalid value  for a form element.
      This allows the caller to control how errors are handled.}
}
\details{
  This uses the generic function \code{\link{validateValue}}
  to compare the user-specified value against the acceptable
  values described the form element description using a method
  suitable for that element type, e.g. \code{HTMLSelectElement}
  or \code{HTMLTextAreaElement}.
}
\value{
  
}
\references{}
\author{Duncan Temple Lang <duncan@wald.ucdavis.edu>}


\seealso{
 \code{\link{validateValue}}
 \code{\link{formQuery}}
}
\examples{

\dontrun{
if(require(XML)) {
 u = system.file("testInputs", "dump.html", package = "odbAccess")
 
# formElements = htmlTreeParse(u, handlers = formElementHandlers(u, FALSE))$value()$elements
 form = getHTMLFormDescription(u, handlers = formElementHandlers(u, FALSE))

 args = 
  list(
   species="elegans",
   upload="",
   list="",
   flank3="0",
   flank5="0",
   feature="Gene Models",
   dump = "Plain TEXT",
   orientation = "Relative to feature",
   relative = "Chromosome",
   DNA ="flanking sequences only"
  )

 checkFormArgs(form, args)

   # This should fail.
 args$dump = "Foo"
 try(checkFormArgs(formElements, args))

  # Fail because the feature is from the wrong set.
 args$species = "briggsae"
 try(checkFormArgs(form, args))




 args$species = "briggsae"
 try(checkFormArgs(formElements, args))



 f = getHTMLFormDescription(system.file("testInputs", "SpeakEasy.html", package = "odbAccess"))

  # Fails because of box3.
 try(checkFormArgs(f, list("some_text" = "Duncan", box="box3", choice="He", radbut="oops")))

  # 2 values for radbut
 try(checkFormArgs(f$elements, list("some_text" = "Duncan", box="box1", choice="He", radbut=c("eep", "oop"))))

   # some_text String too long.
 try(checkFormArgs(f, list("some_text" = paste(rep("A", 60), collapse=""), box="box1", choice="He", radbut=c("eep", "oop"))))

  # Argument name d doesn't match.
 try(checkFormArgs(f, list("some_text" = "Duncan", box="box1", choice="He", d=c("eep", "oop"))))
}
}
}
\keyword{IO}

