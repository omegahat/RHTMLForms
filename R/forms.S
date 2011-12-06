# Make this into a method since we can just call this with
# the arguments and an HTMLFormDescription object and pick the
# pieces out ourselves. Done!

# Since we are not calling this interactively but merely generate
# code that does call it, perhaps we should avoid the complexity.
#

formQuery =
function(args, url, formDescription, ..., .extraArgs = character(), .opts = list(), .checkArgs = TRUE, .addSubmit = TRUE, style = 'POST', curl = getCurlHandle())
{
    # library(RCurl)

    if(.checkArgs) 
      args = checkFormArgs(formDescription, args)


#    if(inherits(formDescription, "DynamicHTMLFormDescription")) 
#      formDescription = getActualFormDescription(args, formDescription)

    
    formElements <- formDescription$elements
    formAttributes <- formDescription$form
    
        # ensure that the hiddens are in the argument list
        # Assign them in each element function call, so no return value.
    sapply(formElements,
           function(x) {
             if(inherits(x, "HTMLHiddenElement"))
               args[[x$name]] <<- x$value
           })


        # Allow only one HTMLImageElement, and add it 
    imageEls = sapply(formElements, inherits, "HTMLImageElement")
    if(any(imageEls)) {
       # Take the last one.
      x = formElements[[max(which(imageEls))]]
      args[[paste(x$name, "x", sep=".")]] = "1"
      args[[paste(x$name, "y", sep=".")]] = "1"
      if(sum(imageEls) > 1)
        warning("Using only one HTMLImageElement: ", x$name)
    }


       # Include a submit field if we want. Some forms require it, e.g. goStat.
    if(.addSubmit) {
        submitEls = sapply(formElements, inherits, "HTMLSubmitElement")
        if(any(submitEls)) {
          x = formElements[[max(which(submitEls))]]      
          args[[x$name]] = x$value
        }
    }

    if(length(.extraArgs))
        args[names(.extraArgs)] = .extraArgs


       # Use GET or POST to submit
       # See 
       #  http://www.w3.org/TR/html401/interact/forms.html#h-17.13.4
       # for info on enctype and x-www-form-encoded
       # and also info on idempotent requests and using GET by default.
       if(tolower(formAttributes[["method"]]) == "get"
             || (!is.na(formAttributes["enctype"]) && formAttributes["enctype"] == "x-www-form-encoded"))
            getForm(url, .params = args, .opts = curlOptions(..., .opts = .opts), style = style, curl = curl)
       else
            postForm(url, .params = args, .opts = curlOptions(..., .opts = .opts), style = style, .contentEncodeFun = curlPercentEncode, curl = curl)
}



if(FALSE) {
   # We don't actually need these. They are more covenience routines and since
   # we generate the code, we can arrange to call it directly.
setOldClass("HTMLFormDescription")
setOldClass("HTMLFormAttributes")
setOldClass("HTMLFormElementsList")


setGeneric("formQuery", function(args, url, formElements, formAttributes, ..., .opts = list(), .checkArgs = TRUE)
                           standardGeneric("formQuery"))

setMethod("formQuery",
          signature("ANY", "missing", "HTMLFormDescription"),
          function(args, url, formElements, formAttributes, ..., .opts = list(), .checkArgs = TRUE) {
            formQuery(args, formElements$url, formElements$elements, formElements$form, ..., .opts = .opts, .checkArgs = .checkArgs)
          })

setMethod("formQuery",
          signature("ANY", "character", "HTMLFormDescription"),
          function(args, url, formElements, formAttributes, ..., .opts = list(), .checkArgs = TRUE) {
            formQuery(args, url, formElements$elements, formElements$form, ..., .opts = .opts, .checkArgs = .checkArgs)
          })


setMethod("formQuery",
          signature("ANY", "character", "HTMLFormElementsList", "HTMLFormAttributes"),

)
}




