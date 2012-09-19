formElementHandlers = 
function(url = NULL, checkDynamic = TRUE, dropButtons = TRUE) 
{
  #formElementHandlers is a function to deparse the html/xml form, used as input to htmlTreeParse

  # We gather information about the different form elements in these variables
  # These are updated across calls to the different HTML element actions/handlers
  # and are shared by these handler functions but local to this call to formHandlers.

  # input, textarea, select,  option, button, file, reset, submit.
  #  THe following are not important from our perspective as they are
  #  GUI layout controls:
  #      fieldset, legend, label, optgroup,
  #
  # Can't deal with JavaScript without an interpreter and don't want to try to translate.

  # To deal with multiple forms.
  # Simplest thing is to do a 2 pass solution which first gets the <FORM>
  # elements and then pass each of these through the parser a second time.
  # Otherwise, we combine all the different elements across forms into 
  # the "non-local" variables.  Can do in other ways too.


  # <input type="password">


  #variables to store tags and tag attributes 


  formAttrs = NULL   # the collection of attributes on the <FORM> tag, telling us how
                     # how to post the query, etc.


   # Names of the <INPUT type="hidden"> elements. These won't appear in the 
   # formal arguments of the R function, but need to go into the query of course.
  hidden = list()


    # The collection of form element objects describing the different components.
    # this is where all the information is now stored with full class information
    # to help in providing generic functions for creating the functions and processing
    # the requests.
  elements = list()


  addElement =
    function(el) {
         # have to handle the degenerate case where there is no name
         # which arises when we have, e.g. a <input type=submit>
      if(is.null(el$name))
        return(el)
      
      val = mergeFormElements(el, elements)
      elements[[el$name]] <<- val
      el
    }


    #TAG: textarea
    # also called for <input type="text"...>
  textarea = function(x) {
    addElement(htmlTextAreaElement(x))    
    x
  }
    
  #TAG: select
  # Need to handle multiple = true option.

  select =
    function(x) {
       if(!(dropButtons && xmlGetAttr(x, "type") == "button"))
          addElement(htmlSelectElement(x))
       x
    }

  #TAG: input
  # This handles the generic <input>  form element.
  input = function(x) {
     addElement(htmlInputElement(x))
     x
  }

    #TAG: form
    # store the attributes in the formAttrs field that will be returned to describe the
    # appropriate method for submitting the query/"form".
  form = function(x) {
     formAttrs <<- xmlAttrs(x)
     class(formAttrs) <<- "HTMLFormAttributes"
    
     x
  }

      #To return the collection of form elements after the parsing is done and they
      # have been collected.
  values = function() {

    class(elements) <- "HTMLFormElementsList"
    
    tmp =  list(elements = elements,
                form = formAttrs, 
                url = url,
                hidden = hidden
               )

    class(tmp) <- "HTMLFormDescription"  # S3 class for the moment.

     if(checkDynamic)
       tmp = checkDynamicForm(tmp)
    
    tmp
  }

    # Return the list of function handlers for the parser.
  ans = list(form = form, select = select, input = input, textarea = textarea, values=values)

  class(ans) <- c("HTMLFormParser", "HTMLParserHandler")

  ans
}

method = 
function(desc)
 UseMethod("desc")

method.HTMLFormDescription =
function(desc)
{
  desc$formAttributes["method"]
}


"method<-.HTMLFormDescription" =
function(desc, value)
{
  value = toupper(value)
  if(!( value %in% c("GET", "POST")))
    warning("unrecognized method for HTML form submission: ", value)

  desc$formAttributes["method"] <- value
  desc
}



getHTMLFormDescriptionViaHandlers =
  #
  #
function(url, ..., multi = TRUE, handlers)
{
  if(missing(handlers)) {
    if(multi)
      handlers = multiFormElementHandlers(url)
    else  
      handlers = formElementHandlers(url)
  }

  h = htmlTreeParse(url, handlers = handlers, ...)

  if(inherits(handlers, "HTMLFormParser") && "values" %in% names(handlers) && is.function(handlers$values)) {
    ans = handlers$values()
    if(missing(multi) && inherits(handlers, "HTMLMultiFormParser")
         && length(ans) == 1 && inherits(ans[[1]], "HTMLFormDescription"))
      return(ans[[1]])

    ans
  } else
    h
}


fixURL =
function(url, relative)
{
  if(is.na(relative))
     return(url)

  if(is.na(url))
     return(relative)
  
  uri = parseURI(url)
  rel = parseURI(relative)
  ans = if(uri$scheme == "")  {
                # perhaps we should copy just update uri
                # with rel and return uri.
                # In other words, we may need to merge more than query and path.
            if(substring(uri$path, 1, 1) == "/")
               rel$path = uri$path
            else
               rel$path = paste(dirname(rel$path), uri$path, sep = "/")
            rel$query = uri$query  # need to add any query part
            rel
         } else
            uri

  as(ans, "character")
}

multiFormElementHandlers =
  #
  #
function(url = NULL, checkDynamic = TRUE, dropButtons = TRUE)
{
  forms <- NULL

  form =
    function(node) {

      elements = findFormElements(node, dropButtons = dropButtons)

      tmp = xmlAttrs(node)
      if(!("method" %in% names(tmp)))
          tmp["method"] = "get"

      tmp["action"] = fixURL(tmp["action"], url)
      
      f = list(formAttributes = tmp,
               elements = elements,
               url = tmp["action"]
              )

      if(!is.null(f$formAttributes))
         class(f$formAttributes) = "HTMLFormAttributes"

      class(f) <- "HTMLFormDescription"

#      if(checkDynamic) 
#        f = checkDynamicForm(f)

        # put it in the collection of forms.
      forms[[ length(forms) + 1 ]] <<- f

      names(forms)[length(forms)] <<- ifelse(!is.null(f$formAttributes) &&
                                               "name" %in% names(f$formAttributes), f$formAttributes["name"], "")

      f
   }

   ans = list(values = function(dynamic = checkDynamic) {
                         names(forms) = sapply(forms, function(x) x$formAttributes["action"])
                         if(dynamic)
                           lapply(forms, checkDynamicForm)
                         else
                           forms
                       },
              select = htmlSelectElement,
              input = htmlInputElement,
              textarea = htmlTextAreaElement,
              form = form
             )

  class(ans) <- c("HTMLMultiFormParser", "HTMLFormParser", "HTMLParserHandler")
  ans
}  


checkDynamicForm =
function(form)
{
  tmp = getDynamicHTMLFormDescription(form)
  if(inherits(tmp, "DynamicFormElementPath")) {
    class(form) <- c("DynamicHTMLFormDescription", class(form))
    form$dynamicElements = tmp
  }

  form
}  


findFormElements =
  #
  # This is a recursive function that examines its children
  # and determines which, if any, are HTMLFormElement objects.
  # If any of the children are XMLNode objects, it recursivel descends
  # those.

  # There is an older version that did this without persistent variables
  # and closures but it was more obscure. Check the CVS versions.
function(node, level = 0, dropButtons = TRUE)
{
 formElements <- list()

  f = function(node, level = 0) {
 
   if(inherits(node, "HTMLFormElement")) {
      if(!is.null(node$name)) {
         formElements[[node$name]] <<- mergeFormElements(node, formElements)
         return(TRUE)
      } else
         return(FALSE)

   } else if(inherits(node, "XMLNode") && xmlSize(node)) {
      return(xmlSApply(node, f, level = level + 1))
   }

   # warning("dealing with ", class(node)[1], " in findFormElements")

   FALSE
  }


   # now run the function.
  f(node)

  class(formElements) = "HTMLFormElementsList"   

  formElements
}

