# The function to create functions is now in generator.R and named
# writeFunction.
#

createArgList =
  #
  # The goal is to create the formal parameter list for
  # the function.
  # The steps are to
  #   a) identify the required arguments and those with default values
  #   b) create the text giving the formal parameters.
  #
  # This converts the names from HTML names to R names.
function(formDescription, url = character(), defaultCurlOptions = getDefaultFormCurlOptions(formDescription, url),
          reader = NULL, isPost = FALSE, cleanArgs = NULL)
{
    # Force these.
   defaultCurlOptions
   
      # Discard any submit, reset and image buttons.
   desc = getArgFormElements(formDescription$elements)

   names(desc) = escapeArgNames(names(desc))

     # Now identify the form elements that have a default and that are hidden
   isHidden = sapply(desc, function(x) inherits(x, "HTMLHiddenElement"))

   if(length(desc) == 0)
     isHidden = logical()


     # the default values for the parameters with parameters.
   defaultParams = lapply(desc, getDefaultValue)
   hasDefault = !sapply(defaultParams, function(x) inherits(x, "NoDefaultValue"))

     # The complement of these two conditions makes the element a required parameter.
   isRequired = !(isHidden | hasDefault)


   names(desc) = fixNames(names(desc))
   
    # names of the required parameters.
   requiredParams = names(desc)[isRequired]


    # If there are no required arguments, have to avoid the first , 
   if(length(requiredParams))
     required = paste(requiredParams, collapse=",\n\t ")
   else
     required = character(0)

   argsWithDefaults = character(0)
   if(any(hasDefault))
      argsWithDefaults = paste(names(hasDefault)[hasDefault], defaultParams[hasDefault], sep = " = ", collapse=",\n\t ")
   
   paste("\t",
         c(required,
           argsWithDefaults,
           paste(".url", if(length(url)) paste("= '", url, "'", collapse="", sep="") else ""),
           "...",
           paste(".reader = ",  paste(deparse(reader), collapse= "\n")),
           ".formDescription = NULL", 
           paste(".opts = ", paste(deparse(defaultCurlOptions), collapse="\n")),
           if(isPost) "style = 'POST'",
           ".curl = getCurlHandle()",
           paste(".cleanArgs = ",  paste(deparse(cleanArgs), collapse= "\n"))           
          ),
         collapse = ",\n")
}


fixNames =
  #
  # convert legal HTML names into legal R names.
  ##  map names of form elements to R names (e.g. knows-al to knowsAl or knows.al)
  ## - and _ get mapped to .
  ## We may want to do the same thing that is done in reading CSV files in R
  ## for column names.
function(x)
{
  if("allow_" %in% names(formals(make.names)))
    make.names(x, allow_ = TRUE)
  else
    gsub("[-_]", ".", x)
}


capitalize =
  # Capitalize the first letter of a word.
function(x, lowerRemainder = TRUE)
{
    remainder = substring(x, 2)
    paste(toupper(substring(x, 1, 1)),
          ifelse(lowerRemainder, tolower(remainder), remainder),
          sep="")
}


getDefaultValue = 
function(x)
{
  UseMethod("getDefaultValue")
}  


getDefaultValue.default =
function(x)
{
  if("defaultValue" %in% names(x) && !is.na(x$defaultValue))
     deparse(as.character(x$defaultValue))
  else {
     tmp = character(0)
     class(tmp) = "NoDefaultValue"
     tmp
  }
}


getDefaultValue.HTMLCheckboxElement =
function(x)
{
  if("defaultValue" %in% names(x) && !is.na(x$defaultValue))
     deparse(as.character(x$defaultValue))
  else
     deparse("")  # character(0))
}


getDefaultValue.HTMLTextAreaElement =
function(x)
{
  if("defaultValue" %in% names(x) && !is.na(x$defaultValue))
     deparse(as.character(x$defaultValue))
  else
     deparse("")       #character(0))
}

getDefaultValue.HTMLFileElement =
function(x)
{
  if("defaultValue" %in% names(x) && !is.na(x$defaultValue))
     deparse(as.character(x$defaultValue))
  else
     deparse("")       #character(0))
}



getDefaultValue.HTMLSelectElement =
function(x)
{
  if("defaultValue" %in% names(x) && !is.na(x$defaultValue))
     deparse(as.character(x$defaultValue))
  else
     deparse(as.character(names(x$options)[1]))
}




getDefaultFormCurlOptions =
function(formDescription, url)
{
  list(referer = formDescription$url)
}


