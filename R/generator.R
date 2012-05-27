writeFunction =
#  acon can also be given as a connection now, e.g.
#   con = file("manyFunctions.R", "w")
#   odbAccess("....", "foo", con)
#   odbAccess("....", "bar", con)
#   close(con)
#
#  or even write to a textConnection() to avoid disk:
#    con = textConnection("SillyName", "w")
#    odbAccess("http://www.wormbase.org/db/searches/advanced/dumper", "foo", con)
#    source(textConnection(paste(SillyName, collapse="\n")))
#    close(con)
#
#
# To easily customize the creation of the function we may use classes
# or use hooks/functions that get called at certain points just like
# handler functions for tags in the HTML parsing. These hook events would be 
# when we write the function definition,  close the function definition, 
# write the expected argument values, check the arguments, send the query, ... 
#
#

  
function(formDescription, funcName,
          reader = NULL,
          url = character(), con = paste("/tmp/", funcName, ".R", sep=""),
           insertFormDescription = TRUE, verbose = FALSE,
           formElements = NULL, addSubmit = TRUE,
            processURLArgs = formDescription$formAttributes["method"] == "POST")
{

    # The caller should have already built the form description, either 
    # by parsing the HTML document themself earlier 
    # or even manually creating the description.
    # 
    # Could use methods to do dispatching for the different types of arguments.
    # We will do this when we move to S4 classes after things are working.
   if(!inherits(formDescription, "HTMLFormDescription")) 
      stop("You should provide a form description here. See getFormDescription().")


   isPost = if(!is.na(formDescription$formAttributes["method"]))
               tolower(formDescription$formAttributes["method"]) == "post"
            else
               FALSE

   if(length(formElements) && !inherits(formElements, "list")) {
     els = formDescription$elements = formDescription$elements[formElements]
     class(els) = class(formDescription$elements) = "HTMLFormElementsList"
   } else {

        # What about image buttons, etc.
        # Shouldn't we just filter all of the default ones for getArgFormElements?
     els = getArgFormElements(formDescription$elements, "HTMLResetElement")
   }



    # Should assign a value to url, not to formDescription$url.
   if(length(url)) {
     formDescription$url = url
   } else if("action" %in% names(formDescription$formAttributes)) {
       action = formDescription$formAttributes["action"]
       if(action != "") 
          formDescription$url = toString.URI(mergeURI(URI(action), URI(formDescription$url)))
   }

   extraArgs = list()
   exArgs = character()
   if(processURLArgs && grepl("\\?", formDescription$url)) {
      tmp = strsplit(formDescription$url, "\\?")[[1]]
      formDescription$url = tmp[1]
      tmp = strsplit(tmp[2], "[&=]")[[1]]
      i = seq(1, by = 2, length = length(tmp)/2)
      extraArgs = structure(as.list(tmp[i + 1]), names = tmp[i])

      exArgs = paste(sprintf("'%s' = '%s'", tmp[i], tmp[i+1]), collapse = ", ")
   }
     
   arglist = createArgList(formDescription, formDescription$url, reader = reader, isPost = isPost)
     # Really want to synchronize these.
   argNames = names(getArgFormElements(els))
#   argNames = escapeArgNames(argNames)


     # allow the caller to provide a connection onto which we will write the
     # function. This allows her to cumulate functions in a single "stream"/connection.
   if(!inherits(con, "connection")) {
      con = file(con, open = "w")
      on.exit(close(con))
   }

     # Make certain the connection is open and arrange to close it if we open it.
   if(!isOpen(con)) {
      open(con, "w")
      on.exit(close(con))
   }
  
       #Write out the function definition, i.e. assignment and argument list.
   cat(funcName, ifelse(length(funcName), "=", ""), "\n function(\n", paste(arglist, collapse = ",\n\t"), ")\n{\n", file = con)

   if(insertFormDescription) {
           # Write out the form element description list.
       cat("\n\tif(is.null(.formDescription))\n\t\t .formDescription =\n\t\t\t\t",
           paste(deparse(formDescription), collapse = "\n\t\t\t\t"),
           "\n\n",
           file = con)
   }

   cat("\targs = list(\n", paste(paste("\t\t", "'", argNames, "'", sep=""),
                                 #fixNames(argNames),
                                 escapeArgNames(argNames),
                                 sep=" = ", collapse=",\n"),
                     ")\n\n",
        file = con)
   

       # Force url to be evaluated.
   cat("\n\n\n", "if(!length(.url))  stop('no url supplied')\n\n", file = con)
   
       # pass these to form and submit the query
   cat("\tans = formQuery(args, .url, .formDescription, ..., .opts = .opts",
                    ", .addSubmit = ", ifelse(addSubmit, "TRUE", "FALSE"), ", curl = .curl",
                    paste(", .extraArgs = c(", exArgs, ")"),
                    if(isPost) ", style = style",
                    ")\n", sep="", file = con)


     # Handle the case where we have a .reader function.
   cat("\tif(!is.null(.reader) && is.function(.reader)) {\n\t\tif(inherits(.reader, 'AsIs'))\n\t\tans = .reader(ans)\n\telse {\n\tif(inherits(.reader, \"HTMLParseHandlerGenerator\"))\n\t\t\t.reader = .reader()\n\t\tans = htmlTreeParse(ans, asText = TRUE, handlers = .reader)\n\t\tif(inherits(ans, \"HTMLParseHandler\"))\n\t\t\tans = ans$value()\n}}\n\n", file = con)
   
   cat("\tans\n}\n\n\n", file = con)

   if(length(funcName) && funcName != "")
     cat("class(", funcName, ") <- HTMLFormGeneratedFunction\n\n\n")

      # DONE
   if(verbose)
     cat("Function ", funcName, "is written to file/connection", summary(con)$description, "\n")

  summary(con)$description
}

dQuote =
function(x)
{
  sprintf('"%s"', x)
}


escapeArgNames =
function(els)
{
  i = grep("[-$@!#%]", els)
  if(length(i))
     els[i] = sprintf("`%s`", els[i])
  els
}

getArgFormElements =
      # Discard any submit, reset and image buttons.  
function(desc, targetClasses = c("HTMLSubmitElement", "HTMLResetElement", "HTMLImageElement", "HTMLHiddenElement"))
{
  if(inherits(desc, "HTMLFormDescription"))
    desc = desc$elements
  
  desc[!sapply(desc, function(x) inherits(x, targetClasses))]
}  





createFunction =
  #
  # We reuse the function above rather than constructing the function object
  # directly here.  To do so would involve a song and dance with alist
  # to get arguments with no defaults in the formals. Since we have 
  # to compute these, this is tricky and so would make things obscure.
  # We can do it later, if we want.
  # 
  # The approach here is to create a text connection and have writeFunction() above
  # create the text of the function on that.
  #
function(formDescription, url = character(), verbose = FALSE,
          formElements = NULL, addSubmit = TRUE, reader = NULL,
           processURLArgs = formDescription$formAttributes["method"] == "POST")
{
  con = textConnection(".sillyName", "w", local = TRUE)
  writeFunction(formDescription, character(), url, con, verbose = verbose,
                 insertFormDescription = FALSE, formElements = formElements, reader = reader)

  on.exit(close(con))
   # Now read that object back into R as a function
  f = eval(parse(text = textConnectionValue(con)))

  if(processURLArgs) {  
     formDescription$url = gsub("\\?.*", "", formDescription$url)
     if("action" %in% names(formDescription$formAttributes))
        formDescription$formAttributes["action"] = gsub("\\?.*", "", formDescription$formAttributes["action"])
  }
  
    # Now set the default value of formDescription to the one we have here.
  formals(f)[[".formDescription"]] = formDescription

    # and patch it up since it is created here.
  environment(f) <- globalenv()    
  
  f  
}  
