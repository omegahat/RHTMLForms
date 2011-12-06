errorHandler =
function()
{
    errors = character()
    function(err) {
            if(all)
              errors <<- append(errors, err)
            else
              stop(err)
    }
}

checkFormArgs =
function(desc, args, all = TRUE, ehandler = errorHandler())
{  
  UseMethod("checkFormArgs")
}


checkFormArgs.DynamicHTMLFormDescription =
  #
  # Works for dynamic html forms by checking
  # the value of the argument corresponding to the
  # element used for the pivot of the form description
  # and then recursively validating the other
  # values by descending the path of descriptions
  # corresponding to the particular values for given
  # arguments.
  
function(desc, args, all = TRUE, ehandler = errorHandler())
{
 dyn = desc$dynamicElements
 val = args[[dyn$elementName]]
 validateValue(dyn$description, val)

 idx <- pmatch(val, dyn$description$options)
 if(is.na(idx))
   idx <- pmatch(val, names(dyn$description$options))

  # Drop the value of this argument from args and continue checking.
 which = match(dyn$elementName, names(args))
 args = args[-which]

 args = checkFormArgs(dyn$values[[idx]], args, all = all, ehandler = ehandler)

 errors = environment(ehandler)$errors
 if(length(errors)) {
   problemArgs = names(desc)[is.na(match(names(desc), names(args)))]
   msg = paste(length(errors), " error(s) validating the form arguments ", paste(problemArgs, collapse=", "))
   stop(paste(c(msg, errors), collapse="\n\t"))   
 }

 args[[dyn$elementName]] <- val
 
 args
}

checkFormArgs.HTMLFormDescription <-
#
# This is part of the run-time support
# and needs to be available when the function we create
# is being run, not when it is created.
#
# desc is the HTMLFormDescription.
#
#
function(desc, args, all = TRUE,
          ehandler = function(err) {
            if(all)
              errors <<- append(errors, err)
            else
              stop(err)
          }
         )
{
 desc =  desc$elements

 # Two choices to handle errors:
 #  i) throw error immediately
 #  ii) cumulate errors and report all at the same time

 if(!inherits(desc, "HTMLFormElementsList"))
   stop("checkFormArgs requires an object of class HTMLFormElementList as the first argument.")
  
 errors = character()

  # Check that all the arguments correspond to form elements.
  # Currently, we don't check that all form elements have an argument so that this
  # allows us to provide values for a subset of the elements.
 if(!all(names(args) %in% names(desc))) {
   stop("Some arguments don't correspond to form elements: ", paste(names(args)[is.na(match(names(args), names(desc)))], collapse = ", "))
 }

 isHidden = sapply(desc, inherits, "HTMLHiddenElement")
 desc = desc[!isHidden]
 
 
 for(i in names(desc)) {
       # skip hidden elements.
   if(inherits(desc[[i]], "HTMLHiddenElement"))
     next

   obj = args[[i]]

   if(is.null(obj) && !(i %in% names(args)))
     next
   
   if(all)
      ok = tryCatch(validateValue(desc[[i]], obj), error = ehandler)
    else
      ok = validateValue(desc[[i]], obj)


   if(inherits(ok, "try-error"))
     ehandler(ok$message)
#   else if(!ok) 
#     ehandler(paste("argument", i, "failed"))
   else
     args[[i]] = ok
 }

 if(length(errors)) {
   problemArgs = names(desc)[is.na(match(names(desc), names(args)))]
   msg = paste(length(errors), " error(s) validating the form arguments ", paste(problemArgs, collapse=", "))
   stop(paste(c(msg, errors), collapse="\n\t"))
 }

 args
}

validateValue =
function(desc, value, ...)
{
  UseMethod("validateValue")
}


validateValue.HTMLRadioElement =
function(desc, value, ...)
{
    # for the moment, just call HTMLSelectElement's method
    # rather than introduce an intermediate class which
    # both of these extend.

  if(length(value) == 0 || (length(value) == 1 && value == ""))
    return(value)
  
  if(length(value) != 1)
    stop("Only one value can be selected for ", desc$name, " out of ",
           paste(unique(c(desc$options, names(desc$options))), collapse=", "),
           ". You have ", length(value), ".")
  validateValue.HTMLSelectElement(desc, value)
}  


validateValue.HTMLCheckboxElement =
function(desc, value, ...)
{
  if(length(value) == 0 || (length(value) == 1 && value == ""))
    return(value)  

    # for the moment, just call HTMLSelectElement's method
    # rather than introduce an intermediate class which
    # both of these extend.
  validateValue.HTMLSelectElement(desc, value, multiOk = TRUE)
}


par.match =
function(x, table, nomatch = NA_integer_, duplicates.ok = FALSE)
{
   i = (x == table)
   if(any(i))
     ok =  which(i)
   else
     ok = pmatch(x, table)
}

validateValue.HTMLSelectElement =
function(desc, value, options = desc$options, multiOk = FALSE, ...)
{
  if(length(value) != 1 && !multiOk)
    stop("Can supply only one value for `", desc$name, "': ", length(value), " supplied!")

  if(all(is.na(names(desc$options))))
     names(desc$options) = desc$options

  ok <- rep(NA, length(value))

  if(length(names(options))) {
     # pmatch doesn't handle pmatch("", c("", "a", "b"))
     ok = par.match(value, names(options))
  }

  if(any(is.na(ok))) {
     ok[is.na(ok)] = par.match(value[is.na(ok)], options)
   }

  if(any(is.na(ok)))
    stop(desc$name, " must take  a value in the set: ",
               paste("'", unique(c(options, names(options))),"'", sep="", collapse = ", "),
         ".  Not '", value, "'")

    
  names(options)[ok]
}

validateValue.HTMLTextAreaElement =
function(desc, value, ...)
{

  if(length(value) > 1) {
    value = paste(value, collapse = "\n")
    warning("Only single strings are valid values for HTML textarea/textentry elements. Pasting them together as lines of a single string")
  }
  
    # Need to check the length of the string 
  if("maxlength" %in% names(desc$nodeAttributes) &&
          nchar(value) > as.integer(desc$nodeAttributes[["maxlength"]]))
    stop("String is too long ", nchar(value),
               " for this HTML textarea. Limit for this textarea is ", as.integer(desc$nodeAttributes[["maxlength"]]))
    
  value
}

validateValue.default =
function(desc, value, ...)
{
  value
}  

