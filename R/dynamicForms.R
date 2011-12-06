#
# The functions here are for dealing with forms that are dynamic
# in the sense that changing a value on the form causes it to 
# dynamically determine different possible values for  the other
# form elements.
# www.wormbase.org/db/searches/advanced/dumper is an example
# where one can change the species from elegans to briggsae and
# the possible feature sets that one can have returned changes.
#
#

#
# The basic idea is that, as we get a description of the form, we find the onChange = submit() elements of
# a form. For each of these, we resubmit the form for each possible value of that element and get
# different form descriptions.  We can then merge these different form descriptions or keep them in a list
# and validate our inputs, determine the hidden fields, etc. based on the inputs for the dynamic elements.
#
#
#
#


#
# Now to validate these dynamic forms, we basically walk down the relevant path.
# We are given a list of named arguments and the forme description.
# The form is a dynamic form and has the name of the element,
#  its possible values and  a parallel list of form
# descriptions for each of these options.
# So we get the value for this element and we match it agains the possible
# values (i.e. against the values and/or names). If there is no match, we raise an error.
# Otherwise, we have found out which form description to use next.  Then, we strip off the
# value for this argument from the list of all arguments and validate 


getDynamicHTMLFormDescription =
  # omit allows us to restrict the elements of interest while still
  # having them in the description for completeness.

  #
  # The idea is that we find the first DynamicHTMLFormElement
  # and we go through the different options and compute the 
  # form description for the remaining elements.
  # We peel away the elements that we have already dealt with
  # and leave the description for the sub-form.
  # At the end of this recursive call, we end up with a hierarchy
  # of form descriptions.
  # We have the first dynamic form element and nodes for each
  # of its possible values.
  # Each node is another dynamic form description with nodes
  # for each of its options and descriptions for the remaining
  # form elements.
  #
  # It is possible that the entire structure of the form changes.
  # In that case, we don't want to reduce the elements in the sub-forms but rather
  # hold onto the entire new form for that value fo the element and validate entirely.
function(desc, omit = character(), drop = TRUE, ..., verbose = FALSE)
{
   # Discard the elements that we are omitting.
 if(length(omit)) {
   idx = match(omit, names(desc$elements), 0)
   k = class(desc$elements)
   desc$elements <- desc$elements[-idx]
   class(desc$elements) = k
 }

  # If no more elements left as a result of omitting them, just return the description
  # as there are definitely no more dynamic components left.
 if(length(desc$elements) == 0)
   return(desc)

   # Now find the dynamic components.
 dyn = sapply(desc$elements, inherits, "DynamicHTMLFormElement")
 if(!any(dyn)) 
    return(desc)

  # 
 pivot = desc$elements[[min(which(dyn))]]
 
  # We will need to submit the form for each value of this dynamic element, so
  # get the URI.  If the URI changes depending on the value, we are out of luck!!
 url = mergeURI(URI(desc$formAttributes["action"]), URI(desc$url)) 

  # Prepare the return value with the pivot information and we will build up
  # the branches by looping over the possible values.
 descriptions = list(elementName = pivot$name,
                     description = pivot,
                     values = list())   
  
 omit = c(omit, pivot$name)

 for(i in names(pivot$options)) {
       # Create the arguments for the submission. We may need to include them all.
     args = list(i)
     names(args)[1] = pivot$name

     if(verbose) 
       cat("Checking ", pivot$name, " - option", i, "\n")
     
      #XX we may need to provide all the arguments rather than just this one.
      # or perhaps cumulate them for the elements we have already deal with.
      # We have the defaults and the possible values from the original description.
     page = formQuery(args, toString(url), desc, .checkArgs = FALSE, ...)
      # Make certain that we turn the checkDynamic off here to avoid recursively.
     tmp = getHTMLFormDescription(page, asText = TRUE, handlers = multiFormElementHandlers(url, checkDynamic = FALSE))
     tmp = getDynamicHTMLFormDescription(tmp, omit = omit)

       # Now remove the elements that we are omitting. This leaves a subset of the form.
     if(drop) {
        idx = match(omit, names(tmp$elements), 0)
        
        if(any(is.na(idx))) {
          k = class(tmp$elements)
          tmp$elements = tmp$elements[is.na(idx)]
          class(tmp$elements) = k
        }

        class(tmp) <- c("HTMLFormSubset", class(tmp))        
     }

     descriptions$values[[i]] = tmp
 }

 class(descriptions) <- c("DynamicFormElementPath")

 descriptions
}

