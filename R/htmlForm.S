usesJS = usesJavaScript =
function(form)
{
   any(sapply(form$elements, inherits, "DynamicHTMLFormElement"))
}

getHTMLFormElement =
function(node, name = xmlName(node), dropButtons = TRUE)
{
   type = tolower(xmlGetAttr(node, "type", NA))
   if(is.na(type))
     type = "text"
   
   if(dropButtons && ( (name == "input" && type %in% c("submit", "button", "reset", "image")) || name == "button"))
      return(NULL)
     
   f = switch(tolower(name),
               "select" = htmlSelectElement,
               "input" = htmlInputElement,
               "text" = htmlTextAreaElement,
               "textarea" = htmlTextAreaElement)

   f(node)
}

htmlSelectElement =
  #
  # From an XML node.
  #
function(x)
{
    name = getElementName(x)

    optNodes = getNodeSet(x, ".//option")

         # The value of the option is either the "value" attribute or
         # the text within the option.
    vals = sapply(optNodes, function(x) {
                            val = xmlGetAttr(x, "value", xmlGetAttr(x, "VALUE", NA))
                            if(is.na(val))
                              val = xmlValue(x)

                            val
                        })

      #XXX really should only deal with <option> elements.
    txt = sapply(optNodes, xmlValue)
    names(txt) = vals
    names(vals) = vals

   
    el = list(name = name,  options = txt)
    class(el) = c("HTMLSelectElement", "HTMLFormElement")


       #get which are selected
    selection = sapply(optNodes, function(x) 
                                 !is.na(xmlGetAttr(x, "selected", xmlGetAttr(x, "SELECTED", NA)))      
                         )

     # Have to make certain this is a vector, and not a list.
    selection = unlist(selection)

    if(any(selection)) {
      if(sum(selection) > 1)
        warning("select item ", name, " has more than one value selected as a default")
      
      el$defaultValue = vals[selection]
    }


    # Now look for a onchange = submit() call to determine if we have to fetch
    # a different form
    onchange = xmlGetAttr(x, "onchange", xmlGetAttr(x, "onChange", NA))
    if(!is.na(onchange)) { # && length(grep("submit", onchange))) {
      el$onChange = TRUE
      class(el) <- c(class(el), "DynamicHTMLFormElement")
    }
    
    el
}


htmlInputElement =
function(x)
{
    name = getElementName(x)
    type = tolower(xmlGetAttr(x, "type",  xmlGetAttr(x, "TYPE", "text")))
    checked = xmlGetAttr(x, "checked",  xmlGetAttr(x, "CHECKED", NA)) # to get the defaults
    value = xmlGetAttr(x, "value", xmlGetAttr(x, "VALUE", NA))

    el = list(name = name)
    
    if(!is.null(name) && !is.na(name)) {
      if (type == "text") {
          return(htmlTextAreaElement(x))
      } else if (type == "radio" || type == "checkbox"){
         if(!is.na(checked)) 
           el$defaultValue = value

         el$options = value
         names(el$options) = xmlValue(x)
         if(names(el$options) == "" || length(names(el$options)) == 0)
            names(el$options) <- value
         class(el) = ifelse(type=="radio", "HTMLRadioElement", "HTMLCheckboxElement") 
      }
      else {
        # hidden
          # If there is already an element here, append to it rather than overwrite it.
          # e.g. .cgifields in dumper.html for www.wormbase.org

         if(type == "file")
           el$defaultValue = ""
         
         el$value = value
         class(el) = paste("HTML", capitalize(type), "Element", sep="")         
      }
    } else if (type == "button") {
        el$value = value
        el$onclick = xmlGetAttr(x, "onclick", NULL)
    }

    if(tolower(type) == "hidden"){
      class(el) = "HTMLHiddenElement"
    }

    class(el) = c(class(el), "HTMLFormElement")

    el
}


getElementName =
function(x)
{  
   xmlGetAttr(x, "name",  xmlGetAttr(x, "NAME", xmlGetAttr(x, "id")))
}

htmlTextAreaElement =
function(x)
{
    name = getElementName(x)

    el = list(name = name)
    class(el) = c("HTMLTextAreaElement", "HTMLFormElement")

    el$nodeAttributes = xmlAttrs(x)
    
    if (!is.null(name) && !is.na(name)){ #get the default
       val= xmlGetAttr(x, "value", xmlGetAttr(x, "VALUE", NA))

       if(is.na(val))
         val = xmlValue(x)

       if(length(val) == 0)
         val = ""
       
       el$defaultValue = val
    }

    el
}  


mergeFormElements =
    # Function that takes a new form element and puts it into the collection,
    # merging it with existing ones of that name if appropriate.
function(el, elements, name = el$name)
{
       if(is.null(name))
         return(el)

       if(name %in% names(elements))  {
          val = elements[[name]]

          if(inherits(el, "HTMLHiddenElement")) {
             val$value <- c(val$value, el$value)
            
          }  else if(inherits(el, c("HTMLRadioElement",  "HTMLCheckboxElement"))) {
               # Add default value if it is in this element.
             if("defaultValue" %in% names(el)) {
               if(inherits(el, "HTMLRadioElement") && length(val$defaultValue))
                  warning("Multiple radio buttons selected for element '", name, "'")
               else
                  val$defaultValue = c(val$defaultValue, el$defaultValue)
             }

            val$options <- append(val$options, el$options)
          } else if(inherits(el, "HTMLSubmitElement")) {
             val$value <- c(val$value, el$value)
          } else if(inherits(el, "HTMLImageElement")) {
             val$value <- c(val$value, el$value)
          } else
              val <- append(val, el)
        } else
           val <- el
       
        val
}

