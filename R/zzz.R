getHTMLFormDescription =
  # Second version!
  # This uses getNodeSet to fetch the nodes of interest rather than the handlers.
function(url, dropButtons = TRUE, ..., baseURL = docName(doc))
{
  if(is(url, "HTMLInternalDocument"))
     doc = url
  else {
     doc = htmlParse(url, ...)
  }
  
  formNodes = getNodeSet(doc, "//form")
  ans = lapply(formNodes, processForm, baseURL, dropButtons)
  names(ans) = sapply(formNodes, xmlGetAttr, "name", "")

  if(is.na(baseURL) && any(sapply(ans, function(x) length(grep(":", x$url)) == 0)) && "asText" %in% names(list(...)))
       warning("one or more forms have a missing URL.  Consider specifying a value for baseURL.")
  
  ans
}

processFormElements =
function(node, dropButtons = TRUE)
{
  nodes = getNodeSet(node, ".//input|.//select|.//button|.//textarea")

  ans = lapply(nodes, getHTMLFormElement, dropButtons = dropButtons)
    # Some of the resulting elements will be NULL, e.g. corresponding to image buttons.
    # We keep only the non-NULL ones.
  w = !sapply(ans, is.null)
  ans = ans[ w ]

  if(length(ans) == 0)
    return(NULL)
  
  ids = sapply(ans, function(x) { tmp = x[["name"]]
                                  if(is.null(tmp))
                                     tmp= x[["id"]]
                                  if(is.null(tmp))
                                     NA
                                  else
                                     tmp
                                })
  # ids = sapply(nodes, xmlGetAttr, "name")

  if(any(duplicated(ids)))
     ans = tapply(ans, ids, mergeElements)

  names(ans) = sapply(ans, `[[`, "name")
  class(ans) = "HTMLFormElementsList"
         
  ans
}

mergeElements = 
#
# Merge radio elements.
#
function(x)
{
  ans = x[[1]]
  if(length(x) == 1)
     return(ans)


  hasDefault = sapply(x, function(x) "defaultValue" %in% names(x))
  if(any(hasDefault))
     ans$defaultValue = x[hasDefault][[1]]$defaultValue
  ans$options = sapply(x, `[[`, "options")
  ans
}



processForm =
function(node, url, dropButtons = TRUE)
{
      tmp = xmlAttrs(node)
      if(!("method" %in% names(tmp)))
          tmp["method"] = "get"

      tmp["action"] = fixURL(tmp["action"], url)
      
      f = list(formAttributes = tmp,
               elements = processFormElements(node, dropButtons),
               url = tmp["action"]
              )

      if(!is.null(f$formAttributes))
         class(f$formAttributes) = "HTMLFormAttributes"

      class(f) <- "HTMLFormDescription"

#      if(checkDynamic) 
#        f = checkDynamicForm(f)

      f
}
