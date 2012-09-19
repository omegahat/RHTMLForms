print.HTMLFormDescription =
  function(x, showHidden = FALSE, ...)
{
  cat("HTML Form:", x$url, "\n")
  for(i in names(x$elements)) {
    el = x$elements[[i]]
    if(!inherits(el, c("HTMLResetElement", if(!showHidden) "HTMLHiddenElement")))
       print(el, ...)
  }

}

print.HTMLFormElement =
function(x, ...)
{  
    cat(x$name, ": ", sep = "")
    if("defaultValue" %in% names(x) && !is.na(x$defaultValue))
       cat("[", x$defaultValue, "]  ")

    if("value" %in% names(x) && !is.na(x$value))
      cat(paste(unique(c(names(x$value), XML:::trim(x$value))), sep=", "))
    else if("options" %in% names(x))
       cat(paste(unique(c(names(x$options), XML:::trim(x$options))), sep=", ", collapse=", "))

   cat("\n")
}
