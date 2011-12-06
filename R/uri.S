#
# These will go into a different package some time soon.
#
#

URI =
  #
  # Constructor for creating a URI either from another URI
  # or from its constituent parts.
  # 
function(u, path = "/", protocol = "http", port = 80)
{
  # Perhaps just parse here and the substitute.
  #  u = URI("http://www.omegahat.org/index.html", port = 100) doesn't work.
  
  if(inherits(u, "URI")) {
     # Use the existing object and update the supplied fields given as arguments.
   if(!missing(path))
     u[["path"]] = path
   if(!missing(protocol))
     u[["protocol"]] = protocol
   if(!missing(port))
     u[["port"]] = port

   return(u)
 }
 
  # parse the URL
 if(length(grep(":", u)) > 0) {
   uri = parseURL(u)
 } else {

   if(missing(path))
    path = u

   u = NA
   if(missing(protocol)) {
     protocol = NA  # "file"
     port = NA
   }
   
   uri = list(host = u, path = path, protocol = protocol, port = port)
   class(uri) = "URI"
 }

 uri
}

parseURL =
  #
  # parse a string into a URI object
  #
  # Should use parseURI() in the XML package
  #
function(u, isFile = FALSE)
{
  if(length(grep("(news|ftp|http[s]?):", u)) == 0)
    els = c(path = u, host = NA, protocol = NA)
  else {
     x = gsub("(news|ftp|http[s]?):(//)?([^/]+)(/.*)?", "\\1;\\3;\\4", u)

     els = strsplit(x, ";")[[1]]

     names(els) = c("protocol", "host", "path")[1:length(els)]

     if(length(grep(":", els["host"])) > 0) {
        tmp = strsplit(els["host"], ":")[[1]]
        els["port"] = as.integer(tmp[2])
        els["host"] = tmp[1]
     }

     if(!("path" %in% names(els)))
       els["path"] = "/"     
   }

  class(els) = "URI"

  els
}


# Taken from form.S in odbAccess for the moment.

toString.URI =
  #
  # Convert a URI object into a string value.
  #
function(x, ...)
{
 paste(x[["protocol"]], ":", "//",
        x[["host"]],
       ifelse("port" %in% names(x) && !is.na(x[["port"]]), paste(":", x[["port"]], sep=""), ""),
       ifelse("path" %in% names(x), x[["path"]], ""),
       sep="")
}

mergeURI =
  #
  # Combine one URI specification with a base URI.
  #
  # This does not handle collapsing the .. and .
  # in a path.
function(u, url, reduce = FALSE)
{
  if(!hasHost(u))
    u[["host"]] = url[["host"]]

  if(isRelative(u)) {
    d = dirname(url[["path"]])
    u[["path"]] = paste(d, u[["path"]], sep= ifelse(length(grep("/$", d)), "", "/"))
  }

  if(!hasProtocol(u)) {
    u[["protocol"]] = vvv = ifelse(hasProtocol(url), url[["protocol"]], "http")
  }

  if(reduce && u[["path"]] != "") {
    u[["path"]] = simplifyPath(u[["path"]])    
  }

  u
}

hasProtocol =
  #
  # Determine whether the specified URI has a valid protocol value
  #
function(u)
{
  "protocol" %in% names(u) && !is.na(u[["protocol"]])
}  

hasHost =
  #
  # Determine whether the specified URI has a valid host value
  #  
function(u)
{
  !(is.na(u[["host"]])  || length(u[["host"]]) == 0 || u[["host"]] == "" || ("path" %in% names(u) && u[["host"]] == u[["path"]]))
}  

isRelative =
  #
  # Determine whether the specified URI path is realtive or an absolute value.
  #
function(u)
{
  if(!("path" %in% names(u)))
    return(FALSE)
  
  length(grep("^/", u[['path']]) )  ==  0
}  


simplifyPath =
function(path, sep = "/")
{
  els = strsplit(path, sep)

  sapply(els, function(x, collapse) {

            # remove the 'this directory' entries, i.e. the .
           x = x[x != "."]

           idx = which(x == "..")
           if(length(idx)) {
             x = x[-c(idx, idx-1)]
           }

          if(length(x) == 1)
            x = c("", x)
           
          paste(x, collapse=collapse)
       }, collapse=sep)
}  
