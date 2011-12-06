library(RHTMLForms)
f = getHTMLFormDescription("http://www.google.com")
g = createFunction(f[[1]])
g("R XML")


#

readGoogleResults =
function(txt)
{
  doc = htmlParse(txt, asText = TRUE, error = function(...)NULL)
  nodes = getNodeSet(doc, "//a[@class='l']")
  structure(sapply(nodes, xmlGetAttr, "href"),
            names = sapply(nodes, xmlValue))
}
library(XML)
g = createFunction(f[[1]], reader = I(readGoogleResults))

