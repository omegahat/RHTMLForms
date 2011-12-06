library(RHTMLForms)
ff = getHTMLFormDescription("http://www.itis.gov/servlet/SingleRpt/SingleRpt")
fun = createFunction(ff[[1]])
o = fun(search_value = "dog")

