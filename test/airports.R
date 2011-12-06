library(RHTMLForms)
forms1 = getHTMLFormDescription("http://cats.airports.faa.gov/reports/reports.cfm")
if(FALSE) {
    # We seem to be inferring the action during the creation of the form description. We could
    # leave it until creating the function and then we wouldn't have this problem.
  forms1[[1]]$formAttributes[["action"]] = "http://cats.airports.faa.gov/reports/rpt126.cfm"
    # we don't need to set the URL, but should for consistency.
  forms1[[1]]$url = "http://cats.airports.faa.gov/reports/rpt126.cfm"
  fun1 = createFunction(forms1[[1]])
} else {
   # An alternative is to explicitly specify the URL in the call to createFunction().
  fun1 = createFunction(forms1[[1]], url = "http://cats.airports.faa.gov/reports/rpt126.cfm")
}

o1 = fun1(AirportID = "OAK", Year = "2009", view = "Excel", followlocation = TRUE)

#library(XML)
#htmlParse(o1)
