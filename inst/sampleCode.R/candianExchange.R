
library(RHTMLForms)
url = "http://banqueducanada.ca/en/rates/exchange-avg.html"
ff = getHTMLFormDescription(url)
fun = createFunction(ff[[2]])
o = fun(ExchangeRatesByWeek_daterange = "12 months")

