if(FALSE) {
postForm("http://www.expasy.ch/tools/protscale.html",
         "sequence" = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
         scale = "Molecular weight",
         window = "5",
         weight_edges = "100",
         weight_var = "linear",
         norm = "no",
         submit = "Submit", .checkparams = TRUE)
}

library(RHTMLForms)
f = getHTMLFormDescription("http://www.expasy.ch/tools/protscale.html")
fun = createFunction(f[[2]])

if(FALSE)
  o = fun(prot_id = "P05130", weight_var = "exponential", style = "POST")
