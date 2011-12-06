#
# Source this into R with the checkFormArgs() defined from utils.R
# in this package.
#
#
#  foo()  # TRUE 
#  foo("c")
#  foo("D") # FALSE
#  bar()   # TRUE
#  bar("D") # FALSE

foo =
function(a = "a", b = "D")
{
  
 inputs = list(a = letters[1:4], b = c("A", "D", "A string"))
 fixed = character()

 checkFormArgs(sys.parent(), inputs, fixed, c("a", "b"))

 TRUE
}

bar = 
function(x = "c", y = "A") 
{
  foo(x, y)
}

