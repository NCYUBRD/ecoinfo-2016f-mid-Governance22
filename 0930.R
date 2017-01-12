require(vegan)
##try http://if https://URLs are not supported
source("https://bioconductor.org/biocLite.R")
biocLite("limma")
browseVignettes("limma")
library("limma")
limmaUsersGuide()
2**32
fib <- function(n)
{if (length(n) > 1) return(sapply(n, fib)) # accept a numeric vector
  if (n == 1) return(1) # first seed value
  if (n == 2) return(1) # second seed value
  return(fib(n-1)+fib(n-2)) # use recursion}
# print first five Fibonacci numbers
fib(1)
fib(2)
fib(3)
fib(4)
fib(5)

# verify the Fibonacci sequence 1 through 10
(actual <- fib(1:10))
(expected <- c(1,1,2,3,5,8,13,21,34,55))
all.equal(actual,expected)
fib <- function(n)
{
  if (length(n) >1) return(sapply(n,fib))
  if (n == 1) return(1)
  if (n == 2) return(1)
  return(fib(n-1)+fib(n-2))
  }
fib(5)
fib(1)
fib(2)
fib(3)
fib(4)
log(2)
