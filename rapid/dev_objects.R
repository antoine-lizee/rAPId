
# Closures and prototype functions ----------------------------------------

test <- function(a) {
  function(.) {
    return(a)
  }
}

# see https://code.google.com/p/r-proto/#FAQ
# Watch the instantiating environments...

# proto breaks the closure
tP <- proto(tt = test(5), b = 56)
tP$tt
tP$tt()

tP3 <- proto(b = 56)
tP3$tt <- test(5)
tP3$tt
tP3$tt()

# closure remains
tP2 <- proto(expr = {
  tt <- test(5)
  b <- 56})
tP2$tt
tP2$tt()

tP4 <- proto(b = 56)
tP4[["tt"]] <- test(5) 
tP4$tt
tP4$tt()

tP5 <- proto(tt = test(5), b = 56, funEnvir = FALSE)
tP5$tt
tP5$tt()


# ellipsis handling -------------------------------------------------------

testE <- function(...) {
  str(list(...))
  str(alist(...))
}

testE(a = 65, b = c(435, 345), c = 2+2, 456, "wert")


# argument passing --------------------------------------------------------

testF <- function(...) {
  print(eval(substitute(alist(...))))
}

args <- list(a = 456)
eval(as.call(c(list(quote(testF), quote(.)), args)))

