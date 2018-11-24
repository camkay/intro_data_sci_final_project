###############################
###spround
###############################

.spround <- function(x, digits = 2, l_zero = TRUE) {
  #argument check for x
  if (is.numeric(x) | is.character(x)) {
    if (is.character(x) == TRUE) {
      x <- as.numeric(x)
    }
  } else { 
    #stop("X must be a string or numeric.")
  } 
  #argument check for digits
  if (is.numeric(digits)) {
  } else {
    stop("Digits must be numeric.")
  }
  #argument check for l_zero
  if (l_zero == TRUE | l_zero == FALSE) {
  } else {
    stop("l_zero must be a boolean (i.e., true or false).")
  }
  #convert values
  x <- sprintf(paste("%.", digits, "f", sep = ""),
               round(x, digits))
  x[grepl("0.000", x)] <- 0.001
  if (l_zero == FALSE) {
    x[grepl("^0", x)] <- sub("0", "", x[grepl("^0", x)])
  }
  return(x)
}


