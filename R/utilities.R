checkNA <- function(x, stop = TRUE) {
  containsNAs <- any(is.na(x))
  if (containsNAs & stop) {
    stop(paste0('Input object of class ', class(x), ' contains NAs which are not handled by the function.'))
  } else {
    containsNAs
  }
}

checkMatrixOrDF <- function(x) {
  if (!(is.matrix(x) | is.data.frame(x))) {
    stop('Input should be a matrix or data.frame!')
  }
}