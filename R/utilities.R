checkNA <- function(x, stop = TRUE) {
  containsNAs <- any(is.na(x))
  if (containsNAs & stop) {
    stop(paste0('Input object of class ', class(x), ' contains NAs which are not handled by the function.'))
  } else {
    containsNAs
  }
}