checkNA <- function(x, stop = TRUE) {
  containsNAs <- any(is.na(x))
  if (containsNAs & stop) {
    stop(paste0('Input object of class ', class(x), ' contains NAs which are not handled by the function.'))
  } else {
    containsNAs
  }
}

checkMatrixOrDF <- function(x) {
  if (!(is.matrix(x) | is.data.frame(x) | is.big.matrix(x))) {
    stop('Input should be a matrix or data.frame!')
  }
}

# getGroupCVFoldIDs <- function(df, groupColname, nFolds, seed = NULL) {
#   if (!is.null(seed)) {
#     set.seed(seed)
#   }
#   trainingSetIndexes <- groupKFold(df[, groupColname], k = nFolds)
#   
#   if (length(trainingSetIndexes) < nFolds) {
#     stop('Error: number of folds returned in getGroupCVFoldIDs is less than k. 
#          This can sometimes occur due to the implementation of caret::groupKFold.
#          Try running getGroupCVFoldIDs with a different seed.')
#   }
#   
#   foldIDs <- integer(nrow(df))
#   
#   # groupKFold returns a list of training set indexes. Therefore to identify the
#   # rows that should have foldIDs == i, we find the indexes that are not present
#   # in trainingSetIndexes[[i]]
#   for(i in seq(1:nFolds)) {
#     foldIndex <- setdiff(1:nrow(df), trainingSetIndexes[[i]])
#     print(foldIndex)
#     foldIDs[foldIndex] <- i
#   }
#   
# 
#   foldIDs
# }


#' Given a vector of group IDs 'groups', returns a vector where each element
#' is an integer representing the fold the corresponding element in 'groups'
#' is assigned to. Elements with the same group ID will always be assigned to
#' the same fold.
#'
#' @param groups A vector of group IDs.
#' @param nFolds An integer representing the number of folds.
#' @param seed An integer to use as the random seed.
#'
#' @return A list containing foldIDs, groupToFoldMap and foldToNElementsMap.
#' @export
getGroupCVFoldIDs <- function(groups, nFolds, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  counts <- table(groups)
  elementsPerFold <- length(groups) / nFolds
  
  usedGroups <- character()
  currentFoldNumber <- 1
  groupToFoldMap <- list()
  foldToNElementsMap <- list()
  currentFoldNElements <- 0
  
  for (i in seq(length(counts))) {
    remainingGroups <- setdiff(names(counts), usedGroups)
    sampledGroup <- sample(remainingGroups, size = 1)
    usedGroups <- c(usedGroups, sampledGroup)
    currentFoldNElements <- currentFoldNElements + counts[[sampledGroup]]
    groupToFoldMap[[sampledGroup]] <- currentFoldNumber
    if ((currentFoldNElements >= elementsPerFold) || (i == length(counts))) {
      foldToNElementsMap[[currentFoldNumber]] <- currentFoldNElements
      if (currentFoldNumber < nFolds) {
        currentFoldNumber <- currentFoldNumber + 1
        currentFoldNElements <- 0
      }
    }
  }
  foldIDs <- sapply(groups, function(x) {
    groupToFoldMap[[as.character(x)]]
  })
  
  list(foldIDs = foldIDs, groupToFoldMap = groupToFoldMap, foldToNElementsMap = foldToNElementsMap)
}