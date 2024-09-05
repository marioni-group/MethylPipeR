#' Title
#'
#' @param x 
#' @param stop_if_na
#' @keywords internal
#'
#' @return NULL
checkNA <- function(x, stop_if_na = TRUE) {
  containsNAs <- any(is.na(x))
  if (containsNAs & stop_if_na) {
    stop(paste0("Input object of class ",
                class(x),
                " contains NAs which are not handled by the function."))
  } else {
    containsNAs
  }
}

#' Title
#'
#' @param x 
#' @keywords internal
#'
#' @return NULL
checkMatrixOrDF <- function(x) {
  if (!(is.matrix(x) | is.data.frame(x) | bigmemory::is.big.matrix(x))) {
    stop("Input should be a matrix or data.frame!\n")
  }
}

#' Title
#'
#' @param method 
#' @keywords internal
#'
#' @return NULL
checkMethod <- function(method) {
  if (!(method %in% c("glmnet", "rf", "bart", "biglasso"))) {
    stop("method must be one of: glmnet, rf, bart, biglasso")
  }
}

#' Title
#'
#' @param type 
#' @keywords internal
#'
#' @return NULL
checkType <- function(type) {
  if (!(type %in% c("survival", "binary", "continuous"))) {
    stop("type must be one of: survival, binary, continuous")
  }
}

#' Title
#'
#' @param method 
#' @param type 
#' @keywords internal 
#' @return NULL
checkMethodPackageInstalled <- function(method, type) {
  if (method == "bart") {
    if (!requireNamespace("BART", quietly = TRUE)) {
      stop("Package BART required for model method 'bart'")
    }
  } else if (method == "rf") {
    if (type == "binary" | type == "continuous") {
      if (!requireNamespace("randomForest", quietly = TRUE)) {
        stop(paste0("Package randomForest required for model method 'rf' and type ", type))
      }
    } else if (type == "survival") {
      if (!requireNamespace("randomForestSRC", quietly = TRUE)) {
        stop(paste0("Package randomForestSRC required for model method 'rf' and type ", type))
      }
    }
  } else if (method == "biglasso") {
    if (!requireNamespace("biglasso", quietly = TRUE)) {
      stop("Package biglasso required for model method 'biglasso'")
    }
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
  
  list(foldIDs = foldIDs, 
       groupToFoldMap = groupToFoldMap, 
       foldToNElementsMap = foldToNElementsMap)
}


#' For binary outcomes. Create a vector of (integer) fold assignments with equal numbers of cases (and controls) across folds.
#' Used to ensure there are as many cases as possible in all folds, particularly in imbalanced datasets.
#'
#' @param data A dataframe containing a column specified by classColname
#' @param classColname A string corresponding to the binary class label (must contain only integers: 1 and 0)
#' @param nFolds The number of folds to assign
#' @param seed An integer to set for the random seed
#'
#' @return A vector of integers specifying the fold ID assigned to each row in data
#' @export
getBalancedCVFoldIDs <- function(data, classColname, nFolds, seed = NULL) {
  tic('Assigning balanced training folds')
  # For cases and controls: shuffle indexes and assign an integer 1:nFolds on a round robin basis
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # Save original row names to reorder foldIDs at the end.
  originalRowNames <- row.names(data)
  
  caseRowNames <- row.names(data[data[, classColname] == 1, ])
  caseRowNames <- sample(caseRowNames)
  caseFoldIDs <- 1:length(caseRowNames) %% nFolds + 1
  names(caseFoldIDs) <- caseRowNames
  
  controlRowNames <- row.names(data[data[, classColname] == 0, ])
  controlRowNames <- sample(controlRowNames)
  controlFoldIDs <- 1:length(controlRowNames) %% nFolds + 1
  names(controlFoldIDs) <- controlRowNames
  
  foldIDs <- c(caseFoldIDs, controlFoldIDs)
  # Reorder according to original row names
  foldIDs <- foldIDs[originalRowNames]
  # foldids <- sapply(1:length(foldIDs), function(num) {foldIDs[[toString(num)]]})
  toc(log = TRUE)
  return(foldIDs)
}
