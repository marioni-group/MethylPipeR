
# registerDoMC(cores = 3)
# registerDoParallel(3)

# Convert M-values to beta values
mToBeta <- function(m) {
  2^m/(2^m + 1)
}

# Mean imputation for methylation data
imputeMean <- function(data) {
  for (i in 1:ncol(data)) {
    data[is.na(data[, i]), i] <- mean(data[, i], na.rm = TRUE)
  }
  data
}

# Obtain a vector containing all of the features (Sentrix IDs) that correspond to the top number.of.features by coefficient of variance
getFilterByCoefOfVarIDs <- function(data, numberOfFeatures) {
  stdDevs <- apply(data, 2, sd)
  means <- apply(data, 2, mean)
  coefsOfVar <- abs(stdDevs / means)
  sortedCoefsOfVar <- sort(coefsOfVar, index.return = TRUE, decreasing = TRUE)
  cpgIDs <- colnames(data)[sortedCoefsOfVar$ix]
  cpgIDs[1:numberOfFeatures]
}

# Get 450k IDs from annotation file
get450kIDs <- function(filepath = "/Cluster_Filespace/Marioni_Group/Yipeng/data/GS/IHD_pipeline/EPIC_AnnotationObject_df.rds") {
  annotations <- readRDS(filepath)
  rownames(annotations)[annotations[, 'Methyl450_Loci'] == 'TRUE']
}

#' createClassWeights
#'
#' @param classes A vector or list of binary 1 and 0 values corresponding to e.g. positive and negative classes respectively.
#'
#' @return A vector of the same length as classes. Each element is the weight for the corresponding element in classes.
#' @export
createClassWeights <- function(classes) {
  weights <- rep(1, length(classes))
  weights[which(classes == 1)] <- length(classes) / (2 * length(which(classes == 1)))
  weights[which(classes != 1)] <- length(classes) / (2 * length(which(classes != 1)))
  weights
}

fitModelCVFunctions <- list(
  'logistic' = function(x, y, penalty, weights, nFoldsCV = 3, randomSeed = NA, foldID = NULL, prescaled = FALSE, typeMeasure = 'default', penaltyFactor = NULL) {
    if (!is.na(randomSeed)) {
      set.seed(randomSeed)
    }
    if (penalty == 'ridge') {
      alpha <- 0
    } else if (penalty == 'lasso') {
      alpha <- 1
    } else if (penalty == 'elnet') {
      alpha <- 0.5
    } else {
      alpha <- NA
    }
    if (is.null(penaltyFactor)) {
      cv.glmnet(x, y, alpha = alpha, family = 'binomial', weights = weights, nfolds = nFoldsCV, foldid = foldID, parallel = TRUE, standardize = !prescaled, type.measure = typeMeasure)
    } else {
      cv.glmnet(x, y, alpha = alpha, family = 'binomial', weights = weights, nfolds = nFoldsCV, foldid = foldID, parallel = TRUE, standardize = !prescaled, type.measure = typeMeasure, penalty.factor = penaltyFactor)
    }
  },
  'cox' = function(x, y, penalty, weights, nFoldsCV = 3, randomSeed = NA, foldID = NULL, prescaled = FALSE, penaltyFactor = NULL) {
    if (!is.na(randomSeed)) {
      set.seed(randomSeed)
    }
    if (penalty == 'ridge') {
      alpha <- 0
    } else if (penalty == 'lasso') {
      alpha <- 1
    } else if (penalty == 'elnet') {
      alpha <- 0.5
    } else {
      alpha <- NA
    }
    if (is.null(penaltyFactor)) {
      cv.glmnet(x, y, alpha = alpha, family = 'cox', weights = weights, nfolds = nFoldsCV, foldid = foldID, parallel = TRUE, standardize = !prescaled)
    } else {
      cv.glmnet(x, y, alpha = alpha, family = 'cox', weights = weights, nfolds = nFoldsCV, foldid = foldID, parallel = TRUE, standardize = !prescaled, penalty.factor = penaltyFactor)
    }
  }
)

predictModelCVFunctions <- list(
  'logistic' = function(model, data, predictType = 'class') {
    as.numeric(predict(model, data, type = predictType, s = 'lambda.min'))
  },
  'cox' = function(model, data) {
    predict(model, data, type = 'link', s = 'lambda.min')[, 1]
  }
)


