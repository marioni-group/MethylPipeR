# glmnet predict is the same for all three model types
predictMPRModelglmnet <- function(model, data, ...) {
  as.numeric(predict(model$model, data, ...))
}

predictMPRModelbiglasso <- function(model, data, ...) {
  predict(model$model, data, ...)
}

predictMPRModelBinaryBART <- function(model, data, ...) {
  arguments <- list(...)
  bartMeanOrMedian <- arguments[['bartMeanOrMedian']]
  # If bartMeanOrMedian parameter not provided, set to default of 'mean'
  if (is.null(bartMeanOrMedian)) {
    bartMeanOrMedian <- 'mean'
  }
  # Remove bartMeanOrMedian from arguments list to be passed on
  arguments[['bartMeanOrMedian']] <- NULL
  
  # Prepend model and data to arguments list to pass to do.call
  arguments <- c(list(object = model$model, newdata = data), arguments)
  
  bartPredictResult <- do.call(predict, arguments)
  if (bartMeanOrMedian == 'mean') {
    apply(bartPredictResult$prob.test, 2, mean)
  } else if (bartMeanOrMedian == 'median') {
    apply(bartPredictResult$prob.test, 2, median)
  }
}

predictMPRModelBinaryRF <- function(model, data, ...) {
  rfPredictResult <- predict(model$model, newdata = data, ...)
  rfPredictResult
}

# predictMPRModelSurvivalBART <- function(model, data, ...) {
#   arguments <- list(...)
#   bartSurvivalModel <- model$model
#   predictionTimePoint <- arguments[['predictionTimePoint']]
#   numberOfTimePoints <- bartSurvivalModel$K
#   predictionTimePointIndex <- match(predictionTimePoint, bartSurvivalModel$times)
#   
#   # Given an index for an individual (a row in the dataset), a time point and the total number of time points, returns the corresponding column index in bart.survival.model$surv.test (or any result with the same structure).
#   getBartResultColumn <- function(individual, timePointIndex, nTimePoints) {
#     (individual - 1) * nTimePoints + timePointIndex
#   }
#   
#   survivalMeanPredictions <- sapply(1:nrow(testData), function(individual) {
#     bartSurvivalModel$surv.test.mean[[getBartResultColumn(individual, predictionTimePointIndex, numberOfTimePoints)]]
#   })
#   
#   thresholdTTEResult <- thresholdTTE(testTarget,
#                                      list(testData,
#                                           survivalMeanPredictions),
#                                      predictionTimePoint)
#   testTarget <- thresholdTTEResult$targetFiltered
#   row.names(testTarget) <- NULL
#   testData <- thresholdTTEResult$objectsFiltered[[1]]
#   survivalMeanPredictions <- thresholdTTEResult$objectsFiltered[[2]]
#   thresholdTTECounts <- thresholdTTEResult$counts
#   thresholdTTEResult <- NULL
#   gc()
#   
#   # event.probability is calculated as 1 - survival probability
#   eventPredictions <- 1 - survivalMeanPredictions
# }

predictMPRModelContinuousBART <- function(model, data, ...) {
  arguments <- list(...)
  bartMeanOrMedian <- arguments[['bartMeanOrMedian']]
  # If bartMeanOrMedian parameter not provided, set to default of 'mean'
  if (is.null(bartMeanOrMedian)) {
    bartMeanOrMedian <- 'mean'
  }
  # Remove bartMeanOrMedian from arguments list to be passed on
  arguments[['bartMeanOrMedian']] <- NULL
  
  # Prepend model and data to arguments list to pass to do.call
  arguments <- c(list(object = model$model, newdata = data), arguments)
  
  bartPredictResult <- do.call(predict, arguments)
  if (bartMeanOrMedian == 'mean') {
    apply(bartPredictResult, 2, mean)
  } else if (bartMeanOrMedian == 'median') {
    apply(bartPredictResult, 2, median)
  }
}

predictMPRModelContinuousRF <- function(model, data, ...) {
  rfPredictResult <- predict(model$model, newdata = data, ...)
  rfPredictResult
}