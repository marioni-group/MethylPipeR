# glmnet predict is the same for all three model types
predictMPRModelglmnet <- function(model, data, ...) {
  as.numeric(predict(model$model, data, ...))
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