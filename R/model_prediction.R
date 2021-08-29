#' Title
#'
#' @param model 
#' @param data 
#' @param ... 
#'
#' @return
#' @export
predictMPRModel <- function(model, data, bartMeanOrMedian = 'mean', ...) {
  checkNA(data)
  checkMatrixOrDF(data)
  if (S3Class(model) != 'MPRModel') {
    stop('predictMPRModel: model must be of class mprModel')
  }
  type <- model$modelType
  method <- model$modelMethod
  
  predictFunctionLookup <- list(
    'binary' = list(
      'glmnet' = function() {
        as.numeric(predict(model$model, data, ...))
      },
      'bart' = function() {
        # TODO: implement
      },
      'rf' = function() {
        # TODO: implement
      }
    ),
    'survival' = list(
      'glmnet' = function() {
        as.numeric(predict(model$model, data, ...))
      },
      'bart' = function() {
        # TODO: implement
      },
      'rf' = function() {
        # TODO: implement
      }
    ),
    'continuous' = list(
      'glmnet' = function() {
        as.numeric(predict(model$model, data, ...))
      },
      'bart' = function() {
        bartPredictResult <- predict(model$model, newdata = data, ...)
        if (bartMeanOrMedian == 'mean') {
          apply(bartPredictResult, 2, mean)
        } else if (bartMeanOrMedian == 'median') {
          apply(bartPredictResult, 2, 'median')
        }
      },
      'rf' = function() {
        rfPredictResult <- predict(model$model, newdata = data, ...)
        rfPredictResult
      }
    )
  )
  
  predictResult <- predictFunctionLookup[[type]][[method]]()
  predictResult
}