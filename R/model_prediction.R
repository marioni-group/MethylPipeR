#' predictMPRModel
#'
#' @param model An MPRModel object. This is typically returned from a call to fitMPRModel.
#' @param data The data.frame/matrix that the model will be applied to.
#' @param ... Other arguments to be passed to the method-specific prediction function.
#'
#' @return The result from the method-specific predict function.
#' @export
predictMPRModel <- function(model, data, bartMeanOrMedian = 'mean', ...) {
  checkNA(data)
  checkMatrixOrDF(data)
  if (S3Class(model) != 'MPRModel') {
    stop('predictMPRModel: model must be of class MPRModel')
  }
  type <- model$modelType
  method <- model$modelMethod
  
  predictFunctionLookup <- list(
    'binary' = list(
      'glmnet' = function() {
        as.numeric(predict(model$model, data, ...))
      },
      'bart' = function() {
        bartPredictResult <- predict(model$model, newdata = data, ...)
        if (bartMeanOrMedian == 'mean') {
          apply(bartPredictResult$prob.test, 2, mean)
        } else if (bartMeanOrMedian == 'median') {
          apply(bartPredictResult$prob.test, 2, median)
        }
      },
      'rf' = function() {
        rfPredictResult <- predict(model$model, newdata = data, ...)
        rfPredictResult
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
          apply(bartPredictResult, 2, median)
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