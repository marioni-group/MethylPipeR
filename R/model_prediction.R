#' Title
#'
#' @param model 
#' @param data 
#' @param ... 
#'
#' @return
#' @export
predictMPRModel <- function(model, data, ...) {
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
        # TODO: implement
      },
      'rf' = function() {
        # TODO: implement
      }
    )
  )
  
  predictResult <- predictFunctionLookup[[type]][[method]]()
  predictResult
}