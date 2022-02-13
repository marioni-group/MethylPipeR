#' predictMPRModel
#'
#' @param model An MPRModel object. This is typically returned from a call to fitMPRModel.
#' @param data The data.frame/matrix that the model will be applied to.
#' @param ... Other arguments to be passed to the method-specific prediction function.
#'
#' @return The result from the method-specific predict function.
#' @export
predictMPRModel <- function(model, data,  ...) {
  checkNA(data)
  checkMatrixOrDF(data)
  if (S3Class(model) != 'MPRModel') {
    stop('predictMPRModel: model must be of class MPRModel')
  }
  type <- model$modelType
  method <- model$modelMethod
  
  predictFunctionLookup <- list(
    'binary' = list(
      'glmnet' = predictMPRModelglmnet,
      'biglasso' = predictMPRModelbiglasso,
      'bart' = predictMPRModelBinaryBART,
      'rf' = predictMPRModelBinaryRF
    ),
    'survival' = list(
      'glmnet' = predictMPRModelglmnet,
      'biglasso' = predictMPRModelbiglasso
    ),
    'continuous' = list(
      'glmnet' = predictMPRModelglmnet,
      'biglasso' = predictMPRModelbiglasso,
      'bart' = predictMPRModelContinuousBART,
      'rf' = predictMPRModelContinuousRF
    )
  )
  
  predictResult <- predictFunctionLookup[[type]][[method]](model, data, ...)
  predictResult
}