calculateMetricsAtThreshold <- function(predicted, actual, threshold) {
  confusionMatrix(as.factor(predicted > threshold), as.factor(actual == 1), positive = 'TRUE')
}


#' Title
#'
#' @param nullModelResponse 
#' @param fullModelResponse 
#' @param target 
#'
#' @return
#' @export
plotMPRIncrementalModelConfusionMatrix <- function(nullModelResponse, fullModelResponse, target) {
  
  thresholds <- seq(0.1, 1, 0.1)
  nullModelConfusionMatrices <- lapply(thresholds, function(threshold) {
    calculateMetricsAtThreshold(nullModelResponse, target, threshold)
  })
  fullModelConfusionMatrices <- lapply(thresholds, function(threshold) {
    calculateMetricsAtThreshold(fullModelResponse, target, threshold)
  })
  names(nullModelConfusionMatrices) <- thresholds
  names(fullModelConfusionMatrices) <- thresholds
  
  nullTP <- sapply(thresholds, function(threshold) {
    nullModelConfusionMatrices[[toString(threshold)]]$table[2, 2]
  })
  fullTP <- sapply(thresholds, function(threshold) {
    fullModelConfusionMatrices[[toString(threshold)]]$table[2, 2]
  })
  
  nullTN <- sapply(thresholds, function(threshold) {
    nullModelConfusionMatrices[[toString(threshold)]]$table[1, 1]
  })
  fullTN <- sapply(thresholds, function(threshold) {
    fullModelConfusionMatrices[[toString(threshold)]]$table[1, 1]
  })
  
  nullFP <- sapply(thresholds, function(threshold) {
    nullModelConfusionMatrices[[toString(threshold)]]$table[2, 1]
  })
  fullFP <- sapply(thresholds, function(threshold) {
    fullModelConfusionMatrices[[toString(threshold)]]$table[2, 1]
  })
  
  nullFN <- sapply(thresholds, function(threshold) {
    nullModelConfusionMatrices[[toString(threshold)]]$table[1, 2]
  })
  fullFN <- sapply(thresholds, function(threshold) {
    fullModelConfusionMatrices[[toString(threshold)]]$table[1, 2]
  })
  
  par(mfrow = c(2, 2))
  
  plot(thresholds, fullTP, type = 'o', col = 'blue', pch = 'o',
       xlab = 'Threshold', ylab = 'Number of true positives')
  points(thresholds, nullTP, type = 'o', col = 'red', pch = '*')
  lines(thresholds, nullTP, col = 'red')
  legend('topright', legend = c('null', 'full'), col = c('red', 'blue'), lty = 1)
  
  plot(thresholds, fullTN, type = 'o', col = 'blue', pch = 'o',
       xlab = 'Threshold', ylab = 'Number of true negatives')
  points(thresholds, nullTN, col = 'red', pch = '*')
  lines(thresholds, nullTN, col = 'red')
  
  plot(thresholds, fullFP, type = 'o', col = 'blue', pch = 'o',
       xlab = 'Threshold', ylab = 'Number of false positives')
  points(thresholds, nullFP, col = 'red', pch = '*')
  lines(thresholds, nullFP, col = 'red')
  
  plot(thresholds, fullFN, type = 'o', col = 'blue', pch = 'o',
       xlab = 'Threshold', ylab = 'Number of false negatives')
  points(thresholds, nullFN, col = 'red', pch = '*')
  lines(thresholds, nullFN, col = 'red')
  
  mtext('True/False Positives/Negatives vs. Classification Threshold')
}

#' Title
#'
#' @param incrementalModelResult 
#'
#' @return
#' @export
plotMPRIncrementalModelROC <- function(incrementalModelResult) {
  full <- incrementalModelResult$full$model
  null <- incrementalModelResult$null$model
  rocs <- performance::performance_roc(full, null)
  plot(rocs)
}