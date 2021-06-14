
#' trainAndTestRF
#'
#' @param trainXs A matrix/data.frame corresponding to X (variables) in the training set. Columns should correspond to features
#' @param trainY A vector/list corresponding to Y (labels) in the training set.
#' @param testXs A matrix/data.frame corresponding to X (variables) in the test set. Columns should correspond to the same features in trainXs. If not provided, trainXs is used as testXs.
#' @param testY A vector/list corresponding to Y (labels) in the test set. If not provided, trainY is used as testY.
#' @param metric 'AUC' or 'PRAUC'. The metric to be calculated on test set prediction.
#' @param ntree The number of trees to fit in the model.
#' @param mtry The number of features to be selected at random and considered at each node during tree fitting.
#' @param nodesize The minimum size of terminal nodes.
#' @param pipelineRunInformation The object originally created by beginPipelineRun.
#' @param logOverride NULL, TRUE or FALSE. Used to override the 'log' entry in pipelineRunInformation. Ignored if NULL.
#'
#' @return A list containing the model and the metric calculated for test set prediction.
#' @export
trainAndTestRF <- function(trainXs, trainY, testXs = NULL, testY = NULL, metric = 'AUC', ntree = 500, mtry = NULL, nodesize = NULL, pipelineRunInformation, logOverride = NULL) {
  tic('Train and test random forest model')
  # If mtry is not set, set default mtry for randomForest
  if (is.null(mtry)) {
    mtry <- if (!is.null(trainY) && !is.factor(trainY)) max(floor(ncol(trainXs)/3), 1) else floor(sqrt(ncol(trainXs)))
  }

  if (is.null(testXs)) {
    testXs <- trainXs
  }
  if (is.null(testY)) {
    testY <- trainY
  }

  set.seed(pipelineRunInformation[['random seed']])
  model <- randomForest(trainXs, y = trainY, xtest = testXs, ytest = testY, ntree = ntree, mtry = mtry, nodesize = nodesize)
  if (metric == 'AUC') {
    metricResult <- pROC::roc(testY, model$test$votes[, 2])$auc
  } else if (metric == 'PRAUC') {
    metricResult <- MLmetrics::PRAUC(model$test$votes[, 2], testY)
  }
  toc(log = TRUE)
  if (metric == 'AUC') {
    tic(paste0('ntree = ', ntree, ', mtry = ', mtry, ', nodesize = ', nodesize, ', AUC = ', metricResult))
    toc(log = TRUE)
  } else if (metric == 'PRAUC') {
    tic(paste0('ntree = ', ntree, ', mtry = ', mtry, ', nodesize = ', nodesize, ', AUC = ', metricResult))
    toc(log = TRUE)
  }

  # logOverride is used to override the 'log' entry in pipelineRunInformation. Useful when training models within cross-validation and we don't want all models to be saved.
  if (is.null(logOverride)) {
    logging <- pipelineRunInformation[['log']]
  } else {
    logging <- logOverride
  }

  if (logging) {
    tic('Save random forest model')
    saveRDS(model, paste0(pipelineRunInformation[['log folder path']], 'random_forest_model_', pipelineRunInformation[['start timestamp']], '.rds'))
    toc(log = TRUE)
  }

  list(model = model, testMetric = metricResult)
}

rfCVIter <- function(dataset, labels, foldIDs, metric, ntree, mtry, nodesize, pipelineRunInformation) {
  tic(paste0('Random forest CV iteration: ntree = ', ntree, ', mtry = ', mtry, ', nodesize = ', nodesize))
  foldIDSet <- unique(foldIDs)
  nFolds <- length(foldIDSet)
  metricResults <- sapply(foldIDSet, function(foldID) {
    testIndex <- foldIDs == foldID
    testXs <- dataset[testIndex, ]
    testY <- labels[testIndex]
    trainXs <- dataset[!testIndex, ]
    trainY <- labels[!testIndex]
    # Here we set logOverride to FALSE so that models are not saved for the CV iterations.
    rfResult <- trainAndTestRF(trainXs, trainY, testXs, testY, metric, ntree, mtry, nodesize, pipelineRunInformation, logOverride = FALSE)
    rfResult$testMetric
  })
  meanMetricResult <- mean(metricResults)
  toc(log = TRUE)
  tic(paste0('mean_metric = ', meanMetricResult))
  toc(log = TRUE)
  meanMetricResult
}

#' rfCV
#'
#' @param dataset A matrix/data.frame corresponding to X (variables) to be used in the cross-validation.
#' @param labels A vector/list corresponding to Y (labels) to be used in the cross-validation.
#' @param foldIDs A vector/list with the same length as labels denoting for each row in the dataset, which fold it will be assigned to.
#' @param metric 'AUC' or 'PRAUC', the metric that will be used to determine the optimal hyperparameters in the grid search.
#' @param ntrees A vector/list containing all the values of ntree to be tried in the grid search.
#' @param mtrys A vector/list containing all the values of mtry to be tried in the grid search.
#' @param nodesizes A vector/list containing all the values of nodesize to be tried in the grid search.
#' @param pipelineRunInformation The object originally created by beginPipelineRun.
#' @param testXs A matrix/data.frame corresponding to the test set X (variables) used to evaluate the final model. If NULL, the training set is used as the test set.
#' @param testY A vector/list corresponding to the test set Y (labels) to be used to evaluate the final model. If NULL, the training set labels are used.
#'
#' @return A data.frame showing for each combination of hyperparameters, the (mean) metric calculated on the test fold predictions.
#' @export
rfCV <- function(dataset, labels, foldIDs, metric = 'AUC', ntrees, mtrys, nodesizes, pipelineRunInformation, testXs = NULL, testY = NULL) {
  tic('Random forest hyperparameter optimisation using grid search')
  meanMetricResults <- data.frame(ntree = integer(0), mtry = integer(0), nodesize = integer(0), mean_metric = double(0))
  for (ntree in ntrees) {
    for (mtry in mtrys) {
      for (nodesize in nodesizes) {
        cvIterResult <- rfCVIter(dataset, labels, foldIDs, metric, ntree, mtry, nodesize, pipelineRunInformation)
        meanMetricResults <- rbind(meanMetricResults, list(ntree = ntree, mtry = mtry, nodesize = nodesize, mean_metric = cvIterResult))
      }
    }
  }
  toc(log = TRUE)

  meanMetricResultsSorted <- meanMetricResults[order(meanMetricResults$mean_metric, decreasing = TRUE),]

  ntreeBest <- meanMetricResultsSorted[1, 'ntree']
  mtryBest <- meanMetricResultsSorted[1, 'mtry']
  nodesizeBest <- meanMetricResultsSorted[1, 'nodesize']

  rfResult <- trainAndTestRF(trainXs = dataset,
                             trainY = labels,
                             testXs = testXs,
                             testY = testY,
                             metric = metric,
                             ntree = ntreeBest,
                             mtry = mtryBest,
                             nodesize = nodesizeBest,
                             pipelineRunInformation = pipelineRunInformation)

  model <- rfResult$model

  rfTrainResponse <- model$votes[, 2]
  rfTestResponse <- model$test$votes[, 2]

  if (pipelineRunInformation[['log']]) {
    saveRDS(rfTrainResponse, paste0(pipelineRunInformation[['log folder path']], 'random_forest_cv_final_model_train_response_', pipelineRunInformation[['start timestamp']], '.rds'))
    saveRDS(rfTestResponse, paste0(pipelineRunInformation[['log folder path']], 'random_forest_cv_final_model_test_response_', pipelineRunInformation[['start timestamp']], '.rds'))
  }

  list(rfResult = rfResult,
       meanMetricResults = meanMetricResults,
       rfTrainResponse = rfTrainResponse,
       rfTestResponse = rfTestResponse)
}
