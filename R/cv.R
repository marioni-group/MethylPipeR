# rfsrcCV <- function(data) {
#   nHyperparameters <- length(gridSearchValues)
# } TODO: Complete

rfTrainAndTest <- function(trainXs, trainY, testXs, testY, metric, ntree, mtry, nodesize) {
  # browser()
  model <- randomForest(x = trainXs, y = trainY, ntree = ntree, mtry = mtry, nodesize = nodesize)
  predictions <- predict(model, testXs)
  
  if (metric == 'AUC') {
    # metricResult <- pROC::roc(testY, predictions)$auc
    metricResult <- MLmetrics::AUC(predictions, testY)
  } else if (metric == 'PRAUC') {
    metricResult <- MLmetrics::PRAUC(predictions, testY)
  } else if (metric == 'RMSE') {
    metricResult <- MLmetrics::RMSE(predictions, testY)
  }
  
  list(model = model, metricResult = metricResult)
}

rfCVIteration <- function(dataset, labels, foldIDs, metric, ntree, mtry, nodesize) {
  # browser()
  foldIDSet <- unique(foldIDs)
  nFolds <- length(foldIDSet)
  metricResults <- sapply(foldIDSet, function(foldID) {
    testIndex <- foldIDs == foldID
    testXs <- dataset[testIndex, ]
    testY <- labels[testIndex]
    trainXs <- dataset[!testIndex, ]
    trainY <- labels[!testIndex]
    rfResult <- rfTrainAndTest(trainXs, trainY, testXs, testY, metric, ntree, mtry, nodesize)
    rfResult$metricResult
  })
  meanMetricResult <- mean(metricResults)
  meanMetricResult
}

#' Title
#'
#' @param xs 
#' @param y 
#' @param nFolds 
#' @param foldIDs 
#' @param metric 
#' @param seed 
#' @param ... 
#'
#' @return
#' @export
rfCVGridSearch <- function(xs, y, nFolds, foldIDs = NULL, metric = NULL, seed = NULL, ...) {
  # browser()
  extraParams <- list(...)
  extraParamNames <- names(extraParams)
  if ('ntrees' %in% extraParamNames) {
    ntrees <- extraParams$ntrees
  } else {
    ntrees <- c(500)
  }
  if ('mtrys' %in% extraParamNames) {
    mtrys <- extraParams$mtrys
  } else {
    # From randomForest defaults
    mtrys <- if (!is.null(y) && !is.factor(y)) max(floor(ncol(xs)/3), 1) else floor(sqrt(ncol(xs)))
  }
  if ('nodesizes' %in%  extraParamNames) {
    nodesizes <- extraParams$nodesizes
  } else {
    # From randomForest defaults
    nodesizes <- if (!is.null(y) && !is.factor(y)) 5 else 1
  }
  
  if (!is.null(seed)){
    set.seed(seed)
  }
  if (is.null(foldIDs)) {
    foldIDs <- sample(rep(1:nFolds, length.out = nrow(xs)))
  }
  
  meanMetricResults <- data.frame(ntree = integer(0), mtry = integer(0), nodesize = integer(0), meanMetric = double(0))
  
  for (ntree in ntrees) {
    for (mtry in mtrys) {
      for (nodesize in nodesizes) {
        cvIterResult <- rfCVIteration(xs, y, foldIDs, metric, ntree, mtry, nodesize)
        meanMetricResults <- rbind(meanMetricResults, list(ntree = ntree, mtry = mtry, nodesize = nodesize, mean_metric = cvIterResult))
      }
    }
  }
  
  meanMetricResultsSorted <- meanMetricResults[order(meanMetricResults$mean_metric, decreasing = TRUE),]
  
  ntreeBest <- meanMetricResultsSorted[1, 'ntree']
  mtryBest <- meanMetricResultsSorted[1, 'mtry']
  nodesizeBest <- meanMetricResultsSorted[1, 'nodesize']
  
  # rfResult <- rfTrainAndTest(trainXs = xs,
  #                            trainY = y,
  #                            testXs = testXs,
  #                            testY = testY,
  #                            metric = metric,
  #                            ntree = ntreeBest,
  #                            mtry = mtryBest,
  #                            nodesize = nodesizeBest)
  
  model <- randomForest(x = xs, y = y, ntree = ntreeBest, mtry = mtryBest, nodesize = nodesizeBest)
  model
}