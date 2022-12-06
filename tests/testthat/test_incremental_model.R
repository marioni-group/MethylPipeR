tmpSessionLogFolder <- paste0(tempdir(), "/")
initLogs(sessionLogFolder = tmpSessionLogFolder, note = "Test note.")

# Test incremental models
dummyTrainXs <- matrix(rnorm(1000), ncol = 10)
dummyTrainY <- rbinom(100, 1, 0.2)

dummyTestXs <- matrix(rnorm(1000), ncol = 10)
dummyTestY <- rbinom(100, 1, 0.2)

dummyCovariatesTable <- data.frame(matrix(rnorm(300), ncol = 3))
binarydummyCovariatesTable <- dummyCovariatesTable

continuousglmnetModel <- fitMPRModel(type = "continuous", method = "glmnet", trainXs = dummyTrainXs, trainY = rnorm(100), alpha = 0.5)
score <- predictMPRModel(continuousglmnetModel, dummyTestXs, s = continuousglmnetModel$model$lambda[[1]])
covColnames <- colnames(dummyCovariatesTable)
dummyCovariatesTable$score <- score
dummyCovariatesTable$y <- rnorm(100)
continuousIncrementalModel <- fitMPRModelIncremental(dummyCovariatesTable, yColname = "y", covColnames = covColnames, scoreColname = "score", family = "gaussian")

binaryglmnetModel <- fitMPRModel(type = "binary", method = "glmnet", trainXs = dummyTrainXs, trainY = dummyTrainY, alpha = 0.5)
binaryscore <- predictMPRModel(binaryglmnetModel, dummyTestXs, s = binaryglmnetModel$model$lambda[[1]])
binarycovColnames <- colnames(binarydummyCovariatesTable)
binarydummyCovariatesTable$score <- binaryscore
binarydummyCovariatesTable$y <- rnorm(100)
binaryIncrementalModel <- fitMPRModelIncremental(binarydummyCovariatesTable, yColname = "y", covColnames = binarycovColnames, scoreColname = "score", family = "gaussian")


# Make this binary
# plotMPRIncrementalModelConfusionMatrix(continuousIncrementalModel$null$response,
#                                        continuousIncrementalModel$full$response,
#                                        dummyTestY)

plotMPRIncrementalModelROC(binaryIncrementalModel)
