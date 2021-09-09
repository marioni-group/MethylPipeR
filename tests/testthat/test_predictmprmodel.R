dummyTrainXs <- matrix(rnorm(1000), ncol = 10)
dummyTrainY <- rbinom(100, 1, 0.2)

dummyTestXs <- matrix(rnorm(1000), ncol = 10)
dummyTestY <- rbinom(100, 1, 0.2)

dummyTrainYContinuous <- rnorm(100)
dummyTestYContinuous <- rnorm(100)

# Test continuous models
continuousglmnetModel <- fitMPRModel(type = 'continuous', method = 'glmnet', trainXs = dummyTrainXs, trainY = dummyTrainYContinuous, alpha = 0.5)
continuousglmnettestPredictions <- predictMPRModel(continuousglmnetModel, dummyTestXs, s = continuousglmnetModel$model$lambda[[1]])

expect_equal(length(continuousglmnettestPredictions), nrow(dummyTestXs))

continuousbartModel <- fitMPRModel(type = 'continuous', method = 'bart', trainXs = dummyTrainXs, trainY = dummyTrainYContinuous)
continuousbarttestPredictions <- predictMPRModel(continuousbartModel, dummyTestXs)

expect_equal(length(continuousbarttestPredictions), nrow(dummyTestXs))

continuousrfModel <- fitMPRModel(type = 'continuous', method = 'rf', trainXs = dummyTrainXs, trainY = dummyTrainYContinuous)
continuousrftestPredictions <- predictMPRModel(continuousrfModel, dummyTestXs)

expect_equal(length(continuousrftestPredictions), nrow(dummyTestXs))

# Test binary models
binaryglmnetModel <- fitMPRModel(type = 'binary', method = 'glmnet', trainXs = dummyTrainXs, trainY = dummyTrainY, alpha = 0.5)
binaryglmnettestPredictions <- predictMPRModel(binaryglmnetModel, dummyTestXs, s = binaryglmnetModel$model$lambda[[1]])

expect_equal(length(binaryglmnettestPredictions), nrow(dummyTestXs))

binarybartModel <- fitMPRModel(type = 'binary', method = 'bart', trainXs = dummyTrainXs, trainY = dummyTrainY)
binarybarttestPredictions <- predictMPRModel(binarybartModel, dummyTestXs)

expect_equal(length(binarybarttestPredictions), nrow(dummyTestXs))

binaryrfModel <- fitMPRModel(type = 'binary', method = 'rf', trainXs = dummyTrainXs, trainY = dummyTrainY)
binaryrftestPredictions <- predictMPRModel(binaryrfModel, dummyTestXs)

expect_equal(length(binaryrftestPredictions), nrow(dummyTestXs))