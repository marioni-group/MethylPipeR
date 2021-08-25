dummyTrainXs <- matrix(rnorm(1000), ncol = 10)
dummyTrainY <- rbinom(100, 1, 0.2)

dummyTestXs <- matrix(rnorm(1000), ncol = 10)
dummyTestY <- rbinom(100, 1, 0.2)

# Test continuous models
continuousglmnetModel <- fitMPRModel(type = 'continuous', method = 'glmnet', trainXs = dummyTrainXs, trainY = rnorm(100), alpha = 0.5)
continuousglmnettestPredictions <- predictMPRModel(continuousglmnetModel, dummyTestXs, s = continuousglmnetModel$model$lambda[[1]])

expect_equal(length(continuousglmnettestPredictions), nrow(dummyTestXs))

continuousbartModel <- fitMPRModel(type = 'continuous', method = 'bart', trainXs = dummyTrainXs, trainY = rnorm(100))
continuousbarttestPredictions <- predictMPRModel(continuousbartModel, dummyTestXs)

expect_equal(length(continuousbarttestPredictions), nrow(dummyTestXs))
