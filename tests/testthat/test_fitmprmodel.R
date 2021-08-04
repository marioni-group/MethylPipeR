dummyTrainXs <- matrix(rnorm(1000), ncol = 10)
dummyTrainY <- rbinom(100, 1, 0.2)

dummyTestXs <- matrix(rnorm(1000), ncol = 10)
dummyTestY <- rbinom(100, 1, 0.2)

# Test binary models
binaryglmnetModel <- fitMPRModel(type = 'binary', method = 'glmnet', trainXs = dummyTrainXs, trainY = dummyTrainY, alpha = 0.5)
expect_s3_class(binaryglmnetModel, 'MPRModel')
expect_s3_class(binaryglmnetModel$model, 'glmnet')

binarybartModel <- fitMPRModel(type = 'binary', method = 'bart', trainXs = dummyTrainXs, trainY = dummyTrainY)
expect_s3_class(binarybartModel, 'MPRModel')
expect_s3_class(binarybartModel$model, 'pbart')

binaryrfModel <- fitMPRModel(type = 'binary', method = 'rf', trainXs = dummyTrainXs, trainY = dummyTrainY)
expect_s3_class(binaryrfModel, 'MPRModel')
expect_s3_class(binaryrfModel$model, 'randomForest')

# Test continuous models
continuousglmnetModel <- fitMPRModel(type = 'continuous', method = 'glmnet', trainXs = dummyTrainXs, trainY = rnorm(100), alpha = 0.5)
expect_s3_class(continuousglmnetModel, 'MPRModel')
expect_s3_class(continuousglmnetModel$model, 'glmnet')

# Test survival models
dummySurvTrainY <- data.frame(list('time_to_event' = runif(100, 0, 10),
                                   'Event' = dummyTestY))

survivalglmnetModel <- fitMPRModel(type = 'survival', method = 'glmnet', 
                                   trainXs = dummyTrainXs, trainY = dummySurvTrainY, 
                                   tteColname = 'time_to_event', eventColname = 'Event')
expect_s3_class(survivalglmnetModel, 'MPRModel')
expect_s3_class(survivalglmnetModel$model, 'glmnet')

# Test NA checking
dummyTestY[[10]] <- NA
expect_error(do.call(fitMPRModel, type = 'binary', method = 'glmnet', 
                     trainXs = dummyTrainXs, trainY = dummyTrainY, 
                     testXs = dummyTestXs, testY = dummyTestY, alpha = 0.5)) 
dummyTrainXs[10,10] <- NA
expect_error(do.call(fitMPRModel, type = 'binary', method = 'glmnet', 
                     trainXs = dummyTrainXs, trainY = dummyTrainY, 
                     testXs = dummyTestXs, testY = dummyTestY, alpha = 0.5))

dummyTrainY[[10]] <- NA
expect_error(do.call(fitMPRModel, type = 'binary', method = 'glmnet', 
                     trainXs = dummyTrainXs, trainY = dummyTrainY, 
                     testXs = dummyTestXs, testY = dummyTestY, alpha = 0.5))

dummyTrainXs[10,10] <- NA
expect_error(do.call(fitMPRModel, type = 'binary', method = 'glmnet', 
                     trainXs = dummyTrainXs, trainY = dummyTrainY, 
                     testXs = dummyTestXs, testY = dummyTestY, alpha = 0.5))