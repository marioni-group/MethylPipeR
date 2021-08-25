dummyTrainXs <- matrix(rnorm(1000), ncol = 10)
dummyTrainY <- rbinom(100, 1, 0.2)

dummyTestXs <- matrix(rnorm(1000), ncol = 10)
dummyTestY <- rbinom(100, 1, 0.2)

dummyCovariatesTable <- matrix(rnorm(300), ncol = 3)

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

continuousbartModel <- fitMPRModel(type = 'continuous', method = 'bart', trainXs = dummyTrainXs, trainY = rnorm(100))
expect_s3_class(continuousbartModel, 'MPRModel')
expect_s3_class(continuousbartModel$model, 'wbart')

# Test survival models
dummySurvTrainY <- data.frame(list('time_to_event' = runif(100, 0, 10),
                                   'Event' = dummyTrainY))
dummySurvTestY <- data.frame(list('time_to_event' = runif(100, 0, 10),
                                   'Event' = dummyTestY))

survivalglmnetModel <- fitMPRModel(type = 'survival', method = 'glmnet', 
                                   trainXs = dummyTrainXs, trainY = dummySurvTrainY, 
                                   tteColname = 'time_to_event', eventColname = 'Event')
survivalrfModel <- fitMPRModel(type = 'survival', method = 'rf',
                               trainXs = dummyTrainXs, trainY = dummySurvTrainY,
                               tteColname = 'time_to_event', eventColname = 'Event')

expect_s3_class(survivalglmnetModel, 'MPRModel')
expect_s3_class(survivalglmnetModel$model, 'glmnet')

expect_s3_class(survivalrfModel, 'MPRModel')

# Test survival columns not matching those specified
expect_error(do.call(fitMPRModel, type = 'survival', method = 'glmnet', 
                                  trainXs = dummyTrainXs, trainY = dummySurvTrainY, 
                                  tteColname = 'tte', eventColname = 'Event'))
expect_error(do.call(fitMPRModel, type = 'survival', method = 'glmnet', 
                     trainXs = dummyTrainXs, trainY = dummySurvTrainY, 
                     eventColname = 'event'))
colnames(dummySurvTestY) <- c('tte', 'Event')
expect_error(do.call(fitMPRModel, type = 'survival', method = 'glmnet', 
                     trainXs = dummyTrainXs, trainY = dummySurvTrainY,
                     testXs = dummyTestXs, testY = dummySurvTestY))
colnames(dummySurvTestY) <- c('time_to_event', 'event')
expect_error(do.call(fitMPRModel, type = 'survival', method = 'glmnet', 
                     trainXs = dummyTrainXs, trainY = dummySurvTrainY,
                     testXs = dummyTestXs, testY = dummySurvTestY))

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

# Test incremental models
dummyTrainXs <- matrix(rnorm(1000), ncol = 10)
dummyTrainY <- rbinom(100, 1, 0.2)

dummyTestXs <- matrix(rnorm(1000), ncol = 10)
dummyTestY <- rbinom(100, 1, 0.2)

dummyCovariatesTable <- data.frame(matrix(rnorm(300), ncol = 3))

continuousglmnetModel <- fitMPRModel(type = 'continuous', method = 'glmnet', trainXs = dummyTrainXs, trainY = rnorm(100), alpha = 0.5)
score <- predictMPRModel(continuousglmnetModel, dummyTestXs, s = continuousglmnetModel$model$lambda[[1]])
covColnames <- colnames(dummyCovariatesTable)
dummyCovariatesTable$score <- score
dummyCovariatesTable$y <- rnorm(100)
continuousIncrementalModel <- fitMPRModelIncremental(dummyCovariatesTable, yColname = 'y', covColnames = covColnames, scoreColname = 'score', family = 'gaussian')