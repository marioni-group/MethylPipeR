library(bigmemory)

initLogs(sessionLogFolder = 'C:/Users/s2092119/Documents/PhD/Omics Prediction of Incident Disease/R Package/MethylPipeR-UI_logs/', note = 'Test note.')

dummyTrainXs <- matrix(rnorm(1000), ncol = 10)
dummyTrainY <- rbinom(100, 1, 0.2)

dummyTestXs <- matrix(rnorm(1000), ncol = 10)
dummyTestY <- rbinom(100, 1, 0.2)

dummyCovariatesTable <- matrix(rnorm(300), ncol = 3)

# Test binary models
binaryglmnetModel <- fitMPRModel(type = 'binary', method = 'glmnet', trainXs = dummyTrainXs, trainY = dummyTrainY, alpha = 0.5)
expect_s3_class(binaryglmnetModel, 'MPRModel')
expect_s3_class(binaryglmnetModel$model, 'glmnet')

binarybiglassoModel <- fitMPRModel(type = 'binary', method = 'biglasso', trainXs = dummyTrainXs, trainY = dummyTrainY, alpha = 0.5)
expect_s3_class(binarybiglassoModel, 'MPRModel')
expect_s3_class(binarybiglassoModel$model, 'biglasso')

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

continuousbiglassoModel <- fitMPRModel(type = 'continuous', method = 'biglasso', trainXs = dummyTrainXs, trainY = rnorm(100), alpha = 0.5)
expect_s3_class(continuousbiglassoModel, 'MPRModel')
expect_s3_class(continuousbiglassoModel$model, 'biglasso')

continuousbartModel <- fitMPRModel(type = 'continuous', method = 'bart', trainXs = dummyTrainXs, trainY = rnorm(100))
expect_s3_class(continuousbartModel, 'MPRModel')
expect_s3_class(continuousbartModel$model, 'wbart')

continuousrfModel <- fitMPRModel(type = 'continuous', method = 'rf', trainXs = dummyTrainXs, trainY = rnorm(100))
expect_s3_class(continuousrfModel, 'MPRModel')
expect_s3_class(continuousrfModel$model, 'randomForest')

# Test survival models
dummySurvTrainY <- data.frame(list('time_to_event' = runif(100, 0, 10),
                                   'Event' = dummyTrainY))
dummySurvTestY <- data.frame(list('time_to_event' = runif(100, 0, 10),
                                   'Event' = dummyTestY))

survivalglmnetModel <- fitMPRModel(type = 'survival', method = 'glmnet', 
                                   trainXs = dummyTrainXs, trainY = dummySurvTrainY, 
                                   tteColname = 'time_to_event', eventColname = 'Event')
survivalbiglassoModel <- fitMPRModel(type = 'survival', method = 'biglasso', 
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

