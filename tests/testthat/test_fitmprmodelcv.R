initLogs(sessionLogFolder = 'C:/Users/s2092119/Documents/PhD/Omics Prediction of Incident Disease/R Package/MethylPipeR-UI_logs/')

dummyTrainXs <- matrix(rnorm(1000), ncol = 10)
dummyTrainY <- rbinom(100, 1, 0.2)

dummyTestXs <- matrix(rnorm(1000), ncol = 10)
dummyTestY <- rbinom(100, 1, 0.2)

dummyCovariatesTable <- matrix(rnorm(300), ncol = 3)

binaryrfModel <- fitMPRModelCV(type = 'binary', method = 'rf', trainXs = dummyTrainXs, trainY = dummyTrainY, mtrys = c(1, 2), nodesizes = c(1, 5, 10))
expect_s3_class(binaryrfModel, 'MPRModel')
expect_s3_class(binaryrfModel$model, 'randomForest')

continuousrfModel <- fitMPRModelCV(type = 'continuous', method = 'rf', trainXs = dummyTrainXs, trainY = rnorm(100), mtrys = c(1, 2), nodesizes = c(1, 5, 10))
expect_s3_class(continuousrfModel, 'MPRModel')
expect_s3_class(continuousrfModel$model, 'randomForest')