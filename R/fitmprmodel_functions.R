fitMPRModelBinaryglmnet <- function(trainXs,
                                    trainY,
                                    testXs,
                                    testY,
                                    tteColname,
                                    eventColname,
                                    parallel,
                                    seed,
                                    ...) {
  glmnet(x = trainXs, y = trainY, family = "binomial", ...)
}

fitMPRModelBinarybiglasso <- function(trainXs,
                                      trainY,
                                      testXs,
                                      testY,
                                      tteColname,
                                      eventColname,
                                      parallel,
                                      seed,
                                      ...) {
  # To avoid 'LongVector not supported' error
  nCols <- ncol(trainXs)
  trainXsBig <- cbindBM(as.big.matrix(trainXs[, 1:(nCols / 2)]),
                        as.big.matrix(trainXs[, (nCols / 2 + 1):nCols]))
  biglasso(trainXsBig, trainY, family = "binomial", ...)
}

fitMPRModelBinaryBART <- function(trainXs,
                                  trainY,
                                  testXs,
                                  tteColname,
                                  eventColname,
                                  testY,
                                  parallel,
                                  seed,
                                  ...) {
  if (is.null(testXs)) {
    testXs <- trainXs
  }
  if (parallel) {
    mc.gbart(x.train = trainXs,
             y.train = trainY,
             x.test = testXs,
             type = "pbart",
             seed = seed,
             ...)
  } else {
    gbart(x.train = trainXs,
          y.train = trainY,
          x.test = testXs,
          type = "pbart",
          seed = seed,
          ...)
  }
}

fitMPRModelBinaryRF <- function(trainXs,
                                trainY,
                                testXs,
                                testY,
                                tteColname,
                                eventColname,
                                parallel,
                                seed,
                                ...) {
  set.seed(seed)
  randomForest(x = trainXs, y = as.factor(trainY), ...)
}

fitMPRModelContinuousglmnet <- function(trainXs,
                                        trainY,
                                        testXs,
                                        testY,
                                        tteColname,
                                        eventColname,
                                        parallel,
                                        seed,
                                        ...) {
  glmnet(x = trainXs, y = trainY, family = "gaussian", ...)
}

fitMPRModelContinuousbiglasso <- function(trainXs,
                                          trainY,
                                          testXs,
                                          testY,
                                          tteColname,
                                          eventColname,
                                          parallel,
                                          seed,
                                          ...) {
  # To avoid 'LongVector not supported' error
  nCols <- ncol(trainXs)
  trainXsBig <- cbindBM(as.big.matrix(trainXs[, 1:(nCols / 2)]),
                        as.big.matrix(trainXs[, (nCols / 2 + 1):nCols]))
  biglasso(trainXsBig, trainY, family = "gaussian", ...)
}

fitMPRModelContinuousBART <- function(trainXs,
                                      trainY,
                                      testXs,
                                      testY,
                                      tteColname,
                                      eventColname,
                                      parallel,
                                      seed,
                                      ...) {
  if (is.null(testXs)) {
    testXs <- trainXs
  }
  if (parallel) {
    mc.gbart(x.train = trainXs,
             y.train = trainY,
             x.test = testXs,
             type = "wbart",
             seed = seed, 
            ...)
  } else {
    gbart(x.train = trainXs,
          y.train = trainY,
          x.test = testXs,
          type = "wbart",
          seed = seed,
          ...)
  }
}

fitMPRModelContinuousRF <- function(trainXs,
                                    trainY,
                                    testXs,
                                    testY,
                                    tteColname,
                                    eventColname,
                                    parallel,
                                    seed,
                                    ...) {
  set.seed(seed)
  randomForest(x = trainXs, y = trainY, ...)
}

fitMPRModelSurvivalglmnet <- function(trainXs,
                                      trainY,
                                      testXs,
                                      testY,
                                      tteColname,
                                      eventColname,
                                      parallel,
                                      seed,
                                      ...) {
  glmnet(x = trainXs,
         y = Surv(trainY[, tteColname], trainY[, eventColname]),
         family = "cox",
         ...)
}

fitMPRModelSurvivalbiglasso <- function(trainXs,
                                        trainY,
                                        testXs,
                                        testY,
                                        tteColname,
                                        eventColname,
                                        parallel,
                                        seed,
                                        ...) {
  # To avoid 'LongVector not supported' error
  nCols <- ncol(trainXs)
  trainXsBig <- cbindBM(as.big.matrix(trainXs[, 1:(nCols / 2)]),
                        as.big.matrix(trainXs[, (nCols / 2 + 1):nCols]))

  # BigLasso requires the y matrix column names to be 'time' and 'status'
  trainY <- trainY[, c(tteColname, eventColname)]
  colnames(trainY) <- c("time", "status")
  biglasso(trainXsBig, as.matrix(trainY), family = "cox", ...)
}

fitMPRModelSurvivalBART <- function(trainXs,
                                    trainY,
                                    testXs,
                                    testY,
                                    tteColname,
                                    eventColname,
                                    parallel,
                                    seed,
                                    ...) {
  mc.surv.bart(x.train = trainXs, y.train = trainY, ...)
}

fitMPRModelSurvivalRF <- function(trainXs,
                                  trainY,
                                  testXs,
                                  testY,
                                  tteColname,
                                  eventColname,
                                  parallel,
                                  seed,
                                  ...) {
  trainDF <- cbind(trainXs, trainY)
  rfsrc(as.formula(paste0("Surv(",
                          tteColname,
                          ", ",
                          eventColname,
                          ") ~ .")
                   ),
        trainDF,
        seed = seed,
        ...)
}
