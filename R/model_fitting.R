
#' Title
#'
#' @param type 
#' @param method 
#' @param trainXs 
#' @param trainY 
#' @param testXs 
#' @param testY 
#' @param tteColname
#' @param eventColname
#' @param parallel 
#' @param seed 
#' @param ... 
#'
#' @return
#' @export
fitMPRModel <- function(type, # 'binary', 'survival', or 'continuous'
                        method, # 'glmnet', 'bart', or 'rf'
                        trainXs, 
                        trainY, 
                        testXs = NULL, 
                        testY = NULL,
                        tteColname = 'time_to_event',
                        eventColname = 'Event',
                        parallel = FALSE,
                        seed = NULL,
                        save = TRUE,
                        ...) {
  
  # Check input type and presence of missing values. The rest of the function assumes complete data
  # If trainY and testY are tables i.e. when type == 'survival', only the columns specified by tteColname and eventColname are checked for NAs 
  checkNA(trainXs)
  if (type == 'survival') {
    checkNA(trainY[, tteColname])
    checkNA(trainY[, eventColname])
  } else {
    checkNA(trainY)
  }
  
  checkMatrixOrDF(trainXs)
  if (!is.null(testXs)) {
    checkNA(testXs)
    checkMatrixOrDF(testXs)
  }
  if (!is.null(testY)) {
    if (type == 'survival') {
      checkNA(testY[, tteColname])
      checkNA(testY[, eventColname])
    } else {
      checkNA(testY)
    }
    
  }
  
  # If using a survival model, check that the specified column names exist
  if (type == 'survival') {
    if (!(tteColname %in% colnames(trainY))) {
      stop(paste0('Specified tteColname ', tteColname, ' not present in trainY'))
    }
    if (!(eventColname %in% colnames(trainY))) {
      stop(paste0('Specified eventColname ', eventColname, ' not present in trainY'))
    }
    if (!is.null(testY)) {
      if (!(tteColname %in% colnames(testY))) {
        stop(paste0('Specified tteColname ', tteColname, ' not present in testY'))
      }
      if (!(eventColname %in% colnames(testY))) {
        stop(paste0('Specified eventColname ', eventColname, ' not present in testY'))
      }
    }
  }
  
  fitFunctionLookup <- list(
    'binary' = list(
      'glmnet' = function() {
        glmnet(x = trainXs, y = trainY, family = 'binomial', ...)
      },
      'bart' = function() {
        if (is.null(testXs)) {
          testXs <- trainXs
        }
        if (parallel) {
          mc.gbart(x.train = trainXs, y.train = trainY, x.test = testXs, type = 'pbart', seed = seed, ...)
        } else {
          gbart(x.train = trainXs, y.train = trainY, x.test = testXs, type = 'pbart', seed = seed, ...)
        }
      },
      'rf' = function() {
        # if (is.null(testXs)) {
        #   testXs <- trainXs
        # }
        # if (is.null(testY)) {
        #   testY <- trainY
        # }
        randomForest(x = trainXs, y = as.factor(trainY), ...)
      }
    ),
    'survival' = list(
      'glmnet' = function() {
        glmnet(x = trainXs, y = Surv(trainY[, tteColname], trainY[, eventColname]), family = 'cox', ...)
      },
      'bart' = function() {
        mc.surv.bart(x.train = trainXs, y.train = trainY, ...)
      },
      'rf' = function() {
        trainDF <- cbind(trainXs, trainY)
        rfsrc(as.formula(paste0('Surv(', tteColname, ', ', eventColname, ') ~ .')), trainDF, seed = seed, ...)
      }
    ),
    'continuous' = list(
      'glmnet' = function() {
        glmnet(x = trainXs, y = trainY, family = 'gaussian', ...)
      },
      'bart' = function() {
        if (is.null(testXs)) {
          testXs <- trainXs
        }
        if (parallel) {
          mc.gbart(x.train = trainXs, y.train = trainY, x.test = testXs, type = 'wbart', seed = seed, ...)
        } else {
          gbart(x.train = trainXs, y.train = trainY, x.test = testXs, type = 'wbart', seed = seed, ...)
        }
      },
      'rf' = function() {
        # if (is.null(testXs)) {
        #   testXs <- trainXs
        #   testY <- trainY
        # }
        set.seed(seed)
        randomForest(x = trainXs, y = trainY, ...)
      }
    )
  )
  
  
  model <- fitFunctionLookup[[type]][[method]]()
  modelObject <- structure(list(model = model, modelType = type, modelMethod = method), class = 'MPRModel')
  saveMPRModelObject(modelObject)
  modelObject
}

#' Title
#'
#' @param type 
#' @param method 
#' @param trainXs 
#' @param trainY 
#' @param testXs 
#' @param testY 
#' @param tteColname 
#' @param eventColname 
#' @param parallel 
#' @param seed 
#' @param nFolds 
#' @param foldID 
#'
#' @return
#' @export
#'
#' @examples
fitMPRModelCV <- function(type, # 'binary', 'survival', or 'continuous'
                          method, # 'glmnet', 'bart', or 'rf'
                          trainXs, 
                          trainY, 
                          testXs = NULL, 
                          testY = NULL,
                          tteColname = 'time_to_event',
                          eventColname = 'Event',
                          parallel = FALSE,
                          seed = NULL,
                          nFolds = 3,
                          foldID = NULL,
                          ...) {
  # Check input type and presence of missing values. The rest of the function assumes complete data
  checkNA(trainXs)
  checkNA(trainY)
  checkMatrixOrDF(trainXs)
  if (!is.null(testXs)) {
    checkNA(testXs)
    checkMatrixOrDF(testXs)
  }
  if (!is.null(testY)) {
    checkNA(testY)
  }
  
  # If using a survival model, check that the specified column names exist
  if (type == 'survival') {
    if (!(tteColname %in% colnames(trainY))) {
      stop(paste0('Specified tteColname ', tteColname, ' not present in trainY'))
    }
    if (!(eventColname %in% colnames(trainY))) {
      stop(paste0('Specified eventColname ', eventColname, ' not present in trainY'))
    }
    if (!is.null(testY)) {
      if (!(tteColname %in% colnames(testY))) {
        stop(paste0('Specified tteColname ', tteColname, ' not present in testY'))
      }
      if (!(eventColname %in% colnames(testY))) {
        stop(paste0('Specified eventColname ', eventColname, ' not present in testY'))
      }
    }
  }
  fitFunctionLookup <- list(
    'binary' = list(
      'glmnet' = function() {
        cv.glmnet(x = trainXs, y = trainY, family = 'binomial', nfolds = nFolds, foldid = foldID, ...)
      },
      'bart' = function() {
        if (is.null(testXs)) {
          testXs <- trainXs
        }
        if (parallel) {
          mc.gbart(x.train = trainXs, y.train = trainY, x.test = testXs, type = 'pbart', seed = seed, ...)
        } else {
          gbart(x.train = trainXs, y.train = trainY, x.test = testXs, type = 'pbart', seed = seed, ...)
        }
      },
      'rf' = function() {
        rfCVResult <- rfCVGridSearch(xs = trainXs, y = as.factor(trainY), nFolds = nFolds, foldID = foldID, metric = 'AUC', seed = seed, ...)
        rfCVResult
      }
    ),
    'survival' = list(
      'glmnet' = function() {
        cv.glmnet(x = trainXs, y = Surv(trainY[, tteColname], trainY[, eventColname]), family = 'cox', nfolds = nFolds, foldid = foldID, ...)
      },
      'bart' = function() {
        mc.surv.bart(x.train = trainXs, y.train = trainY)
      },
      'rf' = function() {
        # TODO: replace with CV version
        trainDF <- cbind(trainXs, trainY)
        rfsrc(as.formula(paste0('Surv(', tteColname, ', ', eventColname, ') ~ .')), trainDF, seed = seed)
      }
    ),
    'continuous' = list(
      'glmnet' = function() {
        cv.glmnet(x = trainXs, y = trainY, family = 'gaussian', nfolds = nFolds, foldid = foldID, ...)
      },
      'bart' = function() {
        # TODO: Complete
      },
      'rf' = function() {
        rfCVResult <- rfCVGridSearch(xs = trainXs, y = trainY, nFolds = nFolds, foldID = foldID, metric = 'RMSE', seed = seed, ...)
        rfCVResult
      }
    )
  )
  model <- fitFunctionLookup[[type]][[method]]()
  structure(list(model = model, modelType = type, modelMethod = method), class = 'MPRModel')
}

#' Title
#'
#' @param X 
#' @param yColname 
#' @param covColnames 
#' @param scoreColname 
#' @param family 
#'
#' @return
#' @export
#'
#' @examples
fitMPRModelIncremental <- function(X, yColname, covColnames, scoreColname, family = 'binomial') {
  nullFormulaString <- paste(yColname, paste(covColnames, collapse = ' + '), sep = ' ~ ')
  nullModel <- glm(as.formula(nullFormulaString), family = family, data = X)
  
  fullFormulaString <- paste(nullFormulaString, scoreColname, sep = ' + ')
  fullModel <- glm(as.formula(fullFormulaString), family = family, data = X)
  
  nullModelResponse <- predict(nullModel, type = 'response')
  fullModelResponse <- predict(fullModel, type = 'response')
  
  
  # nullAUC <- pROC::roc(X[, yColname], nullModelResponse)$auc
  # fullAUC <- pROC::roc(X[, yColname], fullModelResponse)$auc
  # nullPRAUC <- MLmetrics::PRAUC(nullModelResponse, X[, yColname])
  # fullPRAUC <- MLmetrics::PRAUC(fullModelResponse, X[, yColname])
  
  list(null = list(formula = nullFormulaString,
                   model = nullModel,
                   response = nullModelResponse
                   # auc = nullAUC,
                   # prauc = nullPRAUC
                   ),
       full = list(formula = fullFormulaString,
                   model = fullModel,
                   response = fullModelResponse
                   # auc = fullAUC,
                   # prauc = fullPRAUC
                   )
  )
}