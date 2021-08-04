
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
                        method, # 'glmnet', 'bart', or 'continuous'
                        trainXs, 
                        trainY, 
                        testXs = NULL, 
                        testY = NULL,
                        tteColname = 'time_to_event',
                        eventColname = 'Event',
                        parallel = FALSE,
                        seed = NULL,
                        ...) {
  
  # Check for NAs. The rest of the function assumes complete data
  checkNA(trainXs)
  checkNA(trainY)
  if (!is.null(testXs)) {
    checkNA(testXs)
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
        if (is.null(testXs)) {
          testXs <- trainXs
        }
        if (is.null(testY)) {
          testY <- trainY
        }
        randomForest(x = trainXs, y = as.factor(trainY), xtest = testXs, ytest = as.factor(testY), ...)
      }
    ),
    'survival' = list(
      'glmnet' = function() {
        glmnet(x = trainXs, y = Surv(trainY[, tteColname], trainY[, eventColname]), family = 'cox', ...)
      },
      'bart' = function() {
        mc.surv.bart(x.train = trainXs, y.train = trainY)
      },
      'rf' = rfsrc # TODO: replace with custom function
    ),
    'continuous' = list(
      'glmnet' = function() {
        glmnet(x = trainXs, y = trainY, family = 'gaussian', ...)
      },
      'bart' = mc.gbart,
      'rf' = randomForest # TODO: replace with custom function
    )
  )
  
  
  model <- fitFunctionLookup[[type]][[method]]()
  return(structure(list(model = model), class = 'MPRModel'))
}

fitMPRModelCV <- function() {
  
}

