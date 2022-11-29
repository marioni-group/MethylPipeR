
#' fitMPRModel
#'
#' @param type A string representing the type of model. Can be \code{'binary'},
#'   \code{'survival'} or \code{'continuous'}.
#' @param method The modelling method. Currently supports \code{'glmnet'},
#'   \code{'bart'} and \code{'rf'}.
#' @param trainXs The training data matrix/data.frame.
#' @param trainY The training response variable. This should be a vector if
#'   \code{type = 'binary'}/\code{'continuous'} or a data.frame/matrix with
#'   column names corresponding to \code{tteColname} and \code{eventColname} if
#'   \code{type = 'survival'}.
#' @param testXs The test data matrix/data.frame.
#' @param testY The test response variable. This should be a vector if
#'   \code{type = 'binary'}/\code{'continuous'} or a data.frame/matrix with
#'   column names corresponding to \code{tteColname} and \code{eventColname} if
#'   \code{type = 'survival'}.
#' @param tteColname A string corresponding to the time-to-event column name in
#'   \code{trainY}/\code{testY}. Only required if \code{type == 'survival'}.
#' @param eventColname A string corresponding to the event column name in
#'  \code{trainY}/\code{testY}. Only required if \code{type == 'survival'}.
#' @param parallel A boolean specifying whether parallel computation should be
#'   used in model fitting.
#' @param seed An integer to set the random seed to for model fitting.
#' @param save A boolean specifying if the model object should be saved to the
#'   logs.
#' @param saveSuffix Optional - a custom suffix to include in the file name of
#'   the saved model object after the timestamp.
#' @param ... Other arguments to be passed to the method-specific fitting
#'   function.
#'
#' @return A MPRModel object with the fitted model.
#' @export
fitMPRModel <- function(type, # 'binary', 'survival', or 'continuous'
                        method, # 'glmnet', 'bart', or 'rf'
                        trainXs,
                        trainY,
                        testXs = NULL,
                        testY = NULL,
                        tteColname = "time_to_event",
                        eventColname = "Event",
                        parallel = FALSE,
                        seed = NULL,
                        save = TRUE,
                        saveSuffix = NULL,
                        ...) {

  # Check input type and presence of missing values. The rest of the function
  # assumes complete data
  # If trainY and testY are tables i.e. when type == 'survival', only the
  # columns specified by tteColname and eventColname are checked for NAs
  checkNA(trainXs)
  if (type == "survival") {
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
    if (type == "survival") {
      checkNA(testY[, tteColname])
      checkNA(testY[, eventColname])
    } else {
      checkNA(testY)
    }
  }

  # If using a survival model, check that the specified column names exist
  if (type == "survival") {
    if (!(tteColname %in% colnames(trainY))) {
      stop("Specified tteColname ",tteColname, " not present in trainY\n")
    }
    if (!(eventColname %in% colnames(trainY))) {
      stop("Specified eventColname ", eventColname, " not present in trainY\n")
    }
    if (!is.null(testY)) {
      if (!(tteColname %in% colnames(testY))) {
        stop("Specified tteColname ", tteColname, " not present in testY\n")
      }
      if (!(eventColname %in% colnames(testY))) {
        stop("Specified eventColname ", eventColname, " not present in testY\n")
      }
    }
  }

  fitFunctionLookup <- list(
    "binary" = list(
      "glmnet" = fitMPRModelBinaryglmnet,
      "biglasso" = fitMPRModelBinarybiglasso,
      "bart" = fitMPRModelBinaryBART,
      "rf" = fitMPRModelBinaryRF
    ),
    "survival" = list(
      "glmnet" = fitMPRModelSurvivalglmnet,
      "biglasso" = fitMPRModelSurvivalbiglasso,
      "bart" = fitMPRModelSurvivalBART,
      "rf" = fitMPRModelSurvivalRF
    ),
    "continuous" = list(
      "glmnet" = fitMPRModelContinuousglmnet,
      "biglasso" = fitMPRModelContinuousbiglasso,
      "bart" = fitMPRModelContinuousBART,
      "rf" = fitMPRModelContinuousRF
    )
  )

  logSessionLines(
    "Fitting MPRModel.",
    paste0("Model type: ", type),
    paste0("Model method: ", method),
    paste0("Using cross-validation for hyperparameter selection: ", FALSE),
    "Additional fitMPRModel parameters:",
    capture.output(print(list(...)))
  )
  model <- fitFunctionLookup[[type]][[method]](trainXs,
                                               trainY,
                                               testXs,
                                               testY,
                                               tteColname,
                                               eventColname,
                                               parallel,
                                               seed,
                                               ...)
  modelObject <- structure(list(model = model,
                                modelType = type,
                                modelMethod = method),
                           class = "MPRModel")
  if (save) {
    saveMPRModelObject(modelObject, saveSuffix)
  }
  modelObject
}

#' fitMPRModelCV
#'
#' @inheritParams fitMPRModel
#' @param nFolds The number of cross-validation folds to use.
#' @param foldID A vector of integers with length equal to the number of rows in
#'   \code{trainXs}. Each element represents the fold number assigned to the
#'   corresponding row.
#' @param ... The remaining parameters to be passed to the cross-validation function.
#'
#' @return
#' @export
fitMPRModelCV <- function(type, # 'binary', 'survival', or 'continuous'
                          method, # 'glmnet', 'bart', or 'rf'
                          trainXs,
                          trainY,
                          testXs = NULL,
                          testY = NULL,
                          tteColname = "time_to_event",
                          eventColname = "Event",
                          parallel = FALSE,
                          seed = NULL,
                          save = TRUE,
                          nFolds = 3,
                          foldID = NULL,
                          saveSuffix = NULL,
                          ...) {
  # Check input type and presence of missing values. The rest of the function
  # assumes complete data
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
  if (type == "survival") {
    if (!(tteColname %in% colnames(trainY))) {
      stop("Specified tteColname ", tteColname, " not present in trainY\n")
    }
    if (!(eventColname %in% colnames(trainY))) {
      stop("Specified eventColname ", eventColname, " not present in trainY\n")
    }
    if (!is.null(testY)) {
      if (!(tteColname %in% colnames(testY))) {
        stop("Specified tteColname ", tteColname, " not present in testY\n")
      }
      if (!(eventColname %in% colnames(testY))) {
        stop("Specified eventColname ",
             eventColname,
             " not present in testY\n")
      }
    }
  }
  fitFunctionLookup <- list(
    "binary" = list(
      "glmnet" = function() {
        cv.glmnet(x = trainXs,
                  y = trainY,
                  family = "binomial",
                  nfolds = nFolds,
                  foldid = foldID,
                  ...)
      },
      "biglasso" = function() {
        nCols <- ncol(trainXs)

        trainXsBig <- cbindBM(as.big.matrix(trainXs[, 1:(nCols / 2)]),
                              as.big.matrix(trainXs[, (nCols / 2 + 1):nCols]))
        cv.biglasso(trainXsBig,
                    trainY,
                    family = "binomial",
                    nfolds = nFolds,
                    cv.ind = foldID,
                    ...)
      },
      "bart" = function() {
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
      },
      "rf" = function() {
        rfCVResult <- rfCVGridSearch(xs = trainXs,
                                     y = as.factor(trainY),
                                     nFolds = nFolds,
                                     foldID = foldID,
                                     metric = "AUC",
                                     seed = seed,
                                     ...)
        rfCVResult
      }
    ),
    "survival" = list(
      "glmnet" = function() {
        cv.glmnet(x = trainXs,
                  y = Surv(trainY[, tteColname],
                           trainY[, eventColname]),
                  family = "cox",
                  nfolds = nFolds,
                  foldid = foldID,
                  ...)
      },
      "biglasso" = function() {
        nCols <- ncol(trainXs)

        # BigLasso requires the y matrix column names to be 'time' and 'status'
        trainY <- trainY[, c(tteColname, eventColname)]
        colnames(trainY) <- c("time", "status")

        trainXsBig <- cbindBM(as.big.matrix(trainXs[, 1:(nCols / 2)]),
                              as.big.matrix(trainXs[, (nCols / 2 + 1):nCols]))
        cv.biglasso(trainXsBig,
                    trainY,
                    family = "cox",
                    nfolds = nFolds,
                    cv.ind = foldID,
                    ...)
      },
      "bart" = function() {
        mc.surv.bart(x.train = trainXs, y.train = trainY)
      },
      "rf" = function() {
        # TODO: replace with CV version
        trainDF <- cbind(trainXs, trainY)
        rfsrc(as.formula(paste0("Surv(",
                                tteColname,
                                ", ",
                                eventColname,
                                ") ~ .")),
              trainDF,
              seed = seed)
      }
    ),
    "continuous" = list(
      "glmnet" = function() {
        cv.glmnet(x = trainXs,
                  y = trainY,
                  family = "gaussian",
                  nfolds = nFolds,
                  foldid = foldID,
                  ...)
      },
      "biglasso" = function() {
        nCols <- ncol(trainXs)

        trainXsBig <- cbindBM(as.big.matrix(trainXs[, 1:(nCols / 2)]),
                              as.big.matrix(trainXs[, (nCols / 2 + 1):nCols]))
        cv.biglasso(trainXsBig,
                    trainY,
                    family = "gaussian",
                    nfolds = nFolds,
                    cv.ind = foldID,
                    ...)
      },
      "bart" = function() {
        # TODO: Complete
      },
      "rf" = function() {
        rfCVResult <- rfCVGridSearch(xs = trainXs,
                                     y = trainY,
                                     nFolds = nFolds,
                                     foldID = foldID,
                                     metric = "RMSE",
                                     seed = seed,
                                     ...)
        rfCVResult
      }
    )
  )
  logSessionLines(
    "Fitting MPRModel.",
    paste0("Model type: ", type),
    paste0("Model method: ", method),
    paste0("Using cross-validation for hyperparameter selection: ", TRUE),
    "Additional fitMPRModel parameters:",
    capture.output(print(list(...)))
  )
  model <- fitFunctionLookup[[type]][[method]]()
  modelObject <- structure(list(model = model,
                                modelType = type,
                                modelMethod = method),
                           class = "MPRModel")
  if (save) {
    saveMPRModelObject(modelObject, saveSuffix)
  }
  modelObject
}

#' fitMPRModelIncremental
#'
#' @param X A data.frame containing columns corresponding to \code{yColname},
#'   \code{covColnames} and \code{scoreColname}.
#' @param yColname The name of the column representing the response variable.
#' @param covColnames A vector of names representing the covariate columns.
#' @param scoreColname The name of the score column.
#' @param family The GLM family string. \code{'binomial'} for binary response.
#'   \code{'gaussian'} for continuous response.
#'
#' @return A list containing results for the null (covariates only) model and
#'   full model (score and covariates). For each model, the formula, response
#'   and \code{\link[stats]{glm}} model object are provided.
#' @export
fitMPRModelIncremental <- function(X,
                                   yColname,
                                   covColnames,
                                   scoreColname,
                                   family = "binomial") {
  nullFormulaString <- paste(yColname,
                             paste(covColnames, collapse = " + "),
                             sep = " ~ ")
  nullModel <- glm(as.formula(nullFormulaString), family = family, data = X)

  fullFormulaString <- paste(nullFormulaString, scoreColname, sep = " + ")
  fullModel <- glm(as.formula(fullFormulaString), family = family, data = X)

  nullModelResponse <- predict(nullModel, type = "response")
  fullModelResponse <- predict(fullModel, type = "response")


  # nullAUC <- pROC::roc(X[, yColname], nullModelResponse)$auc
  # fullAUC <- pROC::roc(X[, yColname], fullModelResponse)$auc
  # nullPRAUC <- MLmetrics::PRAUC(nullModelResponse, X[, yColname])
  # fullPRAUC <- MLmetrics::PRAUC(fullModelResponse, X[, yColname])

  list(
    null = list(
      formula = nullFormulaString,
      model = nullModel,
      response = nullModelResponse
      # auc = nullAUC,
      # prauc = nullPRAUC
    ),
    full = list(
      formula = fullFormulaString,
      model = fullModel,
      response = fullModelResponse
      # auc = fullAUC,
      # prauc = fullPRAUC
    )
  )
}
