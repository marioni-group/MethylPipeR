
#' To be called at the beginning of a pipeline run.
#'
#' @param note A string describing the pipeline run. This will be saved in the
#'   pipeline run log.
#' @param randomSeed A number to be used as the random seed in pipeline function
#'   calls.
#' @param log TRUE iff the pipeline log should be saved to file.
#' @param logFolderPath if log is TRUE, the log file will be saved in the folder
#'   specified by logFolderPath.
#' @return A list containing pipeline run information. Many of the pipeline
#' functions require this list to be passed in as an argument.
#' @export
beginPipelineRun <- function(note, randomSeed = 42, log = TRUE, logFolderPath) {
  pipelineRunInformation <- list()
  pipelineRunInformation[["note"]] <- note
  pipelineRunInformation[["log"]] <- log
  pipelineRunInformation[["log folder path"]] <- logFolderPath
  pipelineRunInformation[["random seed"]] <- randomSeed
  pipelineRunInformation[["start timestamp"]] <- format(Sys.time(),
                                                        "%Y_%m_%d_%H_%M_%S")
  if (log) {
    write(paste0(pipelineRunInformation[["start timestamp"]], ": ", note),
          paste0(logFolderPath, "run_notes.txt"), append = TRUE)
    tic(paste0("Finished pipeline run. Log files have been saved in ",
               logFolderPath))
  } else {
    tic("Finished pipeline run. Log files have not been saved")
  }
  print("Starting pipeline run.")

  return(pipelineRunInformation)
}


#' To be called at the end of a pipeline run. Saves the pipelineRunInformation 
#' to a file. Saves the pipeline timing logs to a file.
#'
#' @param pipelineRunInformation The list of pipeline run information entries
#'   (originally output by \code{beginPipelineRun}).
#' @export
endPipelineRun <- function(pipelineRunInformation) {
  if (pipelineRunInformation[["log"]]) {
    # Export pipeline run information list
    saveRDS(pipelineRunInformation,
            paste0(pipelineRunInformation[["log folder path"]],
                   "pipeline_run_information_",
                   pipelineRunInformation[["start timestamp"]],
                   ".rds"))
  }
  # toc for whole pipeline run tictoc
  toc(log = TRUE)

  # Write timing logs to file
  if (pipelineRunInformation[["log"]]) {
    lapply(tic.log(), write, paste0(pipelineRunInformation[["log folder path"]],
                                    "pipeline_tictoc_",
                                    pipelineRunInformation[["start timestamp"]],
                                    ".txt"),
           append = TRUE,
           ncolumns = 1000)
  }
}


#' Load data from a file. Currently supports .rds and .csv files
#'
#' @param dataFilepath The filepath of the data to be loaded. Currently supports
#'   '.rds' and '.csv' endings
#' @return The loaded data object
#' @export
loadData <- function(dataFilepath) {
  tic(paste0("Load data from ", dataFilepath))
  if (endsWith(tolower(dataFilepath), ".rds")) {
    loadedData <- readRDS(dataFilepath)
  } else if (endsWith(tolower(dataFilepath), ".csv")) {
    loadedData <- read.csv(dataFilepath)
  } else {
    print("File format suffix not recognised.")
    loadedData <- NULL
  }
  toc(log = TRUE)
  return(loadedData)
}


#' Fits a penalised logistic regression model with cross-validation.
#'
#' @param xs A matrix/data.frame corresponding to X variables. Columns should
#'   correspond to features.
#' @param y A matrix/data.frame with a column named 'Event' corresponding to a
#'   binary label.
#' @param penalty \code{'lasso'}/\code{'elnet'}/\code{'ridge'} A string
#'   specifying the penalty.
#' @param weights A vector of weights corresponding to the training data rows to
#'   be used in model fitting.
#' @param nFoldsCV The number of folds to be used in cross-validation (default
#'   is 3)
#' @param foldID A vector of numbers specifying a cross-validation fold. Each
#'   entry should correspond to a row in \code{xs}.
#' @param pipelineRunInformation The object originally created by
#'   \code{\link{beginPipelineRun}}
#' @param modelLabel A string that will act as a label for this function call.
#'   The label will be used as a prefix in the 'penalty' and 'number of folds'
#'   entries in pipeline.run.information. It will also be used as a prefix for
#'   the saved object filenames in this function call.
#' @param prescaled A boolean specifying if the columns in \code{xs} have been
#'   scaled before the call to fitLogisticModel e.g. using \code{scale()}. If
#'   set to TRUE, this sets \code{standardize = FALSE} in the internal call to
#'   \code{cv.glmnet}
#' @param typeMeasure A string specifying the argument to type.measure in
#' \code{cv.glmnet}. Defaults to \code{'default'}.
#' @param penaltyFactor A vector of numbers indicating variable-specific weights
#'   for the lambda penalty.
#' @return The glmnet logistic model object.
#' @export
fitLogisticModel <- function(xs,
                             y,
                             penalty,
                             weights,
                             nFoldsCV = 3,
                             foldID = NULL,
                             pipelineRunInformation,
                             modelLabel,
                             prescaled = FALSE,
                             typeMeasure = "default",
                             penaltyFactor = NULL) {
  tic("Fit logistic regression model")
  checkMatrixOrDF(xs)
  pipelineRunInformation[[paste0(modelLabel, " penalty")]] <- penalty
  pipelineRunInformation[[paste0(modelLabel, " number of folds")]] <- nFoldsCV
  model <- fitModelCVFunctions[["logistic"]](xs,
                                             y[, "Event"],
                                             penalty = penalty,
                                             weights = weights,
                                             nFoldsCV,
                                             pipelineRunInformation[["random seed"]],
                                             foldID,
                                             prescaled = prescaled,
                                             typeMeasure = typeMeasure,
                                             penaltyFactor = penaltyFactor)
  toc(log = TRUE)
  if (pipelineRunInformation[["log"]]) {
    tic(paste0("Save logistic ",
               penalty,
               " regression model. Label: ",
               modelLabel))
    saveRDS(model, paste0(pipelineRunInformation[["log folder path"]],
                          modelLabel, "_logistic_model_fit_",
                          pipelineRunInformation[["start timestamp"]],
                          ".rds"))
    toc(log = TRUE)
  }

  return(model)
}


#' Outputs predictions given a logistic model, data and labels.
#'
#' @param model A glmnet logistic regression model.
#' @param xs A matrix/data.frame corresponding to X variables. Columns should
#'   correspond to features.
#' @param pipelineRunInformation The object originally created by
#'   \code{\link{beginPipelineRun}}
#' @param modelLabel A string that will act as a label for this function call.
#'   The label will be used as a prefix for the saved object file names in this
#'   function call.
#' @param predictType A string specifying the prediction type (\code{'class'} or
#'   \code{'response'}).
#' @return A vector of model predictions.
#' @export
predictLogisticModel <- function(model,
                                 xs,
                                 pipelineRunInformation,
                                 modelLabel,
                                 predictType = "class") {
  tic("Predict using fit logistic regression model")
  checkMatrixOrDF(xs)
  modelPredictions <- predictModelCVFunctions[["logistic"]](model,
                                                            xs,
                                                            predictType)
  toc(log = TRUE)

  if (pipelineRunInformation[["log"]]) {
    tic("Save model predictions")
    saveRDS(modelPredictions,
            paste0(pipelineRunInformation[["log folder path"]],
                   modelLabel, "_logistic_model_predictions_",
                   pipelineRunInformation[["start timestamp"]],
                   ".rds"))
    toc(log = TRUE)
  }

  return(modelPredictions)
}


#' Given model predictions and labels, calculates binary classification
#' performance metrics
#'
#' @param modelPredictions A vector of model predictions.
#' @param y A matrix/data.frame with a column named 'Event' corresponding to a
#'   binary label.
#' @param pipelineRunInformation The object originally created by
#'   \code{\link{beginPipelineRun}}.
#' @param modelLabel A string that will act as a label for this function call.
#'   The label will be used as a prefix for the saved object filenames in this
#'   function call.
#' @return Object containing model metrics.
#' @export
calculateMetricsBinaryClassifier <- function(modelPredictions,
                                             y,
                                             pipelineRunInformation,
                                             modelLabel) {
  tic("Calculate logistic regression model metrics")
  modelMetrics <- as.matrix(confusionMatrix(as.factor(modelPredictions),
                                            as.factor(y[, "Event"]),
                                            positive = "1"),
                            what = "classes")
  pipelineRunInformation[[paste0(modelLabel, " model metrics")]] <- modelMetrics
  toc(log = TRUE)

  if (pipelineRunInformation[["log"]]) {
    tic("Save logistic regression model metrics")
    write.csv(modelMetrics,
              paste0(pipelineRunInformation[["log folder path"]],
                     modelLabel,
                     "_logistic_model_metrics_",
                     pipelineRunInformation[["start timestamp"]],
                     ".csv"))
    toc(log = TRUE)
  }
  return(modelMetrics)
}

#' Fit glmnet penalised cox model on training data.
#'
#' @param xs A matrix/data.frame corresponding to X variables. Columns should
#'   correspond to features.
#' @param target A matrix/data.frame with columns named 'Event' and
#'   'time_to_event'. 'Event' should correspond to a binary label.
#'   'time_to_event' should correspond to a positive numerical value
#'   representing the survival time (when Event == 1) or the time-to-censoring
#'   (when Event == 0)
#' @param penalty \code{'lasso'}/\code{'elnet'}/\code{'ridge'} A string
#'   specifying the penalty.
#' @param weights A vector of weights corresponding to the training data rows to
#'   be used in model fitting.
#' @param nFoldsCV The number of folds to be used in cross-validation (default
#'   is 3)
#' @param foldID A vector of numbers specifying a cross-validation fold. Each
#'   entry should correspond to a row in \code{xs}.
#' @param pipelineRunInformation The object originally created by
#'   \code{\link{beginPipelineRun}}.
#' @param modelLabel A string that will act as a label for this function call.
#'   The label will be used as a prefix for the saved object filenames in this
#'   function call.
#' @param prescaled A boolean specifying if the columns in \code{xs} have been
#'   scaled before the call to \code{\link{fitLogisticModel}} e.g. using
#'   \code{scale()}. If set to TRUE, this sets \code{standardize = FALSE} in the
#'   internal call to \code{cv.glmnet}
#' @param penaltyFactor A vector of numbers indicating variable-specific weights
#'   for the lambda penalty.
#' @return The fit penalised cox model glmnet object.
#' @export
fitCoxModel <- function(xs,
                        target,
                        penalty,
                        weights,
                        nFoldsCV = 3,
                        foldID = FALSE,
                        pipelineRunInformation,
                        modelLabel,
                        prescaled = FALSE,
                        penaltyFactor = NULL) {
  tic(paste0("Fit Cox ", penalty, " regression model. Label: ", modelLabel))
  checkMatrixOrDF(xs)
  pipelineRunInformation[[paste0(modelLabel, " penalty")]] <- penalty
  pipelineRunInformation[[paste0(modelLabel, " number of folds")]] <- nFoldsCV
  model <- fitModelCVFunctions[["cox"]](xs,
                                        Surv(target[, "time_to_event"],
                                             target[, "Event"]),
                                        penalty = penalty,
                                        weights = weights,
                                        nFoldsCV,
                                        pipelineRunInformation[["random seed"]],
                                        foldID,
                                        prescaled = prescaled,
                                        penaltyFactor = penaltyFactor)
  toc(log = TRUE)
  if (pipelineRunInformation[["log"]]) {
    tic(paste0("Save Cox regression model. Label: ", modelLabel))
    saveRDS(model,
            paste0(pipelineRunInformation[["log folder path"]],
                   modelLabel,
                   "_cox_model_fit_",
                   pipelineRunInformation[["start timestamp"]],
                   ".rds"))
    toc(log = TRUE)
  }
  return(model)
}

#' Calculate linear model predictions using glmnet Cox model.
#'
#' @param model A Cox glmnet model object.
#' @param xs A matrix/data.frame corresponding to X (variables). Columns should
#'   correspond to features.
#' @param pipelineRunInformation The object originally created by
#'   \code\{link{beginPipelineRun}}.
#' @param modelLabel A string that will act as a label for this function call.
#'   The label will be used as a prefix for the saved object file names in this
#'   function call.
#' @return A vector of model predictions.
#' @export
predictCoxModel <- function(model, xs, pipelineRunInformation, modelLabel) {
  tic(paste0("Predict using Cox regression model. Label: ", modelLabel))
  checkMatrixOrDF(xs)
  modelPredictions <- predictModelCVFunctions[["cox"]](model, xs)
  toc(log = TRUE)
  if (pipelineRunInformation[["log"]]) {
    tic(paste0("Save model predictions. Label: ", modelLabel))
    saveRDS(modelPredictions,
            paste0(pipelineRunInformation[["log folder path"]],
                   modelLabel, "_cox_model_predictions_",
                   pipelineRunInformation[["start timestamp"]],
                   ".rds"))
    toc(log = TRUE)
  }
  return(modelPredictions)
}

#' Retrieve non-zero coefficients from a glmnet model
#'
#' @param model A glmnet model object.
#' @param pipelineRunInformation The object originally created by
#'   \code{\link{beginPipelineRun}}.
#' @param modelLabel A string that will act as a label for this function call.
#'   The label will be used as a prefix for the saved object filenames in this
#'   function call.
#' @return A data.frame containing the non-zero model coefficients.
#' @export
getNonZeroCoefficients <- function(model, pipelineRunInformation, modelLabel) {
  tic("Calculate non-zero coefficients")
  modelCoefficients <- coef(model, s = "lambda.min")
  nonZeroModelCoefficients <- as.data.frame(modelCoefficients[which(modelCoefficients[, 1] != 0), ])
  toc(log = TRUE)

  if (pipelineRunInformation[["log"]]) {
    tic(paste0("Save non-zero coefficients. Label: ", modelLabel))
    write.csv(nonZeroModelCoefficients,
              paste0(pipelineRunInformation[["log folder path"]],
                     modelLabel,
                     "_model_non_zero_model_coefficients_",
                     pipelineRunInformation[["start timestamp"]],
                     ".csv"))
    toc(log = TRUE)
  }
  return(nonZeroModelCoefficients)
}


#' Fit coxph model with a predictor variable and a set of covariates.
#'
#' @param modelPredictions A vector of model predictions.
#' @param targetTable A matrix/data.frame with columns named 'Event' and
#'   'time_to_event'. 'Event' should correspond to a binary label.
#'   'time_to_event' should correspond to a positive numerical value
#'   representing the survival time (when Event == 1) or the time-to-censoring
#'   (when Event == 0)
#' @param covariatesTable A table that includes columns corresponding to
#'   covariates to be included in the model.
#' @param covariateColnames A named list with names corresponding to the
#'   covariate column names to be used in the model. The values in the list
#'   should correspond to the actual column name in \code{covariatesTable}
#' @param pipelineRunInformation The object originally created by
#' \code{\link{beginPipelineRun}}.
#' @param modelLabel A string that will act as a label for this function call.
#'   The label will be used as a prefix for the saved object filenames in this
#'   function call.
#' @return A named list consisting of the following elements:
#'   \code{'coxPHModel'} is the fit coxph model object (from the \code{survival}
#'   library). \code{'coxPHModelSummary'} is the result from calling
#'   \code{summary()} on \code{coxPHModel}. \code{'pseudoR2'} is the pseudo
#'   r-squared value for the model. \code{'hazardRatio'} is the hazard ratio for
#'   the predictor variable.
fitCoxPH <- function(modelPredictions,
                     targetTable,
                     covariatesTable,
                     covariateColnames,
                     pipelineRunInformation,
                     modelLabel) {
  tic(paste0("Fit coxph. Model label: ", modelLabel))
  coxPHData <- cbind.data.frame(time_to_event = targetTable[, "time_to_event"],
                                Event = targetTable[, "Event"],
                                modelPredictions = modelPredictions)
  for (coxPHDataColname in names(covariateColnames)) {
    coxPHData[, coxPHDataColname] <- covariatesTable[, covariateColnames[[coxPHDataColname]]]
  }
  coxPHModel <- coxph(as.formula(paste0("Surv(time_to_event, Event) ~ scale(modelPredictions)+",
                                        paste(names(covariateColnames),
                                              collapse = "+"))), 
                      coxPHData)

  coxPHModelSummary <- summary(coxPHModel)
  pseudoR2 <- coxPHModelSummary$rsq[[1]]
  hazardRatio <- coxPHModelSummary$conf.int[, -2] %>%
    cbind(pval = coxPHModelSummary$coefficients[, 5]) %>%
    head(1)
  toc(log = TRUE)

  if (pipelineRunInformation[["log"]]) {
    tic("Save coxph objects")
    saveRDS(coxPHModelSummary,
            paste0(pipelineRunInformation[["log folder path"]],
                   modelLabel, "_coxph_model_summary_",
                   pipelineRunInformation[["start timestamp"]],
                   ".rds"))
    toc(log = TRUE)
  }
  return(list(coxPHModel = coxPHModel,
              coxPHModelSummary = coxPHModelSummary,
              pseudoR2 = pseudoR2,
              hazardRatio = hazardRatio))
}


#' Assigns each row in the input data a fold number randomly while ensuring the
#' same number of cases/controls are assigned to each fold.
#'
#' @param data A matrix/data.frame with a column named <class.colname>
#'   corresponding to a binary indicator of case/control.
#' @param classColname The name of the column corresponding to a binary
#'   indicator of case/control.
#' @param nFolds The number of folds to be assigned.
#' @param pipelineRunInformation The object originally created by
#'   \code{\link{beginPipelineRun}}.
#' @return A vector with elements corresponding to rows in data. Each element is
#'   a number between 1-\code{nFolds} and represents which cross-validation fold
#'   the row is assigned to.
#' @export
assignTrainingFolds <- function(data,
                                classColname,
                                nFolds,
                                pipelineRunInformation) {
  tic("Assigning balanced training folds")
  # For cases and controls: shuffle indexes and assign an integer 1:nFolds on a
  # round robin basis
  set.seed(pipelineRunInformation[["random seed"]])

  # Save original row names to reorder foldIDs at the end.
  originalRowNames <- row.names(data)

  caseRowNames <- row.names(data[data[, classColname] == 1, ])
  caseRowNames <- sample(caseRowNames)
  caseFoldIDs <- 1:length(caseRowNames) %% nFolds + 1
  names(caseFoldIDs) <- caseRowNames

  controlRowNames <- row.names(data[data[, classColname] == 0, ])
  controlRowNames <- sample(controlRowNames)
  controlFoldIDs <- 1:length(controlRowNames) %% nFolds + 1
  names(controlFoldIDs) <- controlRowNames

  foldIDs <- c(caseFoldIDs, controlFoldIDs)
  # Reorder according to original row names
  foldIDs <- foldIDs[originalRowNames]
  # foldids <- sapply(1:length(foldIDs), function(num) {foldIDs[[toString(num)]]})
  toc(log = TRUE)
  return(foldIDs)
}

#' removeDeathsFromControls
#'
#' @param targetToFilter A matrix or data.frame with a binary 1/0 column named
#'   'dead' and a binary column named 'Event'
#' @param objectsToFilter A list of matrix or data.frame objects each with the
#'   same number of rows as targetToFilter. These objects will also have the
#'   same rows removed as targetToFilter.
#'
#' @return A list with elements \code{targetFiltered} (a matrix/data.frame) and
#'   \code{objectsToFilter} (a list of matrix/data.frames) corresponding to
#'   \code{targetToFilter} and \code{objectsToFilter} with rows corresponding to
#'   deaths in controls (dead == 1, Event == 0) removed
#' @export
removeDeathsFromControls <- function(targetToFilter, objectsToFilter) {
  tic(paste0("Remove deaths from controls"))
  deathControlIndexes <- (targetToFilter[, "dead"] == 1) & (targetToFilter[, "Event"] == 0)
  rowsToKeep <- !deathControlIndexes
  targetToFilter <- targetToFilter[rowsToKeep, ]
  objectsToFilter <- lapply(objectsToFilter, function(objectToFilter) {
    if (is.data.frame(objectToFilter) || is.matrix(objectToFilter)) {
      objectToFilter[rowsToKeep, ]
    } else if (is.vector(objectToFilter)) {
      objectToFilter[rowsToKeep]
    } else {
      stop("objectsToFilter not a data.frame, matrix or vector\n")
    }
  })
  toc(log = TRUE)
  gc()
  return(list(targetFiltered = targetToFilter,
              objectsFiltered = objectsToFilter))
}

removeByThreshold <- function(targetToFilter,
                              objectsToFilter,
                              variableName,
                              threshold,
                              lessThan = TRUE) {
  comparisonString <- ifelse(lessThan, "less than", "greater than or equal to")
  tic(paste0("Remove rows with ",
             variableName,
             " ",
             comparisonString,
             " ",
             threshold))
  rowsToKeep <- targetToFilter[, variableName] < threshold

  if (lessThan) {
    # If less.than is TRUE, keep the inverse selection
    rowsToKeep <- !rowsToKeep
  }

  targetToFilter <- targetToFilter[rowsToKeep, ]
  objectsToFilter <- lapply(objectsToFilter, function(objectToFilter) {
    if (is.data.frame(objectToFilter) || is.matrix(objectToFilter)) {
      objectToFilter[rowsToKeep, ]
    } else if (is.vector(objectToFilter)) {
      objectToFilter[rowsToKeep]
    } else {
      stop("objectToFilter not data.frame, matrix or vector\n")
    }
  })
  toc(log = TRUE)
  gc()
  return(list(targetFiltered = targetToFilter, objectsFiltered = objectsToFilter))
}

#' thresholdTTE
#'
#' @param targetToFilter A matrix or data.frame including a numerical column
#'   named 'time_to_event' and a binary 1/0 column named 'Event'. eventColname
#'   and/or tteColname must be specified if either or both of these columns do
#'   not have the default names.
#' @param objectsToFilter A list of matrix/data.frame objects each with the same
#'   number of rows as targetToFilter. These will have the same rows (according
#'   to index) removed as targetToFilter
#' @param threshold The time-to-event threshold which will be used.
#' @param eventColname The name for the event column. Must be specified if not
#'   'Event'.
#' @param tteColname The name for the time-to-event column. Must be specified if
#'   not 'time_to_event'
#'
#' @return A list with elements \code{targetFiltered} and \code{objectsFiltered}
#'   corresponding to \code{targetToFilter} and \code{objectsToFilter} after
#'   thresholding. Thresholding involves the following: Rows with Event == 1
#'   (cases) and time_to_event > threshold will be converted to Event == 0
#'   (controls). Rows with Event == 0 and time_to_event <= threshold will be
#'   removed.
#' @export
thresholdTTE <- function(targetToFilter,
                         objectsToFilter,
                         threshold,
                         eventColname = "Event",
                         tteColname = "time_to_event") {
  casesIndex <- targetToFilter[, eventColname] == 1
  controlsIndex <- targetToFilter[, eventColname] == 0
  lessThanOrEqualToThresholdIndex <- targetToFilter[, tteColname] <= threshold
  greaterThanThresholdIndex <- targetToFilter[, tteColname] > threshold

  casesKeptIndex <- casesIndex & lessThanOrEqualToThresholdIndex
  casesToControlsIndex <- casesIndex & greaterThanThresholdIndex
  controlsKeptIndex <- controlsIndex & greaterThanThresholdIndex
  controlsRemovedIndex <- controlsIndex & lessThanOrEqualToThresholdIndex

  # Convert cases to controls
  targetToFilter[casesToControlsIndex, eventColname] <- 0

  # Filter rows
  rowsKeptIndex <- casesKeptIndex | casesToControlsIndex | controlsKeptIndex
  targetToFilter <- targetToFilter[rowsKeptIndex, ]
  objectsToFilter <- lapply(objectsToFilter, function(objectToFilter) {
    if (is.data.frame(objectToFilter) || is.matrix(objectToFilter)) {
      objectToFilter[rowsKeptIndex, ]
    } else if (is.vector(objectToFilter)) {
      objectToFilter[rowsKeptIndex]
    } else {
      stop("objectToFilter not data.frame, matrix or vector\n")
    }
  })
  list(
    targetFiltered = targetToFilter,
    objectsFiltered = objectsToFilter,
    counts = list(
      casesKepty = sum(casesKeptIndex),
      casesToControls = sum(casesToControlsIndex),
      controlsKept = sum(controlsKeptIndex),
      controlsRemoved = sum(controlsRemovedIndex)
    )
  )
}


# Exploratory analysis functions
createTTEDensityPlot <- function(target,
                                 pipelineRunInformation,
                                 title = "Time-to-event by Case/Control",
                                 xlabel = "TTE",
                                 ylabel = "Density") {
  tic("Create time-to-event by Event density plot")
  densityPlot <- ggplot(target, aes(x = time_to_event, fill = Event)) +
    geom_density(alpha = 0.5, position = "identity") +
    ggtitle(title) +
    xlab(xlabel) +
    ylab(ylabel)

  if (pipelineRunInformation[["log"]]) {

  }
  toc(log = TRUE)
  return(densityPlot)
}

#' incrementalTest
#' Fits incremental logistic models (a null model and a full model corresponding
#' to covariates only and covariates+score respectively).
#'
#' @param X A matrix/data.frame with columns corresponding to the labels, all
#'   covariates and the score to be evaluated.
#' @param yColname The name of the column in X corresponding to the labels.
#' @param covColnames A list/vector of the column names corresponding to
#'   covariates.
#' @param scoreColname The name of the column in \code{X} corresponding to the
#'   score to be evaluated.
#' @param family The \code{\link[stats]{glm}} model family. Default is
#'   \code{'binomial'} for logistic regression.
#' @param pipelineRunInformation The object originally created by
#'   \code{\link{beginPipelineRun}}.
#' @return A list containing results for the null and full models. For both
#'   model types, the formula string, glm model object, response, AUC and PRAUC
#'   are returned.
#' @export
incrementalTest <- function(X,
                            yColname,
                            covColnames,
                            scoreColname,
                            family = "binomial",
                            pipelineRunInformation) {
  tic("Fit incremental logistic model")
  nullFormulaString <- paste(yColname,
                             paste(covColnames, collapse = " + "), sep = " ~ ")
  nullModel <- glm(as.formula(nullFormulaString), family = family, data = X)

  fullFormulaString <- paste(nullFormulaString, scoreColname, sep = " + ")
  fullModel <- glm(as.formula(fullFormulaString), family = family, data = X)

  nullModelResponse <- predict(nullModel, type = "response")
  fullModelResponse <- predict(fullModel, type = "response")


  nullAUC <- pROC::roc(X[, yColname], nullModelResponse)$auc
  fullAUC <- pROC::roc(X[, yColname], fullModelResponse)$auc
  nullPRAUC <- MLmetrics::PRAUC(nullModelResponse, X[, yColname])
  fullPRAUC <- MLmetrics::PRAUC(fullModelResponse, X[, yColname])

  if (pipelineRunInformation[["log"]]) {
    saveRDS(nullModel,
            paste0(pipelineRunInformation[["log folder path"]],
                   "incremental_logistic_null_model_",
                   pipelineRunInformation[["start timestamp"]],
                   ".rds"))
    saveRDS(fullModel,
            paste0(pipelineRunInformation[["log folder path"]],
                   "incremental_logistic_full_model_",
                   pipelineRunInformation[["start timestamp"]],
                   ".rds"))
    saveRDS(nullModelResponse,
            paste0(pipelineRunInformation[["log folder path"]],
                   "incremental_logistic_null_model_response_",
                   pipelineRunInformation[["start timestamp"]],
                   ".rds"))
    saveRDS(fullModelResponse,
            paste0(pipelineRunInformation[["log folder path"]],
                   "incremental_logistic_full_model_response_",
                   pipelineRunInformation[["start timestamp"]],
                   ".rds"))
  }

  toc(log = TRUE)
  list(
    null = list(
      formula = nullFormulaString,
      model = nullModel,
      response = nullModelResponse,
      auc = nullAUC,
      prauc = nullPRAUC
    ),
    full = list(
      formula = fullFormulaString,
      model = fullModel,
      response = fullModelResponse,
      auc = fullAUC,
      prauc = fullPRAUC
    )
  )
}
