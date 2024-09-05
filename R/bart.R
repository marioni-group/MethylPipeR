
checkBARTAvailable <- function(functionName) {
  if (!requireNamespace("BART", quietly = TRUE)) {
    stop(
      paste0('Package "BART" is required for the function ', functionName, '.'),
      call. = FALSE
    )
  }
}

#' fitBARTSurvival
#'
#' @param trainXs A matrix/data.frame of X variables to be used in the model
#'   fitting. Columns should correspond to variables.
#' @param trainTarget A matrix/data.frame including columns named 'Event' and
#'   'time_to_event' corresponding to a binary indicator of event/censoring and
#'   the time to event/censoring respectively.
#' @param testXs A matrix/data.frame of out-of-sample data to be used in model
#'   evaluation. The set of variables should be the same as those in
#'   \code{trainXs}.
#' @param pipelineRunInformation The object originally created by
#'   \code{\link{beginPipelineRun}}.
#' @param sparse Set to \code{TRUE} if the sparsity inducing Dirichlet prior
#'   should be used for splitting rule variable selection.
#' @param mcCores The number of cores to be used by the MCMC sampler.
#' @param nTree The number of trees to be used by BART.
#' @param k The value of the hyperparameter k.
#' @param nSkip The number of MCMC samples that should be treated as burn-in and
#' discarded.
#' @param type \code{'pbart'} (probit) or \code{'lbart'} (logit)
#'
#' @return The fit BART survival model object of type
#'   \code{\link[BART]{surv.bart}}.
#' @export
fitBARTSurvival <- function(trainXs,
                            trainTarget,
                            testXs,
                            pipelineRunInformation,
                            sparse = FALSE,
                            mcCores = 10,
                            nTree = 100,
                            k = 2,
                            nSkip = 500,
                            type = "pbart") {
  checkBARTAvailable("fitBARTSurvival")
  tic("Fit BART survival model.")
  model <- BART::mc.surv.bart(
    x.train = trainXs,
    times = trainTarget[, "time_to_event"],
    delta = trainTarget[, "Event"],
    x.test = testXs,
    ntree = nTree,
    ndpost = 1000,
    keepevery = 10L,
    printevery = 10L,
    nskip = nSkip,
    seed = pipelineRunInformation[["random seed"]],
    mc.cores = mcCores,
    sparse = sparse,
    k = k,
    type = type
  )
  toc(log = TRUE)

  if (pipelineRunInformation[["log"]]) {
    tic("Save BART survival model.")
    saveRDS(
      model,
      paste0(
        pipelineRunInformation[["log folder path"]],
        "bart_survival_model_",
        pipelineRunInformation[["start timestamp"]],
        ".rds"
      )
    )
    toc(log = TRUE)
  }

  model
}

cvBARTNYearOnset <- function(xs,
                             target,
                             nTreeList,
                             nFolds,
                             foldID,
                             predictionTimePoint,
                             pipelineRunInformation,
                             sparse = FALSE,
                             mcCores = 10,
                             kList = c(2)) {
  checkBARTAvailable("cvBARTNYearOnset")
  cvTrainAndValidate <- function(nTree, k, validationFoldID) {
    validationIndex <- foldID == validationFoldID
    trainIndex <- !validationIndex

    validationXs <- xs[validationIndex, ]
    validationTarget <- target[validationIndex, ]

    trainXs <- xs[trainIndex, ]
    trainTarget <- target[trainIndex, ]

    bartSurvivalModel <- fitBARTSurvival(trainXs,
      trainTarget,
      validationXs,
      pipelineRunInformation,
      sparse,
      mcCores,
      nTree,
      k = k
    )

    numberOfTimePoints <- bartSurvivalModel$K
    predictionTimePointIndex <- match(
      predictionTimePoint,
      bartSurvivalModel$times
    )

    # Given an index for an individual (a row in the dataset), a time point and
    # the total number of time points, returns the corresponding column index in
    # bart.survival.model$surv.test (or any result with the same structure).
    getBARTResultColumn <- function(individual, timePointIndex, nTimePoints) {
      (individual - 1) * nTimePoints + timePointIndex
    }

    survivalMeanPredictions <- sapply(
      1:nrow(validationXs),
      function(individual) {
        bartSurvivalModel$surv.test.mean[[getBARTResultColumn(
          individual,
          predictionTimePointIndex,
          numberOfTimePoints
        )]]
      }
    )

    # Perform TTE thresholding for validation set and survival.predictions
    thresholdTTEResult <- thresholdTTE(
      validationTarget,
      list(
        validationXs = validationXs,
        survivalMeanPredictions = survivalMeanPredictions
      ),
      predictionTimePoint
    )
    validationTarget <- thresholdTTEResult$targetFiltered
    row.names(validationTarget) <- NULL
    validationXs <- thresholdTTEResult$objectsFiltered[[1]]
    survivalMeanPredictions <- thresholdTTEResult$objectsFiltered[[2]]
    thresholdTTEResult <- NULL
    gc()

    # event.probability is calculated as 1 - survival probability
    eventPredictions <- 1 - survivalMeanPredictions

    # Calculate AUC
    rocResult <- roc(validationTarget[, "Event"], eventPredictions)
    aucResult <- auc(rocResult)

    # Free up memory
    trainXs <- NULL
    trainTarget <- NULL
    validationXs <- NULL
    validationTarget <- NULL
    bartSurvivalModel <- NULL
    survivalMeanPredictions <- NULL
    invisible(gc())

    message(paste0(
      "validationFoldID = ",
      validationFoldID,
      ", ntree = ",
      nTree,
      ", k = ",
      k,
      ", auc = ",
      aucResult
    ))
    aucResult
  }

  cvIter <- function(nTree, k) {
    aucs <- sapply(1:nFolds, function(foldNumber) {
      cvTrainAndValidate(nTree, k, foldNumber)
    })
    mean(aucs)
  }


  meanAUCs <- data.frame(
    nTree = integer(0),
    k = integer(0),
    auc = double(0),
    stringsAsFactors = FALSE
  )

  for (nTree in nTreeList) {
    for (k in kList) {
      meanAUC <- cvIter(nTree, k)
      meanAUCs <- rbind(meanAUCs, list(ntree = nTree, k = k, auc = meanAUC))
    }
  }
  meanAUCs
}
