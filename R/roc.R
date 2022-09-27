
#' multipleROC
#' Plot multiple ROC or PRROC curves with associated AUC values in the legend
#'
#' @param labelsList todo
#' @param predictionsList todo
#' @param title todo
#' @param pipeline.run.information todo
#' @param yMetric todo
#' @param xMetric todo
#' @param auc todo
#' @param legendPosition todo
#'
#' @return todo
#' @export
multipleROC <- function(labelsList,
                        predictionsList,
                        title,
                        pipeline.run.information,
                        yMetric = "tpr",
                        xMetric = "fpr",
                        auc = "ROC",
                        legendPosition = "bottomright") {
  tic("Create ROC curves.")
  # Create ROCR prediction and performance objects
  methodsList <- names(labelsList)
  predictionObjects <- lapply(methodsList, function(method) {
    prediction(predictionsList[[method]], labelsList[[method]])
  })
  performanceObjects <- lapply(predictionObjects, function(predictionObject) {
    performance(predictionObject, yMetric, xMetric)
  })

  # aucValues <- lapply(predictionObjects, function(predictionObject) {
  #   performance(predictionObject, 'auc')@y.values[[1]]
  # })

  if (auc == "ROC") {
    aucValues <- lapply(predictionObjects, function(predictionObject) {
      performance(predictionObject, "auc")@y.values[[1]]
    })
  } else if (auc == "PR") {
    aucValues <- lapply(methodsList, function(method) {
      PRAUC(predictionsList[[method]], labelsList[[method]])
    })
  }

  # qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
  # colourPalette = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
  # Remove first 4 colours
  # colourPalette <- colourPalette[5:length(colourPalette)]

  colourPalette <- brewer.pal(n = 8, "Dark2")

  plot(performanceObjects[[1]], col = colourPalette[[1]], main = title)
  if (length(methodsList) > 1) {
    lapply(2:length(methodsList), function(methodIndex) {
      plot(performanceObjects[[methodIndex]],
           col = colourPalette[[methodIndex]],
           add = TRUE)
    })
  }

  if (!is.null(auc)) {
    legendText <- lapply(1:length(methodsList), function(i) {
      paste0(methodsList[[i]], "; AUC = ", signif(aucValues[[i]], 3))
    })
  } else {
    legendText <- methodsList
  }


  # legendText <- lapply(1:length(methodsList), function(i) {
  #   paste0(methodsList[[i]], '; AUC = ', signif(aucValues[[i]], 3))
  # })

  legend(legendPosition,
         legend = legendText,
         bty = "n",
         col = colourPalette[1:length(methodsList)],
         lty = rep(1, length(methodsList)),
         lwd = rep(5, length(methodsList)))
  if (plotPRBaseline & auc == "PR") {
    baselinePrecision <- sum(labelsList[[1]] == 1) / length(labelsList[[1]])
    abline(h = baselinePrecision)
  }
  rocPlot <- recordPlot()

  toc(log = TRUE)
  list(rocPlot = rocPlot,
       performanceObjects = performanceObjects,
       aucValues = aucValues)
}
