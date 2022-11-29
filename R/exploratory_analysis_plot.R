
formatPlot <- function(plotToFormat, title, xLabel, yLabel) {
  plotToFormat + ggtitle(title) + xlab(xLabel) + ylab(yLabel)
}

#' plotSingleVariableDensity
#'
#' @param plotData
#' @param plotVariable
#' @param title
#' @param xLabel
#' @param yLabel
#' @param pipelineRunInformation
#'
#' @return ggplot object
#' @export
plotSingleVariableDensity <- function(plotData,
                                      plotVariable,
                                      title,
                                      xLabel,
                                      yLabel,
                                      pipelineRunInformation) {
  tic(paste0("Created plot for ", plotVariable))
  p <- ggplot(plotData, aes(x = !!sym(plotVariable))) +
    geom_density(colour = "black", fill = "white")
  p <- formatPlot(p, title, xLabel, yLabel)
  if (pipelineRunInformation[["log"]]) {
    ggsave(paste0(pipelineRunInformation[["log folder path"]],
                  "_",
                  title,
                  ".svg"),
           p)
  }
  toc(log = TRUE)
  p
}

#' plotVariableByGroupDensity
#'
#' @param plotData data.frame containing variables to be plotted (at least two
#'   column titled <plotVariable> and <groupVariable>)
#' @param plotVariable The name of the variable to be plotted
#' @param groupVariable The name of the variable representing the individual
#'   groups to be plotted
#' @param title The title of the plot
#' @param xLabel The x-axis label
#' @param yLabel The y axis label
#' @param pipelineRunInformation the object originally returned by
#' \code{beginPipelineRun}
#' @param filenamePrefix The prefix for the file name of the saved plot object
#'
#' @return The ggplot object for the created grouped density plot
#' @export
plotVariableByGroupDensity <- function(plotData,
                                       plotVariable,
                                       groupVariable,
                                       title,
                                       xLabel,
                                       yLabel,
                                       pipelineRunInformation,
                                       filenamePrefix = NULL) {
  tic(paste0("Created plot for ", plotVariable, " grouped by ", groupVariable))
  p <- ggplot(plotData,
              aes(x = !!sym(plotVariable), fill = !!sym(groupVariable))) +
    geom_density(alpha = 0.5, position = "identity")
  p <- formatPlot(p, title, xLabel, yLabel)
  if (pipelineRunInformation[["log"]] & !is.null(filenamePrefix)) {
    ggsave(paste0(pipelineRunInformation[["log folder path"]],
                  filenamePrefix,
                  "_",
                  pipelineRunInformation[["start timestamp"]],
                  ".png"),
           p)
  }
  toc(log = TRUE)
  p
}
