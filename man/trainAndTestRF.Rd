% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rf.R
\name{trainAndTestRF}
\alias{trainAndTestRF}
\title{trainAndTestRF}
\usage{
trainAndTestRF(
  trainXs,
  trainY,
  testXs = NULL,
  testY = NULL,
  metric = "AUC",
  ntree = 500,
  mtry = NULL,
  nodesize = NULL,
  pipelineRunInformation,
  logOverride = NULL
)
}
\arguments{
\item{trainXs}{A matrix/data.frame corresponding to X (variables) in the
training set. Columns should correspond to features}

\item{trainY}{A vector/list corresponding to Y (labels) in the
training set.}

\item{testXs}{A matrix/data.frame corresponding to X (variables) in the test
set. Columns should correspond to the same features in \code{trainXs}. If
not provided, \code{trainXs} is used as \code{testXs}.}

\item{testY}{A vector/list corresponding to Y (labels) in the test set. If
not provided, \code{trainY} is used as \code{testY}.}

\item{metric}{'AUC' or 'PRAUC'. The metric to be calculated on test set
prediction.}

\item{ntree}{The number of trees to fit in the model.}

\item{mtry}{The number of features to be selected at random and considered at
each node during tree fitting.}

\item{nodesize}{The minimum size of terminal nodes.}

\item{pipelineRunInformation}{The object originally created by
\code{\link{beginPipelineRun}}.}

\item{logOverride}{\code{NULL}, \code{TRUE} or \code{FALSE}. Used to override
the 'log' entry in \code{pipelineRunInformation}. Ignored if NULL.}
}
\value{
A list containing the model and the metric calculated for test set
  prediction.
}
\description{
trainAndTestRF
}
