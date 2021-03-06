% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pipeline_modules.R
\name{predictLogisticModel}
\alias{predictLogisticModel}
\title{Outputs predictions given a logistic model, data and labels.}
\usage{
predictLogisticModel(
  model,
  xs,
  pipelineRunInformation,
  modelLabel,
  predictType = "class"
)
}
\arguments{
\item{model}{A glmnet logistic regression model.}

\item{xs}{A matrix/data.frame corresponding to X variables. Columns should correspond to features.}

\item{pipelineRunInformation}{The object originally created by begin.pipeline.run.}

\item{modelLabel}{A string that will act as a label for this function call. The label will be used as a prefix for the saved object filenames in this function call.}

\item{predictType}{A string specifying the prediction type ('class' or 'response').}
}
\value{
A vector of model predictions.
}
\description{
Outputs predictions given a logistic model, data and labels.
}
