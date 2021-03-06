% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pipeline_modules.R
\name{incrementalTest}
\alias{incrementalTest}
\title{incrementalTest
Fits incremental logistic models (a null model and a full model corresponding to covariates only and covariates+score respectively).}
\usage{
incrementalTest(
  X,
  yColname,
  covColnames,
  scoreColname,
  family = "binomial",
  pipelineRunInformation
)
}
\arguments{
\item{X}{A matrix/data.frame with columns corresponding to the labels, all covariates and the score to be evaluated.}

\item{yColname}{The name of the column in X corresponding to the labels.}

\item{covColnames}{A list/vector of the column names corresponding to covariates.}

\item{scoreColname}{The name of the column in X corresponding to the score to be evaluated.}

\item{family}{The glm model family. Default is 'binomial' for logistic regression.}

\item{pipelineRunInformation}{The object originally created by beginPipelineRun.}
}
\value{
A list containing results for the null and full models. For both model types, the formula string, glm model object, response, AUC and PRAUC are returned.
}
\description{
incrementalTest
Fits incremental logistic models (a null model and a full model corresponding to covariates only and covariates+score respectively).
}
