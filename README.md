# MethylPipeR
This package provides functions for running machine learning prediction pipelines with a focus on systematic and reproducible experiments.

## Installation from GitHub
To install MethylPipeR in R, use the following:
```r
remotes::install_github('marioni-group/methylpiper')
```

## Pipeline Basics
Below is an example of a simple MethylPipeR script which fits a glmnet model to simulated training data with a continuous outcome. Cross validation is performed to select a value for lambda.
Prediction is then performed using a simulated test set. An incremental model is also fit to the test set using the predicted score and a set of covariates.

```r
library(MethylPipeR)

initLogs('/path/to/log/folder/')

trainingData <- readRDS('/path/to/training/data')
trainingTarget <- readRDS('/path/to/training/target')

mprModel <- fitMPRModelCV(type = 'continuous',
                          method = 'glmnet',
                          trainXs = trainingData,
                          trainY = trainingTarget,
                          seed = 42,
                          save = TRUE)

mprModelTrainingPredictions <- predictMPRModel(mprModel,
                                               data = trainingData,
                                               s = 'lambda.min')

testData <- readRDS('/path/to/test/data')
testTarget <- readRDS('/path/to/test/target')

mprModelTestPredictions <- predictMPRModel(mprModel,
                                           data = testData,
                                           s = 'lambda.min')

testCovariates <- readRDS('/path/to/test/covariates')

incrementalDF <- as.data.frame(testCovariates)
incrementalDF$score <- mprModelTestPredictions
incrementalDF$y <- testTarget

incrementalResult <- fitMPRModelIncremental(X = incrementalDF, yColname = 'y', covColnames = colnames(testCovariates), scoreColname = 'score', family = 'gaussian')
```

### Step-by-step
To set up a MethylPipeR session, first load the package and specify a folder for logs to go into. (This string must end in '/').
```r
library(MethylPipeR)

initLogs('/path/to/log/folder/')
```

Next we load the training data (matrix or data.frame with rows corresopnding to individuals and columns corresponding to variables e.g. CpG sites).
We also load the target (vector with each element i corresponding to the ith row in the training data).
```r
trainingData <- readRDS('/path/to/training/data')
trainingTarget <- readRDS('/path/to/training/target')
```

We next fit a glmnet model with continuous output. Cross-validation is used to determine the value of lambda. Training set predictions are then obtained.
```r
mprModel <- fitMPRModelCV(type = 'continuous',
                          method = 'glmnet',
                          trainXs = trainingData,
                          trainY = trainingTarget,
                          seed = 42,
                          save = TRUE)

mprModelTrainingPredictions <- predictMPRModel(mprModel,
                                               data = trainingData,
                                               s = 'lambda.min')
```

Similarly, we load the test data and target files and obtain test set predictions.
```r
testData <- readRDS('/path/to/test/data')
testTarget <- readRDS('/path/to/test/target')

mprModelTestPredictions <- predictMPRModel(mprModel,
                                           data = testData,
                                           s = 'lambda.min')
```

For incremental modeling, we load a matrix/data.frame of covariates e.g. age, sex and BMI. Rows should correspond to the same individuals as in testData. We then create the data.frame required for the incremental model fitting. 
```r
testCovariates <- readRDS('/path/to/test/covariates')

incrementalDF <- as.data.frame(testCovariates)
incrementalDF$score <- mprModelTestPredictions
incrementalDF$y <- testTarget
```

incrementalDF now has columns corresponding to the predicted score, target y and covariates. The incremental model is fit by running the following:
```r
incrementalResult <- fitMPRModelIncremental(X = incrementalDF, yColname = 'y', covColnames = colnames(testCovariates), scoreColname = 'score', family = 'gaussian')
```

Note: currently, console output no longer shows in the R console after initLogs is run. This is due to the output being piped into the console log file. This can be viewed in the folder specified in the initLogs call.

# Old MethylPipeR format
The remainder of this document describes the old format for MethylPipeR. This will be deprecated in the future but has been used in previous pipeline experiments and is included here for reference.
## Pipeline Basics
Pipeline instances are defined in an R script with the following function calls:

```r
runInformation <- beginPipelineRun(note = 'This is an example pipeline instance',
                                   randomSeed = 42,
                                   log = TRUE,
                                   logFolderPath = '/path/to/log/folder/')

endPipelineRun(pipelineRunInformation = runInformation)
```

The calls to `beginPipelineRun` and `endPipelineRun` are the bare minimum required to set up a pipeline run and all other pipeline code should go in between these lines.

`beginPipelineRun` returns an object containing information about the pipeline run. This object is required for many of the pipeline functions and should be passed as the value of any parameter named `pipelineRunInformation`

### Logging
If `log` is set to `TRUE` in `beginPipelineRun`, MethylPipeR will automatically log information about the pipeline and save function outputs to file in the folder specified by the `logFolderPath` parameter.
The information and outputs saved are:
* A line in run_notes.txt containing the timestamp of the pipeline run and value of `note` used in `beginPipelineRun`.
* pipeline_tictoc_\<timestamp\>.txt - shows the running time of each pipeline function called. NOTE: these should only be used to give an idea of running times rather than precise benchmarking.
* pipeline_run_information_\<timestamp\>.rds - the `pipelineRunInformation` from the corresponding pipeline run.
* Any function outputs that were saved during the pipeline run. The filenames of these will have the format \<`modelLabel`\>\_\<function_specific_prefix\>\_\<timestamp\>.\<rds/csv\>

NOTE: The pipeline run information and running times files are only written once `endPipelineRun` has been called, therefore if errors occur during the pipeline run, these will not be written.


## Example incremental EpiScore pipeline run

```r
library(MethylPipeR)
runInformation <- beginPipelineRun(note = 'MethylPipeR package test run',
                                   logFolderPath = '/path/to/logs/',
                                   log = FALSE)

cpgMatrixTrain <- loadData('/path/to/data/cpg_matrix_train.rds')
covariatesTableTrain <- loadData('/path/to/data/covariates_table_train.csv')

logisticModel <- fitLogisticModel(xs = cpgMatrixTrain,
                                  y = covariatesTableTrain[, 'Event', drop = FALSE],
                                  penalty = 'lasso',
                                  weights = NULL,
                                  pipelineRunInformation = runInformation,
                                  modelLabel = 'lasso')

cpgMatrixTest <- loadData('/path/to/data/cpg_matrix_test.rds')
covariatesTableTest <- loadData('/path/to/data/covariates_table_test.csv')

covariatesTableTest$methyl <- predictLogisticModel(logisticModel,
                                                   xs = cpgMatrixTest,
                                                   pipelineRunInformation = runInformation,
                                                   modelLabel = 'lasso_episcore',
                                                   predictType = 'link')

nonZeroCoefficients <- getNonZeroCoefficients(model = logisticModel,
                                              pipelineRunInformation = runInformation,
                                              modelLabel = 'lasso')

incrementalResult <- incrementalTest(covariatesTableTest,
                                     yColname = 'Event',
                                     covColnames = c('age', 'sex', 'bmi'),
                                     scoreColname = 'methyl',
                                     family = 'binomial',
                                     pipelineRunInformation = runInformation)

endPipelineRun(runInformation)
```

### Line-by-line explanation
Here we load the package and call the essential function, `beginPipelineRun` for setting up a pipeline run. This sets the path for where logs/model output will be saved, whether logging will be performed and a note describing the run.
```r
library(MethylPipeR)
runInformation <- beginPipelineRun(note = 'MethylPipeR package test run',
                                   logFolderPath = '/path/to/logs/',
                                   log = FALSE)
```
We then load the training data objects. Here this consists of a matrix of CpG intensity values and a table of covariates and labels. The loadData function automatically identifies the file type based on the extension.
```r
cpgMatrixTrain <- loadData('/path/to/data/cpg_matrix_train.rds')
covariatesTableTrain <- loadData('/path/to/data/covariates_table_train.csv')
```
Next, we fit a logistic lasso model on the CpGs. `drop = FALSE` is used here when passing `y` as the function is expecting a data.frame or matrix with a column named `'Event'` (rather than a vector/list).
```r
logisticModel <- fitLogisticModel(xs = cpgMatrixTrain,
                                  y = covariatesTableTrain[, 'Event', drop = FALSE],
                                  penalty = 'lasso',
                                  weights = NULL,
                                  pipelineRunInformation = runInformation,
                                  modelLabel = 'lasso')
```
We then load the test data in the same way as the training data and obtain the logistic model predictions (test set EpiScores). These are placed in a new column (named `'methyl'`) in the test covariates table. The non-zero coefficients are also obtained to identify the selected CpGs.
```r
cpgMatrixTest <- loadData('/path/to/data/cpg_matrix_test.rds')
covariatesTableTest <- loadData('/path/to/data/covariates_table_test.csv')

covariatesTableTest$methyl <- predictLogisticModel(logisticModel,
                                                   xs = cpgMatrixTest,
                                                   pipelineRunInformation = runInformation,
                                                   modelLabel = 'lasso_episcore',
                                                   predictType = 'link')
                                                   
nonZeroCoefficients <- getNonZeroCoefficients(model = logisticModel,
                                              pipelineRunInformation = runInformation,
                                              modelLabel = 'lasso')
```
The incremental models are then fit using `incrementalTest`. The returned list contains the fitted covariates and covariates+EpiScore models.
```r
incrementalResult <- incrementalTest(covariatesTableTest,
                                     yColname = 'Event',
                                     covColnames = c('age', 'sex', 'bmi'),
                                     scoreColname = 'methyl',
                                     family = 'binomial',
                                     pipelineRunInformation = runInformation)
```
Finally `endPipelineRun` is called to initiate saving of the log files and runInformation object.
```r
endPipelineRun(runInformation)
```

## Example Pipeline Run
Below is an example pipeline run for training and testing a logistic lasso binary classification model.
```r
library(MethylPipeR)

runInformation <- beginPipelineRun(note = 'Logistic lasso pipeline example', 
                                   log = TRUE, 
                                   logFolderPath = '/path/to/log/folder/')

trainingData <- loadData('/path/to/data/training_data.rds')
trainingTarget <- loadData('/path/to/data/training_target.csv')
trainingWeights <- loadData('/path/to/data/training_weights.rds')
trainingFolds <- assignTrainingFolds(data = trainingTarget, 
                                     classColname = 'Event', 
                                     nFolds = 3, 
                                     pipelineRunInformation = runInformation)

logisticModel <- fitLogisticModel(xs = trainingData,
                                  y = trainingTarget[, 'Event', drop = FALSE],
                                  penalty = 'lasso',
                                  weights = trainingWeights,
                                  nFoldsCV = 3,
                                  foldID = trainingFolds,
                                  pipelineRunInformation = runInformation,
                                  modelLabel = 'logistic_lasso')

logisticModelTrainPredictions <- predictLogisticModel(model = logisticModel,
                                                      xs = trainingData,
                                                      pipelineRunInformation = runInformation,
                                                      modelLabel = 'logistic_lasso_train')

endPipelineRun(runInformation)
```

In `fitLogisticModel`, the inputs are as follows:
xs: a matrix or `data.frame` with each column corresponding to a feature.
y: a matrix or `data.frame` with a numerical column named `Event` with 1 = positive and 0 = negative.

## Prediction Models
The following prediction models are currently available in MethylPipeR:
* Penalised logistic regression (lasso, elastic net, ridge)
* Penalised Cox proportional-hazards survival model (linear predictor)
* Random forest for classification
