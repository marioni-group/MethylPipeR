# MethylPipeR
This package provides functions for running machine learning prediction pipelines with a focus on systematic and reproducible experiments.

## Installation from GitLab
As the repository is currently private, you will need to create a GitLab personal access token to pass into the `install_gitlab` function.
To do this, navigate to 'User Settings' on GitLab (click your avatar in the top right of the web UI) and select 'Access Tokens' on the left hand menu.
Under 'Personal Access Tokens', enter a name for your new token and make sure to select all the 'read' permissions under 'Scopes'. 
Click 'Create personal access token' and copy the generated token.

In R, use the following:
```r
remotes::install_gitlab('marioni-group/methylpiper', auth_token = 'INSERT AUTH TOKEN HERE')
```


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
