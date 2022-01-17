# MethylPipeR Developer Guide
This guide is for developers wishing to add functionality to MethylPipeR such as additional models.
It describes the implementation of model fitting and prediction and outlines the steps required to make a new model available in the package.

## MPRModel class
The `fitMPRModel` function (see next section) returns an MPRModel object. This is essentially a list that contains a `model`, a `modelType` and `modelMethod`.
`model` is the method-specific model object, for example a randomForest object if `rf` is specified as the method in `fitMPRModel`.
`modelType` corresponds to the `type` selected in `fitMPRModel(CV)` and likewise, `modelMethod` corresponds to the chosen `method`.
Additional attributes that are general to all models may be added to the MPRModel class in the future.

## Model fitting
The generic functions for model fitting are in `R/model_fitting.R`. This contains `fitMPRModel`, `fitMPRModelCV` and `fitMPRModelIncremental`.

### `fitMPRModel`
This is the generic MethylPipeR model (MPRModel) fitting function called by the user. Parameters for `fitMPRModel` are detailed in the `MethylPipeR_<current_version>.pdf` document.
`fitMPRModel` firstly performs input validation checks such as making sure the input data is in a matrix/data.frame and contains no missing values.
It then selects a specific fitting function (stored in `fitFunctionLookup`) based on the provided model `type` and `method`. These are currently defined in `R/fitmprmodel_functions.R`.
If `save == TRUE`, the returned `MPRModel` object is also saved to a file using MethylPipeR's built-in logging functionality.

### `R/fitmprmodel_functions.R`
Each function in this file corresponds to a model `type` and `method` combination and is named fitMPRModel<type><method>.
The function must have exactly the following set of parameters:
* trainXs - The training data matrix/data.frame.
* trainY - The training response variable. This should be a vector if type = 'binary'/'continuous' or a data.frame/matrix with column names corresponding to tteColname and eventColname if type = 'survival'.
* testXs - The test data matrix/data.frame.
* testY - The test response variable. This should be a vector if type = 'binary'/'continuous' or a data.frame/matrix with column names corresponding to tteColname and eventColname if type = 'survival'.
* tteColname - A string corresponding to the time-to-event column name in trainY/testY. Only required if type == 'survival'.
* eventColname - A string corresponding to the event column name in trainY/testY. Only required if type == 'survival'.
* parallel - A boolean specifying whether parallel computation should be used in model fitting.
* seed - An integer to set the random seed to for model fitting.
* ... - Other arguments that may be used by fitMPRModel<type><method> or passed on to a fitting function.

### Adding a new model (fitting)
This can be performed for a new model type and method in the following steps:
1. Add a new function in `fitmprmodel_functions.R`. The naming convention for this is 'fitMPRModel<type><method>'. It must have the parameters which were specified in the previous section and return the trained model.
2. Add an entry in the `fitFunctionLookup` list for the newly added function. This essentially a two-level key-value lookup with the first level indexed by type and second indexed by method.

## Prediction on new data
The generic function for applying models to new data are in `R/model_prediction.R`. This contains `predictMPRModel`.

### `predictMPRModel`
This is the generic MethylPipeR model predict function called by the user. Parameters are detailed in the `MethylPipeR_<current_version>.pdf` document.
`predictMPRModel` firstly performs input validation checks such as making sure the data is in a matrix/data.frame, contains no missing values and that the model is an object of type MPRModel.

### `R/predictmprmodel_functions.R`
Each function in this file corresponds to a model `type` and `method` combination and is named predictMPRModel<type><method>.
The function must have exactly the following set of parameters:
* model - An MPRModel object. This is typically returned from a call to fitMPRModel.
* data - The data.frame/matrix that the model will be applied to.
* ... - Other arguments that may be uesd by predictMPRModel<type><method> or passed on to a fitting function.

### Adding a new model (prediction)
This can be performed for a new model type and method in the following steps:
