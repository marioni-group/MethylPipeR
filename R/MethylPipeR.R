#' @importFrom BART mc.surv.bart mc.gbart gbart
#' @importFrom pROC roc auc
#' @importFrom glmnet cv.glmnet glmnet
#' @importFrom doParallel registerDoParallel
#' @importFrom tictoc tic toc tic.log
#' @importFrom caret confusionMatrix groupKFold
#' @importFrom survival Surv coxph
#' @importFrom ggplot2 ggplot geom_density ggtitle xlab ylab ggsave aes
#' @importFrom dplyr %>% sym
#' @importFrom ROCR prediction performance
#' @importFrom RColorBrewer brewer.pal brewer.pal.info
#' @importFrom grDevices recordPlot
#' @importFrom graphics legend lines par points
#' @importFrom stats sd as.formula coef glm predict median
#' @importFrom utils head read.csv write.csv capture.output
#' @importFrom ROCR prediction performance
#' @importFrom MLmetrics PRAUC
#' @importFrom randomForest randomForest
#' @importFrom randomForestSRC rfsrc
#' @importFrom graphics abline
#' @importFrom performance performance_roc
#' @importFrom bigmemory big.matrix is.big.matrix as.big.matrix
#' @importFrom biglasso biglasso cv.biglasso
#' @importFrom bigmemoryExt cbindBM
#' @importFrom methods S3Class
#' @import e1071
#' @import see
utils::globalVariables(c("Event", "plotPRBaseline", "time_to_event"))
