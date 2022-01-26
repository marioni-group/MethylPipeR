#' To be called at the beginning of a MethylPipeR session. Initialises logging and model saving functionality.
#'
#' @param sessionLogFolder The path of the folder that logs and model objects should be saved to. Must end in "/".
#'
#' @return
#' @export
initLogs <- function(sessionLogFolder) {
  sessionStartTimestamp <- format(Sys.time(), '%Y_%m_%d_%H_%M_%S')
  options(mprSessionStartTimestamp = sessionStartTimestamp)
  options(mprSessionLogFolder = sessionLogFolder)
  sessionLogFilename <- paste0('session_log_', sessionStartTimestamp, '.txt')
  options(mprSessionLogFilename = sessionLogFilename)
  options(mprSessionLogFilepath = paste0(sessionLogFolder, sessionLogFilename))
  sessionConsoleFilepath <- paste0(sessionLogFolder, 'console_log_', sessionStartTimestamp, '.txt')
  options(mprSessionConsoleFilepath = sessionConsoleFilepath)
  sink(sessionConsoleFilepath, split = TRUE)
  initSessionLogFile()
}

initSessionLogFile <- function() {
  sessionStartTimestamp <- getOption('mprSessionStartTimestamp')
  sessionLogFilepath <- getOption('mprSessionLogFilepath')
  sessionLogFile <- file(sessionLogFilepath)
  writeLines(paste0('Starting MethylPipeR-UI session. Timestamp: ', sessionStartTimestamp), sessionLogFile)
  close(sessionLogFile)
}


#' Writes lines to the session log.
#'
#' @param ... The strings to be written to the session log. One string per line.
#'
#' @return
#' @export
#'
logSessionLines <- function(...) {
  sessionLogFilepath <- getOption('mprSessionLogFilepath')
  lineList <- list(...)
  lineList <- lapply(lineList, function(txt) {
    paste0(format(Sys.time(), '[%H:%M:%S] '), txt)
  })
  lineList$file = sessionLogFilepath
  lineList$sep = '\n'
  lineList$append = TRUE
  do.call(cat, lineList)
}


#' Saves a MPRModel object to the logging folder.
#'
#' @param model The MPRModel object to be saved.
#'
#' @return
#' @export
saveMPRModelObject <- function(model) {
  sessionStartTimestamp <- getOption('mprSessionStartTimestamp')
  sessionLogFolder <- getOption('mprSessionLogFolder')
  folderPath <- paste0(sessionLogFolder, 'models_', sessionStartTimestamp, '/')
  dir.create(paste0(folderPath))
  filePath <- paste0(folderPath, model$modelType, '_', model$modelMethod, '_', format(Sys.time(), '%Y_%m_%d_%H_%M_%S'), '.rds')
  saveRDS(model, file = filePath)
  logSessionLines(paste0('Saved model in ', filePath))
}