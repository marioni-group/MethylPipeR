#' To be called at the beginning of a MethylPipeR session. Initialises logging
#' and model saving functionality.
#'
#' @param sessionLogFolder The path of the folder that logs and model objects
#'   should be saved to. Must end in "/".
#' @param note A note which will be saved at the top of the session log - can
#'   help to identify a specific pipeline run when checking the pipeline logs.
#' @return
#' @export
initLogs <- function(sessionLogFolder,
                     note = "MethylPipeR pipeline run (no note given).") {
  sessionStartTimestamp <- format(Sys.time(), "%Y_%m_%d_%H_%M_%S")
  sessionLogFilename <- paste0("session_log_", sessionStartTimestamp, ".txt")
  sessionConsoleFilepath <- paste0(sessionLogFolder,
                                   "console_log_",
                                   sessionStartTimestamp,
                                   ".txt")
  
  options(mprSessionStartTimestamp = sessionStartTimestamp,
          mprSessionLogFolder = sessionLogFolder,
          mprSessionLogFilename = sessionLogFilename,
          mprSessionLogFilepath = paste0(sessionLogFolder, sessionLogFilename),
          mprSessionConsoleFilepath = sessionConsoleFilepath)
  sink(sessionConsoleFilepath, split = TRUE)
  initSessionLogFile(note)
}

initSessionLogFile <- function(note) {
  sessionStartTimestamp <- getOption("mprSessionStartTimestamp")
  sessionLogFilepath <- getOption("mprSessionLogFilepath")
  sessionLogFile <- file(sessionLogFilepath)
  # writeLines(list(paste0("Starting MethylPipeR session. Timestamp: ", sessionStartTimestamp), note), sessionLogFile)
  close(sessionLogFile)
  logSessionLines(paste0("Starting MethylPipeR session. Timestamp: ", sessionStartTimestamp), note)
}


#' Writes lines to the session log.
#'
#' @param ... The strings to be written to the session log. One string per line.
#' @return
#' @export
#'
logSessionLines <- function(...) {
  sessionLogFilepath <- getOption("mprSessionLogFilepath")
  lineList <- list(...)
  lineList <- lapply(lineList, function(txt) {
    paste0(format(Sys.time(), "[%H:%M:%S] "), txt)
  })
  lineList$file <- sessionLogFilepath
  lineList$sep <- "\n"
  lineList$append <- TRUE
  do.call(cat, lineList)
}


#' Saves a MPRModel object to the logging folder.
#'
#' @param model The MPRModel object to be saved.
#' @param suffix Optional - a custom suffix to include in the file name after
#'   the timestamp.
#'
#' @return
#' @export
saveMPRModelObject <- function(model, suffix = NULL) {
  sessionStartTimestamp <- getOption("mprSessionStartTimestamp")
  sessionLogFolder <- getOption("mprSessionLogFolder")
  folderPath <- paste0(sessionLogFolder, "models_", sessionStartTimestamp, "/")
  dir.create(paste0(folderPath))
  filePath <- paste0(folderPath,
                     model$modelType,
                     "_",
                     model$modelMethod,
                     "_",
                     format(Sys.time(), "%Y_%m_%d_%H_%M_%S"),
                     suffix,
                     ".rds")
  saveRDS(model, file = filePath)
  logSessionLines(paste0("Saved model in ", filePath))
}
