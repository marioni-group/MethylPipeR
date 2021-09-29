initLogs(sessionLogFolder = 'C:/Users/s2092119/Documents/PhD/Omics Prediction of Incident Disease/R Package/MethylPipeR-UI_logs/')
sessionLogContents <- readLines(file(getOption('mprSessionLogFilepath')))
expect_equal(sessionLogContents, paste0('Starting MethylPipeR-UI session. Timestamp: ', getOption('mprSessionStartTimestamp')))

logSessionLines('Test line 1', 'Test line 2')
# TODO: implement test to check written lines