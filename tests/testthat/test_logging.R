tmpSessionLogFolder <- paste0(tempdir(), "/")

initLogs(sessionLogFolder = tmpSessionLogFolder, note = "Test note.")

sessionLogContents <- readLines(file(getOption("mprSessionLogFilepath")))[[1]]
# This test needs fixing to correctly form the expected line(s).
# expect_equal(sessionLogContents, paste0("Starting MethylPipeR session. Timestamp: ", getOption("mprSessionStartTimestamp")))

logSessionLines("Test line 1", "Test line 2")
# TODO: implement test to check written lines
