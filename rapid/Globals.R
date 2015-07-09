## Global.R
# This file gather configuration information specific to the rapid runtime.
# Unless you really know what you're doing, you shouldn't change any of this.


# File names and paths ----------------------------------------------------

RAPID_DIR = "rapid"
RAPID_APP_DIR = "app"
RAPID_SRC_DIR = "source"
RAPID_STARTUP_NAME = "Startup.R"
RAPID_CONFIG_FILE = "Config.R"


# Helpers -----------------------------------------------------------------

checkRootDir <- function(directory = "./", 
                         errorMsg = "Script has to be run from the project root directory.") {
  if (all(c(RAPID_DIR, RAPID_APP_DIR, RAPID_SRC_DIR) %in% dir(directory))) {
    normalizePath(directory)
  } else {
    stop(errorMsg)
  }
}

sourceConfig <- function() {
  source(file.path(checkRootDir(), RAPID_CONFIG_FILE))
}

getStartupFile <- function() {
  file.path(checkRootDir(), RAPID_DIR, RAPID_STARTUP_NAME)
}

getActions <- function() {
  dir(checkRootDir(), RAPID_APP_DIR)
}