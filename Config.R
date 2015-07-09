## Config.R
# Main configuration file for the app. Should be at the root of your project.

DEBUG = TRUE

# APACHE SETUP ------------------------------------------------------------
# These options are used to configure the web server.

# Path to the rapache module.
R_MOD_PATH = "/usr/lib/apache2/modules/mod_R.so"
# Ports for the API
PORT = 80
DEBUG_PORT = 3082
# Main server name for the url serving your app
SERVER_NAME = "beaujolais.ucsf.edu"
# Alternate servernames
ADDITIONAL_SERVER_NAMES = c(
  "localhost",
  "127.0.0.1")
# The root url of the api. Should include the trailing slash.
ROOT_URL = "/"


# API OPTIONS -------------------------------------------------------------

# Name of the action that displays information relative to the R session.
# Use an empty string in order to avoid exposing this information. 
INFO_ACTION =


# DEBUG -------------------------------------------------------------------
# This section modifies the configuration variables when in debug mode.
# It is designed to be unchanged.

if (DEBUG) {
  PORT = DEBUG_PORT
  SERVER_NAME = "localhost"
  ADDITIONAL_SERVER_NAMES = "127.0.0.1"
  INFO_ACTION = "getInfo"
}

