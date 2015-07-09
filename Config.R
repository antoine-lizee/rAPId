## Config.R
# Main configuration file for the app. Should be at the root of your project.

DEBUG = F

# APACHE SETUP ------------------------------------------------------------

# Path to the rapache module
R_MOD_PATH = "/usr/lib/apache2/modules/mod_R.so"
# Port of the API
PORT = 80
# Main server name serving your app
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
INFO_ACTION = "getInfo"
