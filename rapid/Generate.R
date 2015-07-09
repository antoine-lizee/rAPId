## Generate.R
# This file is supposed to be sourced in order to create the configuration
# file for Apache.
# This must be run from the project root directory.

library(whisker)

rm(list = ls())
source("rapid/Globals.R")
sourceConfig()


# Prepare -----------------------------------------------------------------

template <- readLines(file.path(RAPID_DIR, "rapid.conf.mustache"))
data <- list(
  date = Sys.time(),
  R_MOD_PATH = R_MOD_PATH,
  PORT = PORT,
  SERVER_NAME = SERVER_NAME,
  ADDITIONAL_SERVER_NAMES = ADDITIONAL_SERVER_NAMES,
  INFO_ACTION = INFO_ACTION,
  ROOT_URL = ROOT_URL,
  actions = getActions(),
  startup_script_path = getStartupFile())


# Generate ----------------------------------------------------------------

writeLines(whisker.render(template, data), file.path("rapid.conf"))


# Talk --------------------------------------------------------------------

cat("
The config file has been succesfully generated.
  -> [If not already done] Create a link from the Apache site configuration directory to the generated file. The command would be something like:
  sudo ln -s `pwd`/rapid/rapid.conf /etc/apache2/sites-available/ 
  -> [If not already done] Enable the configuration file. The command should be something like:
  sudo a2ensite rapid
  -> Restart apache, with the command:
  sudo service apache2 restart
")
