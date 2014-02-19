tryCatch({
  
  require(ggplot2)
  require(rjson)
  message("This is a message - Launching of the app server")
  cat("Launching API... \n")
  
  ## PARAMETERS
  APIversion <- "1.0.2";
  
  
  ## source needed files
  source('/media/FD/BIOSCREEN/R/BIOPROD/PE_getConnection.R')
  source('/media/FD/BIOSCREEN/R/BIOPROD/PE_Subpop_functions.R')
  source('/media/FD/BIOSCREEN/R/BIOPROD/WEB_BaseFunctions.R')
  
  ## Source functions
  source('/media/FD/API/CEapp/API_baseFunctions.R')
  source('/media/FD/API/CEapp/API_functions.R')
  
  cat("Successful launch \n")
}, 
error = print)
#, finally= cat("LAUNCHING API PROCESS TERMINATED ") )