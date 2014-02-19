## CE API v1
##Copyright Antoine Lizee 2013-10 @ MSBIOSCREEN - UCSF

# Can be tested with something like: 
# curl  -G 
#       --data-urlencode "s_KT=DiseaseDuration" 
#       -d "r_KT=15+22&s_Ys=ActualEDSS,MSSS&KT0=20&s_Vs=AgeAtExam,Gender&V0s=20+60,M"  
#       --digest --user epic:EPIC 
#       beaujolais.ucsf.edu/CEAPI/getContext | tee >(tail -n 1 | python -mjson.tool) 


## SET THE HEADER
# Direct header
setHeader(header='X-Powered-By', value='rApache')
setHeader(header='X-API', value='CEAPI v0.1')
# MIME type
setContentType(type='application/json')
# setContentType(type='text/plain')

## Source what need to be fresh
# source('/media/FD/BIOSCREEN/R/BIOPROD/PE_getConnection.R')
  
## Launch execution of the commands ------

tryCatch(withCallingHandlers({
  ## Source functions ( TO PUT IN LAUNCH WHEN READY)
  #   source('/media/FD/API/CEapp/API_functions.R')
  
  method <- SERVER$method
  finalJSON <- switch(method,
                      OPTIONS = createOPTIONSJson(),
                      GET = createGETJson(),
                      throwErrorWithJson("IMPL"))         
  sendBin(finalJSON)
}, error = printErrorToStderr), # Send any error to the apache error log
error = function(e) sendBin(throwErrorWithJson(e=e)) ) # Send the error to the user through a JSON object.


