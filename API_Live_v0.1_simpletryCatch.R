## SET THE HEADER
# Direct header
setHeader(header='X-Powered-By', value='rApache')
setHeader(header='X-API', value='CEAPI v0.1')
# MIME type
setContentType(type='application/json')
# setContentType(type='text/plain')


tryCatch({ 
  ## Source functions ( TO PUT IN LAUNCH WHEN READY)
  source('/media/FD/API/CEapp/API_functions.R')
  
  method <- SERVER$method
  finalJSON <- switch(method,
                      OPTIONS = createOPTIONSJson(),
                      GET = , #createGETJson(),
                      createDEFAULTJson("The HTTP Method used is (not yet) recognized by the API"))         
  sendBin(finalJSON)
},
         error = function(e) {
           print(class(e))
           print(e)
           print("MOUHAHAHAHAHA")},
         finally = {print("TEST FINALLY")}
)
