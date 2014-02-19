## SET THE HEADER
# Direct header
setHeader(header='X-Powered-By', value='rApache')
setHeader(header='X-API', value=paste0('CEAPI v', APIversion))
# MIME type
setContentType(type='application/json')
# setContentType(type='text/plain')

## Source functions ( TO PUT IN LAUNCH WHEN READY)
# source('/media/FD/API/CEapp/API_functions.R')

homeJSONlist <- list( META = list(type = "meta - descriptor",
                                  describing = "Contextualisation Engine - API",
                                  APIversion = APIversion),
                      DATA = list(message = "This is the home of the CE API. Welcome!",
                                  actions = list( supported = "/getContext",
                                                  beta = c("/getContext"),
                                                  meta = "/getRInfo [html]"),
                                  actionsExpectedMethods = list( OPTIONS = "get the syntax of the queries and the description of the action",
                                                                 GET = "query the action")
                      )
)

homeJSON <- toJSON(homeJSONlist)

sendBin(homeJSON)