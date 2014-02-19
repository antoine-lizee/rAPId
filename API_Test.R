## SET THE HEADER
# Direct header
setHeader(header='X-Powered-By', value='rApache')
setHeader(header='X-API', value='CEAPI v0.1')
# MIME type
# setContentType(type='application/json')
setContentType(type='text/plain')


## DATA
print(str(GET))
cat('---------\n')
print(str(POST))
cat('---------\n')
print(ls())
cat('---------\n')
print(SERVER)
cat('---------\n')
