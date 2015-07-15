
library(proto)

# Helpers for proto programming -------------------------------------------

newWithDefaults <- function(protoName, protoArgs) {
  # This function creates a constructor for a proto trait, but checking for missing values when these are required.
  # The required values are specified by setting their default Value as "NULL".
  return(function(., ...) {
    # Collect the arguments passed to .$new()
    newArgs <- list(...)
    newArgNames <- names(newArgs)
    # Loop through the arguments of the specification
    for (protoArgName in names(protoArgs)) {
      # If the specified argument is not passed to .$new()...
      if (!protoArgName %in% newArgNames) {
        if (is.null(protoArgs[[protoArgName]])) {
          # ...throw an error when it's compulsory (because specified as "NULL")
          stop("PROTO: You created an instance of the Trait '", protoName, "' without specifying the needed parameter '", protoArgName, "' .")
        } else {
          # ...add it to the arguments
          newArgs[[protoArgName]] <- protoArgs[[protoArgName]]
        }
      }
    }
    # Call proto(., ...) with the modified version of the arguments
    # protoCall <- as.call(c( list(quote(proto), quote(.)), args ))
    # print(protoCall)
    # eval(protoCall)
    eval(as.call(c(list(quote(.$proto)), newArgs)))
  })
}


# Argument helpers --------------------------------------------------------

defaultArgumentCheck <- function(., value) {
  # This is the default argument check function. If there is expected values specified,
  # it checks that the passed argument is wihin these.
  # It also does some basic type checks.
  # TODO: support lambda & other functions.
  switch(.$expectedValueType,
         "string" = checkWithinValues(., value, .$expectedValues),
         "int"=, "integer"= checkWithinRange(., value))
  
}

checkWithinExpectedValues <- function(., value, values) {
  if (!is.na(values) & !is.null(.$expectedValues, values)) {
    if (!value %in% values) {
      stop(simpleError(
        paste0("ARGS: Wrong Value for "
               , .$name
               , ifelse(is.na(.$description), "", paste0(" the ", .$description))
               , ". Please refer to the embedded reference available with an OPTIONS call to "
               , .$rapid.action
               , "."
        )))
    }
  }
}


# Argument definition -----------------------------------------------------

Argument <- proto(new = newWithDefaults(protoName = "Argument",
                                         protoArgs = list(name = NULL
                                                          , description = NULL
                                                          , expectedValueType = NA
                                                          , expectedValues = NA
                                                          , defaultValue = "<none>"))
                   , check = defaultArgumentCheck
                   , funEnvir = FALSE)


# Backup Argument definition ----------------------------------------------

ArgumentBckp <- proto(new = function(.
                                     , name = NULL
                                     , description = NULL
                                     , expectedValueType = NA
                                     , expectedValues = NA
                                     , defaultValue = "<none>"
                                     , ...) {
  for (protoArg in c("name", "description")) {
    if (eval(parse(text = paste0("missing(", protoArg, ")")))) {
      stop("PROTO: You created an instance of the Trait 'Argument' without specifying the needed parameter '", protoArg, "'.")
    }
  }
  proto(.
        , name = name
        , description = description
        , expectedValueType = expectedValueType
        , expectedValues = expectedValues
        , default = default
        , ...)}
  , check = defaultArgumentCheck)
