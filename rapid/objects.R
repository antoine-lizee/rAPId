
library(proto)

# Helpers for proto programming -------------------------------------------

newWithDefaults <- function(protoName, protoArgs) {
  # This function creates a constructor for a proto trait, but checking for missing values when these are required.
  # The required values are specified by setting their default Value as "NULL".
  # The only significant disadvantage of using this function is the lack of auto-completion when creating new objects,
  # because of the use of the ellipsis in the returned function. Could be fixed by using the formals()<- setter. TODO
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

checkWithinValues <- function(., value) {
  values <- .$expectedValues
  if (isAnNA(values) || isAnNA(value) || value %in% values) {
    return(value)
  }
  stop(simpleError(
    buildArgErrorMessage(., "Wrong value for ")
  ))
}

checkWithinRange <- function(., value) {
  rethrowWithMessage({
    min <- .$expectedValues$min
    max <- .$expectedValues$max
    isInRange <- value >= .$expectedValues$min && value <= .$expectedValues$max
  }, buildApiDevErrorMessage(., "Wrong range specification."))
  if (isInRange %in% FALSE) { #NA passes
    stop(simpleError(buildArgErrorMessage(., "Value is not in range for ")))
  }
  return(value)
}

defaultArgumentCheck <- function(., value) {
  # This is the default argument check function. If there is expected values specified,
  # it checks that the passed argument is wihin these.
  # It also does some basic type checks.
  # TODO: support lambda & other functions.
  switch(.$expectedValueType,
         "string" = checkWithinValues(., value),
         "integer"= {
           value = throwWithMessage(toInt(value), buildArgErrorMessage(., "Wrong value type for "))
           if (is.list(.$expectedValues)) {
             checkWithinRange(., value)
           } else {
             checkWithinValues(., value)
           }
         },
         "any" = checkWithinValues(., value))
}


# Misc Helpers ------------------------------------------------------------

isAnNA <- function(x) {
  length(x) == 1 && is.na(x)
}

buildApiDevErrorMessage <- function(., msg) {
  paste0("API_DEV::ARG: "
         , msg
         , .$name
         , ifelse(is.na(.$description), "", paste0(" the ", .$description))
         , ". Please check your argument definitions for the action '"
         , .$rapid.action
         , "'."
  )
}

buildArgErrorMessage <- function(., msg) {
  paste0("ARGS: "
         , msg
         , .$name
         , ifelse(is.na(.$description), "", paste0(" the ", .$description))
         , ". Please refer to the embedded reference available with an OPTIONS call to '"
         , .$rapid.action
         , "'."
  )
}

throwWithMessage <- function(expr, errorMsg) {
  # This is for a custom error
  tryCatch(expr = expr, error = function(e) stop(simpleError(errorMsg)))
}

rethrowWithMessage <- function(expr, errorMsg) {
  # This is for a native error for which we customize the output
  tryCatch(expr = expr, error = function(e) {
    e$message <- paste0(errorMsg, "\nOriginal error msg:\n", e$message)
    stop(e)
  })
}

toInt <- function(string) {
  if (string == "") {
    return(NA)
  } 
  res <- strtoi(string)
  if (is.na(res)) {
    stop()
  } else {
    return(res)
  }
}


# Argument definition -----------------------------------------------------

Argument <- proto(new = newWithDefaults(protoName = "Argument",
                                        protoArgs = list(name = NULL
                                                         , description = NULL
                                                         , expectedValueType = "any"
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
