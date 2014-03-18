### DEFAULT VALUES ----------

s_Y0 <- "ActualEDSS"
YNames <- getFiltersFromDB(connection, type=c("NUM", "DISC"))
filterNames <- getFiltersFromDB(connection)[,1]
filterValues <- lapply(filterNames, getDVFromDB, connection)
names(filterValues) <- filterNames
KTNames <- getKTFromDB(connection)


### JSON creation ---------------

createOPTIONSJson <- function() {
  OPTIONlist <- list( META = list(type = "meta - descriptor",
                                  describing = "Contextualisation Engine - main function",
                                  APIversion = APIversion,
                                  Rinfo = R.version.string),
                      DATA = list(
                        actionDescribed = "/getContext",
                        description = c("This is the main, global function of the CE API."
                                        ,"It lets you query a contextualization environment in the form of aggregated values for the variable(s) of interest."
                                        ,"These values will be given for different 'time points' given in the specified dimension for the temporal key (KT), within a range of choice."
                                        ,"Example: you may want to get the aggregated values for 'ActualEDSS' (the value of interest), within the range '12 18' of 'DiseaseDuration', while applyting filter '3 8' on 'MSSS'."
                        ),
                        expectedMEthod = "GET",
                        expectedSyntax = c("When/if there are several items in one argument, seperate them with a simple space. Ex: a range would be '13 17', a couple of categories would be 'UNI BI', etc.",
                                           "When you need to pass several arguments to the same field of the request, seperate these arguments by a underscore. Ex: I f you want to get results for several values, you should give: 'MSSS_EDSS_FSSC1'.",
                                           "Please DO NOT specify any field in several instances, as the first field only bearing a name will be taken into account."
                        ),
                        expectedArguments = list(s_KT = list (description = "Name of the temporal key",
                                                              expectedValueType = "one string",
                                                              expectedValues = KTNames,
                                                              default = '<required>'),
                                                 r_KT = list (description = "Range of the tempral key",
                                                              expectedValueType = "2 integer values",
                                                              expectedValues = "Depends on the TK, see 'Errors'",
                                                              default = '<required>'),
                                                 KT0 = list (description = "Time point (value of KT) on which to apply the filters",
                                                             expectedValueType = "1 integer value",
                                                             expectedValues = "Within r_KT",
                                                             default = "r_KT[2]"),
                                                 s_Vs = list (description = "The n filter names",
                                                               expectedValueType = "n strings, served by n different arguments seperated by ','",
                                                               expectedValues = filterNames,
                                                               default = "<empty>"),
                                                 V0s = list (description = "The n filter values",
                                                              expectedValueType = "n different arguments seperated by ','. Could be string(s) within the different categories, or one range of value in the form of 'V1 V2'",
                                                              expectedValues = filterValues,
                                                              default = "<empty>"),
                                                 s_Ys = list (description = "The m aggregated values",
                                                              expectedValueType = "one or more string arguments describing the numerical values that you want the contextualisation for.",
                                                              expectedValues = YNames,
                                                              default = s_Y0),
                                                 options = list(description = "The option that can control the behavior of the getContext action of the API",
                                                               expectedValueType = "A succession of key words, each designing an option",
                                                               expectedValues = "light",
                                                               valueMeaning = "light = send back the minimum information, scraping off the aggregation meta data and some fields of the META.")
                        )
                      )
  )
  return(toJSON(OPTIONlist))           
  
}

createGETJson <- function() {
  list[argList, APIWARNINGS, optionList] <- getArgs()
#   return(test(expression({
#     print(argList)
#     cat('----\n')
#     print(APIWARNINGS)
#     cat('----\n')
#     dput(argList)
#     cat('----\n')
#     print(lapply(GET, parseHTMLArg))
#   })))
  
  ## Launch the engine and get the data back
  list[CONTEXT, FILTDATA] <- do.call(getContextFull, argList)
  
  ##Check for missing / low data
  lowCountThre <- 4
  v_LowCountWarning <- sapply(FILTDATA, function(x) any(x[[2]] < lowCountThre))
  if (any(v_LowCountWarning)){
    APIWARNINGS <- list(APIWARNINGS, "LOW_COUNT_WARNING" = list(value = names(FILTDATA)[v_LowCountWarning]), description = paste(
      "Some results are aggregated over very few points for the variables attached in 'value', you shouldn't use it for the contextualization. This warning is displayed when there is less than ", lowCountThre, "values for at least one aggregation."))
  }
  
  ## Transform the data
  CONTEXT <- lapply(CONTEXT, transposeDF)
  
  ## Construct (the list for) the JSON
  if (optionList$light)
  {
    GETlist <- list(META = list(type = "result - light",
                                WARNINGS = APIWARNINGS),
                    DATA = list(context = CONTEXT)
    )
  } else {
    GETlist <- list(META = list(type = "result",
                                action = "Contextualisation Engine - API",
                                APIversion = APIversion,
                                WARNINGS = APIWARNINGS,
                                parsedArguments = argList[-4],
                                parsedOptions=optionList),
                    DATA = list(context = CONTEXT,
                                aggregationNumbers = FILTDATA)
    )
  }
  
  return(toJSON(GETlist))
}



### Parsing HTTP Arguments  -----------------

parseHTMLArg <- function(x) {
  if (is.null(x))
    return(NULL)
  else
    t <- strsplit(strsplit(x, ',')[[1]],' ')
}

checkV0s <- function(v_s_V, l_V0) {
  if (any(b_v <- ! v_s_V %in% filterNames)) {
    stop(simpleError(paste0("ARGS: '", paste(v_s_V[b_v], collapse = "'; '"),"' are wrong filter names. Please refer to the embedded reference available with an OPTIONS call to /getContext.")))
  }
  b <- v_s_V %in% YNames
  l_V0[b] <- lapply(l_V0[b], as.numeric)
  return(l_V0)
}

checks_KT <- function(s_KT) {
  if (!s_KT %in% KTNames)
    stop(simpleError("ARGS: Wrong Value for s_KT, the name of the temporal Key. Please refer to the embedded reference available with an OPTIONS call to /getContext."))
}

checks_Ys <- function(s_Ys) {
  if (any(b_v <- ! s_Ys %in% YNames))
    stop(simpleError(paste0("ARGS: Wrong Value for s_Ys ,'",paste(s_Ys[b_v], collapse = "'; '"), "'. Please refer to the embedded reference available with an OPTIONS call to /getContext.")))
}

getArgs <- function() {
  args <- lapply(GET, parseHTMLArg)
  APIWARNINGS <- c()
  # s_KT
  if (is.null(args$s_KT)){
    s_KT = 'MISSING'
  }  else {
    s_KT <- args$s_KT[[1]]
  }
  # r_KT
  if (is.null(args$r_KT)){
    r_KT = 'MISSING'
  }  else {
    r_KT <- as.numeric(args$r_KT[[1]])
  }
  # V0s
  if (is.null(args$V0s)){
    l_V0 <- NULL
    APIWARNINGS <- c(APIWARNINGS, "ARGS_WARNING: no value for V0s, passing no filters")
  }  else {
    l_V0 <- args$V0s
  }
  # s_Vs
  if (is.null(args$s_Vs)){
    v_s_V <- NULL
    APIWARNINGS <- c(APIWARNINGS, "ARGS_WARNING: no value for s_Vs, passing no filters")
  }  else {
    v_s_V <- unlist(args$s_Vs)
  }
  # r_KT
  if (is.null(args$KT0) && (r_KT != 'MISSING')){
    KT0 <- r_KT[2]
    APIWARNINGS <- c(APIWARNINGS, paste("ARGS_WARNING: no value for KT0, passing default value of", KT0) )
  }  else {
    KT0 <- as.numeric(args$KT0[[1]])
  }
  # s_Ys
  if (is.null(args$s_Ys)){
    s_Ys <- s_Y0
    APIWARNINGS <- c(APIWARNINGS, paste("ARGS_WARNING: no value for s_Ys, passing default value of", s_Ys) )
  }  else {
    s_Ys <- unlist(args$s_Ys)
  }
  
  if ((b_r <- r_KT == 'MISSING') | (b_s <- s_KT == 'MISSING')){
    stop(simpleError(paste("ARGS_ERROR: No value has been given for", paste(c({if (b_r) 'r_KT'}, {if (b_s)'s_KT'}), collapse=' and '))))
  }
  
  # options
  optionList = list(light = F, fakeTestOption = F)
  if (is.null(args$options)){
    options <- 'NONE'
  }  else {
    v_options <- unlist(args$options)
    optionList[grep(paste(v_options, collapse="|"), names(optionList), ignore.case=T)] <- T
  }
  
  checks_KT(s_KT)
  checks_Ys(s_Ys)
  l_V0 <- checkV0s(v_s_V, l_V0)

  argList <- list(r_KT = r_KT,
                  KT0 = KT0,
                  s_KT = s_KT,
                  DBcon = connection,  
                  l_V0 = l_V0,
                  v_s_V = v_s_V,
                  s_Ys = s_Ys)    
  
  return(list(argList,APIWARNINGS, optionList))
  
}
# 
test <- function(EXPR, type='TEXT') {
  if (type == 'TEXT'){
    setContentType(type='text/plain')
    eval(EXPR, parent.frame())
    return("")
  }
  else if (type == 'JSON') {
    printed <- capture.output(eval(EXPR,parent.frame()))
    return(createDEFAULTJson(printed))    
  }
}

### Preparing the JSON - Formatting -------

transposeDF <- function(df) {
  indexValue <- df[[1]]
  df2 <- data.frame(t(df[2:length(df)]))
  colnames(df2) <- indexValue
  return(df2)
}
  

