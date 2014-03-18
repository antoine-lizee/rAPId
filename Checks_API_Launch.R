## This is a script that will try to run through the files and check for dependencies.
## It can be used at API startup if in debug mode.
#
#
# Copyright Antoine Lizee @ MSBIOSCREEN - UCSF


message('Launching the verification script for the API code.')

errorHandler <- function(e) {
  if (grepl("library|require", ec <- e$call)){
    package.name <- substr(ec, start=7, stop = nchar(ec))
    stop("PACKAGE_ERR: Missing package, please intall the package '", package.name,"' on your system")
  }
}

grep('^[ \t]*source', scan('./API_Launch.R', what='character', sep = '\n'), value = T)


exploreNode <- function(filename, verbose = T) {
  
  if (verbose) {cat("Scanning", filename, "...................\n")}
  
  if(!file.exists(filename)) {stop(paste("SOURCE_ERR:Missing dependency file", filename, "have you installed all the ressource?"))}
  
  fileStrings <- scan(filename, what='character', sep = '\n', comment.char='#', quiet=T) # scan the file
  fileStringsNS <- gsub("\\s", "", fileStrings) # remove all the white space and affiliated
  # get the sourced Files
  sourceLines <- grep('^source', fileStringsNS, value = T) 
  sourcedFiles <- sapply(strsplit(sourceLines, "source\\(\\'|\\'"), '[', 2)
  # get the package names
  packageLines <- grep('^require|^library', fileStringsNS, value = T)
  packageNames <- sapply(strsplit(packageLines, "library\\(|require\\(|\\)|library\\(\\'|require\\(\\'|\\'"), '[', 2)
  
  if (verbose & !all(NABools <- is.na(sourcedFiles))) {
    cat("..Found these sourced files : \n", paste(sourcedFiles[!NABools], collapse="\n"), '\n', sep = "")
  }
  
  if (!all(NABoolp <- is.na(packageNames))) {
    if (verbose) cat("..Found these packages : \n", paste(packageNames[!NABoolp], collapse="\n"), '\n', sep="")
    dataPackage <- data.frame(file = filename, package = packageNames)
  } else {dataPackage <- NULL}
    
  if (length(sourcedFiles)!=0) {
    for (filename_i in sourcedFiles[!NABools]) {
      dataPackage <- rbind(dataPackage, exploreNode(filename_i, verbose))
    }
  }
  return(dataPackage)
}

testPackages <- function(packageNames) {
  return(packageNames %in% installed.packages()[,1])
}