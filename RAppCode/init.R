
simulateDist <- function(n, mu, sig, distFamily="gaussian") {
  switch(distFamily,
         "gaussian" = rnorm(n,mu,sig),
         "poisson" = rpois(n,mu),
         stop("ARGS_ERR: distribution family not recognised or suported"))
}

getPValue <- function(mux, # The mean of the base distribution
                      mu0 = 0, # The sample mean you want to test for the probability of more extreme
                      sdx, # The standard distribution of the base distribution
                      nx = 9, # The number of sample drawn for the computation of the sample mean
                      simulate = F, # Do you need to simulate ?
                      nSimulations = 10000, # Number of simulatins
                      distFamily = "gaussian" # Pick your base distribution
                      ) {
  # Specify the default value for sd 
  if (missing(sdx)) {
    sdx = switch(distFamily,
                "gaussian" = 1,
                "poisson" = NA,
                NA)
  }
  
  # Get the analytical results
  if (!simulate) {
    if (distFamily == "gaussian") {
      stderr <- sdx/sqrt(nx)
      tstat <- (mux - mu0)/stderr
      return(pnorm(-abs(tstat)))
      
    } else if (distFamily == "poisson") {
      if (!is.na(sd)) warning("ARG_WARN: standard deviation is not used for univariate distribution family ", distFamily)
      if (mu0 < mux) {
        pval <- ppois(nx * mu0, nx * mux, lower.tail=T)
      } else {
        pval <- ppois(nx * mu0, nx * mux, lower.tail=F)
      }
      
    }
  } 
  
  # Get the simulated results
  else {
    muVec <- replicate(n=nSimulations, expr = sum(simulateDist(nx,mux,sdx,distFamily))) / nx
    if (mu0 < mux) {
      return(mean(muVec<mu0))
    } else {
      return(mean(muVec>mu0))
    }
  }
}


# Tests -------------------------------------------------------------------

if (test <- TRUE) {
  
  ## Simple tests
  mux <- 2
  nx_V <- c(2,3,5,10,100)
  sdx <- 1
  mu0_V <- c(1,2.1,2.5,3,5)
  distFamily_V <- c("gaussian", "poisson")
  nSimulations_V <- c(NA, 1e4, 1e5)
  simulate_V <- c(FALSE, TRUE, TRUE)
  for (distFamily_i in distFamily_V) {
    cat("### Family:", distFamily_i, '\n')
    for (nx_i in nx_V) {
      cat("sample size:", nx_i, "\n")
      testResults <- sapply(mu0_V, 
                            function(mu0_i) sapply(1:length(nSimulations_V), 
                                                   function(i) suppressWarnings(getPValue(mux,
                                                                                          mu0 = mu0_i, 
                                                                                          sdx, 
                                                                                          nx = nx_i,
                                                                                          simulate = simulate_V[i], 
                                                                                          nSimulations = nSimulations_V[i], 
                                                                                          distFamily = distFamily_i ) )) )
      dimnames(testResults) <- list(nSimulations_V, mu0_V)
      print(testResults)
      cat("\n")
    }
    cat("\n")
  }
  
  ## Gaussians & co. Why not student? Explained by examples
  mux <- 2
  nx <- 3
  sdx <- 1
  distFamily="gaussian"
  nSimulations <- 100000
  # muVec <- replicate(n=nSimulations, expr = sum(simulateDist(nx,mux,sdx,distFamily))) / nx
  xVec <- sapply(1:nSimulations, function(ni) {simulateDist(nx,mux,sdx,distFamily)})
  muVec <- apply(xVec, 2, sum) / nx
  sdVec <- sqrt(apply((xVec - kronecker(matrix(1,3,1), t(muVec)))^2, 2, sum) / (nx-1) )
  # sdVec2 <- sqrt(apply((xVec)^2, 2, sum) / nx - muVec^2)
  X <- seq(from=min(muVec), to=max(muVec), length.out=nSimulations/50)
  hist(muVec,X, freq=F )
  lines(density(muVec), col = "green", lwd = 3)
  lines(X, dnorm(X, mux, sdx) , col = "blue", lwd = 2)
  lines(X, dnorm(X, mux, sdx/sqrt(nx)), col = "red", lwd = 2)
  
  # hist(mux + (muVec - mux)*sqrt(nx)/sdVec, breaks=200000, freq=F, xlim=c(min(muVec), max(muVec)) )
  # lines(density(muVec, bw="SJ"), col = "green", lwd = 3)
  # lines(X, dnorm(X, mux, sdx) , col = "blue", lwd = 2)
  # lines(X, dt((X-mux)*sqrt(nx)/sdx, df=(nx-1)) * sqrt(nx)*sdx, col = "red", lwd = 2)
  
  tstatVec <- (muVec - mux)/(sdVec/sqrt(nx))
  tstatVec <- tstatVec[tstatVec > -4 & tstatVec < 4]
  X2 <- seq(from=min(tstatVec), to=max(tstatVec), length.out=nSimulations/50)
  hist(tstatVec, breaks=X2, freq=F )
  lines(density(tstatVec), col = "green", lwd = 3)
  lines(X2, dt(X2, df=(nx-1)), col = "red", lwd = 2)
  
  hist(sdVec,100, freq=F)
  lines(
    XSD <- seq(from=min(sdVec), to=max(sdVec), length.out=nSimulations/50),
    dgamma(XSD, (EST <- MASS::fitdistr(sdVec, densfun="gamma")$estimate)[1], EST[2]),
    col = "black", lwd = 2 )
  
  ## Poisson
  mux <- 2
  nx <- 10
  sdx <- 1
  distFamily="poisson"
  nSimulations <- 100000
  muVec <- replicate(n=nSimulations, expr = sum(simulateDist(nx,mux,sdx,distFamily))) / nx
  values <- sort(unique(muVec))
  X <- c(- (offset <- diff(values)[1]/2), values + offset)
  hist(muVec, X, freq = F)
  lines(values2 <- floor(min(values)):ceiling(max(values)), dpois(values2, lambda=mux), col = "blue", lwd = 2)
  points(values, dpois(values * nx, lambda=mux*nx) * nx, col = "red", pch = 8)
  
  # hist(mux + (muVec - mux)*sqrt(nx)/sdVec, breaks=200000, freq=F, xlim=c(min(muVec), max(muVec)) )
  # lines(density(muVec, bw="SJ"), col = "green", lwd = 3)
  # lines(X, dnorm(X, mux, sdx) , col = "blue", lwd = 2)
  # lines(X, dt((X-mux)*sqrt(nx)/sdx, df=(nx-1)) * sqrt(nx)*sdx, col = "red", lwd = 2)

}


