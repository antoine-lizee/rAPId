
simulateDist <- function(n, mu, sd, distFamily="gaussian") {
  switch(distFamily,
         "gaussian" = rnorm(n,mu,sd),
         "poisson" = rpois(n,mu),
         stop("ARGS_ERR: distribution family not recognised or suported"))
}

getPValue <- function(mux, mu0 = 1, sd,  nx = 9,  simulate = F, nSimulations = 10000, distFamily = "gaussian") {
  # Specify the default value for sd 
  if (missing(sd)) {
    sd = switch(distFamily,
                "gaussian" = 1,
                "poisson" = NA,
                NA)
  }
  # Get the analytical results
  if (!simulate) {
    if (distFamily == "gaussian") {
      df <- nx - 1
      stderr <- sd/sqrt(nx)
      tstat <- (mux - mu0)/stderr
      return(pt(-abs(tstat), df))
    } else if (distFamily == "poisson") {
      if (!is.na(sd)) warning("ARG_WARN: sd is not used for univariate distribution family ", distFamily)
      if (mu0 < mux) {
        tstat <- ppois(nx * mu0, nx * mux, lower.tail=T)
      } else {
        tstat <- ppois(nx * mu0, nx * mux, lower.tail=F)
      }
    }
  } 
  # Get the simulated results
  else {
    muVec <- replicate(n=nSimulations, expr = sum(simulateDist(nx,mux,sd,distFamily))) / nx
    if (mu0 < mux) {
      return(mean(muVec<mu0))
    } else {
      return(mean(muVec>mu0))
    }
  }
}



mux <- 2
nx <- 3
nSimulations <- 10000
# muVec <- replicate(n=nSimulations, expr = sum(simulateDist(nx,mux,sd,distFamily))) / nx
xVec <- sapply(1:nSimulations, function(ni) {simulateDist(nx,mux,sd,distFamily)})
muVec <- sum()
X <- seq(from=min(muVec), to=max(muVec), length.out=nSimulations/50)
hist(muVec,X, freq=F )
lines(density(muVec, bw="SJ"), col = "green", lwd = 3)
lines(X, dnorm(X, mux, sd) , col = "blue", lwd = 2)
lines(X, dt((X-mux)*sqrt(nx)/sd, df=(nx-1)) * sqrt(nx)*sd, col = "red", lwd = 2)
# lines(X, dnorm(X, mux, sd) * 50 * diff(range(muVec)), col = "blue", lwd = 2)
# lines(X, dt((X-mux)*sqrt(nx)/sd, df=(nx-1)) * sqrt(nx)*sd* 50 * diff(range(muVec)), col = "red", lwd = 2)
