


getPValue <- function(mux, mu0 = 0, dmu = NA, sd,  nx = 10,  simulate = F, nSimulations = 10000, family = "gaussian") {
  # Specify the default value for sd 
  if missing(sd) {
    sd = switch(family,
                "gaussian" = 1,
                "poisson" = NA,
                NA)
  }
  # Get the analytical results
  if (!simulate) {
    if (family == "gaussian") {
      df <- nx - 1
      stderr <- sd/sqrt(nx)
      if (is.na(dmu)) {
        dmu = mux - mu0
      }
      tstat <- dmu/stderr
      return(pt(-abs(tstat), df))
    } elseif (family == "poisson") {
      if (!is.na(sd)) Warning("sd is not used")
    }
  } 
  # Get the simulated results
  else {
    muMat <- replicate(n=10000, expr= mean()
  }
}


