calcZonalRegimePars <- function(polygonID, firePolys = firePolys,
                                landscapeAttr = sim$landscapeAttr,
                                firePoints = sim$firePoints,
                                epochLength = epochLength, maxSizeFactor) {
  idx <- firePolys$PolyID == polygonID
  tmpA <- firePoints[idx, ]
  landAttr <- landscapeAttr[[polygonID]]
  cellSize = landAttr$cellSize
  nFires <- dim(tmpA)[1]
  if (nFires == 0) {
    return(NULL)
  }
  rate <- nFires / (epochLength * landAttr$burnyArea)   # fires per ha per yr

  pEscape <- 0
  xBar <- 0
  xMax <- 0
  lxBar <- NA
  maxFireSize <- cellSize   #note that maxFireSize has unit of ha NOT cells!!!
  xVec <- numeric(0)

  if (nFires > 0) {
    #calculate escaped fires
    #careful to subtract cellSize where appropriate
    xVec <- tmpA$SIZE_HA[tmpA$SIZE_HA > cellSize]

    if (length(xVec) > 0) {
      pEscape <- length(xVec) / nFires
      xBar <- mean(xVec)
      lxBar <- mean(log(xVec))
      xMax <- max(xVec)

      zVec <- log(xVec / cellSize)
      if (length(zVec) < 30)
        warning(paste("Less than 30 \"large\" fires in zone", polygonID, ".",
                      "T estimates may be unstable.\n",
                      "\tConsider using a larger area and/or longer epoch.\n"))
      hdList <- HannonDayiha(zVec)  #defined in sourced TEutilsNew.R
      That <- hdList$That
      if (That == -1) {
        warning(
          sprintf(
            "Hannon-Dahiya convergence failure in zone %s.\n\tUsing sample maximum fire size",
            polygonID
          )
        )
        maxFireSize <- xMax * maxSizeFactor  #just to be safe, respecify here
      } else {
        maxFireSize <- exp(That) * cellSize
        if (!(maxFireSize > xMax)) {
          warning(
            sprintf("Dodgy maxFireSize estimate in zone %s.\n\tUsing sample maximum fire size.",polygonID)
          )
          maxFireSize <- xMax * maxSizeFactor
        }
        #missing BEACONS CBFA truncated at 2*xMax. Their reasons don't apply here.
      }
    } else {
      message(paste("no fires larger than cellsize in ", polygonID, ". Default values used."))
    }
  } else {
    message(paste("Insufficient data for polygon ", polygonID, ". Default values used."))
  }

  #verify estimation results are reasonable. That=-1 indicates convergence failure.
  #need to addd a name or code for basic verification by Driver module, and time field
  #to allow for dynamic regeneration of disturbanceDriver pars.
  #browser()
  if (maxFireSize < 1){
    warning("this can't happen")
    maxFireSize = cellSize
  }
  return(list(ignitionRate = rate,
              pEscape = pEscape,
              xBar = xBar,
              #mean fire size
              lxBar = lxBar,
              #mean log(fire size)
              xMax = xMax,
              #maximum observed size
              emfs_ha = maxFireSize  #Estimated Maximum Fire Size in ha
  )
  )
}
