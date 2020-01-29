#Buffers polygon, generates index raster
genSimLand <- function(coreLand, buffDist, landscapeLCC = NULL) {
  tempDir <- tempdir()
  #Buffer study Area. #rbind had occasional errors before makeUniqueIDs = TRUE
  #TODO: Investigate why some polygons fail
  bfireRegimePoly <- buffer(coreLand, buffDist)
  if (!gIsValid(bfireRegimePoly, byid = FALSE)) {
    bfireRegimePoly <- gBuffer(bfireRegimePoly, width = 0)
  }
  bfireRegimePoly <-  gDifference(bfireRegimePoly, spgeom2 = coreLand, byid = FALSE)
  polyLandscape <- rbind.SpatialPolygons(coreLand, bfireRegimePoly, makeUniqueIDs = TRUE) #
  polyLandscape$zone <- c("core", "buffer")
  polyLandscape$Value <- c(1, 0)

  #Generate flammability raster -- either from an existing landscapeLCC or de novo/download
  landscapeLCC <- if (is.null(landscapeLCC)) {
    Cache(prepInputsLCC, destinationPath = tempDir, studyArea = polyLandscape, 
          useSAcrs = TRUE, omitArgs = "destinationPath", filename2 = NULL)
  } else {
    Cache(postProcess, landscapeLCC, studyArea = polyLandscape, 
          useSAcrs = TRUE, filename2 = NULL)
  }
  
  
  landscapeFlam <- defineFlammable(landscapeLCC)
  #Generate landscape Index raster
  polySF <- st_as_sf(polyLandscape)
  landscapeIndex <- fasterize(polySF, landscapeLCC, "Value")

  calibrationLandscape <- list(polyLandscape, landscapeIndex, landscapeLCC, landscapeFlam)
  names(calibrationLandscape) <- c("fireRegimePoly", "landscapeIndex", "lcc", "flammableMap")
  return(calibrationLandscape)
}



#make design
#dT <- data.frame("igLoc" = index, p0 = 0.1, p = 0.23)

#this version of makeDesign is the simplest possible...

makeDesign <- function(indices, targetN, pEscape = 0.1, pmin, pmax, q = 1) {
  #TODO: Fix makeDesign to work if polygons have no fires
  sampleSize <- round(targetN / pEscape)
  cellSample <- sample(indices, sampleSize, replace = TRUE)
  pVec <- runif(sampleSize)^q
  pVec <- pVec * (pmax - pmin) + pmin

  #derive p0 from escapeProb
  #steal code from scfmRegime and friends.

  p0 <- 1 - (1 - pEscape)^0.125  #assume 8 neighbours
  #the preceding approximation seems inadequate in practice.
  #when implemented in scfmDriver, make use of correct derivation of p0 from pEscape based on L
  Temp <- data.frame("igLoc" = cellSample, "p0" = p0, "p" = pVec)
  return(Temp)
}

executeDesign <- function(L, dT, maxCells) {
  # extract elements of dT into a three column matrix where column 1,2,3 = igLoc, p0, p

  iter <- 0
  f <- function(x, L, ProbRas) { ## L, P are rasters, passed by reference
    iter <<- iter + 1
    currentTime <- Sys.time()
    diffTime <- currentTime - startTime
    units(diffTime) <- "secs"
    timePer <- as.numeric(diffTime) / iter
    timeLeft <- (NROW(dT) - iter) * timePer
    timeLeft <- round(as.difftime(timeLeft, units = "mins")/60, 1)
    nrowDT <- NROW(dT)
    if (iter %% 200 == 0)
      message("  ", iter, " of ", nrowDT, " total; estimated time remaining: ",
              format(timeLeft, units = "mins"))


    threadsDT <- getDTthreads()
    setDTthreads(1)
    on.exit({setDTthreads(threadsDT)}, add = TRUE)

    i <- x[1]
    p0 <- x[2]
    p <- x[3]

    nbrs <- as.vector(SpaDES.tools::adj(x = L, i, pairs = FALSE, directions = 8))
    #nbrs < nbrs[which(L[nbrs] == 1)] #or this?
    nbrs <- nbrs[L[nbrs] == 1] #only flammable neighbours please. also, verify NAs excluded.
    #nbrs is a vector of flammable neighbours.
    nn <- length(nbrs)
    res <- c(nn, 0, 1)
    if (nn == 0)
      return(res) #really defaults
    #P is still flammableMap.

    ProbRas[nbrs] <- p0
    #Now it is 1, 0, p0, and NA
    spreadState0 <- SpaDES.tools::spread2(landscape = L,
                                          start = i,
                                          iterations = 1,
                                          spreadProb = ProbRas,
                                          asRaster = FALSE)

    tmp <- nrow(spreadState0)
    res[2:3] <- c(tmp - 1,tmp)
    if (tmp == 1) # the fire did not spread
      return(res)
    ProbRas[] <- L[]*p
    spreadState1 <- SpaDES.tools::spread2(landscape = L,
                                          start = spreadState0,
                                          spreadProb = ProbRas,
                                          asRaster = FALSE,
                                          maxSize = maxCells)
    #calculate return data
    res[3] <- nrow(spreadState1)
    return(res)
  }

  probRas <- raster(L)
  probRas[] <- L[]

  startTime <- Sys.time()
  res <- Cache(apply, dT, 1, f, L, ProbRas = probRas) # Parallelizing isn't efficient here. ~TM 15Feb19
  res <- data.frame("nNeighbours" = res[1,], "initSpreadEvents" = res[2,], "finalSize" = res[3,])

  #cbind dT and res, then select the columns we need
  x <- cbind(dT,res)
  x <- x[x$finalSize > 1,]
  return(x)
}

#This is a wrapper on makeDesign and executeDesign
makeAndExecuteDesign <- function(...){

  dots <- list(...)
  designTable <- makeDesign(indices = dots$indices, targetN = dots$targetN,
                            pmin = dots$pmin, pmax = dots$pmax,
                            pEscape = dots$pEscape)

  cD <- executeDesign(L = dots$L,
                      dT = designTable,
                      maxCells = dots$maxCells)

  return(cD)
}


calibrateFireRegimePolys <- function(polygonType, regime,
                                     targetN,  landAttr, cellSize, fireRegimePolys,
                                     buffDist, pJmp, pMin, pMax, neighbours, landscapeLCC = NULL) {

  maxBurnCells <- as.integer(round(regime$emfs_ha / cellSize)) #will return NA if emfs is NA
  if (is.na(maxBurnCells)) {
    warning("maxBurnCells cannot be NA... there is a problem with scfmRegime")
    maxBurnCells = 1
  }
  landAttr <- landAttr[[polygonType]] #landAttr may not be equal length as regime due to invalid polygons
  message("generating buffered landscapes...")
  #this function returns too much data to be worth caching (4 rasters per poly)
  if (is(fireRegimePolys, "quosure"))
    fireRegimePolys <- eval_tidy(fireRegimePolys)
  calibLand <- genSimLand(fireRegimePolys[fireRegimePolys$PolyID == polygonType,], buffDist = buffDist,
                          landscapeLCC = landscapeLCC)

  #Need a vector of igniteable cells
  #Item 1 = L, the flammable Map
  #Item 2 = B (aka the landscape Index) this denotes buffer
  #Item 3 = igLoc(index of igniteable cells) L[igloc] == 1 &&(B[igLoc]) == 1 (ie within core)
  index <- 1:ncell(calibLand$flammableMap)
  index[calibLand$flammableMap[] != 1 | is.na(calibLand$flammableMap[])] <- NA
  index[calibLand$landscapeIndex[] != 1 | is.na(calibLand$landscapeIndex[])] <- NA
  index <- index[!is.na(index)]
  if (length(index) == 0)
    stop("polygon has no flammable cells!")

  message(paste0("calibrating for polygon ", polygonType, " (Time: ", Sys.time(), ")"))

  #these functions have been wrapped to allow for simpler caching
  cD <- Cache(makeAndExecuteDesign,
              indices = index,
              targetN = targetN,
              pmin = pMin, pmax = pMax,
              pEscape = ifelse(regime$pEscape == 0, 0.1, regime$pEscape),
              L = calibLand$flammableMap,
              maxCells = maxBurnCells,
              userTags = c('scfmDriver', "executeDesign", polygonType),
              omitArgs = c("indices"))

  calibModel <- scam::scam(finalSize ~ s(p, bs = "micx", k = 20), data = cD)

  xBar <- regime$xBar / cellSize

  if (xBar > 0) {
    #now for the inverse step.
    Res <- try(stats::uniroot(unirootFunction,
                              calibModel, xBar, # "..."
                              interval = c(min(cD$p), max(cD$p)),
                              extendInt = "no",
                              tol = 0.00001
    ), silent = TRUE)
    if (class(Res) == "try-error") {
      #TODO: should pick the closest value (of min and max) if error is value not of opposite sign
      pJmp <- min(cD$p)
      message("the loess model may underestimate the spread probability for polygon ", polygonType)
    } else {
      pJmp <- Res$root
    }
  } else {
    calibModel <- "No Model"
    Res <- "No Uniroot result"
  }
  #check convergence, and out of bounds errors etc
  w <- landAttr$nNbrs
  w <- w / sum(w)
  hatPE <- regime$pEscape
  if (hatPE == 0) {
    # no fires in polygon zone escapted
    p0 <- 0
  } else if (hatPE == 1) {
    # all fires in polygon zone escaped
    p0 <- 1
  } else {
    res <- optimise(escapeProbDelta,
                    interval = c(hatP0(hatPE, neighbours),
                                 hatP0(hatPE, floor(sum(w * 0:8)))),
                    tol = 1e-4,
                    w = w,
                    hatPE = hatPE)
    p0 <- res[["minimum"]]
    #It is almost obvious that the true minimum must occurr within the interval specified in the
    #call to optimise, but I have not proved it, nor am I certain that the function being minimised is
    #monotone.
  }
  #don't forget to scale by number of years, as well, if your timestep is ever != 1yr
  rate <- regime$ignitionRate * cellSize
  #fireRegimeModel and this module must agree on an annual time step. How to test / enforce?
  pIgnition <- rate
  #approximate Poisson arrivals as a Bernoulli process at cell level.
  #for Poisson rate << 1, the expected values are the same, partially accounting
  #for multiple arrivals within years. Formerly, I used a poorer approximation
  #where 1-p = P[x==0 | lambda=rate] (Armstrong and Cumming 2003).
  driverResult <- list(
    pSpread = pJmp,
    p0 = p0,
    naiveP0 = hatP0(regime$pEscape, 8),
    pIgnition = pIgnition,
    maxBurnCells = maxBurnCells,
    calibModel = calibModel,
    uniroot.Res = Res
  )
  return(driverResult)

}

unirootFunction <- function(x, cM, xBar) {
  predict(cM, list("p" = x)) - xBar
}
