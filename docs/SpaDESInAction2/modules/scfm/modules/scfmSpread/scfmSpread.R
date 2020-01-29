# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = "scfmSpread",
  description = "model fire spread",
  keywords = c("fire", "spread", "scfm"),
  authors = c(person("Steve", "Cumming", email = "stevec@sbf.ulaval.ca", role = c("aut")),
              person("Ian", "Eddy", email = "ian.eddy@canada.ca", role = c("aut"))),
  childModules = character(),
  version = numeric_version("1.1.0.9002"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "scfmSpread.Rmd"),
  reqdPkgs = list("data.table", "magrittr", "raster", "reproducible",
                  "SpaDES.tools", "PredictiveEcology/LandR@development"),
  parameters = rbind(
    defineParameter("neighbours", "numeric", 8, NA, NA, "Number of immediate cell neighbours"),
    defineParameter("pSpread", "numeric", 0.23, 0, 1, desc = "spread module for BEACONs"),
    defineParameter("returnInterval", "numeric", 1.0, NA, NA, desc = "Time interval between burn events"),
    defineParameter("startTime", "numeric", start(sim), NA, NA, desc = "Simulation time at which to initiate burning"),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", 1, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    #defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    #defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".useCache", "character", c(".inputObjects", "init"), NA, NA,
                    desc = "Internal. Can be names of events or the whole module name; these will be cached by SpaDES")
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "scfmDriverPars", objectClass = "list", desc = "fire modules' parameters"),
    expectsInput(objectName = "spreadState", objectClass = "data.table", desc = "see SpaDES.tools::spread2"),
    expectsInput(objectName = "flammableMap", objectClass = "RasterLayer", desc = "binary map of landscape flammability")
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "burnMap", objectClass = "RasterLayer", desc = "cumulative burn map"),
    createsOutput(objectName = "burnDT", objectClass = "data.table", desc = "data table with pixel IDs of most recent burn"),
    createsOutput(objectName = "rstCurrentBurn", object = "RasterLayer", desc = "annual burn map"),
    createsOutput(objectName = "pSpread", object = "RasterLayer", desc = "spread probability applied to flammabiliy Map"),
    createsOutput(objectName = "burnSummary", object = "data.table", desc = "describes details of all burned pixels")
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.scfmSpread = function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,
    init = {
      sim <- Init(sim)
      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$startTime, "scfmSpread", "burn", 7.5)
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "scfmSpread", "plot", 
                           eventPriority = .last())
    },
    plot = {
      burnMap <- sim$burnMap
      Plot(burnMap, legend = FALSE, title = "burn map")
      sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "scfmSpread", "plot", 
                           eventPriority = .last())
    },
    burn = {
      if (LandR::scheduleDisturbance(sim$rstCurrentBurn, currentYear = time(sim))) {
        
        if (!is.null(sim$spreadState)) {
          ## we really want to test if the data table has any rows
          if (NROW(sim$spreadState[state == "activeSource"]) > 0)
            sim <- Burnemup(sim)
        }
      }
      sim <- scheduleEvent(sim, time(sim) + P(sim)$returnInterval, "scfmSpread", "burn", eventPriority = 7.5)
    },
    
    plot = {
      if (!is.null(sim$rstCurrentBurn)){
        Plot(sim$rstCurrentBurn, new = TRUE, col = c("grey", "red"),
             title = paste0("annual burn ", time(sim)))
        Plot(sim$burnMap, title = "Cumulative Burn", new = TRUE)
      }
      sim <- scheduleEvent(sim, eventTime = time(sim) + P(sim)$.plotInterval, "scfmSpread", "plot", eventPriority = 8)
    },
    warning(paste("Undefined event type: '", events(sim)[1, "eventType", with = FALSE],
                  "' in module '", events(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

Init <- function(sim) {
  sim$burnMap <- sim$flammableMap
  sim$burnMap[] <- sim$flammableMap[] * 0  # 0 * NA = NA
  if ("scfmDriverPars" %in% ls(sim)) {
    if (length(sim$scfmDriverPars) > 1) {
      pSpread <- raster(sim$flammableMap)
      for (x in names(sim$scfmDriverPars)) {
        pSpread[sim$landscapeAttr[[x]]$cellsByZone] <- sim$scfmDriverPars[[x]]$pSpread
      }
      pSpread[] <- pSpread[] * (sim$flammableMap[])
    } else {
      pSpread <- sim$flammableMap * sim$scfmDriverPars[[1]]$pSpread
    }
  } else {
    pSpread <- sim$pSpread * sim$flammableMap
  }
  sim$pSpread <- pSpread
  #Create empty data table to store each year's burn data
  sim$burnSummary <- data.table("igLoc" = numeric(0),
                                "N" = numeric(0),
                                "year" = numeric(0),
                                "areaBurned" = numeric(0),
                                "polyID" = numeric(0))
  
  return(invisible(sim))
}

Burnemup <- function(sim) {
  ## name is a homage to Walters and Hillborne
  
  # maxSizes <- unlist(lapply(sim$scfmDriverPars, function(x) x$maxBurnCells))
  # activeLoci <- unique(sim$spreadState$initialLocus) # indices[sim$spreadState$active]
  #we prevent multiple ignitions, which shouldn't happen anyway.
  # maxSizes <- maxSizes[sim$cellsByZone[activeLoci, "zone"]]
  threadsDT <- getDTthreads()
  setDTthreads(1)
  on.exit({setDTthreads(threadsDT)}, add = TRUE)
  
  sim$burnDT <- SpaDES.tools::spread2(sim$flammableMap,
                                      start = sim$spreadState,
                                      spreadProb = sim$pSpread,
                                      #spreadState = sim$spreadState,
                                      directions = P(sim)$neighbours,
                                      # maxSize = maxSizes,  #not sure this works
                                      asRaster = FALSE)
  sim$rstCurrentBurn <- sim$vegMap #This preserves NAs
  names(sim$rstCurrentBurn) <- NULL
  sim$rstCurrentBurn[!is.na(sim$rstCurrentBurn)] <- 0 #reset annual burn
  sim$rstCurrentBurn[sim$burnDT$pixels] <- 1 #update annual burn
  sim$rstCurrentBurn@data@attributes <- list("Year" == time(sim))
  
  sim$burnMap[sim$burnDT$pixels] <- 1 #update cumulative burn
  sim$burnMap <- setColors(sim$burnMap, value = c("grey", "red"))
  sim$ageMap[sim$burnDT$pixels] <- 0 #update age
  
  #get fire year, pixels burned, area burned, poly ID of all burned pixels
  tempDT <- sim$burnDT[, .(.N), by = "initialPixels"]
  tempDT$year <- time(sim)
  tempDT$areaBurned <- tempDT$N * sim$landscapeAttr[[1]]$cellSize
  tempDT$polyID <- sim$fireRegimeRas[tempDT$initialPixels]
  setnames(tempDT, c("initialPixels"), c("igLoc"))
  sim$burnSummary <- rbind(sim$burnSummary, tempDT)
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  
  
  return(invisible(sim))
}
