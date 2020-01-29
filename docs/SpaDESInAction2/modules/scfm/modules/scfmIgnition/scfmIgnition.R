# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = "scfmIgnition",
  description = "start a random number of fires",
  keywords = c("fire scfm ignition"),
  authors = c(person(c("Steve", "Cumming"), "Last", email = "email@example.com", role = c("aut", "cre"))),
  childModules = character(),
  version = numeric_version("1.1.0.9002"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "scfmIgnition.Rmd"),
  reqdPkgs = list("raster", "SpaDES.tools", "PredictiveEcology/LandR@development"),
  parameters = rbind(
    #need a Flash parameter controlling fixed number of fires, a la Ratz (1995)
    defineParameter("pIgnition", "numeric", 0.001, 0, 1, desc = "per cell and time ignition probability"),
    defineParameter("startTime", "numeric", start(sim), NA, NA, desc = "simulation time of first ignition"),
    defineParameter("returnInterval", "numeric", 1.0, NA, NA, desc = "interval between main events"),
    defineParameter(".useCache", "character", c(".inputObjects", "init"), NA, NA,
                    desc = "Internal. Can be names of events or the whole module name; these will be cached by SpaDES")
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "scfmDriverPars", objectClass = "list", desc = "fire modules' parameters"),
    expectsInput(objectName = "flammableMap", objectClass = "RasterLayer", desc = "map of flammability"),
    expectsInput(objectName = "landscapeAttr", objectClass = "list", desc = "")
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "ignitionLoci", objectClass = "numeric", desc = ""),
    createsOutput(objectName = "pIg", objectClass = "numeric", desc = "ignition probability raster")
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.scfmIgnition = function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,
    init = {
      sim <- Init(sim)
      sim <- scheduleEvent(sim, P(sim)$startTime, "scfmIgnition", "ignite", eventPriority = 7.5)
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "scfmIgnition", "plot", eventPriority = 7.5)
    },
    ignite = {
      if (LandR::scheduleDisturbance(sim$rstCurrentBurn, currentYear = time(sim))) {
        sim <- Ignite(sim)
      }
      sim <- scheduleEvent(sim, time(sim) + P(sim)$returnInterval, "scfmIgnition", "ignite", eventPriority = 7.5)
    },
    warning(paste("Undefined event type: '", events(sim)[1, "eventType", with = FALSE],
                  "' in module '", events(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

Init <- function(sim) {

  #if either of these is a map, it needs to have NAs in the right place
  #and be conformant with flammableMap
  if ("scfmDriverPars" %in% ls(sim)) {
    if (length(sim$scfmDriverPars) > 1) {
      pIg <- raster(sim$flammableMap)
      for (x in names(sim$scfmDriverPars)) {
        pIg[sim$landscapeAttr[[x]]$cellsByZone] <- sim$scfmDriverPars[[x]]$pIgnition
      }
      pIg[] <- pIg[] * (sim$flammableMap[])
    } else {
      pIg <- sim$scfmDriverPars[[1]]$pIgnition #and pIg is a constant from scfmDriver
    }
  } else {
    pIg <- P(sim)$pIgnition #and pIg is a constant from the parameter list
  }
  sim$pIg <- pIg

  sim$ignitionLoci <- NULL

  return(invisible(sim))
}

### template for your event1
Ignite <- function(sim) {
  sim$ignitionLoci <- NULL #initialise FFS
  ignitions <- lapply(names(sim$scfmDriverPars),
                      function(polygonType,
                               landscapeAttr = sim$landscapeAttr,
                               pIg = sim$pIg) {
    cells <- landscapeAttr[[polygonType]]$cellsByZone
    if (is(pIg, "Raster")) {
      cells[which(runif(length(cells)) < pIg[cells])]
    } else {
      cells[which(runif(length(cells)) < pIg)]
    }
  })
  #resample generates a random permutation of the elements of ignitions
  #so that we don't always sequence in map index order. EJM pointed this out.
  sim$ignitionLoci <- SpaDES.tools:::resample(unlist(ignitions))

  return(invisible(sim))
}

.inputObjects <- function(sim) {

 if (!suppliedElsewhere(object = "scfmReturnInterval", sim = sim, where = "sim"))
   sim$scfmReturnInterval <- P(sim)$returnInterval

  return(invisible(sim))
}
