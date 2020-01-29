
# Everything in this file gets sourced during simInit, and all functions and objects
#  are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = "ageModule",
  description = "Creates and maintains a raster called ageMap",
  keywords = c("forest age", "modelling course", "Lab 5"),
  authors = c(person(c("Steve", "G"), "Cumming", email="stevec@sbf.ulaval.ca", role=c("aut", "cre"))),
  childModules = character(),
  version = numeric_version("0.9.0"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "ageModule.Rmd"),
  reqdPkgs = list("raster","RColorBrewer"),
  parameters = rbind(
    defineParameter("initialAge", "numeric", 99.0, 0, 1e4, desc =  "initial age"),
    defineParameter("maxAge","numeric", 200, 0, 2**16-1, desc = "maximum age for plotting"),
    defineParameter("returnInterval", "numeric", 1.0, NA, NA, desc = "Time interval between aging aevents"),
    defineParameter("startTime", "numeric", start(sim), NA, NA, desc = "Simulation time at which to initiate aging"),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur")
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "flammableMap", objectClass = "RasterLayer", desc = "map of flammability vegetation"),
    expectsInput(objectName = "ageMap", objectClass = "RasterLayer",
                 desc = "stand age map in study area, default is Canada national stand age map",
                 sourceURL = "http://tree.pfc.forestry.ca/kNN-StructureStandVolume.tar"),
    expectsInput(objectName = "studyArea", objectClass = "SpatialPolygonsDataFrame",
                 desc = "study area template",
                 sourceURL = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/district/ecodistrict_shp.zip"),
    expectsInput(objectName = "rasterToMatch", objectClass = "RasterLayer", desc = "template raster for raster GIS operations. Must be supplied by user")
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "ageMap", objectClass = "RasterLayer", desc = "map of vegetation age")
  )
))

## event types
#   - type `init` is required for initiliazation

doEvent.ageModule = function(sim, eventTime, eventType, debug = FALSE) {
  if (eventType == "init") {
    ### check for more detailed object dependencies:
    ### (use `checkObject` or similar)

    # do stuff for this event
    sim <- Init(sim)

    # schedule future event(s)
    sim <- scheduleEvent(sim, P(sim)$startTime, "ageModule", "age", eventPriority = 7.5)
    sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "ageModule", "plot", eventPriority = 7.5)
    sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "ageModule", "save", eventPriority = 7.5)
  } else if (eventType=="age") {
    # do stuff for this event
    sim <- Age(sim)

    # schedule the next event
    sim <- scheduleEvent(sim, time(sim) + P(sim)$returnInterval,
                         "ageModule", "age")
  } else if (eventType == "plot") {

    Plot(sim$ageMap, legendRange=c(0, P(sim)$maxAge))
    sim <- scheduleEvent(sim,
                         time(sim) + P(sim)$.plotInterval,
                         "ageModule", "plot")

  }  else {
    warning(paste("Undefined event type: '", events(sim)[1, "eventType", with = FALSE],
                  "' in module '", events(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  }
  return(invisible(sim))
}


### template initilization
Init <- function(sim) {

 dPath <- dataPath(sim)
 preProcess(url = "http://tree.pfc.forestry.ca/kNN-StructureStandVolume.tar",
            destinationPath = file.path(dPath, "age"))
 ageMap <- prepInputs(targetFile = file.path(dPath, "NFI_MODIS250m_kNN_Structure_Stand_Age_v0.tif"),
                      archive = file.path(dPath, "NFI_MODIS250m_kNN_Structure_Stand_Age_v0.zip"),
                      studyArea = sim$studyArea,
                      rasterToMatch = sim$rasterToMatch,
                      destinationPath = file.path(dPath, "age"),
                      overwrite = TRUE,
                      filename2 = TRUE,
                      userTags = c("ageMap"),
                      method = "ngb")

 sim$ageMap <- ageMap

 # we will use our colour choices, not whatever may have come with the loaded map.
 setColors(sim$ageMap, n = 10, colorRampPalette(c("LightGreen", "DarkGreen"))(10))
 #temporary until we buid the rest of the modules
  return(invisible(sim))
}

### template for save events
Save <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sim <- saveFiles(sim)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

Age <- function(sim) {

  sim$ageMap <- setValues(sim$ageMap, pmin(P(sim)$maxAge, getValues(sim$ageMap)+ P(sim)$returnInterval))

  return(invisible(sim))
}

.inputObjects <- function(sim) {
  dPath <- dataPath(sim)

  if (!suppliedElsewhere("studyArea", sim)) {
    message("study area not supplied. Using random polygon in Alberta")

    studyArea <- pemisc::randomStudyArea(size = 2000000000, seed = 23657)
    sim$studyArea <- studyArea
  }

  return(invisible(sim))
}
