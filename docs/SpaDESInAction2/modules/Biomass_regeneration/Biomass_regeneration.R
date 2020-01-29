# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects, use sim$xxx, and are thus globally available
# to all modules. Functions can be used without sim$ as they are namespaced, like functions
# in R packages. If exact location is required, functions will be: sim$<moduleName>$FunctionName
defineModule(sim, list(
  name = "Biomass_regeneration",
  description = paste("Post-disturbance biomass regeneration module for LandR. Simulates post-fire mortality,",
                      "regeneration and serotiny as part of the same event - all occurring sequentially immeadiately after fire.",
                      "Mortality assumed to be 100%, serotiny and regeneration algorithms taken from LANDIS-II Biomass Succession extension, v3.2.1"),
  keywords = c("biomass regeneration", "LandR", "disturbance", "mortality", "vegetation succession", "vegetation model"),
  authors = c(
    person(c("Eliot", "J", "B"), "McIntire", email = "eliot.mcintire@canada.ca", role = c("aut", "cre")),
    person("Yong", "Luo", email = "yluo1@lakeheadu.ca", role = "aut"),
    person("Ceres", "Barros", email = "cbarros@mail.ubc.ca", role = "aut"),
    person(c("Alex", "M."), "Chubaty", email = "achubaty@friresearch.ca", role = "ctb")
  ),
  childModules = character(0),
  version = list(SpaDES.core = "0.2.3.9009",
                 Biomass_regeneration = "0.1.0",
                 LandR = "0.0.3.9000", SpaDES.core = "0.2.7"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "Biomass_regeneration.Rmd"),
  reqdPkgs = list("crayon", "data.table", "raster", ## TODO: update package list!
                  "PredictiveEcology/LandR@development",
                  "PredictiveEcology/pemisc@development"),
  parameters = rbind(
    defineParameter("calibrate", "logical", FALSE, desc = "Do calibration? Defaults to FALSE"),
    defineParameter("fireInitialTime", "numeric", NA,
                    desc = "The event time that the first fire disturbance event occurs"),
    defineParameter("fireTimestep", "numeric", NA,
                    desc = "The number of time units between successive fire events in a fire module"),
    defineParameter("successionTimestep", "numeric", 10L, NA, NA, "defines the simulation time step, default is 10 years")
  ),
  inputObjects = bind_rows(
    expectsInput("cohortData", "data.table",
                 desc = "age cohort-biomass table hooked to pixel group map by pixelGroupIndex at
                 succession time step"),
    expectsInput("inactivePixelIndex", "logical",
                 desc = "internal use. Keeps track of which pixels are inactive"),
    expectsInput("pixelGroupMap", "RasterLayer",
                 desc = "updated community map at each succession time step"),
    expectsInput("rstCurrentBurn", "RasterLayer",
                 desc = "Binary raster of fires, 1 meaning 'burned', 0 or NA is non-burned"),
    expectsInput("species", "data.table",
                 desc = "a table that has species traits such as longevity...",
                 sourceURL = "https://raw.githubusercontent.com/LANDIS-II-Foundation/Extensions-Succession/master/biomass-succession-archive/trunk/tests/v6.0-2.0/species.txt"),
    expectsInput("speciesEcoregion", "data.table",
                 desc = "table defining the maxANPP, maxB and SEP, which can change with both ecoregion and simulation time",
                 sourceURL = "https://raw.githubusercontent.com/LANDIS-II-Foundation/Extensions-Succession/master/biomass-succession-archive/trunk/tests/v6.0-2.0/biomass-succession-dynamic-inputs_test.txt"),
    expectsInput("studyArea", "SpatialPolygonsDataFrame",
                 desc = paste("Polygon to use as the study area.",
                             "Defaults to  an area in Southwestern Alberta, Canada."),
                 sourceURL = ""),
    expectsInput("sufficientLight", "data.frame",
                 desc = "table defining how the species with different shade tolerance respond to stand shadeness",
                 sourceURL = "https://raw.githubusercontent.com/LANDIS-II-Foundation/Extensions-Succession/master/biomass-succession-archive/trunk/tests/v6.0-2.0/biomass-succession_test.txt"),
    expectsInput("treedFirePixelTableSinceLastDisp", "data.table",
                 desc = "3 columns: pixelIndex, pixelGroup, and burnTime. Each row represents a forested pixel that was burned up to and including this year, since last dispersal event, with its corresponding pixelGroup and time it occurred")
  ),
  outputObjects = bind_rows(
    createsOutput("cohortData", "data.table",
                  desc = paste("age cohort-biomass table hooked to pixel group map",
                               "by pixelGroupIndex at succession time step")),
    createsOutput("lastFireYear", "numeric",
                  desc = "Year of the most recent fire year"),
    createsOutput("pixelGroupMap", "RasterLayer",
                  desc = "updated community map at each succession time step"),
    createsOutput("serotinyResproutSuccessPixels", "numeric",
                  desc = "Pixels that were successfully regenerated via serotiny or resprouting. This is a subset of treedBurnLoci"),
    createsOutput("postFireRegenSummary", "data.table",
                  desc = "summary table of species post-fire regeneration"),
    createsOutput("treedFirePixelTableSinceLastDisp", "data.table",
                  desc = "3 columns: pixelIndex, pixelGroup, and burnTime. Each row represents a forested pixel that was burned up to and including this year, since last dispersal event, with its corresponding pixelGroup and time it occurred")
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.Biomass_regeneration <- function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      # do stuff for this event
      sim <- Init(sim)

      ## schedule events
      sim <- scheduleEvent(sim, P(sim)$fireInitialTime,
                           "Biomass_regeneration", "fireDisturbance",
                           eventPriority = 3)
    },
    fireDisturbance = {
      if(!is.null(sim$rstCurrentBurn)) {
        sim <- FireDisturbance(sim)
      } else {
        message(crayon::red(paste0("The Biomass_regeneration module is expecting sim$rstCurrentBurn;\n",
                                   "Currently, it does not exist, so no regeneration will happen")))
      }
      sim <- scheduleEvent(sim, time(sim) + P(sim)$fireTimestep,
                           "Biomass_regeneration", "fireDisturbance",
                           eventPriority = 3)
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

### template initialization
Init <- function(sim) {
  ## check parameters
  if (is.na(P(sim)$fireInitialTime))
    stop(paste("Please provide a value for `P(sim)$fireInitialTime`.",
               "It should match the first year of fire."))
  if (is.na(P(sim)$fireTimestep))
    stop(paste("Please provide a value for `P(sim)$fireTimestep`.",
               "It should match the fire time step (fire frequency)."))
  return(invisible(sim))
}

## Fire disturbance regeneration event
FireDisturbance <- function(sim, verbose = getOption("LandR.verbose", TRUE)) {
  # the presence of valid fire can cause three processes:
  # 1. remove species cohorts from the pixels that have been affected.
  # 2. initiate the post-fire regeneration
  # 3. change of cohortdata and pixelgroup map
  # may be a supplemenatary function is needed to convert non-logical map
  # to a logical map
  if (isTRUE(getOption("LandR.assertions"))) {
    if (!identical(NROW(sim$cohortData), NROW(unique(sim$cohortData, by = c("pixelGroup", "speciesCode", "age", "B"))))) {
      stop("sim$cohortData has duplicated rows, i.e., multiple rows with the same pixelGroup, speciesCode and age")
    }
  }
  # if (time(sim) >= 11) {
  #   browser()
  #   aaaa <<- 1
  # }

  postFirePixelCohortData <- sim$cohortData[0,]
  postFirePixelCohortData[, `:=`(pixelIndex = integer(),
                                 age = NULL, B = NULL, mortality = NULL,
                                 aNPPAct = NULL)]

  # In some cases sumB exists, but not always -- we want to remove it too here.
  if (isTRUE("sumB" %in% colnames(postFirePixelCohortData))) {
    set(postFirePixelCohortData, NULL, "sumB", NULL)
  }

  if (P(sim)$calibrate & is.null(sim$postFireRegenSummary)) {  ## don't overwrite
    sim$postFireRegenSummary <- data.table(year = numeric(),
                                           regenMode = character(),
                                           species = character(),
                                           numberOfRegen = numeric())
  }

  # if (!is.null(sim$rstCurrentBurn)) { # anything related to fire disturbance
  #   if (extent(sim$rstCurrentBurn) != extent(pixelGroupMap)) {
  #     sim$rstCurrentBurn <- raster::crop(sim$rstCurrentBurn, extent(pixelGroupMap))
  #   }
  # }

  ## extract burn pixel indices/groups and remove potentially inactive pixels
  burnedLoci <- which(getValues(sim$rstCurrentBurn) > 0)
  treedBurnLoci <- if (length(sim$inactivePixelIndex) > 0) {
    # These can burn other vegetation (grassland, wetland)
    burnedLoci[!(burnedLoci %in% sim$inactivePixelIndex)] # this is to prevent avaluating the pixels that are inactive
  } else {
    burnedLoci
  }
  treedFirePixelTableSinceLastDisp <- data.table(pixelIndex = as.integer(treedBurnLoci),
                                                 pixelGroup = as.integer(getValues(sim$pixelGroupMap)[treedBurnLoci]),
                                                 burnTime = time(sim))
  ## TODO: Ceres: maybe this should come at the end, lest we introduce pixelGroups taht burned in previous years,
  ## but aren't currently burning
  # sim$treedFirePixelTableSinceLastDisp[, pixelGroup := as.integer(getValues(sim$pixelGroupMap))[pixelIndex]]
  # # append previous year's
  # treedFirePixelTableSinceLastDisp <- rbindlist(list(sim$treedFirePixelTableSinceLastDisp,
  #                                                    treedFirePixelTableSinceLastDisp))

  ## make table spp/ecoregionGroup/age in burnt pixels
  burnedPixelCohortData <- sim$cohortData[pixelGroup %in% unique(treedFirePixelTableSinceLastDisp$pixelGroup)]
  set(burnedPixelCohortData, NULL, c("B", "mortality", "aNPPAct"), NULL)

  ## select the pixels that have burned survivors and assess them
  burnedPixelTable <- treedFirePixelTableSinceLastDisp[pixelGroup %in% unique(burnedPixelCohortData$pixelGroup)]
  ## expadn table to pixels
  burnedPixelCohortData <- burnedPixelTable[burnedPixelCohortData, allow.cartesian = TRUE,
                                            nomatch = 0, on = "pixelGroup"] ##

  ## CALCULATE SIDE SHADE -----------------------------
  # assume the fire burns all cohorts on site - siteShade calc is no longer part of serotiny.
  # sumB is not actually necessary, but added for consistency w/ Biomass_regenerationPM
  set(burnedPixelCohortData, NULL, c("sumB", "siteShade"), 0)
  setkey(burnedPixelCohortData, speciesCode)

  ## DO SEROTINY -----------------------------
  ## assess potential serotiny reg: add sexual maturity to the table and compare w/ age
  ## as long as one cohort is sexually mature, serotiny is activated
  serotinyOutputs <- doSerotiny(burnedPixelCohortData = burnedPixelCohortData,
                                species = sim$species, currentTime = time(sim),
                                treedFirePixelTableSinceLastDisp = treedFirePixelTableSinceLastDisp,
                                sufficientLight = sim$sufficientLight,
                                speciesEcoregion = sim$speciesEcoregion,
                                calibrate = P(sim)$calibrate,
                                postFirePixelCohortData = postFirePixelCohortData,
                                postFireRegenSummary = sim$postFireRegenSummary)

  postFirePixelCohortData <- serotinyOutputs$postFirePixelCohortData
  serotinyPixel <- serotinyOutputs$serotinyPixel

  if (!is.null(serotinyOutputs$postFireRegenSummary))
    sim$postFireRegenSummary <- serotinyOutputs$postFireRegenSummary

  rm(serotinyOutputs)

  ## DO RESPROUTING --------------------------
  ## assess resprouting reproduction:
  ## basically same thing as serotiny
  resproutingOutputs <- doResprouting(serotinyPixel = serotinyPixel,
                                      treedFirePixelTableSinceLastDisp = treedFirePixelTableSinceLastDisp,
                                      burnedPixelCohortData = burnedPixelCohortData,
                                      postFirePixelCohortData = postFirePixelCohortData,
                                      currentTime = time(sim), species = sim$species,
                                      sufficientLight = sim$sufficientLight,
                                      calibrate = P(sim)$calibrate,
                                      postFireRegenSummary = sim$postFireRegenSummary)

  postFirePixelCohortData <- resproutingOutputs$postFirePixelCohortData
  sim$serotinyResproutSuccessPixels <- resproutingOutputs$serotinyResproutSuccessPixels
  if (!is.null(resproutingOutputs$postFireRegenSummary))
    sim$postFireRegenSummary <- resproutingOutputs$postFireRegenSummary

  rm(resproutingOutputs)

  ## ADD NEW COHORTS -----------------------------
  ## add new cohorts to pixels where serotiny/regeneration were activated
  if (NROW(postFirePixelCohortData) > 0) {
    ## redo post-fire pixel groups by adding the maxPixelGroup to their ecoregioMap values
    if (!is.null(sim$serotinyResproutSuccessPixels)) {

      # Add new cohorts to BOTH the sim$cohortData and sim$pixelGroupMap
      ## reclassify pixel groups as burnt (0L)
      if (verbose > 0)
        message(blue("Post serotiny and resprouting"))

      outs <- updateCohortData(newPixelCohortData = postFirePixelCohortData,
                               cohortData = sim$cohortData,
                               pixelGroupMap = sim$pixelGroupMap,
                               currentTime = round(time(sim)),
                               speciesEcoregion = sim$speciesEcoregion,
                               treedFirePixelTableSinceLastDisp = treedFirePixelTableSinceLastDisp,
                               successionTimestep = P(sim)$successionTimestep)

      sim$cohortData <- outs$cohortData
      sim$pixelGroupMap <- outs$pixelGroupMap
      sim$pixelGroupMap[] <- as.integer(sim$pixelGroupMap[])
      ##########################################################
      # rm missing cohorts (i.e., those pixelGroups that are gone due to the fire/treedFirePixelTableSinceLastDisp)
      ##########################################################
    }
  }

  sim$lastFireYear <- time(sim)
  ## TODO: Ceres potential bug fix. Move this from beggining to here.
  sim$treedFirePixelTableSinceLastDisp[, pixelGroup := as.integer(getValues(sim$pixelGroupMap))[pixelIndex]]
  # append previous year's
  treedFirePixelTableSinceLastDisp <- rbindlist(list(sim$treedFirePixelTableSinceLastDisp,
                                                     treedFirePixelTableSinceLastDisp))

  sim$treedFirePixelTableSinceLastDisp <- treedFirePixelTableSinceLastDisp
  return(invisible(sim))
}

## ---------------------------------------------------------------------------
## INPUT OBJECTS

.inputObjects <- function(sim) {
  cacheTags <- c(currentModule(sim), "function:.inputObjects")
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  if (suppliedElsewhere(object = "scfmReturnInterval", sim = sim, where = "sim")) {
    if (P(sim)$fireTimestep != sim$scfmReturnInterval) {
      sim@params$Biomass_regeneration$fireTimestep <- sim$scfmReturnInterval
      message(paste0("Biomass_regeneration detected 'scfm' fire model. Setting fireTimestep to ",
                     sim$scfmReturnInterval, " to match it.")) ## TODO: don't hardcode module interdependencies!
    }
  } else {
    if (is.null(P(sim)$fireTimestep)) {
      P(sim)$fireTimestep <- 1L
      message("fireTimestep is 'NULL'. Setting to 1 unit of time")
    }
  }

  if (!suppliedElsewhere("studyArea", sim)) {
    message("'studyArea' was not provided by user. Using a polygon (6250000 m^2) in southwestern Alberta, Canada")
    sim$studyArea <- randomStudyArea(seed = 1234, size = (250^2)*100)
  }

  ## get LANDISII main input table where species and light requirements tables come from
  if (!suppliedElsewhere("sufficientLight", sim) |
      (!suppliedElsewhere("species", sim))) {
    mainInput <- prepInputsMainInput(url = NULL, dPath, cacheTags) ## uses default URL
  }

  ## read species txt and convert it to data table
  if (!suppliedElsewhere("species", sim)) {
    sim$species <- prepInputsSpecies(url = extractURL("species"), dPath, cacheTags)
  }

  ## make light requirements table
  if (!suppliedElsewhere("sufficientLight", sim)) {
    sufficientLight <- data.frame(mainInput)
    startRow <- which(sufficientLight$col1 == "SufficientLight")
    sufficientLight <- sufficientLight[(startRow + 1):(startRow + 5), 1:7]
    sufficientLight <- data.table(sufficientLight)
    sufficientLight <- sufficientLight[, lapply(.SD, function(x) as.numeric(x))]

    names(sufficientLight) <- c("speciesshadetolerance",
                                "X0", "X1", "X2", "X3", "X4", "X5")
    sim$sufficientLight <- data.frame(sufficientLight)
  }

  #  # input species ecoregion dynamics table
  if (!suppliedElsewhere("speciesEcoregion", sim)) {
    sim$speciesEcoregion <- prepInputsSpeciesEcoregion(url = extractURL("speciesEcoregion"),
                                                       dPath = dPath, cacheTags = cacheTags)
  }

  if (!suppliedElsewhere(sim$treedFirePixelTableSinceLastDisp)) {
    sim$treedFirePixelTableSinceLastDisp <- data.table(pixelIndex = integer(), pixelGroup = integer(),
                                                       burnTime = numeric())
  }

  return(invisible(sim))
}
