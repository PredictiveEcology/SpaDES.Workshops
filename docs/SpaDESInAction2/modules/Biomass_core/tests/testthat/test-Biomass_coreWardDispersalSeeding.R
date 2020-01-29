test_that("test Ward dispersal seeding algorithm",{
  # define the module and path
  module <- list("Biomass_core")
  path <- list(modulePath = "~/GitHub/nrv-succession/code blitz succession/Module_Biomass_core",
               outputPath = "~/output")
  parameters <- list(.progress = list(type = "graphical", interval = 1),
                     .globals = list(verbose = FALSE),
                     Biomass_core = list(.saveInitialTime = NA))

# 1. testing how the species seeds spread into the neighbor empty cells
# the cohort data is set to not allow on site regeneration
  ecoregionMap <- raster(xmn = 50, xmx = 50 + 99 * 100,
                        ymn = 50, ymx = 50 + 99 * 100,
                        res = c(100, 100), val = 1)
  pixelGroupMap <- setValues(ecoregionMap,2)
  c <- expand.grid(data.frame(a = seq(5, 99, by = 9), b = seq(5, 99, by = 9)))
  pixelindex <- (c$a - 1) * 99 + c$b #121
  pixelGroupMap[pixelindex] <- 1
  minRelativeB <- data.table(ecoregion = "eco1", X1 = 0.15,
                             X2 = 0.25, X3 = 0.5, X4 = 0.8,
                             X5 = 0.95, ecoregionGroup = 1)
  sufficientLight <- read.csv("~/GitHub/nrv-succession/code blitz succession/modeltesting-data/sufficientLight.csv",
                              header = TRUE, stringsAsFactor = FALSE)
  lastFireYear <- "noFire"
  activePixelIndex <- 1:9801
  lastReg <- 0
  successionTimestep <- 1
  calibrate <- TRUE
  regenerationOutput <- data.table(seedingAlgorithm = character(), species = character(),
                                   Year = numeric(), numberOfReg = numeric())
  species <- read.csv("~/GitHub/nrv-succession/code blitz succession/modeltesting-data/species.csv",
                      header = TRUE, stringsAsFactors = FALSE)
  species <- data.table(species)[, speciesCode := 1:16]
  speciesEcoregion <- read.csv("~/GitHub/nrv-succession/code blitz succession/modeltesting-data/speciesEcoregion.csv",
                               header = TRUE, stringsAsFactors = FALSE)
  speciesEcoregion <- data.table(speciesEcoregion)[, ecoregionGroup := as.numeric(as.factor(ecoregion))]
  tempsp <- setkey(species[,.(species,speciesCode)], species)
  speciesEcoregion <- setkey(speciesEcoregion,species)[tempsp]
  sufficientLight <- read.csv("~/GitHub/nrv-succession/code blitz succession/modeltesting-data/sufficientLight.csv",
                              header = TRUE, stringsAsFactors = FALSE)
  seedingAlgorithm <- "wardDispersal"
  cohortData <- data.table(pixelGroup = 1, ecoregionGroup = 1L,
                           speciesCode = 7:10, age = 41, B = 8000L,
                           mortality = 50, aNPPAct = 1079.75965551773)
  objects <- list("pixelGroupMap" = pixelGroupMap,
                  "speciesEcoregion" = speciesEcoregion,
                  "species" = species,
                  "successionTimestep" = successionTimestep,
                  "calibrate" = calibrate,
                  "seedingAlgorithm" = seedingAlgorithm,
                  "minRelativeB" = minRelativeB,
                  "sufficientLight" = sufficientLight,
                  "lastFireYear" = lastFireYear,
                  "activePixelIndex" = activePixelIndex,
                  "lastReg" = lastReg,
                  "regenerationOutput" = regenerationOutput,
                  "cohortData" = cohortData,
                  "ecoregionMap" = ecoregionMap)
  mySim <- simInit(times = list(start = 0, end = 2),
                   params = parameters,
                   modules = module,
                   objects = objects,
                   paths = path)
  set.seed(1)

  source(file.path(modulePath(mySim), "Biomass_core", "R", "seedDispersalLANDIS.R"))
  if (exists("Biomass_coreWardDispersalSeeding")) {
    output <- Biomass_coreWardDispersalSeeding(mySim)
  } else {
    output <- mySim$Biomass_coreWardDispersalSeeding(mySim)
  }
  output <- output$regenerationOutput$numberOfReg
  expect_equal(output,c(283,483,288,358))
  rm(pixelGroupMap, output,activePixelIndex,cohortData,objects,mySim)

  pixelGroupMap <- setValues(ecoregionMap,0)
  pixelGroupMap[pixelindex] <- 1
  pixelGroupMap[1:297] <- -1
  activePixelIndex <- 298:9801
  pixelGroupMap[sort(pixelindex)[1:11]] <- 3
  lastFireYear <- 0
  cohortData <- rbind(data.table(pixelGroup = 1, ecoregionGroup = 1L,
                                 speciesCode = 7:10, age = 41, B = 8000L,
                                 mortality = 50, aNPPAct = 1080),
                      data.table(pixelGroup = 3, ecoregionGroup = 1L,
                                 speciesCode = 7:10, age = 1, B = 8000L,
                                 mortality = 50, aNPPAct = 1090))
  inactivePixelIndex <- 1:297
  postFirePixel <- sort(pixelindex)[1:11]
  objects <- list("pixelGroupMap" = pixelGroupMap,
                  "speciesEcoregion" = speciesEcoregion,
                  "species" = species,
                  "successionTimestep" = successionTimestep,
                  "calibrate" = calibrate,
                  "seedingAlgorithm" = seedingAlgorithm,
                  "minRelativeB" = minRelativeB,
                  "sufficientLight" = sufficientLight,
                  "lastFireYear" = lastFireYear,
                  "activePixelIndex" = activePixelIndex,
                  "lastReg" = lastReg,
                  "regenerationOutput" = regenerationOutput,
                  "cohortData" = cohortData,
                  "ecoregionMap" = ecoregionMap,
                  "inactivePixelIndex" = inactivePixelIndex,
                  "postFirePixel" = postFirePixel)
  mySim <- simInit(times = list(start = 0, end = 2),
                   params = parameters,
                   modules = module,
                   objects = objects,
                   paths = path)
  set.seed(1)

  source(file.path(modulePath(mySim), "Biomass_core", "R", "seedDispersalLANDIS.R"))
  if(exists("Biomass_coreWardDispersalSeeding")){
    output <- Biomass_coreWardDispersalSeeding(mySim)
  } else {
    output <- mySim$Biomass_coreWardDispersalSeeding(mySim)
  }
  expect_equal(output$regenerationOutput$numberOfReg,
               c(239, 439, 254, 337))
  expect_equal(unique(output$pixelGroupMap[1:297]), -1)
  rm(pixelGroupMap, ecoregionMap, objects, cohortData, mySim, activePixelIndex)

  # 2. testing on site regeneration in ward dispersal algorithm
  # the neiboring cells are inactive
  ecoregionMap <- raster(xmn = 50, xmx = 50 + 99 * 100,
                         ymn = 50, ymx = 50 + 99 * 100,
                         res = c(100, 100), val = 2)
  pixelGroupMap <- ecoregionMap
  pixelGroupMap[pixelindex] <- 1
  ecoregionMap[pixelindex] <- 1
  activePixelIndex <- pixelindex
  cohortData <- data.table(pixelGroup = 1, ecoregionGroup = 1L,
                           speciesCode = 7:10, age = 41, B = 80L,
                           mortality = 50, aNPPAct = 1079.75965551773)
  objects <- list("pixelGroupMap" = pixelGroupMap,
                  "speciesEcoregion" = speciesEcoregion,
                  "species" = species,
                  "successionTimestep" = successionTimestep,
                  "calibrate" = calibrate,
                  "seedingAlgorithm" = seedingAlgorithm,
                  "minRelativeB" = minRelativeB,
                  "sufficientLight" = sufficientLight,
                  "lastFireYear" = lastFireYear,
                  "activePixelIndex" = activePixelIndex,
                  "lastReg" = lastReg,
                  "regenerationOutput" = regenerationOutput,
                  "cohortData" = cohortData,
                  "ecoregionMap" = ecoregionMap)
  mySim <- simInit(times = list(start = 0, end = 2),
                   params = parameters,
                   modules = module,
                   objects = objects,
                   paths = path)
  set.seed(1)
  if(exists("Biomass_coreWardDispersalSeeding")){
    output <- Biomass_coreWardDispersalSeeding(mySim)
  } else {
    output <- mySim$Biomass_coreWardDispersalSeeding(mySim)
  }
  output <- output$regenerationOutput$numberOfReg
  expect_equal(output,c(64,121,69,84))


# test ward dispersal seeding after fire disturbance




})
