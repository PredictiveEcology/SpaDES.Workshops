test_that("test no dispersal seeding algorithm",{
  # define the module and path
  module <- list("Biomass_core")
  path <- list(modulePath = "~/GitHub/nrv-succession/code blitz succession/Module_Biomass_core",
               outputPath = "~/output")
  parameters <- list(.progress = list(type = "graphical", interval = 1),
                     .globals = list(verbose = FALSE),
                     Biomass_core = list( .saveInitialTime = NA))

  pixelGroupMap <- raster(xmn = 50, xmx = 50 + 100 * 100,
                        ymn = 50, ymx = 50 + 100 * 100,
                        res = c(100,100), val = 1)
  speciesEcoregion <- data.table(year = seq(0, 100, by = 10), ecoregion = "eco1", species = "tsugcana",
                                 establishprob = 0.22, maxANPP = 1096, maxB = 32880,
                                 ecoregionGroup = 1, speciesCode = 1)
  minRelativeB <- data.table(ecoregion = c("eco1", "eco2"), X1 = c(0.15, 0.15),
                             X2 = c(0.25, 0.25), X3 = c(0.5, 0.5), X4 = c(0.8, 0.8),
                             X5 = c(0.95, 0.95), ecoregionGroup = 1:2)
  sufficientLight <- read.csv("~/GitHub/nrv-succession/code blitz succession/modeltesting-data/sufficientLight.csv",
                              header = TRUE, stringsAsFactor = FALSE)
  lastFireYear <- "noFire"
  activePixelIndex <- 1:10000
  lastReg <- 0
  successionTimestep <- 1
  calibrate <- TRUE
  regenerationOutput <- data.table(seedingAlgorithm = character(),species = character(),
                                   Year = numeric(),numberOfReg = numeric())
  species <- read.csv("~/GitHub/nrv-succession/code blitz succession/modeltesting-data/species.csv",
                      header = TRUE, stringsAsFactor = FALSE)
  species <- data.table(species)[species == "tsugcana",]
  sufficientLight <- read.csv("~/GitHub/nrv-succession/code blitz succession/modeltesting-data/sufficientLight.csv",
                              header = TRUE, stringsAsFactor = FALSE)
  seedingAlgorithm <- "noDispersal"
  cohortData <- data.table(pixelGroup = 1, ecoregionGroup = 1L,
                           speciesCode = 1, age = 31, B = 17016L,
                           mortality = 737.567259958833, aNPPAct = 1079.75965551773)
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
                  "cohortData" = cohortData)
  mySim <- simInit(times = list(start = 0, end = 2),
                   params = parameters,
                   modules = module,
                   objects = objects,
                   paths = path)
  set.seed(1)
  if(exists("Biomass_coreNoDispersalSeeding")){
    output <- Biomass_coreNoDispersalSeeding(mySim)
  } else {
    output <- mySim$Biomass_coreNoDispersalSeeding(mySim)
  }
  output <- output$regenerationOutput$numberOfReg
  expect_equal(output,2196)
  rm(output)

  rm(lastFireYear)
  lastFireYear <- 0
  firePixelTable <- data.table(pixelIndex = 1:1000)
  pixelGroupMap[1:1000] <- 0
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
                  "firePixelTable" = firePixelTable)
  mySim <- simInit(times = list(start = 0, end = 2),
                   params = parameters,
                   modules = module,
                   objects = objects,
                   paths = path)
  set.seed(1)
  if (exists("Biomass_coreNoDispersalSeeding")) {
    output <- Biomass_coreNoDispersalSeeding(mySim)
  } else {
    output <- mySim$Biomass_coreNoDispersalSeeding(mySim)
  }
  output <- output$regenerationOutput$numberOfReg
  expect_equal(output,1962)
  rm(output)

  establishprobs <- seq(0.01, 1, length = 10)
  for (establishprob in establishprobs) {
    cohortData <- data.table(pixelGroup = 1, ecoregionGroup = 1L,
                             speciesCode = 1, age = 31, B = 17016L,
                             mortality = 737.567259958833, aNPPAct = 1079.75965551773)
    speciesEcoregion[, establishprob := establishprob]
    species[,shadetolerance := 3]
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
                    "cohortData" = cohortData)
    mySim <- simInit(times = list(start = 0, end = 2),
                     params = parameters,
                     modules = module,
                     objects = objects,
                     paths = path)
    if(exists("Biomass_coreNoDispersalSeeding")){
      output <- Biomass_coreNoDispersalSeeding(mySim)
    } else {
      output <- mySim$Biomass_coreNoDispersalSeeding(mySim)
    }
    output <- output$regenerationOutput$numberOfReg
    expect_equal(output,numeric(0))
  }
})
