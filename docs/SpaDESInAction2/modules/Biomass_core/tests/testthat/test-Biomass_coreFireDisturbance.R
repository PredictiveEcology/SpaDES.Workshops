test_that("test regeneration after fire. ",{
  library(SpaDES)
  library(data.table)
  library(raster)
  # define the module and path
  module <- list("Biomass_core")
  path <- list(modulePath="~/GitHub/nrv-succession/code blitz succession/Module_Biomass_core",
               outputPath="~/output")
  parameters <- list(.progress=list(type="graphical", interval=1),
                     .globals=list(verbose=FALSE),
                     Biomass_core=list( .saveInitialTime=NA))
  pixelGroupMap <- raster(xmn = 50,xmx = 50 + 50*100,
                          ymn = 50,ymx = 50 + 50*100,
                          res = c(100, 100),
                          val = c(rep(1, 200), rep(2, 600), rep(3, 400), rep(4, 400),
                                  rep(5, 600), rep(-1, 300)))
  fireMap <- setValues(pixelGroupMap, 0)
  for(i in 4:49){
    fireMap[c((i*50+10):(i*50+50))] <- 1
  }
  rm(i)
  ecoregionMap <- setValues(pixelGroupMap, 1)
  ecoregionMap[c(1201:2200)] <- 2
  ecoregionMap[c(2201:2500)] <- 3
  cohortData <- data.table(expand.grid(pixelGroup = 1:5,
                                       speciesCode = 1:4),
                           key = c("pixelGroup", "speciesCode"))
  cohortData[pixelGroup <= 3, ecoregionGroup := 1]
  cohortData[pixelGroup > 3, ecoregionGroup := 2]
  cohortData <- cohortData[,.(pixelGroup, ecoregionGroup, speciesCode)]
  cohortData[,':='(age = c(100, 100, 100, 100, 100, 10, 100, 100, 100, 100, 100, 100,
                           100, 100, 100, 100, 100, 100, 100, 100),
                   B = 1000L,
                   mortality = 0,
                   aNPPAct  = 0)]
  speciesEcoregion <- data.table(year = 0,
                                 ecoregionGroup = c(1, 1, 1, 1, 2, 2, 2, 2),
                                 speciesCode = c(1, 2, 3, 4, 1, 2, 3, 4),
                                 maxB = c(23922, 28815, 29106, 27459, 23922, 25883, 26136, 24732),
                                 maxANPP = c(797, 961, 970, 915, 797, 863, 871, 824),
                                 establishprob = c(0.81, 0.85, 0.95, 0.73, 0.81, 0.68, 0.96, 0.96),
                                 key = c("ecoregionGroup", "speciesCode"))

  species <- data.table(species = c("abiebals", "pinubank", "poputrem", "querrubr"),
                        longevity = c(200L, 100L, 100L, 250L),
                        sexualmature = c(25L, 15L, 20L, 25L),
                        shadetolerance = c(5L, 1L, 1L, 3L),
                        firetolerance = c(1L, 3L, 1L, 2L),
                        seeddistance_eff = c(30L, 20L, 1000L, 30L),
                        seeddistance_max = c(160L, 100L, 5000L, 3000L),
                        resproutprob = c(0, 0, 0.9, 0.5),
                        resproutage_min = c(0L, 0L, 0L, 20L),
                        resproutage_max = c(0L, 0L, 100L, 200L),
                        postfireregen = c("none", "serotiny", "resprout", "resprout"),
                        leaflongevity = c(3L, 3L, 1L, 1L),
                        mortalityshape = c(10L, 10L, 10L, 10L),
                        growthcurve = c(0.25, 0.25, 0.25, 0.25),
                        speciesCode = c(1, 2, 3, 4),
                        key = "speciesCode")

  sufficientLight <- data.frame(speciesshadetolerance = 1:5,
                                X0 = seq(1, 0.8, length = 5),
                                X1 = seq(0.8, 0.6, length = 5),
                                X2 = seq(0.6, 0.4, length = 5),
                                X3 = seq(0.4, 0.2, length = 5),
                                X4 = seq(0.2, 0.0, length = 5),
                                X5 = seq(0, 1, length = 5))
  inactivePixelIndex <- 2201:2500
  calibrate <- TRUE

  objects <- list("pixelGroupMap" = pixelGroupMap,
                  "fireMap" = fireMap,
                  "ecoregionMap" = ecoregionMap,
                  "cohortData" = cohortData,
                  "sufficientLight" = sufficientLight,
                  "species" = species,
                  "inactivePixelIndex" = inactivePixelIndex,
                  "calibrate" = calibrate,
                  "speciesEcoregion" = speciesEcoregion)
  mySim <- simInit(times = list(start = 0, end = 2),
                   params = parameters,
                   modules = module,
                   objects = objects,
                   paths = path)
  set.seed(1234)
  if(exists("Biomass_coreFireDisturbance")){
    simOutput <- Biomass_coreFireDisturbance(mySim)
  } else {
    simOutput <- mySim$Biomass_coreFireDisturbance(mySim)
  }
  postFireRegenSummary <- simOutput$postFireRegenSummary
  postFireRegenSummary$species <- as.character(postFireRegenSummary$species)
  expect_equal(postFireRegenSummary,
               data.table(year = 0,
                          regenMode = c("Serotiny", "Resprout", "Resprout"),
                          species = c("pinubank", "poputrem", "querrubr"),
                          numberOfRegen = c(865, 689, 343)))
  expect_equal(sort(unique(simOutput$pixelGroupMap)),
               seq(-1, 13))
  expect_equal(length(Which(simOutput$pixelGroupMap == 0, cell = TRUE)), 47)

  rm(species)
  species <- data.table(species = c("abiebals", "pinubank", "poputrem", "querrubr"),
                        longevity = c(200L, 100L, 100L, 250L),
                        sexualmature = c(25L, 15L, 20L, 25L),
                        shadetolerance = c(5L, 1L, 1L, 3L),
                        firetolerance = c(1L, 3L, 1L, 2L),
                        seeddistance_eff = c(30L, 20L, 1000L, 30L),
                        seeddistance_max = c(160L, 100L, 5000L, 3000L),
                        resproutprob = c(0, 0, 0.9, 0.5),
                        resproutage_min = c(0L, 0L, 0L, 20L),
                        resproutage_max = c(0L, 0L, 100L, 200L),
                        postfireregen = c("none", "none", "resprout", "resprout"),
                        leaflongevity = c(3L, 3L, 1L, 1L),
                        mortalityshape = c(10L, 10L, 10L, 10L),
                        growthcurve = c(0.25, 0.25, 0.25, 0.25),
                        speciesCode = c(1, 2, 3, 4),
                        key = "speciesCode")
  objects <- list("pixelGroupMap" = pixelGroupMap,
                  "fireMap" = fireMap,
                  "ecoregionMap" = ecoregionMap,
                  "cohortData" = cohortData,
                  "sufficientLight" = sufficientLight,
                  "species" = species,
                  "inactivePixelIndex" = inactivePixelIndex,
                  "calibrate" = calibrate,
                  "speciesEcoregion" = speciesEcoregion)
  mySim <- simInit(times = list(start = 1, end = 2),
                   params = parameters,
                   modules = module,
                   objects = objects,
                   paths = path)
  set.seed(1234)
  if(exists("Biomass_coreFireDisturbance")){
    simOutput <- Biomass_coreFireDisturbance(mySim)
  } else {
    simOutput <- mySim$Biomass_coreFireDisturbance(mySim)
  }
  postFireRegenSummary <- simOutput$postFireRegenSummary
  postFireRegenSummary$species <- as.character(postFireRegenSummary$species)
  expect_equal(postFireRegenSummary,
               data.table(year = 1,
                          regenMode = c("Resprout", "Resprout"),
                          species = c("poputrem", "querrubr"),
                          numberOfRegen = c(1456, 719)))
  expect_equal(sort(unique(simOutput$pixelGroupMap)), seq(-1,11))
  expect_equal(length(Which(simOutput$pixelGroupMap == 0, cell = TRUE)), 101)

  rm(species)
  species <- data.table(species = c("abiebals", "pinubank", "poputrem", "querrubr"),
                        longevity = c(200L, 100L, 100L, 250L),
                        sexualmature = c(25L, 15L, 20L, 25L),
                        shadetolerance = c(5L, 1L, 1L, 3L),
                        firetolerance = c(1L, 3L, 1L, 2L),
                        seeddistance_eff = c(30L, 20L, 1000L, 30L),
                        seeddistance_max = c(160L, 100L, 5000L, 3000L),
                        resproutprob = c(0, 0, 0.9, 0.5),
                        resproutage_min = c(0L, 0L, 0L, 20L),
                        resproutage_max = c(0L, 0L, 100L, 200L),
                        postfireregen = c("none", "serotiny", "none", "none"),
                        leaflongevity = c(3L, 3L, 1L, 1L),
                        mortalityshape = c(10L, 10L, 10L, 10L),
                        growthcurve = c(0.25, 0.25, 0.25, 0.25),
                        speciesCode = c(1, 2, 3, 4),
                        key = "speciesCode")
  objects <- list("pixelGroupMap" = pixelGroupMap,
                  "fireMap" = fireMap,
                  "ecoregionMap" = ecoregionMap,
                  "cohortData" = cohortData,
                  "sufficientLight" = sufficientLight,
                  "species" = species,
                  "inactivePixelIndex" = inactivePixelIndex,
                  "calibrate" = calibrate,
                  "speciesEcoregion" = speciesEcoregion)
  mySim <- simInit(times = list(start = 1, end = 2),
                   params = parameters,
                   modules = module,
                   objects = objects,
                   paths = path)
  set.seed(1234)
  if(exists("Biomass_coreFireDisturbance")){
    simOutput <- Biomass_coreFireDisturbance(mySim)
  } else {
    simOutput <- mySim$Biomass_coreFireDisturbance(mySim)
  }
  postFireRegenSummary <- simOutput$postFireRegenSummary
  postFireRegenSummary$species <- as.character(postFireRegenSummary$species)
  expect_equal(postFireRegenSummary,
               data.table(year = 1,
                          regenMode = c("Serotiny"),
                          species = c("pinubank"),
                          numberOfRegen = 865))
  expect_equal(sort(unique(getValues(simOutput$pixelGroupMap))), seq(-1,7))
  expect_equal(length(Which(simOutput$pixelGroupMap == 0, cell = TRUE)), 775)

  rm(species)
  species <- data.table(species = c("abiebals", "pinubank", "poputrem", "querrubr"),
                        longevity = c(200L, 100L, 100L, 250L),
                        sexualmature = c(25L, 15L, 20L, 25L),
                        shadetolerance = c(5L, 1L, 1L, 3L),
                        firetolerance = c(1L, 3L, 1L, 2L),
                        seeddistance_eff = c(30L, 20L, 1000L, 30L),
                        seeddistance_max = c(160L, 100L, 5000L, 3000L),
                        resproutprob = c(0, 0, 0.9, 0.5),
                        resproutage_min = c(0L, 0L, 0L, 20L),
                        resproutage_max = c(0L, 0L, 100L, 200L),
                        postfireregen = c("none", "none", "none", "none"),
                        leaflongevity = c(3L, 3L, 1L, 1L),
                        mortalityshape = c(10L, 10L, 10L, 10L),
                        growthcurve = c(0.25, 0.25, 0.25, 0.25),
                        speciesCode = c(1, 2, 3, 4),
                        key = "speciesCode")
  objects <- list("pixelGroupMap" = pixelGroupMap,
                  "fireMap" = fireMap,
                  "ecoregionMap" = ecoregionMap,
                  "cohortData" = cohortData,
                  "sufficientLight" = sufficientLight,
                  "species" = species,
                  "inactivePixelIndex" = inactivePixelIndex,
                  "calibrate" = calibrate,
                  "speciesEcoregion" = speciesEcoregion)
  mySim <- simInit(times = list(start = 1, end = 2),
                   params = parameters,
                   modules = module,
                   objects = objects,
                   paths = path)
  set.seed(1234)
  if(exists("Biomass_coreFireDisturbance")){
    simOutput <- Biomass_coreFireDisturbance(mySim)
  } else {
    simOutput <- mySim$Biomass_coreFireDisturbance(mySim)
  }
  postFireRegenSummary <- simOutput$postFireRegenSummary
  postFireRegenSummary$species <- as.character(postFireRegenSummary$species)
  expect_equal(nrow(postFireRegenSummary), 0)
  expect_equal(sort(unique(getValues(simOutput$pixelGroupMap))), seq(-1, 5))
  expect_equal(length(Which(simOutput$pixelGroupMap == 0, cell=TRUE)), 1640)
})
