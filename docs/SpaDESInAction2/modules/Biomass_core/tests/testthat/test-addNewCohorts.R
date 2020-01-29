test_that("test add new cohort function",{
  # define the module and path
  module <- list("Biomass_core")
  path <- list(modulePath = "~/GitHub/nrv-succession/code blitz succession/Module_Biomass_core", # TODO: use general path
               outputPath = "~/output") # TODO: use general path
  parameters <- list(.progress = list(type = "graphical", interval = 1),
                     .globals = list(verbose = FALSE),
                     Biomass_core = list(.saveInitialTime = NA))

  pixelGroupMap <- raster(xmn = 50, xmx = 50 + 3 * 100,
                          ymn = 50, ymx = 50 + 3 * 100,
                          res = c(100, 100), val = 1)
  pixelGroupMap[1] <- -1
  pixelGroupMap[2:6] <- 2

  cohortData <- data.table(pixelGroup = 1, ecoregionGroup = 1L,
                           speciesCode = 7, age = 31, B = 30000L,
                           mortality = 50,  aNPPAct = 1079.76, sumB = 30000L)
  newcohortdata1 <- rbindlist(list(cohortData, cohortData, cohortData))[
    , ':='(speciesCode = c(7, 1, 7), pixelIndex = c(7:8, 8))]
  newcohortdata2 <- rbindlist(list(cohortData, cohortData,
                                   cohortData, cohortData))[
                                     ,':='(pixelGroup = 2,
                                           speciesCode = c(7, 7, 7, 1),
                                           pixelIndex = c(4:6, 6))]
  newcohortdata <- rbindlist(list(newcohortdata1,newcohortdata2))[
    , ':='(age = NULL, B = NULL, sumB = NULL, aNPPAct = NULL, mortality = NULL)]

  speciesEcoregion <- data.table(year = 0,
                                 maxANPP = c(886L, 1175L, 1106L, 1202L, 1202L, 1202L, 969L, 1130L,
                                             1017L, 1090L, 1078L, 1096L, 1017L, 1090L, 1078L, 1096L),
                                 maxB = c(26580L, 32880L, 32880L, 32880L, 32880L, 32880L, 29070L, 32880L,
                                          30510L, 32880L, 32340L, 32880L, 30510L, 32700L, 32340L, 32880L),
                                 ecoregionGroup = 1L,
                                 speciesCode = 1:16)
  objects <- list()
  mySim <- simInit(times = list(start = 0, end = 2),
                   params = parameters,
                   modules = module,
                   objects = objects,
                   paths = path)
  output <- addNewCohorts(newcohortdata, cohortData, pixelGroupMap,
                          currentTime = time(mySim), speciesEcoregion = speciesEcoregion)
  mapOutput <- getValues(output$pixelGroupMap)
  mapOutput_compared <- c(-1, 2, 2, 4, 4, 6, 3, 5, 1)
  expect_equal(mapOutput,mapOutput_compared)

  cohortdataOutput <- output$cohortData[,.(pixelGroup, speciesCode, age, B)]

  cohortdataOutput_compared <- data.table(pixelGroup = c(1, 3, 3, 4, 5, 5, 5, 6, 6),
                                          speciesCode = c(rep(7, 4), 1, 7, 7, 1, 7),
                                          age = c(31, 1, 31, 1, 1, 1, 31, 1, 1),
                                          B = c(30000L, 225L, 30000L, 969L, 205L, 225L, 30000L, 886L, 969L))

  expect_equal(cohortdataOutput,cohortdataOutput_compared)
})
