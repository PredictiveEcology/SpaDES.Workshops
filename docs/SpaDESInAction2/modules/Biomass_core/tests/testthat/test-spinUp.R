test_that("test growth and mortality at spinup stage",{
  # define the module and path
  module <- list("Biomass_core")
  path <- list(modulePath="~/GitHub/nrv-succession/code blitz succession/Module_Biomass_core",
               outputPath="~/output")
  parameters <- list(.progress=list(type="graphical", interval=1),
                     .globals=list(verbose=FALSE),
                     Biomass_core=list( .saveInitialTime=NA))
  spinupMortalityfraction <- 0.001
  calibrate <- TRUE
  successionTimestep <- 1
  objects <- list()
  species <- data.table(species = c("abiebals", "acerrubr", "acersacc", "betualle", "betupapy", "fraxamer",
                                    "piceglau", "pinubank", "pinuresi", "pinustro", "poputrem", "querelli",
                                    "querrubr", "thujocci", "tiliamer", "tsugcana"),
                        speciesCode = 1:16)
  mySim <- simInit(times=list(start=0, end=1),
                   params=parameters,
                   modules=module,
                   objects=objects,
                   paths=path)
  cohortData <- data.table(pixelGroup = 1:16,
                           ecoregionGroup = 1L,
                           speciesCode = 1:16,
                           age = c(195, 145, 295, 295, 95, 295, 295, 95, 195, 395, 95, 295, 245, 395, 245, 495),
                           B = 0L,
                           maxANPP = c(886L, 1175L, 1106L, 1202L, 1202L, 1202L, 969L, 1130L, 1017L, 1090L, 1078L, 1096L,
                                       1017L, 1090L, 1078L, 1096L),
                           maxB = c(26580L, 35250L, 33180L, 36060L, 36060L, 36060L, 29070L, 33900L, 30510L, 38150L, 32340L,
                                    32880L, 30510L, 32700L, 32340L, 32880L),
                           maxB_eco = 38150L,
                           longevity = c(200L, 150L, 300L, 300L, 100L, 300L, 300L, 100L, 200L, 400L, 100L, 300L,
                                         250L, 400L, 250L, 500L),
                           mortalityshape = 10L,
                           growthcurve = 0.25)
  if(exists("spinUp")){
    output <- spinUp(cohortData, calibrate, successionTimestep, spinupMortalityfraction, species)
  } else {
    output <- mySim$spinUp(cohortData, calibrate, successionTimestep, spinupMortalityfraction, species)
  }
  newcohortData <- output$cohortData[,.(pixelGroup,speciesCode,B)]
  newcohortData_compared <- data.table(pixelGroup = 1:16,
                                       speciesCode = 1:16,
                                       B = c(372L, 545L, 422L, 458L, 687L, 458L, 369L, 645L,
                                             427L, 384L, 616L, 418L, 403L, 396L, 427L, 387L))
  expect_equal(newcohortData,newcohortData_compared)
  rm(output,cohortData,mySim, newcohortData, newcohortData_compared)
  # for simulation timestep is 4
  cohortData <- data.table(pixelGroup = 1:16,
                           ecoregionGroup = 1L,
                           speciesCode = 1:16,
                           age = c(196, 148, 296, 296,  96, 296, 296,  96, 196, 396,  96, 296, 248, 396, 248, 496),
                           B = 0L,
                           maxANPP = c(886L, 1175L, 1106L, 1202L, 1202L, 1202L, 969L, 1130L, 1017L, 1090L, 1078L, 1096L,
                                       1017L, 1090L, 1078L, 1096L),
                           maxB = c(26580L, 35250L, 33180L, 36060L, 36060L, 36060L, 29070L, 33900L, 30510L, 38150L, 32340L,
                                    32880L, 30510L, 32700L, 32340L, 32880L),
                           maxB_eco = 38150L,
                           longevity = c(200L, 150L, 300L, 300L, 100L, 300L, 300L, 100L, 200L, 400L, 100L, 300L,
                                         250L, 400L, 250L, 500L),
                           mortalityshape = 10L,
                           growthcurve = 0.25)
  successionTimestep <- 4
  objects <- list()
  mySim <- simInit(times=list(start=0, end=1),
                   params=parameters,
                   modules=module,
                   objects=objects,
                   paths=path)
  if(exists("spinUp")){
    output <- spinUp(cohortData, calibrate, successionTimestep, spinupMortalityfraction, species)
  } else {
    output <- mySim$spinUp(cohortData, calibrate, successionTimestep, spinupMortalityfraction, species)
  }
  newcohortData <- output$cohortData[,.(pixelGroup,speciesCode,B)]
  newcohortData_compared <- data.table(pixelGroup = 1:16,
                                       speciesCode = 1:16,
                                       B = c(350L, 424L, 405L, 440L, 602L, 440L, 355L, 566L, 402L, 373L,
                                             540L, 401L, 348L, 384L, 368L, 378L))
  expect_equal(newcohortData,newcohortData_compared)

  rm(cohortData, calibrate, successionTimestep)
  calibrate <- TRUE
  successionTimestep <- 1
  cohortData <- data.table(pixelGroup = 1:16,
                           ecoregionGroup = 1L,
                           speciesCode = 1:16,
                           age = 1,
                           B = 0L,
                           maxANPP = c(886L, 1175L, 1106L, 1202L, 1202L, 1202L, 969L, 1130L, 1017L, 1090L, 1078L, 1096L,
                                       1017L, 1090L, 1078L, 1096L),
                           maxB = c(26580L, 35250L, 33180L, 36060L, 36060L, 36060L, 29070L, 33900L, 30510L, 38150L, 32340L,
                                    32880L, 30510L, 32700L, 32340L, 32880L),
                           maxB_eco = c(38150L, 38150L, 38150L, 38150L, 38150L, 38150L, 38150L, 38150L, 38150L,
                                        38150L, 38150L, 38150L, 38150L, 38150L, 38150L, 38150L),
                           longevity = c(200L, 150L, 300L, 300L, 100L, 300L, 300L, 100L, 200L, 400L, 100L, 300L,
                                         250L, 400L, 250L, 500L),
                           mortalityshape = 10L,
                           growthcurve = 0.25)
  if(exists("spinUp")){
    output <- spinUp(cohortData, calibrate, successionTimestep, spinupMortalityfraction, species)
  } else {
    output <- mySim$spinUp(cohortData, calibrate, successionTimestep, spinupMortalityfraction, species)
  }
  newcohortData <- output$cohortData[,.(pixelGroup,speciesCode,B)]
  newcohortData_compared <- data.table(pixelGroup = 1:16,
                                       speciesCode = 1:16,
                                       B = c(886L, 1175L, 1106L, 1202L, 1202L, 1202L, 969L, 1130L, 1017L,
                                             1090L, 1078L, 1096L, 1017L, 1090L, 1078L, 1096L))
  expect_identical(newcohortData,newcohortData_compared)

  rm(cohortData, calibrate, successionTimestep)
  calibrate <- TRUE
  successionTimestep <- 1
  cohortData <- data.table(pixelGroup = 1:16,
                           ecoregionGroup = 1L,
                           speciesCode = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16),
                           age = c(rep(1,15),495),
                           B = 0L,
                           maxANPP = c(886L, 1175L, 1106L, 1202L, 1202L, 1202L, 969L, 1130L, 1017L, 1090L, 1078L, 1096L,
                                       1017L, 1090L, 1078L, 1096L),
                           maxB = c(26580L, 35250L, 33180L, 36060L, 36060L, 36060L, 29070L, 33900L, 30510L, 38150L, 32340L,
                                    32880L, 30510L, 32700L, 32340L, 32880L),
                           maxB_eco = c(38150L, 38150L, 38150L, 38150L, 38150L, 38150L, 38150L, 38150L, 38150L,
                                        38150L, 38150L, 38150L, 38150L, 38150L, 38150L, 38150L),
                           longevity = c(200L, 150L, 300L, 300L, 100L, 300L, 300L, 100L, 200L, 400L, 100L, 300L,
                                         250L, 400L, 250L, 500L),
                           mortalityshape = 10L,
                           growthcurve = 0.25)
  if(exists("spinUp")){
    output <- spinUp(cohortData, calibrate, successionTimestep, spinupMortalityfraction, species)
  } else {
    output <- mySim$spinUp(cohortData, calibrate, successionTimestep, spinupMortalityfraction, species)
  }
  newcohortData <- output$cohortData[,.(pixelGroup,speciesCode,B)]
  newcohortData_compared <- data.table(pixelGroup = 1:16,
                                       speciesCode = 1:16,
                                       B = c(886L, 1175L, 1106L, 1202L, 1202L, 1202L, 969L, 1130L, 1017L,
                                             1090L, 1078L, 1096L, 1017L, 1090L, 1078L, 387L))
  expect_equal(newcohortData,newcohortData_compared)
})
