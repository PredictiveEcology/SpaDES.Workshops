test_that("test universal dispersal seeding algorithm",{
  # define the module and path
  module <- list("Biomass_core")
  path <- list(modulePath="~/GitHub/nrv-succession/code blitz succession/Module_Biomass_core",
               outputPath="~/output")
  parameters <- list(.progress=list(type="graphical", interval=1),
                     .globals=list(verbose=FALSE),
                     Biomass_core=list( .saveInitialTime=NA))

  pixelGroupMap <- raster(xmn=50,xmx=50+100*100,
                        ymn=50,ymx=50+100*100,
                        res=c(100,100), val=1)
  pixelGroupMap[1:1000] <- -1
  pixelGroupMap[9001:10000] <- 2
  ecoregionMap <- pixelGroupMap
  ecoregionMap[1:1000] <- 3
  ecoregionMap[9001:10000] <- 2
  minRelativeB <- data.table(ecoregion = c("eco1", "eco2"), X1 = c(0.15, 0.15),
                             X2 = c(0.25, 0.25), X3 = c(0.5, 0.5), X4 = c(0.8, 0.8),
                             X5 = c(0.95, 0.95), ecoregionGroup = 1:2)
  sufficientLight <- data.frame(speciesshadetolerance = 1:5,
                                X0 = c(1L, 1L, 1L, 1L, 0L),
                                X1 = c(0L, 1L, 1L, 1L, 0L),
                                X2 = c(0L, 0L, 1L, 1L, 1L),
                                X3 = c(0L, 0L, 0L, 1L, 1L),
                                X4 = c(0L, 0L, 0L, 0L, 1L),
                                X5 = c(0L, 0L, 0L, 0L, 1L))
  lastFireYear <- "noFire"
  activePixelIndex <- 1001:10000
  lastReg <- 0
  successionTimestep <- 1
  calibrate <- TRUE
  regenerationOutput <- data.table(seedingAlgorithm=character(),species=character(),
                                   Year=numeric(),numberOfReg=numeric())
  species <- data.table(species = c("abiebals", "acerrubr", "acersacc", "betualle", "betupapy",
                                    "fraxamer", "piceglau", "pinubank", "pinuresi", "pinustro",
                                    "poputrem", "querelli", "querrubr", "thujocci", "tiliamer",
                                    "tsugcana"),
                        longevity = c(200L, 150L, 300L, 300L, 100L, 300L, 300L,
                                      100L, 200L, 400L, 100L, 300L, 250L, 400L, 250L, 500L),
                        sexualmature = c(25L, 10L, 40L, 40L, 30L, 30L, 25L, 15L,
                                         35L, 40L, 20L, 35L, 25L, 30L, 30L, 30L),
                        shadetolerance = c(5L, 4L, 5L, 4L, 2L, 4L, 3L, 1L, 2L,
                                           3L, 1L, 2L, 3L, 2L, 4L, 5L),
                        firetolerance = c(1L, 1L, 1L, 2L, 2L, 1L, 2L, 3L, 4L,
                                          3L, 1L, 3L, 2L, 1L, 2L, 2L),
                        seeddistance_eff = c(30L, 100L, 100L, 100L, 200L, 70L,
                                             30L, 20L, 20L, 60L, 1000L, 30L, 30L, 45L, 30L, 30L),
                        seeddistance_max = c(160L, 200L, 200L, 400L, 5000L, 140L,200L,
                                             100L, 275L, 210L, 5000L, 3000L, 3000L, 60L, 120L, 100L),
                        resproutprob = c(0, 0.5, 0.1, 0.1, 0.5, 0.1, 0, 0,
                                         0, 0, 0.9, 0.5, 0.5, 0.5, 0.1, 0),
                        resproutage_min = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
                                            0L, 0L, 0L, 20L, 20L, 50L, 0L, 0L),
                        resproutage_max = c(0L, 100L, 60L, 180L, 70L, 70L, 0L, 0L,
                                            0L, 0L, 100L, 280L, 200L, 400L, 200L, 0L),
                        postfireregen = c("none", "none", "none", "none", "resprout",
                                          "none", "none", "serotiny", "none", "none",
                                          "resprout", "resprout", "resprout", "none", "none", "none"),
                        leaflongevity = c(3L, 1L, 1L, 1L, 1L, 1L, 3L, 3L,
                                          3L, 3L, 1L, 1L, 1L, 4L, 1L, 3L),
                        mortalityshape = 10L,
                        growthcurve = 0.25)
  species[,speciesCode:=1:16]
  speciesEcoregion <- data.table(year = 0L,
                                 ecoregion = c(rep("eco1",16), rep("eco2",16)),
                                 species = c("abiebals", "acerrubr", "acersacc", "betualle", "betupapy", "fraxamer", "piceglau",
                                             "pinubank", "pinuresi", "pinustro", "poputrem", "querelli", "querrubr",
                                             "thujocci", "tiliamer", "tsugcana", "abiebals", "acerrubr", "acersacc",
                                             "betualle", "betupapy", "fraxamer", "piceglau", "pinubank", "pinuresi",
                                             "pinustro", "poputrem", "querelli", "querrubr", "thujocci", "tiliamer",
                                             "tsugcana"),
                                 establishprob = c(0.9, 1, 0.82, 0.64, 1, 0.18, 0.58, 1, 0.56, 0.72,
                                                   1, 0.96, 0.66, 0.76, 0.54, 0.22, 0.05, 0.6, 0.3,
                                                   0.24, 0.75, 0.1, 0.5, 0.8, 0.78, 0.7, 0.8, 0.71,
                                                   0.43, 0, 0.06, 0.01),
                                 maxANPP = c(886L, 1175L, 1106L, 1202L, 1202L, 1202L, 969L,
                                             1130L, 1017L, 1090L, 1078L, 1096L, 1017L, 1090L, 1078L, 1096L,
                                             801L, 1058L, 1003L, 1052L, 1052L, 1052L, 875L, 1015L, 916L, 980L,
                                             968L, 984L, 916L, 980L, 968L, 984L),
                                 maxB = c(26580L, 35250L, 33180L, 36060L, 36060L, 36060L,
                                          29070L, 33900L, 30510L, 38150L,
                                          32340L, 32880L, 30510L, 32700L, 32340L, 32880L, 24030L, 31740L,
                                          30090L, 31560L, 31560L, 31560L, 26250L, 30450L, 27480L, 34300L,
                                          29040L, 29520L, 27480L, 29400L, 29040L, 29520L),
                                 ecoregionGroup = c(rep(1,16), rep(2,16)))

  tempsp <- setkey(species[,.(species,speciesCode)],species)
  speciesEcoregion <- setkey(speciesEcoregion,species)[tempsp]
  seedingAlgorithm <- "universalDispersal"
  cohortData <- rbind(data.table(pixelGroup = 1, ecoregionGroup = 1L,
                           speciesCode = 16, age = 31, B = 100L,
                           mortality = 50, aNPPAct = 1080),
                      data.table(pixelGroup = 2, ecoregionGroup = 2L,
                           speciesCode = 16, age = 31, B = 100L,
                           mortality = 50, aNPPAct = 1080))
  objects <- list("pixelGroupMap"=pixelGroupMap,
                  "speciesEcoregion"=speciesEcoregion,
                  "species"=species,
                  "successionTimestep"=successionTimestep,
                  "calibrate"=calibrate,
                  "seedingAlgorithm"=seedingAlgorithm,
                  "minRelativeB"=minRelativeB,
                  "sufficientLight"=sufficientLight,
                  "lastFireYear"=lastFireYear,
                  "activePixelIndex"=activePixelIndex,
                  "lastReg"=lastReg,
                  "regenerationOutput"=regenerationOutput,
                  "cohortData"=cohortData,
                  "ecoregionMap"=ecoregionMap)
  mySim <- simInit(times=list(start=0, end=2),
                   params=parameters,
                   modules=module,
                   objects=objects,
                   paths=path)
  set.seed(2)
  if(exists("Biomass_coreUniversalDispersalSeeding")){
    simOutput <- Biomass_coreUniversalDispersalSeeding(mySim)
  } else {
    simOutput <- mySim$Biomass_coreUniversalDispersalSeeding(mySim)
  }
  output <- simOutput$regenerationOutput[
    ,.(species,numberOfReg)]
  output_compared <- data.table(species=c("acerrubr", "betualle", "betupapy", "fraxamer", "piceglau",
                                          "pinubank", "pinuresi", "pinustro", "poputrem", "querelli",
                                          "querrubr", "thujocci", "tiliamer"),
                                numberOfReg=c(8612, 5282, 8737, 1585, 5101, 8816, 5249, 6514,
                                              8813, 8390, 5631, 6041, 4333))
  expect_equal(output,output_compared)
  expect_equal(unique(simOutput$pixelGroupMap[1:1000]), -1)


  rm(output,output_compared, objects, mySim)
  pixelGroupMap[1001: 2000] <- 0
  pixelGroupMap[2001: 3000] <- 3
  cohortData <- rbind(cohortData[,.(pixelGroup, ecoregionGroup, speciesCode, age, B, mortality, aNPPAct)],
                      data.table(pixelGroup = 3, ecoregionGroup = 1L,
                           speciesCode = 16, age = 1, B = 100L,
                           mortality = 50, aNPPAct = 1080))
  postFirePixel <- 2001:3000
  lastFireYear <- 0
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
                  "postFirePixel" = postFirePixel)
  mySim <- simInit(times = list(start = 0, end = 2),
                   params = parameters,
                   modules = module,
                   objects = objects,
                   paths = path)
  set.seed(1)
  if(exists("Biomass_coreUniversalDispersalSeeding")){
    simOutput <- Biomass_coreUniversalDispersalSeeding(mySim)
  } else {
    simOutput <- mySim$Biomass_coreUniversalDispersalSeeding(mySim)
  }
  output <- simOutput$regenerationOutput[
    ,.(species,numberOfReg)]
  expect_equal(output,
               data.table(species = c("acerrubr", "betualle", "betupapy", "fraxamer", "piceglau",
                                      "pinubank", "pinuresi", "pinustro", "poputrem", "querelli",
                                      "querrubr", "thujocci", "tiliamer"),
                          numberOfReg = c(7622, 4743, 7729, 1341, 4510, 7799, 4624, 5746, 7818, 7458,
                                          5046, 5362, 3815)))

  expect_equal(unique(simOutput$pixelGroupMap[1:1000]), -1)
})
