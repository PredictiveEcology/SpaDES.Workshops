test_that("test Biomass_coreInit",{
  # define the module and path
  library(raster)
  library(data.table)
  module <- list("Biomass_core")
  path <- list(modulePath="~/GitHub/nrv-succession/code blitz succession/Module_Biomass_core",
               outputPath="~/output")
  parameters <- list(.progress=list(type="graphical", interval=1),
                     .globals=list(verbose=FALSE),
                     Biomass_core=list( .saveInitialTime=NA))
  initialCommunitiesMap <- raster(xmn=50,xmx=50+3*100,
                                  ymn=50,ymx=50+3*100,
                                  res=c(100,100), val=c(1,2,NA,2,1,NA,NA,NA,NA))
  ecoregionMap <- setValues(initialCommunitiesMap,c(rep(1,3),rep(2,3),rep(3,3)))
  successionTimestep <- 5
  initialCommunities <- data.table(mapcode=c(1,2,2),description="NA",
                                   species=c("pinubank","pinubank","abiebals"),
                                   age1=c(3,7,10),age2=c(4,12,19))
  ecoregion <- data.table(active=c("yes","yes","no"),mapcode=1:3,
                          ecoregion=c("eco1","eco2","eco3"),AET=600,description="NA")
  species <- data.table(species = c("abiebals", "pinubank"), longevity = c(200L, 100L),
                        sexualmature = c(25L, 15L), shadetolerance = c(5L, 1L),
                        firetolerance = c(1L, 3L), seeddistance_eff = c(30L, 20L),
                        seeddistance_max = c(160L, 100L), resproutprob = c(0, 0),
                        resproutage_min = c(0L, 0L), resproutage_max = c(0L, 0L),
                        postfireregen = c("none", "serotiny"), leaflongevity = c(3L,3L),
                        mortalityshape = c(10L, 10L), growthcurve = c(0.25, 0.25))
  speciesEcoregion <- data.frame(expand.grid(year=0, ecoregion=c("eco1","eco2"),
                                             species=c("abiebals","pinubank")))

  speciesEcoregion$establishprob <- c(0.9,0.5,1,0.8)
  speciesEcoregion$maxANPP <- c(886,801,1130,1015)
  speciesEcoregion$maxB <- c(26580,24030,33900,30450)

  speciesEcoregion2 <- speciesEcoregion
  speciesEcoregion2$year <- 5
  speciesEcoregion2$establishprob=round(speciesEcoregion2$establishprob*0.9,2)
  speciesEcoregion2$maxANPP=as.integer(speciesEcoregion2$maxANPP*1.1)
  speciesEcoregion2$maxB=as.integer(speciesEcoregion2$maxB*1.5)
  speciesEcoregion <- data.table(rbind(speciesEcoregion,speciesEcoregion2))
  rm(speciesEcoregion2)
  calibrate <- TRUE
  cellSize <- 50
  useCache <- TRUE
  spinupMortalityfraction <- 0.001
  objects <- list("initialCommunitiesMap"=initialCommunitiesMap,
                  "ecoregionMap"=ecoregionMap,
                  "successionTimestep"=successionTimestep,
                  "initialCommunities"=initialCommunities,
                  "ecoregion"=ecoregion,
                  "species"=species,
                  "speciesEcoregion"=speciesEcoregion,
                  "calibrate"=calibrate,
                  "cellSize"=cellSize,
                  "useCache"=useCache,
                  "spinupMortalityfraction"=spinupMortalityfraction)
  mySim <- simInit(times=list(start=0, end=2),
                   params=parameters,
                   modules=module,
                   objects=objects,
                   paths=path)
  if(exists("Biomass_coreInit")){
    simOutput <- Biomass_coreInit(mySim)
  } else {
    simOutput <- mySim$Biomass_coreInit(mySim)
  }
  # check the cohortData table
  cohortData <- simOutput$cohortData
  expect_is(cohortData,"data.table")
  # compare simulated Biomass
  cohortDataShort <- setkey(cohortData[,.(pixelGroup,speciesCode,age,B)],pixelGroup,speciesCode,age)
  cohortDataShort_compared <- data.table(pixelGroup=c(11,12,12,12,12,21,22,22,22,22),
                                         speciesCode=c(2,1,1,
                                                   2,2,2,
                                                   1,1,2,2),
                                         age=c(5,10,20,10,15,5,10,20,10,15),
                                         B=c(5028,1065,8523,1559,3454,4515,960,
                                             7718,1399,3079))
  setkey(cohortDataShort_compared,pixelGroup,speciesCode,age)
  expect_equal(cohortDataShort,cohortDataShort_compared)
  # check the pixelGroupMap
  expect_is(simOutput$pixelGroupMap,"RasterLayer")
  expect_equal(getValues(simOutput$pixelGroupMap),
               c(11,12,13,22,21,23,-1,-1,-1))

  # check the calibration mode
  expect_true(exists("spinupOutput",envir=envir(simOutput)))
  expect_is(simOutput$spinupOutput,"data.table")
  expect_equal(names(simOutput$spinupOutput),
               c("pixelGroup", "species", "age",
                 "iniBiomass", "ANPP", "Mortality","finBiomass"))
  expect_true(exists("simulationTreeOutput",envir=envir(simOutput)))
  expect_is(simOutput$simulationTreeOutput,"data.table")
  expect_equal(names(simOutput$simulationTreeOutput),
               c("Year", "siteBiomass", "Species", "Age", "iniBiomass",
                 "ANPP", "Mortality", "deltaB", "finBiomass"))
  expect_true(exists("regenerationOutput",envir=envir(simOutput)))
  expect_is(simOutput$regenerationOutput,"data.table")
  expect_equal(names(simOutput$regenerationOutput),
               c("seedingAlgorithm", "species", "Year", "numberOfReg"))

  # check the inactive pixels
  expect_true(exists("inactivePixelIndex",envir=envir(simOutput)))
  expect_equal(simOutput$inactivePixelIndex,7:9)

  # check the revised species ecoregion table
  speciesEcoregion_revised <- simOutput$speciesEcoregion[,.(year,ecoregion,species,
                                                            establishprob,maxANPP,maxB)]
  speciesEcoregion_compared <- data.table(year=0,
                                          ecoregion=as.factor(c("eco1","eco1","eco2","eco2")),
                                          species=as.factor(c("abiebals","pinubank",
                                                    "abiebals","pinubank")),
                                          establishprob=c(0.81,0.90,0.45,0.72),
                                          maxANPP=c(974,1243,881,1116),
                                          maxB=c(39870,50850,36045,45675))
  expect_equal(speciesEcoregion_revised,speciesEcoregion_compared)




  objects <- list("initialCommunitiesMap"=initialCommunitiesMap,
                  "ecoregionMap"=ecoregionMap,
                  "successionTimestep"=successionTimestep,
                  "initialCommunities"=initialCommunities,
                  "ecoregion"=ecoregion,
                  "species"=species,
                  "speciesEcoregion"=speciesEcoregion,
                  "cellSize"=cellSize,
                  "useCache"=useCache,
                  "spinupMortalityfraction"=spinupMortalityfraction)
  mySim <- simInit(times=list(start=0, end=2),
                   params=parameters,
                   modules=module,
                   objects=objects,
                   paths=path)
  if(exists("Biomass_coreInit")){
    simOutput <- Biomass_coreInit(mySim)
  } else {
    simOutput <- mySim$Biomass_coreInit(mySim)
  }

  # check the calibration mode
  expect_false(exists("spinupOutput",envir=envir(simOutput)))
  expect_false(exists("simulationTreeOutput",envir=envir(simOutput)))
  expect_false(exists("regenerationOutput",envir=envir(simOutput)))




})
