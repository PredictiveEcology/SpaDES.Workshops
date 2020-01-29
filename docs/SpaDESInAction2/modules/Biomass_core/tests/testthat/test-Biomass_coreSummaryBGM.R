test_that("test summary aboveground biomass, growth, mortality. ",{
  library(raster)
  library(data.table)
  module <- list("Biomass_core")
  path <- list(modulePath="~/GitHub/nrv-succession/code blitz succession/Module_Biomass_core",
               outputPath="~/output")
  parameters <- list(.progress=list(type="graphical", interval=1),
                     .globals=list(verbose=FALSE),
                     Biomass_core=list( .saveInitialTime=NA))
  cohortData <- data.table(expand.grid(speciesCode = 1:3,
                                       pixelGroup = 1:5))
  cohortData[,':='(B = seq(700, by = 50, length = 15),
                   aNPPAct = seq(100, by = 20, length = 15),
                   mortality = seq(200, by = 20, length = 15))]
  cohortData[pixelGroup == 1 | pixelGroup == 2, ecoregionGroup := 2]
  cohortData[pixelGroup == 3 | pixelGroup == 4 | pixelGroup == 5, ecoregionGroup := 1]
  cohortData <- cohortData[,.(pixelGroup, ecoregionGroup, speciesCode, age = 50,
                              B, mortality, aNPPAct)]
  pixelGroupMap <- raster(xmn=50,xmx=50+5*100,
                          ymn=50,ymx=50+5*100,
                          res=c(100,100), val=c(rep(5, 5), rep(4, 5), rep(3, 5),
                                                rep(2, 3), rep(-1, 2), rep(1, 3),
                                                rep(-1, 2)))
  species <- data.table(species = c("species1", "species2", "species3"),
                        speciesCode = 1:3)


  ecoregionMap <- setValues(pixelGroupMap, 0)
  ecoregionMap[c(1:15)] <- 1
  ecoregionMap[c(16:18, 21:23)] <- 2
  ecoregionMap[c(19:20, 24:25)] <- 3
  activePixelIndex <- c(1:18, 21:23)
  reproductionData <- data.table(pixelGroup = getValues(pixelGroupMap)[c(1, 3, 5, 7, 9, 16, 22)],
                                 ecoregionGroup = getValues(ecoregionMap)[c(1, 3, 5, 7, 9, 16, 22)],
                                 speciesCode = c(1, 2, 3, 1, 2, 3, 1),
                                 age = 1,
                                 B = c(100, 300, 500, 700, 900, 150, 220),
                                 mortality = 0, aNPPAct = 0)
  cohortData <- rbind(cohortData, reproductionData)
  simulationOutput <- data.table(Ecoregion = numeric(), NofCell = numeric(), Year = numeric(), Biomass = numeric(),
                                 ANPP = numeric(), Mortality = numeric(), Regeneration = numeric())
  simulationOutput_byspecies <- data.table(Ecoregion = numeric(), Species = character(), Year = numeric(),
                                           Biomass = numeric(), ANPP = numeric(), Mortality = numeric())
  cellSize <- 100
  activeEcoregionLength <- data.table(Ecoregion = getValues(ecoregionMap), pixelIndex = 1:ncell(ecoregionMap))[
    ,.(NofCell = length(pixelIndex)), by = Ecoregion]

  objects <- list("cohortData" = cohortData,
                  "pixelGroupMap" = pixelGroupMap,
                  "ecoregionMap" = ecoregionMap,
                  "simulationOutput" = simulationOutput,
                  "cellSize" = cellSize,
                  "activeEcoregionLength" = activeEcoregionLength)
  mySim <- simInit(times=list(start=0, end=2),
                   params=parameters,
                   modules=module,
                   objects=objects,
                   paths=path)
  if(exists("Biomass_coreSummaryBGM")){
    simOutput <- Biomass_coreSummaryBGM(mySim)
  } else {
    simOutput <- mySim$Biomass_coreSummaryBGM(mySim)
  }
  # check the maps
  expect_is(simOutput$simulatedBiomass, "RasterLayer")
  expect_equal(getValues(simOutput$simulatedBiomass),
               c(rep(4950, 5), rep(5200, 5), rep(3150, 5),
                 rep(2850, 3), NA, NA, rep(2470, 3), NA, NA))

  expect_is(simOutput$ANPPMap, "RasterLayer")
  expect_equal(getValues(simOutput$ANPPMap),
               c(rep(1080, 5), rep(900, 5), rep(720, 5),
                 rep(540, 3), NA, NA, rep(360, 3), NA, NA))

  expect_is(simOutput$mortalityMap, "RasterLayer")
  expect_equal(getValues(simOutput$mortalityMap),
               c(rep(1380, 5), rep(1200, 5), rep(1020, 5),
                 rep(840, 3), NA, NA, rep(660, 3), NA, NA))

  # check the outputs
  expect_is(simOutput$simulationOutput, "data.table")
  expect_equal(simOutput$simulationOutput,
               data.table(Ecoregion = c(1, 2),
                          NofCell = c(15, 6),
                          Year = c(0, 0),
                          Biomass = c(4433, 2660),
                          ANPP = c(900, 450),
                          Mortality = c(1200, 750),
                          Regeneration = c(1967, 185)))
})
