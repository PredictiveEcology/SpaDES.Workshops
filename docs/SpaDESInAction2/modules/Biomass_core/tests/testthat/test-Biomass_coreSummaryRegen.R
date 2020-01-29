test_that("test summary regeneration. ",{
  module <- list("Biomass_core")
  path <- list(modulePath="~/GitHub/nrv-succession/code blitz succession/Module_Biomass_core",
               outputPath="~/output")
  parameters <- list(.progress=list(type="graphical", interval=1),
                     .globals=list(verbose=FALSE),
                     Biomass_core=list( .saveInitialTime=NA))
  cohortData <- data.table(expand.grid(age = c(1, 10, 15),
                                       pixelGroup = 1:5))[,B:=10000]
  cohortData[age == 1, B := seq(100, by = 50, length = 5)]

  cellSize <- 100
  pixelGroupMap <- raster(xmn=50,xmx=50+5*100,
                          ymn=50,ymx=50+5*100,
                          res=c(100,100),
                          val=c(rep(5, 5), rep(4, 5), rep(3, 5),
                                rep(2, 3), rep(-1, 2), rep(1, 3),
                                rep(-1, 2)))

  objects <- list("cohortData" = cohortData,
                  "cellSize" = cellSize,
                  "pixelGroupMap" = pixelGroupMap)
  mySim <- simInit(times=list(start=0, end=2),
                   params=parameters,
                   modules=module,
                   objects=objects,
                   paths=path)
  if(exists("Biomass_coreSummaryRegen")){
    simOutput <- Biomass_coreSummaryRegen(mySim)
  } else {
    simOutput <- mySim$Biomass_coreSummaryRegen(mySim)
  }
  expect_is(simOutput$reproductionMap, "RasterLayer")
  expect_equal(getValues(simOutput$reproductionMap),
               c(rep(300, 5), rep(250, 5), rep(200, 5),
                 rep(150, 3), NA, NA, rep(100, 3), NA, NA))
  rm(simOutput, mySim, objects)

  cohortData[age == 1, age := 2]
  objects <- list("cohortData" = cohortData,
                  "cellSize" = cellSize,
                  "pixelGroupMap" = pixelGroupMap)
  mySim <- simInit(times=list(start=0, end=2),
                   params=parameters,
                   modules=module,
                   objects=objects,
                   paths=path)
  if(exists("Biomass_coreSummaryRegen")){
    simOutput <- Biomass_coreSummaryRegen(mySim)
  } else {
    simOutput <- mySim$Biomass_coreSummaryRegen(mySim)
  }
  expect_is(simOutput$reproductionMap, "RasterLayer")
  expect_equal(getValues(simOutput$reproductionMap),
               c(rep(0, 25)))


})
