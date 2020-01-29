test_that("test process of age reclassification",{
  # define the module and path
  module <- list("Biomass_core")
  path <- list(modulePath="~/GitHub/nrv-succession/code blitz succession/Module_Biomass_core",
               outputPath="~/output")
  parameters <- list(.progress=list(type="graphical", interval=1),
                     .globals=list(verbose=FALSE),
                     Biomass_core=list( .saveInitialTime=NA))
  successionTimestep <- 10
  objects <- list()
  mySim <- simInit(times=list(start=0, end=1),
                   params=parameters,
                   modules=module,
                   objects=objects,
                   paths=path)
  cohortData <- data.table(pixelGroup = 1, ecoregionGroup = 1,
                           speciesCode = 16, age = c(11,50), B = 40,
                           mortality = 150, aNPPAct = 999)
  if(exists("ageReclassification")){
    output <- ageReclassification(cohortData, successionTimestep, stage="spinup")
  } else {
    output <- mySim$ageReclassification(cohortData, successionTimestep, stage="spinup")
  }
  cohortData_output <- setkey(output,age)
  cohortData_output_compared <- setkey(data.table(pixelGroup = 1, ecoregionGroup = 1,
                           speciesCode = 16, age = c(10,50), B = 40,
                           mortality = 150, aNPPAct = 999),age)
  expect_equal(cohortData_output,cohortData_output_compared)

  rm(cohortData,cohortData_output,cohortData_output_compared,output)
  cohortData <- data.table(pixelGroup = 1, ecoregionGroup = 1,
                           speciesCode = 16, age = c(1:10,49), B = c(1:11),
                           mortality = 150, aNPPAct = 999)
  if(exists("ageReclassification")){
    output <- ageReclassification(cohortData, successionTimestep, stage="mainsimulaiton")
  } else {
    output <- mySim$ageReclassification(cohortData, successionTimestep, stage="mainsimulaiton")
  }

  cohortData_output <- setkey(output,age)
  cohortData_output_compared <- setkey(data.table(pixelGroup = 1, ecoregionGroup = 1,
                                speciesCode = 16, age = c(9,49), B = c(55,11),
                                mortality = c(1500,150), aNPPAct = c(9990,999)),age)

  expect_equal(cohortData_output,cohortData_output_compared)
})
