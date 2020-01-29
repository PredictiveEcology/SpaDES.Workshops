test_that("test total site biomass (sumB) ",{
  library(SpaDES)
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
  cohortData <- data.table(pixelGroup = c(rep(1,13),rep(2,10)), ecoregionGroup = 1,
                           speciesCode = 16, age = c(1:10,20,30,50,1:10), B = c(1:13,1:10),
                           mortality = 150, aNPPAct = 999)
  # last seeding regeneration time is lastReg =10
  # currentTime is 18, therefore, the newcohorts that regenerated at 10 reach the age of 10
  if(exists("calculateSumB")){
    output <- calculateSumB(cohortData, lastReg=10, currentTime=18, successionTimestep)
  } else {
    output <- mySim$calculateSumB(cohortData, lastReg=10, currentTime=18, successionTimestep)
  }
  cohortData_output <- setkey(output,pixelGroup,age)
  cohortData_output_compared <- setkey(data.table(pixelGroup = c(rep(1,13),rep(2,10)), ecoregionGroup = 1,
                                       speciesCode = 16, age = c(1:10,20,30,50,1:10), B = c(1:13,1:10),
                                       mortality = 150, aNPPAct = 999,
                                       sumB = c(rep(36,13),rep(0,10))),pixelGroup,age)
  expect_equal(cohortData_output,cohortData_output_compared)
  rm(cohortData_output,cohortData_output_compared,output)
  for(i in 19:27){
    if(exists("calculateSumB")){
      output <- calculateSumB(cohortData, lastReg=10, currentTime=i, successionTimestep)
    } else {
      output <- mySim$calculateSumB(cohortData, lastReg=10, currentTime=i, successionTimestep)
    }
    cohortData_output <- setkey(output,pixelGroup,age)
    cohortData_output_compared <- setkey(data.table(pixelGroup = c(rep(1,13),rep(2,10)), ecoregionGroup = 1,
                                                    speciesCode = 16, age = c(1:10,20,30,50,1:10), B = c(1:13,1:10),
                                                    mortality = 150, aNPPAct = 999,
                                                    sumB = c(rep(46,13),rep(10,10))),pixelGroup,age)
    expect_equal(cohortData_output,cohortData_output_compared)
  }
})
