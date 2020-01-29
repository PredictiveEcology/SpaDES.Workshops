test_that("test Growth-related Mortality calculation",{
  library(SpaDES)
  # define the module and path
  module <- list("Biomass_core")
  path <- list(modulePath="~/GitHub/nrv-succession/code blitz succession/Module_Biomass_core",
               outputPath="~/output")
  parameters <- list(.progress=list(type="graphical", interval=1),
                     .globals=list(verbose=FALSE),
                     Biomass_core=list( .saveInitialTime=NA))
  objects <- list()
  mySim <- simInit(times=list(start=0, end=1),
                   params=parameters,
                   modules=module,
                   objects=objects,
                   paths=path)
  cohortData <- data.table(expand.grid(bAP=seq(0.2,2,by=0.2),bPM=seq(0.2,1,by=0.2),age=0:1))
  cohortData[,':='(maxANPP=1096,B=700)]
  if(exists("calculateGrowthMortality")){
    output <- calculateGrowthMortality(cohortData,stage="spinup")
  } else {
    output <- mySim$calculateGrowthMortality(cohortData,stage="spinup")
  }
  Mortality_Growth_output <- round(output$mBio,4)
  Mortality_Growth_output_compared <- c(rep(NA,50), 73.0667, 125.2571, 164.4, 194.8444, 219.2, 219.2,
                            219.2, 219.2, 219.2, 219.2, 146.1333, 250.5143, 328.8, 389.6889,
                            438.4, 438.4, 438.4, 438.4, 438.4, 438.4, 219.2, 375.7714, 493.2,
                            584.5333, 657.6, 657.6, 657.6, 657.6, 657.6, 657.6, 292.2667,
                            501.0286, 657.6, 700, 700, 700, 700, 700, 700, 700, 365.3333,
                            626.2857, 700, 700, 700, 700, 700, 700, 700, 700)
  expect_equal(Mortality_Growth_output,Mortality_Growth_output_compared)
  rm(Mortality_Growth_output,Mortality_Growth_output_compared,output)
  if(exists("calculateGrowthMortality")){
    output <- calculateGrowthMortality(cohortData,stage="mainsimulation")
  } else {
    output <- mySim$calculateGrowthMortality(cohortData,stage="mainsimulation")
  }
  Mortality_Growth_output <- round(output$mBio,4)
  Mortality_Growth_output_compared <- c(rep(c(73.0667, 125.2571, 164.4, 194.8444, 219.2, 219.2,
                                        219.2, 219.2, 219.2, 219.2, 146.1333, 250.5143, 328.8, 389.6889,
                                        438.4, 438.4, 438.4, 438.4, 438.4, 438.4, 219.2, 375.7714, 493.2,
                                        584.5333, 657.6, 657.6, 657.6, 657.6, 657.6, 657.6, 292.2667,
                                        501.0286, 657.6, 700, 700, 700, 700, 700, 700, 700, 365.3333,
                                        626.2857, 700, 700, 700, 700, 700, 700, 700, 700),2))
  expect_equal(Mortality_Growth_output,Mortality_Growth_output_compared)
})
