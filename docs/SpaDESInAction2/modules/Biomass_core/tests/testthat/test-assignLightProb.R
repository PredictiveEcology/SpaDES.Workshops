test_that("test assign light probability for a given species tolerance and site shade",{
  library(SpaDES)
  # define the module and path
  module <- list("Biomass_core")
  path <- list(modulePath="~/GitHub/nrv-succession/code blitz succession/Module_Biomass_core",
               outputPath="~/output")
  parameters <- list(.progress=list(type="graphical", interval=1),
                     .globals=list(verbose=FALSE),
                     Biomass_core=list( .saveInitialTime=NA))
  sufficientLight <- data.frame(speciesshadetolerance=1:5,
                                X0=seq(1,0.8,length=5),
                                X1=seq(0.8,0.6,length=5),
                                X2=seq(0.6,0.4,length=5),
                                X3=seq(0.4,0.2,length=5),
                                X4=seq(0.2,0.0,length=5),
                                X5=seq(0,1,length=5))
  objects <- list()
  mySim <- simInit(times=list(start=0, end=1),
                   params=parameters,
                   modules=module,
                   objects=objects,
                   paths=path)
  cohortData <- data.table(expand.grid(shadetolerance=1:5,siteShade=0:5))

  if(exists("assignLightProb")){
    output <- assignLightProb(sufficientLight, newCohortData = cohortData)
  } else {
    output <- mySim$assignLightProb(sufficientLight, newCohortData = cohortData)
  }
  cohortData[,lightProb:=c(seq(1,0.8,length=5),seq(0.8,0.6,length=5),
                           seq(0.6,0.4,length=5),seq(0.4,0.2,length=5),
                           seq(0.2,0.0,length=5),seq(0,1,length=5))]
  output_compared <- cohortData
  expect_equal(output,output_compared)
})
