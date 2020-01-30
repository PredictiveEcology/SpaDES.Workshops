test_that("test ANPP calculation",{
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
  cohortData <- data.table(expand.grid(bAP=seq(0.1,1,by=0.1),bPM=seq(0.2,1,by=0.2)))
  cohortData[,':='(age=c(rep(0,10),11:50),growthcurve=0.25,maxANPP=1096)]
  if(exists("calculateANPP")){
    output <- calculateANPP(cohortData,stage="spinup")
  } else {
    output <- mySim$calculateANPP(cohortData,stage="spinup")
  }
  ANPP_output <- round(output$aNPPAct,4)
  ANPP_output_compared <- c(rep(NA,10), 381.8944, 408.3121,
                            420.7571, 427.8567, 432.226, 434.9859, 436.7111, 437.7309, 438.2493,
                            438.4, 572.8416, 612.4682, 631.1357, 641.785, 648.339, 652.4788,
                            655.0666, 656.5963, 657.3739, 657.6, 763.7888, 816.6243, 841.5142,
                            855.7134, 864.4519, 869.9718, 873.4221, 875.4617, 876.4985, 876.8,
                            954.736, 1020.7804, 1051.8928, 1069.6417, 1080.5649, 1087.4647,
                            1091.7776, 1094.3271, 1095.6232, 1096)
  expect_equal(ANPP_output,ANPP_output_compared)

  rm(cohortData,ANPP_output,ANPP_output_compared,output)
  cohortData <- data.table(expand.grid(bAP=seq(0.1,1,by=0.1),bPM=seq(0.2,1,by=0.2)))
  cohortData[,':='(age=1:50,growthcurve=0.25,maxANPP=1096)]
  if(exists("calculateANPP")){
    output <- calculateANPP(cohortData,stage="mainsimulation")
  } else {
    output <- mySim$calculateANPP(cohortData,stage="mainsimulation")
  }
  ANPP_output <- round(output$aNPPAct,4)
  ANPP_output_compared <- c(190.9472, 204.1561, 210.3786, 213.9283, 216.113, 217.4929,
                            218.3555, 218.8654, 219.1246, 219.2, 381.8944, 408.3121, 420.7571,
                            427.8567, 432.226, 434.9859, 436.7111, 437.7309, 438.2493, 438.4,
                            572.8416, 612.4682, 631.1357, 641.785, 648.339, 652.4788, 655.0666,
                            656.5963, 657.3739, 657.6, 763.7888, 816.6243, 841.5142, 855.7134,
                            864.4519, 869.9718, 873.4221, 875.4617, 876.4985, 876.8, 954.736,
                            1020.7804, 1051.8928, 1069.6417, 1080.5649, 1087.4647, 1091.7776,
                            1094.3271, 1095.6232, 1096)
  expect_equal(ANPP_output,ANPP_output_compared)
})
