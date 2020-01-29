test_that("test competition calculation at both spinup stage and main simulation stage",{
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
  cohortData <- data.table(pixelGroup = c(rep(1,13),rep(2,10)),
                           age = c(rep(0,5),6:10,20,30,50,rep(0,4),5:10), B = c(1:13,1:10),
                           maxB = 40, sumB = c(rep(36,13),rep(0,10)), mortality = as.numeric(1:23))
  if(exists("calculateCompetition")){
    output <- calculateCompetition(cohortData,stage="spinup")
  } else {
    output <- mySim$calculateCompetition(cohortData,stage="spinup")
  }
  cohortData_output <- setkey(output[,.(pixelGroup,age,bAP,bPM)],
                              pixelGroup,age)
  cohortData_output$bAP <- round(cohortData_output$bAP,4)
  cohortData_output$bPM <- round(cohortData_output$bPM,4)
  cohortData_output_compared <- setkey(data.table(pixelGroup = c(rep(1,13),rep(2,10)),
                                                  age = c(rep(0,5),6:10,20,30,50,rep(0,4),5:10),
                                                  bAP = c(rep(NA,5), 0.6, 0.6364, 0.6667, 0.6923, 0.7143, 0.7333,
                                                          0.75, 0.7647, rep(NA,4), 0.1111, 0.1304, 0.1489, 0.1667,
                                                          0.1837, 0.2),
                                                  bPM = c(rep(NA,5), 0.0809, 0.0937, 0.1063, 0.1189, 0.1314,
                                                          0.1439, 0.1563, 0.1686, rep(NA,4), 0.1135, 0.135, 0.1563,
                                                          0.1774, 0.1984, 0.2193)),
                                       pixelGroup,age)
  expect_equal(cohortData_output,cohortData_output_compared)
  rm(cohortData_output,cohortData_output_compared)
  if(exists("calculateCompetition")){
    output <- calculateCompetition(cohortData,stage="mainsimulation")
  } else {
    output <- mySim$calculateCompetition(cohortData,stage="mainsimulation")
  }
  cohortData_output <- setkey(output[,.(pixelGroup,age,bAP,bPM)],
                              pixelGroup,age)
  cohortData_output$bAP <- round(cohortData_output$bAP,4)
  cohortData_output$bPM <- round(cohortData_output$bPM,4)
  cohortData_output_compared <- setkey(data.table(pixelGroup = c(rep(1,13),rep(2,10)),
                                                  age = c(rep(0,5),6:10,20,30,50,rep(0,4),5:10),
                                                  bAP = c(0.2, 0.3333, 0.4286, 0.5, 0.5556, 0.6, 0.6364, 0.6667, 0.6923,
                                                          0.7143, 0.7333, 0.75, 0.7647, 0.0244, 0.0476, 0.0698, 0.0909,
                                                          0.1111, 0.1304, 0.1489, 0.1667, 0.1837, 0.2),
                                                  bPM = c(0.0122, 0.0236, 0.0347, 0.0456, 0.0563, 0.067, 0.0775, 0.088,
                                                          0.0984, 0.1088, 0.1191, 0.1294, 0.1396, 0.0199, 0.0385, 0.0566,
                                                          0.0744, 0.092, 0.1094, 0.1267, 0.1438, 0.1608, 0.1778)),
                                       pixelGroup,age)

  expect_equal(cohortData_output,cohortData_output_compared)
})
