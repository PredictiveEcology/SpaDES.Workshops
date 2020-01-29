test_that("test site shade calculation",{
  library(SpaDES)
  # define the module and path
  module <- list("Biomass_core")
  path <- list(modulePath="~/GitHub/nrv-succession/code blitz succession/Module_Biomass_core",
               outputPath="~/output")
  parameters <- list(.progress=list(type="graphical", interval=1),
                     .globals=list(verbose=FALSE),
                     Biomass_core=list( .saveInitialTime=NA))
  minRelativeB <- data.table(ecoregion = c("eco1", "eco2"), X1 = c(0.145, 0.15),
                             X2 = c(0.217, 0.25), X3 = c(0.288, 0.5), X4 = c(0.359, 0.8),
                             X5 = c(0.430, 0.95), ecoregionGroup = 1:2)
  speciesEcoregion <- data.table(year=rep(seq(0,100,by=10),2), ecoregion=c(rep("eco1",11),rep("eco2",11)),
                                 species="tsugcana", establishprob=0.22,maxANPP=1096,maxB=32880,
                                 ecoregionGroup=1)
  objects <- list()
  mySim <- simInit(times=list(start=50, end=51),
                   params=parameters,
                   modules=module,
                   objects=objects,
                   paths=path)
  cohortData <- data.table(pixelGroup = 1:20, ecoregionGroup = 1, speciesCode = 1:20,
                           age = 1:20,
                           B = c(rep(300, 5), as.integer(seq(100, 32880,length = 15))),
                           mortality = 50, aNPPAct = 50)
  if(exists("calcSiteShade")){
    output <- calcSiteShade(currentTime = time(mySim), cohortData, speciesEcoregion, minRelativeB)
  } else {
    output <- mySim$calcSiteShade(currentTime = time(mySim), cohortData, speciesEcoregion, minRelativeB)
  }
  output_compared <- data.table(pixelGroup=1:20,siteShade=c(rep(0,7),1:5,rep(5,8)))
  expect_equal(output,output_compared)

  rm(cohortData)
  cohortData <- data.table(pixelGroup = 1:20, ecoregionGroup = 1, speciesCode = 1:20,
                           age = sample(1:5, size=20, replace = TRUE),
                           B = c(rep(300,5),as.integer(seq(100,32880,length = 15))),
                           mortality = 50, aNPPAct  = 100)
  if(exists("calcSiteShade")){
    output <- calcSiteShade(currentTime = time(mySim), cohortData, speciesEcoregion, minRelativeB)
  } else {
    output <- mySim$calcSiteShade(currentTime = time(mySim), cohortData, speciesEcoregion, minRelativeB)
  }
  expect_equal(output,
               data.table(pixelGroup = 1:20, siteShade = 0))

})
