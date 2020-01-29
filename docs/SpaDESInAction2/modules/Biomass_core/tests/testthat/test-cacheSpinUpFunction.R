test_that("test cache function for spinUp",{
  module <- list("Biomass_core")
  path <- list(modulePath="~/GitHub/nrv-succession/code blitz succession/Module_Biomass_core",
               outputPath="~/output")
  parameters <- list(.progress=list(type="graphical", interval=1),
                     .globals=list(verbose=FALSE),
                     Biomass_core=list( .saveInitialTime=NA))
  useCache <- TRUE
  objects <- list("useCache"=useCache)
  mySim <- simInit(times=list(start=0, end=2),
                   params=parameters,
                   modules=module,
                   objects=objects,
                   paths=path)
  cachePath <- tempdir()
  if(dir.exists(file.path(cachePath,"spinUp"))){
    unlink(file.path(cachePath,"spinUp"), recursive = TRUE)
  }
  if(exists("cacheSpinUpFunction")){
    output <- cacheSpinUpFunction(mySim, cachePath = cachePath)
  } else {
    output <- mySim$cacheSpinUpFunction(mySim, cachePath = cachePath)
  }
  expect_is(output$spinUpCache,"function")
  expect_true(dir.exists(file.path(cachePath,"spinUp")))
  expect_true(file.exists(file.path(cachePath,"spinUp","backpack.db")))
  unlink(file.path(cachePath,"spinUp"), recursive = TRUE)

  useCache <- FALSE
  objects <- list("useCache" = useCache)
  mySim <- simInit(times=list(start=0, end=2),
                   params=parameters,
                   modules=module,
                   objects=objects,
                   paths=path)
  if(exists("cacheSpinUpFunction")){
    output <- cacheSpinUpFunction(mySim, cachePath = cachePath)
  } else {
    output <- mySim$cacheSpinUpFunction(mySim, cachePath = cachePath)
  }
  expect_is(output$spinUpCache,"function")
  expect_false(dir.exists(file.path(cachePath,"spinUp")))

})
