library(SpaDES)
library(magrittr)

modulePath <- "~/Documents/GitHub/LandWeb/m/"

inputDir <- file.path(dirname(tempdir()), "Biomass_core", "inputs") %>% checkPath(create = TRUE)
outputDir <- file.path(dirname(tempdir()), "Biomass_core", "outputs")
times <- list(start = 0, end = 10)
parameters <- list()
modules <- list("Biomass_core")
paths <- list(
  cachePath = file.path(outputDir, "cache"),
  modulePath = modulePath,
  inputPath = inputDir,
  outputPath = outputDir
)

mySim <- simInit(times = times, params = parameters, modules = modules, paths = paths)

mySimOut <- spades(mySim, debug = TRUE)
