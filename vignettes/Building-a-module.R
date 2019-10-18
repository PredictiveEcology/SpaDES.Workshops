## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(cache = TRUE, echo = TRUE, eval = FALSE)

## ----scheduleEvent-caribou-----------------------------------------------
#  ## caribouMovement
#  sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval,
#                       "caribouMovement", "plot", .last())

## ----scheduleEvent-fireSpread--------------------------------------------
#  ## fireSpread
#  sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval,
#                       "fireSpread", "plot", .last())

## ------------------------------------------------------------------------
#  module.path <- file.path(dirname(tempdir()), "modules")
#  
#  ## hint: you'll need to download this module first
#  downloadModule("gameOfLife", module.path)
#  
#  ## make a copy and open for editing
#  copyModule("gameOfLife", "gameOfLifeError", module.path)
#  openModules("gameOfLifeError", module.path)

## ------------------------------------------------------------------------
#  sim$world[r*f <= 2] <- FALSE

## ------------------------------------------------------------------------
#  sim$world[!r*f == 3] <- TRUE

## ------------------------------------------------------------------------
#  setPaths(modulePath = module.path)
#  
#  library(igraph)
#  library(raster)
#  
#  X <- 10
#  Y <- 10
#  TYPE <- "blinker" ## see Rmd for other types
#  
#  modules <- list("gameOfLife")
#  parameters <- list(
#    gameOfLife = list(X = X, Y = Y, initialType = TYPE)
#  )
#  times <- list(start = 1, end = 20)
#  
#  clearPlot()
#  dev()
#  
#  mySim_OK <- simInit(times = times, params = parameters, modules = modules)
#  mySim_OK1 <- spades(Copy(mySim_OK))

## ------------------------------------------------------------------------
#  X <- 10
#  Y <- 10
#  TYPE <- "blinker" ## see Rmd for other types
#  
#  modules <- list("gameOfLifeError")
#  parameters <- list(
#    gameOfLifeError = list(X = X, Y = Y, initialType = TYPE)
#  )
#  times <- list(start = 1, end = 20)
#  
#  clearPlot()
#  dev()
#  
#  mySim_ERR <- simInit(times = times, params = parameters, modules = modules)
#  mySim_ERR1 <- spades(Copy(mySim_ERR))

