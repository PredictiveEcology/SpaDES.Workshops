## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(cache = FALSE, echo = TRUE, eval = FALSE)

## ------------------------------------------------------------------------
#  age <- 1
#  for (time in 1:10) {
#    age <- age + 1
#  }

## ----loopsToEvents-------------------------------------------------------
#  age <- 1
#  
#  # define event
#  aging <- age + 1
#  
#  events <- {
#    doEvent("aging")
#    scheduleEvent("aging", when = now + 1)
#  }
#  
#  times = list(start = 1, end = 10)

## ------------------------------------------------------------------------
#  age <- 1
#  for (time in 1:10) {
#    age <- age + 1
#  }

## ------------------------------------------------------------------------
#  age <- 1

## ------------------------------------------------------------------------
#  ... in 1:10

## ------------------------------------------------------------------------
#  age <-  age + 1

## ------------------------------------------------------------------------
#  age <- 1                                 # Initialize
#  
#  # define event
#  aging <- function(age) {
#    age <- age + 1                         # content
#    scheduleEvent("aging", when = now + 1) # step
#  }
#  
#  times = list(start = 1, end = 10)        # bounds
#  

## ------------------------------------------------------------------------
#      eventTime moduleName  eventType
#   1:         0       loop       init
#   2:         0       loop addOneYear
#   3:         1       loop addOneYear
#   4:         2       loop addOneYear
#   5:         3       loop addOneYear
#   6:         4       loop addOneYear
#   7:         5       loop addOneYear
#   8:         6       loop addOneYear
#   9:         7       loop addOneYear
#  10:         8       loop addOneYear
#  11:         9       loop addOneYear
#  12:        10       loop addOneYear

## ------------------------------------------------------------------------
#  newModule("loop", path = getwd())
#  # This will make a new module, and
#  #  tell you how to open it in the message

## ------------------------------------------------------------------------
#  doEvent.loop = function(sim, eventTime, eventType, debug = FALSE) {
#    switch(
#      eventType,
#      init = {
#        sim$age <- 1
#        sim <- scheduleEvent(sim, start(sim), "loop", "addOneYear")
#      },
#      addOneYear = {
#        sim$age <- sim$age + 1
#        sim <- scheduleEvent(sim, time(sim) + 1, "loop", "addOneYear")
#      },
#    )
#    return(invisible(sim))
#  }

## ------------------------------------------------------------------------
#  times <- list(start = 1, end = 10)
#  modulePath <- file.path("~/GitHub/SpaDES.workshops")
#  modules <- list("loop")

## ------------------------------------------------------------------------
#  # Loops
#  age <- 1
#  for (time in 1:10) {
#    age <- age + 1
#  }
#  
#  # Event
#  mySim <- simInit(times = times, modules = modules,
#                   paths = list(modulePath = "~/GitHub/SpaDES.Workshops"))
#  mySimOut <- spades(mySim, debug = TRUE)
#  
#  # Compare them -- yes!
#  mySimOut$age
#  age

