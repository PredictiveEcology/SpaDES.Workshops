
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects, use sim$xxx, and are thus globally available
# to all modules. Functions can be used without sim$ as they are namespaced, like functions
# in R packages. If exact location is required, functions will be: sim$<moduleName>$FunctionName
defineModule(sim, list(
  name = "group_scfm",
  description = NA, #"parent module for scfm family of modules",
  keywords = NA, # c("fire burn hot"),
  authors = c(person(c("Steve", "Cumming"), "Last", email = "email@example.com", role = c("aut", "cre"))),
  childModules = c("ageModule", "scfmDriver", "scfmEscape", "scfmIgnition", "scfmLandcoverInit",
                           "scfmRegime", "scfmSpread"),
  version = list(SpaDES.core = "0.2.3", group_scfm = "0.0.1"),

  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "group_scfm.Rmd")
  )
)
