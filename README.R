## ------------------------------------------------------------------------------------------------------------------------------------------------------------------
workshopPath = "~/SpaDESWorkshop"
modulePath = file.path(workshopPath, "modules")


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------
## Restart your R session so it is clear
## Ctrl-shift-F10 if you are in Rstudio #
source("https://raw.githubusercontent.com/PredictiveEcology/SpaDES-modules/master/R/SpaDES_Helpers.R")


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------
installedPkgs <- installed.packages(.libPaths()[1])
if (!"Require" %in% rownames(installedPkgs))
  install.packages("Require") # to make sure you have 2 dependencies (data.table, remotes)
if (!identical(as.character(packageVersion("Require")), "0.0.11"))
  installGitHubPackage("PredictiveEcology/Require@development") # install latest version of Require


## ----for-isolated-package-folder-----------------------------------------------------------------------------------------------------------------------------------
# This isn't perfect as it will not be totally isolated
# .libPaths(file.path(workshopPath, "R"))
# if you want it fully isolated, you will have to run this file in 2 steps:
# Run this next line, then restart session
# Require::setup(file.path(workshopPath, "R"))
# Then restart your session and run it all again


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------
installSpaDES() 


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------
Require::Require(
  c("PredictiveEcology/LandR@development",
    "PredictiveEcology/pemisc@development",
    "tati-micheletti/usefulFuns",
    "achubaty/amc@development"), 
  upgrade = "never", 
  which = c("Imports", "Depends", "Suggets"))


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------
Sys.which("make")


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------
if (dir.exists(modulePath)) unlink(modulePath, recursive = TRUE)
# LandR Biomass modules (simulation modules)
getModule("PredictiveEcology/Biomass_core", modulePath = modulePath)
getModule("PredictiveEcology/Biomass_regeneration", modulePath = modulePath)

# LandR Biomass modules (data preparation modules)
getModule("PredictiveEcology/Biomass_borealDataPrep", modulePath = modulePath)
getModule("PredictiveEcology/Biomass_speciesData", modulePath = modulePath)

# SCFM fire modules
getModule("PredictiveEcology/scfm@development", modulePath = modulePath, overwrite = TRUE)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------
# If you have been using the binary package manager for Ubuntu, you have to turn it off
if (isTRUE(grepl("packagemanager", getOption("repos")[["CRAN"]]))) 
  options("repos" = c(CRAN = "https://cran.rstudio.com/"))
modulesInstalled <- dir(modulePath)
dependencies <- SpaDES.core::reqdPkgs(module = modulesInstalled, modulePath = modulePath)  

# scfm is actually a collection of modules... the modules are nested in folders
scfmModulePath <- file.path(modulePath, "scfm", "modules")
scfmModulesInstalled = dir(scfmModulePath)

dependencies <- append(dependencies, 
                       SpaDES.core::reqdPkgs(module = scfmModulesInstalled, 
                                             modulePath = scfmModulePath) ) 

needed <- unique(unlist(dependencies, recursive = FALSE))
Require::Require(needed, require = FALSE, upgrade = "never")


