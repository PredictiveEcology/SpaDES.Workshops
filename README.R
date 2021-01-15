## ---------------------------------------------------------------------------------------------------------------------------------------------------------------
## Restart your R session so it is clear
## Ctrl-shift-F10 if you are in Rstudio #
source("https://raw.githubusercontent.com/PredictiveEcology/SpaDES-modules/master/R/SpaDES_Helpers.R")


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------
installSpaDES() 


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------
installGitHubPackage("PredictiveEcology/Require@development") # install latest version of Require


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------
Require::Require(
  c("PredictiveEcology/LandR",
    "PredictiveEcology/pemisc",
    "tati-micheletti/usefulFuns",
    "achubaty/amc@development"), 
  upgrade = "never", 
  which = c("Imports", "Depends", "Suggets"))


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------
Sys.which("make")


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------
workshopPath = "~/SpaDESWorkshop"
modulePath = file.path(workshopPath, "modules")


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------
if (dir.exists(modulePath)) unlink(modulePath, recursive = TRUE)
# LandR Biomass modules (simulation modules)
getModule("PredictiveEcology/Biomass_core", modulePath = modulePath)
getModule("PredictiveEcology/Biomass_regeneration", modulePath = modulePath)

# LandR Biomass modules (data preparation modules)
getModule("PredictiveEcology/Biomass_borealDataPrep", modulePath = modulePath)
getModule("PredictiveEcology/Biomass_speciesData", modulePath = modulePath)

# SCFM fire modules
getModule("PredictiveEcology/scfm", modulePath = modulePath)

