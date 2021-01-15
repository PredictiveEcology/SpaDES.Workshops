## ---------------------------------------------------------------------------------------------------------------------------------------------------------------
## Restart your R session so it is clear
## Ctrl-shift-F10 if you are in Rstudio #
source("https://raw.githubusercontent.com/PredictiveEcology/SpaDES-modules/master/R/SpaDES_Helpers.R")


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------
installGitHubPackage("PredictiveEcology/Require@development") # install latest version of Require


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------
installSpaDES() 


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
# LandR Biomass modules
getModule("PredictiveEcology/Biomass_core", modulePath = modulePath)
getModule("PredictiveEcology/Biomass_borealDataPrep", modulePath = modulePath)
getModule("PredictiveEcology/Biomass_regeneration", modulePath = modulePath)
getModule("PredictiveEcology/Biomass_speciesData", modulePath = modulePath)

# SCFM fire modules
getModule("PredictiveEcology/scfm", modulePath = modulePath)

