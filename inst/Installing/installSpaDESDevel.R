## Restart your R session so it is clear
## Ctrl-shift-F10 if you are in Rstudio #

## Make sure your existing packages are updated, and correct for the version of R
## Get latest versions of key SpaDES packages from CRAN
dependencies <- tools::package_dependencies(c("devtools", "SpaDES"), recursive = TRUE)

## Update any versions of these dependencies that are already on your machine
type <- if (.Platform$OS.type == "windows" || Sys.info()["sysname"] == "Darwin") "binary" else "source"
update.packages(oldPkgs = unique(unlist(dependencies)),
                ask = FALSE, checkBuilt = TRUE, type = type)

## Install any dependencies that are missing --
##   install.packages is not getting correct dependencies
missingPkgs <- dependencies$SpaDES[!(dependencies$SpaDES %in% rownames(installed.packages()))]
if (length(missingPkgs))
  install.packages(missingPkgs, dependencies = FALSE, type = type)

## Install all SpaDES packages
install.packages("SpaDES", dependencies = FALSE)

## For the workshop, there are a key enhancements that are not in the CRAN version
## Restart your R session so it is clear
# Ctrl-shift-F10 if you are in Rstudio #
install.packages("devtools", dependencies = FALSE) # installs the latest version


## Restart your R session again so it is clear
## Ctrl-shift-F10 if you are in Rstudio #
Sys.setenv("R_REMOTES_NO_ERRORS_FROM_WARNINGS"="true") # sometimes, some irrelevant warnings occur
library(devtools)
type <- if (.Platform$OS.type == "windows") "binary" else "source"
install_github("PredictiveEcology/quickPlot@development",
               upgrade = "never", dependencies = TRUE, type = type)
install_github("PredictiveEcology/reproducible@Workshop",
               upgrade = "never", dependencies = TRUE, type = type)
install_github("PredictiveEcology/SpaDES.core@Workshop",
               upgrade = "never", dependencies = TRUE, type = type)
install_github("PredictiveEcology/SpaDES.tools@Workshop",
               upgrade = "never", dependencies = TRUE, type = type)
install_github("PredictiveEcology/SpaDES.experiment@Workshop",
               upgrade = "never", dependencies = TRUE, type = type)


# Predictive Ecology Miscellaneous
install_github("PredictiveEcology/pemisc@Workshop", upgrade = "never",
               dependencies = TRUE, type = type)

# Vegetation Dynamics
install_github("PredictiveEcology/LandR@Workshop", upgrade = "never",
               dependencies = TRUE, type = type)


# Alex's Miscellaneous Code
install_github("achubaty/amc@development", upgrade = "never", dependencies = TRUE,
               type = type)

# Other 'useful functions'
install_github("tati-micheletti/usefun@master", upgrade = "never", dependencies = TRUE,
               type = type)
