# 1. Global script
# Description: 

# 2. Installing necessary libraries and setting options
# devtools::install_github("tati-micheletti/usefun")

# 2a. Libraries
# library("usefun")
library("LandR")
library("SpaDES.core")
library("SpaDES.tools")
library("quickPlot")
library("reproducible")
library("raster")
library("data.table")

# 2b. Options
usrEmail <- if (pemisc::user() %in% c("tmichele", "Tati")) "tati.micheletti@gmail.com" else NULL
googledrive::drive_auth(email = usrEmail)
opts <- options(
  "spades.recoveryMode" = 2,
  "LandR.assertions" = FALSE,
  "LandR.verbose" = 1,
  "reproducible.useMemoise" = TRUE, # Brings cached stuff to memory during the second run
  "reproducible.useNewDigestAlgorithm" = TRUE,  # use the new less strict hashing algo
  "reproducible.useCache" = TRUE,
  "pemisc.useParallel" = TRUE
)

# 3. Project structure

# 4. Setting paths
inputDirectory <- reproducible::checkPath(file.path(getwd(), "inputs"), create = TRUE)
outputDirectory <- reproducible::checkPath(file.path(getwd(), "outputs"), create = TRUE)
modulesDirectory <- reproducible::checkPath(file.path(getwd(), "modules"), create = TRUE)
cacheDirectory <- reproducible::checkPath(file.path(getwd(), "cache"), create = TRUE)

setPaths(cachePath = cacheDirectory,
         modulePath = c(modulesDirectory, 
                        file.path(modulesDirectory, "scfm/modules")),
         inputPath = inputDirectory,
         outputPath = outputDirectory)
paths <- getPaths()

# 5. Time
times <- list(start = 0, end = 20)

# 6. Modules
modulesLandR <- c("Boreal_LBMRDataPrep",
                  "Biomass_core")

modulesFire <- c("scfmIgnition",
                 "scfmEscape",
                 "scfmSpread")

firePrep <-  c("scfmLandcoverInit",
               "scfmRegime",
               "scfmDriver")

modulesAll <- c("Boreal_LBMRDataPrep",
                "Biomass_core")

modules <- list(moduleName)


# 7. Parameters
successionTimestep <- 1L
parameters <- list(
  Biomass_core = list(
    ".plotInitialTime" = times$start
    , "sppEquivCol" = "Boreal"
    , "successionTimestep" = successionTimestep*10
    , ".useCache" = TRUE 
    , ".useParallel" = FALSE
  )
)

parameters <- list(
  Biomass_core = list(
    ".plotInitialTime" = times$start
    , "sppEquivCol" = "Boreal"
    , "successionTimestep" = successionTimestep*10
    , ".useCache" = TRUE 
    , ".useParallel" = FALSE
  ),
  Biomass_regeneration = list(
    "fireTimestep" = successionTimestep,
    "fireInitialTime" = times$start
  ),
  scfmLandcoverInit = list(
    ".plotInitialTime" = NA
  ),
  scfmSpread = list(
    "pSpread" = 0.235,
    "returnInterval" = defaultInterval,
    "startTime" = times$start,
    ".plotInitialTime" = NA,
    ".plotInterval" = defaultPlotInterval,
    ".saveInitialTime" = defaultInitialSaveTime,
    ".saveInterval" = defaultInterval),
  scfmRegime = list(fireCause = "L"), #c("L", "H")
  scfmDriver = list(
    targetN = 1000)
)
# 8. Objects
# 8a. Generate Study Area
polyMatrix <- matrix(c(-118.269387, 61.783558), ncol = 2)
areaSize <- 10000000
set.seed(2020)
studyArea <- SpaDES.tools::randomPolygon(x = polyMatrix, hectares = areaSize) # Create Random polygon in southern Ontario
plot(studyArea)


# 9. SimInit and the simList
mySim <- simInitAndSpades(times = times,
                          params = parameters,
                          modules = as.list(firePrep),
                          paths = getPaths(),
                          loadOrder = firePrep, debug = 1)

# 10. `spades()`


# 11. Checking your outputs

# SCFM needs:
# 1. Ignition:
# scfmDriverPars
# flammableMap
# landscapeAttr
# 2. Escape:
# scfmDriverPars
# ignitionLoci (created by Ignition)
# flammableMap
# 3. Spread
# spreadState (created by Escape)
# flammableMap
# scfmDriverPars
# 
# SCFM CREATES
# burnSummary
# pSpread
# rstCurrentBurn
# burnDT
# burnMap


# ~~~~~~~~~~~~~~~~~~~~~~ SCFM posthoc ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#for comparing parameters of interest
comparePredictions <- function(polyList, simList) {
  out <- lapply(polyList, FUN = function(x, sim = simList) {
    regime <- sim$scfmRegimePars[[x]]
    driver <- sim$scfmDriverPars[[x]]
    landscapeAttr <- sim$landscapeAttr[[x]]
    firePoints <- outSim$firePoints[outSim$firePoints$PolyID == x,]
    hist_MAAB <- sum(firePoints$SIZE_HA[firePoints$SIZE_HA > landscapeAttr$cellSize])/
      (landscapeAttr$burnyArea*(sim@params$scfmRegime$fireEpoch[2] - sim@params$scfmRegime$fireEpoch[1] + 1)) * 100
    #This is a long way of saying, sum of fires/ (flammable landscape * fire epoch )
    #hist_mfs will be NaN if there were no fires larger than one pixel
    pSpread <- driver$pSpread 
    burnSum <- sim$burnSummary[sim$burnSummary$polyID == x,]
    burnSum$N <- as.numeric(burnSum$N)
    burnSum$areaBurned <- as.numeric(burnSum$areaBurned)
    burnSum <- burnSum[burnSum$N > 1]
    mod_MAAB <- sum(burnSum$areaBurned)/(landscapeAttr$burnyArea * (times(sim)$end - times(sim)$start)) * 100
    
    pred <- data.frame("PolyId" = x, #Polygon ID
                       "histMeanSize" = regime$xBar, #The predicted (empirical) mean size of fires
                       "modMeanSize" = mean(burnSum$areaBurned), #The modeled mean size of fires
                       "pSpread" = pSpread, # The spread probability estimated from the SCAM model
                       "hist_MAAB" = hist_MAAB,#The empirical mean annual area burned (from NFDB 1970-2000)
                       "mod_MAAB" = mod_MAAB) #The modelled mean annual area burned
    return(pred)
  })
  return(out)
}

df <- comparePredictions(names(outSim$scfmDriverPars), outSim) %>%
  rbindlist(.)
#Some useful plots
breaks = c(0.20, 0.21, 0.22, 0.23, 0.24, 0.25)
ggplot(df, aes(x = histMeanSize, y = modMeanSize, color = pSpread)) +
  geom_point(aes(histMeanSize, modMeanSize)) + 
  scale_color_gradient(low = "yellow", high = "red")+
  labs(y = "modeled mean fire size size", x = "historical mean size fire size") +
  theme_minimal() + 
  #geom_text(aes(label = PolyId), hjust = 0, vjust = 0)
  geom_abline(slope = 1)


ggplot(df, aes(x = hist_MAAB, y = mod_MAAB, col = pSpread)) +
  geom_point(aes(hist_MAAB, mod_MAAB)) +
  labs(y = "model mean annual area burned (%)", x = "empirical mean annual area burned (%)") +
  scale_color_gradient(low = "yellow", high = "red")+
  theme_minimal() +  #+ 
  geom_abline(slope = 1)
# xlim(c(0, 8000)) + 
# ylim(c(0, 12000)) # + 
