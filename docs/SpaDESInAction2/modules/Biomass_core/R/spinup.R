#' spinUp
#'
#' @param fnList TODO: remove this once the functions are in the package!
#' @param cohortData TODO: description needed
#' @param calibrate TODO: description needed
#' @param successionTimestep TODO: description needed
#' @param spinupMortalityfraction TODO: description needed
#' @param species TODO: description needed
#'
#' @return TODO: description needed
#'
#' @export
#' @importFrom data.table data.table set setkey
#'
spinUp <- compiler::cmpfun(function(cohortData, calibrate, successionTimestep,
                                    spinupMortalityfraction, species) {
  maxAge <- max(cohortData$age, na.rm = TRUE) # determine the pre-simulation length
  set(cohortData, NULL, "origAge", cohortData$age)
  set(cohortData, NULL, c("age", "sumB"), as.integer(0L))
  set(cohortData, NULL, c("mortality", "aNPPAct"), as.numeric(0))
  if (calibrate) {
    spinupOutput <- data.table(pixelGroup = integer(), species = character(),
                               age = integer(), iniBiomass = integer(),
                               ANPP = numeric(), Mortality = numeric(),
                               finBiomass = integer())
  }
  k <- 0
  if (successionTimestep == 1 & maxAge != 1) {
    presimuT_end <- 2
  } else {
    presimuT_end <- 1
  }

  for (presimuT in (maxAge):presimuT_end) {
    message("Spin up time: year ", -presimuT)
    k <- k + 1
    cohortData[origAge == presimuT, age := 1L]
    cohortData[origAge >= presimuT, age := age + 1L]

    if (successionTimestep != 1 &
        as.integer(k/successionTimestep) == k/successionTimestep) {
      cohortData <- ageReclassification(cohortData = cohortData,
                                        successionTimestep = successionTimestep,
                                        stage = "spinup")
    }
    # 1. assign the biomass for the first cohort
    if (nrow(cohortData[age == 2, ]) > 0) {
      lastReg <- k - 1
      cohortData <- calculateSumB(cohortData, lastReg = lastReg, currentTime = k,
                                         successionTimestep = successionTimestep)
      cohortData[age == 2, B := asInteger(pmax(1, maxANPP * exp(-1.6 * sumB / maxB_eco)))]
      cohortData[age == 2, B := asInteger(pmin(maxANPP, B))]
    }
    if (maxAge != 1) {
      # 2. calculate age-related mortality
      cohortData <- calculateAgeMortality(cohortData, stage = "spinup",
                                                 spinupMortalityfraction = spinupMortalityfraction)
      # 3. calculate the actual ANPP
      # calculate biomass Potential, for each cohort
      cohortData <- calculateSumB(cohortData, lastReg = lastReg, currentTime = k - 1,
                                         successionTimestep = successionTimestep)
      cohortData <- calculateCompetition(cohortData, stage = "spinup")
      # calculate ANPP
      cohortData <- calculateANPP(cohortData, stage = "spinup")
      cohortData[age > 0, aNPPAct := pmax(1, aNPPAct - mAge)]
      # calculate growth related mortality
      cohortData <- calculateGrowthMortality(cohortData, stage = "spinup")
      cohortData[age > 0, mBio := pmax(0, mBio - mAge)]
      cohortData[age > 0, mBio := pmin(mBio, aNPPAct)]
      cohortData[age > 0, mortality := mBio + mAge]
      cohortData[age > 0, B := asInteger(B + asInteger(aNPPAct - mortality))]
      set(cohortData, NULL, c("bPM", "mBio"), NULL)
    }
    if (calibrate) {
      if (maxAge != 1) {
        spoutput <- cohortData[origAge >= presimuT, .(pixelGroup, speciesCode, age,
                                                      iniBiomass = B + asInteger(mortality - aNPPAct),
                                                      ANPP = round(aNPPAct, 1),
                                                      Mortality = round(mortality, 1),finBiomass = B)]
        spoutput <- setkey(spoutput, speciesCode)[
          setkey(species[, .(species, speciesCode)], speciesCode), nomatch = 0][
            , speciesCode := species][, species := NULL]

        setnames(spoutput, "speciesCode", "species")
        spinupOutput <- rbind(spinupOutput, spoutput)
        rm(spoutput)
        cohortData[, ':='(bAP = NULL)]
      } else {
        spoutput <- cohortData[origAge >= presimuT, .(pixelGroup, speciesCode, age,
                                                      iniBiomass = 0, ANPP = 0,
                                                      Mortality = 0, finBiomass = B)]
        spoutput <- setkey(spoutput, speciesCode)[
          setkey(species[, .(species, speciesCode)], speciesCode), nomatch = 0][
            , speciesCode := species][ , species := NULL]

        setnames(spoutput, "speciesCode", "species")
        spinupOutput <- rbind(spinupOutput, spoutput)
        rm(spoutput)
      }
    }
    lastnewcohorts <- which(cohortData$origAge == 1)
    if (presimuT == presimuT_end & length(lastnewcohorts) > 0 & maxAge != 1) {
      cohortData <- calculateSumB(cohortData, lastReg = lastReg, currentTime = k,
                                         successionTimestep = successionTimestep)
      cohortData[origAge == 1, B := asInteger(pmax(1, maxANPP * exp(-1.6 * sumB / maxB_eco)))]
      cohortData[origAge == 1, B := asInteger(pmin(maxANPP, B))]
    }
  }
  cohortData[, ':='(age = origAge, origAge = NULL)]
  if (calibrate) {
    all <- list(cohortData = cohortData, spinupOutput = spinupOutput)
  } else {
    all <- list(cohortData = cohortData)
  }
  return(all)
})

# cacheSpinUpFunction <- function(sim, cachePath) {
#   # for slow functions, add cached versions. Then use sim$xxx() throughout module instead of xxx()
#   if (P(sim)$useCache) {
#     sim$spinUpCache <- function(...) {
#       reproducible::Cache(FUN = spinUp, ...)
#     }
#   } else {
#     # Step 3 - create a non-caching version in case caching is not desired
#     #  sim$spinUp <- sim$spinUpRaw
#     sim$spinUpCache <- spinUp
#   }
#   return(invisible(sim))
# }
