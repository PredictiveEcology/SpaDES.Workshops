#' updateSpeciesEcoregionAttributes
#'
#' TODO: description and title needed
#'
#' @param speciesEcoregion TODO: description needed
#' @param currentTime TODO: description needed
#' @param cohortData \code{data.table} TODO: description needed
#'
#' @return updated cohort \code{data.table}
#'
#' @export
#' @importFrom data.table setkey
#' @importFrom LandR speciesEcoregionLatestYear
updateSpeciesEcoregionAttributes <- function(speciesEcoregion, currentTime, cohortData) {
  # the following codes were for updating cohortdata using speciesecoregion data at current simulation year
  # to assign maxB, maxANPP and maxB_eco to cohortData
  specieseco_current <- speciesEcoregionLatestYear(speciesEcoregion, currentTime)

  #specieseco_current <- speciesEcoregion[year <= currentTime]
  specieseco_current <- setkey(specieseco_current[, .(speciesCode, maxANPP, maxB, ecoregionGroup)],
                               speciesCode, ecoregionGroup)
  specieseco_current[, maxB_eco := max(maxB), by = ecoregionGroup]

  cohortData <- specieseco_current[cohortData, on = c("speciesCode", "ecoregionGroup"), nomatch = 0]
  return(cohortData)
}

#' updateSpeciesAttributes
#'
#' TODO: description and title needed
#'
#' @param species TODO: description needed
#' @param cohortData \code{data.table} TODO: description needed
#'
#' @return updated cohort \code{data.table}
#'
#' @export
#' @importFrom data.table setkey
updateSpeciesAttributes <- function(species, cohortData) {
  # to assign longevity, mortalityshape, growthcurve to cohortData
  species_temp <- setkey(species[, .(speciesCode, longevity, mortalityshape, growthcurve)], speciesCode)
  setkey(cohortData, speciesCode)
  cohortData <- cohortData[species_temp, nomatch = 0]
  return(cohortData)
}

#' Calculate total biomass
#'
#' Calculate the total stand biomass that does not include the new cohorts.
#' The new cohorts are defined as the age younger than simulation time step.
#' TODO: update description.
#'
#' @param cohortData \code{data.table} TODO: description needed
#' @param lastReg TODO: description needed
#' @param currentTime TODO: description needed -- rename this to 'time' to match others
#' @param successionTimestep TODO: description needed
#' @param doAssertion TODO: description needed (see LandR for description)
#'
#' @return updated cohort \code{data.table}
#'
#' @export
#' @importFrom data.table copy rbindlist setkey
calculateSumB <- compiler::cmpfun(function(cohortData, lastReg, currentTime, successionTimestep,
                                           doAssertion = getOption("LandR.assertions", TRUE)) {
  nrowCohortData <- NROW(cohortData)

  if (isTRUE(doAssertion))
    message("LandR::vegTypeMapGenerator: NROW(cohortData) == ", nrowCohortData)

  ## use new vs old algorithm based on size of cohortData. new one (2) is faster in most cases.
  ## enable assertions to view timings for each algorithm before deciding which to use.
  algo <- 2 #ifelse(nrowCohortData > 3.5e6, 1, 2) ## TODO: fix error in algo1

  if (isTRUE(doAssertion)) {
    cohortDataCopy <- data.table::copy(cohortData)

    uniqueCohortDataPixelGroup <- unique(cohortData$pixelGroup)

    ## newer (faster) version -- May 29 Eliot
    pixelGroups <- setDT(list(pixelGroupIndex = uniqueCohortDataPixelGroup,
                              temID = 1:length(uniqueCohortDataPixelGroup)))

    ## previous algorithm -- May 29 Eliot changed to above
    pixelGroups2 <- data.table(pixelGroupIndex = uniqueCohortDataPixelGroup,
                               temID = 1:length(uniqueCohortDataPixelGroup))

    if (!identical(pixelGroups, pixelGroups2))
      stop("calculateSumB: pixelGroups not identical to pixelGroups2")

    cutpoints <- sort(unique(c(seq(1, max(pixelGroups$temID), by = 10^4), max(pixelGroups$temID))))
    if (length(cutpoints) == 1)
      cutpoints <- c(cutpoints, cutpoints + 1)
    pixelGroups[, groups := cut(temID, breaks = cutpoints,
                                labels = paste("Group", 1:(length(cutpoints) - 1), sep = ""),
                                include.lowest = TRUE)]

    for (subgroup in paste("Group",  1:(length(cutpoints) - 1), sep = "")) {
      subCohortData <- cohortData[pixelGroup %in% pixelGroups[groups == subgroup, ]$pixelGroupIndex, ]
      set(subCohortData, NULL, "sumB", 0L)
      sumBtable <- if (currentTime == lastReg + successionTimestep - 2) {
        subCohortData[age > successionTimestep, .(tempsumB = sum(B, na.rm = TRUE)), by = pixelGroup]
      } else {
        subCohortData[age >= successionTimestep, .(tempsumB = sum(B, na.rm = TRUE)), by = pixelGroup]
      }

      if (!is.integer(sumBtable[["tempsumB"]]))
        set(sumBtable, NULL, "tempsumB", asInteger(sumBtable[["tempsumB"]]))

      subCohortData <- merge(subCohortData, sumBtable, by = "pixelGroup", all.x = TRUE)
      subCohortData[is.na(tempsumB), tempsumB := 0L][, ':='(sumB = tempsumB, tempsumB = NULL)]

      newcohortData <- if (subgroup == "Group1") {
        subCohortData
      } else {
        rbindlist(list(newcohortData, subCohortData))
      }
      rm(subCohortData, sumBtable)
    }
    rm(cohortData, pixelGroups, cutpoints)
    cohortData <- data.table::copy(cohortDataCopy)
  }

  if (algo == 1 || isTRUE(doAssertion)) {
    ## this older version is typically much slower than the newer one below (Eliot June 2, 2019)
    cohortData1 <- copy(cohortData)
    old1 <- Sys.time()
    oldKey <- checkAndChangeKey(cohortData1, "pixelGroup")
    cohortData1[age >= successionTimestep, sumB := sum(B, na.rm = TRUE), by = "pixelGroup"]
    setorderv(cohortData1, c("sumB"), na.last = TRUE)
    a2 <- cohortData1[, list(sumB2 = sumB[1]), by = "pixelGroup"]
    sumB2 <- a2[cohortData1, on = "pixelGroup"][["sumB2"]]
    sumB2[is.na(sumB2)] <- 0L
    set(cohortData1, NULL, "sumB2", sumB2)
    #if (!isTRUE(all.equal(cohortData1[["sumB"]], cohortData1[["sumB2"]]))) ## TODO: this check fails
    #  stop("Failed test in calculateSumB")
    set(cohortData1, NULL, "sumB2", NULL)
    if (!is.null(oldKey))
      setkeyv(cohortData1, oldKey)
    old2 <- Sys.time()
    if (!is.integer(cohortData1[["sumB"]]))
      set(cohortData1, NULL, "sumB", asInteger(cohortData1[["sumB"]]))
  }

  if (algo == 2 || isTRUE(doAssertion)) {
    ## this newer version is typically much faster than the older one above (Eliot June 2, 2019)
    cohortData2 <- copy(cohortData)
    new1 <- Sys.time()
    oldKey <- checkAndChangeKey(cohortData2, "pixelGroup")
    wh <- which(cohortData2$age >= successionTimestep)
    sumBtmp <- cohortData2[wh, list(N = .N, sumB = sum(B, na.rm = TRUE)), by = "pixelGroup"]
    if ("sumB" %in% names(cohortData2)) set(cohortData2, NULL, "sumB", NULL)
    # create empty column as there are some cases with wh is length 0
    if (length(wh) == 0)
      set(cohortData2, NULL, "sumB", NA_integer_)
    set(cohortData2, wh, "sumB", rep.int(sumBtmp[["sumB"]], sumBtmp[["N"]]))
    setorderv(cohortData2, c("sumB"), na.last = TRUE)
    a <- cohortData2[, list(sumB2 = sumB[1]), by = "pixelGroup"]
    setorderv(cohortData2, c("pixelGroup", "sumB"), na.last = TRUE)
    sumB <- a[cohortData2, on = "pixelGroup"][["sumB2"]]
    sumB[is.na(sumB)] <- 0L
    set(cohortData2, NULL, "sumB", sumB)
    if (!is.null(oldKey))
      setkeyv(cohortData2, oldKey)
    new2 <- Sys.time()
    if (!is.integer(cohortData2[["sumB"]]))
      set(cohortData2, NULL, "sumB", asInteger(cohortData2[["sumB"]]))
  }

  cohortData <- if (algo == 1) copy(cohortData1) else copy(cohortData2)

  if (isTRUE(doAssertion)) {
    if (!exists("oldAlgoSumB")) mod$oldAlgoSumB <- 0
    if (!exists("newAlgoSumB")) mod$newAlgoSumB <- 0
    mod$oldAlgoSumB <- mod$oldAlgoSumB + (old2 - old1)
    mod$newAlgoSumB <- mod$newAlgoSumB + (new2 - new1)

    print(paste("Biomass_core:calculateSumB: new algo", mod$newAlgoSumB))
    print(paste("Biomass_core:calculateSumB: old algo", mod$oldAlgoSumB))

    setkeyv(cohortData, c("pixelGroup", "speciesCode", "age"))
    setkeyv(cohortData1, c("pixelGroup", "speciesCode", "age"))
    setkeyv(cohortData2, c("pixelGroup", "speciesCode", "age"))
    setkeyv(newcohortData, c("pixelGroup", "speciesCode", "age"))
    setcolorder(newcohortData, names(cohortData))

    if (!identical(newcohortData, cohortData)) {
      stop("calculateSumB: new algorithm differs from old algorithm")
    }

    #if (!identical(cohortData1, cohortData2)) {
    #  stop("calculateSumB: cohortData1 not identical to cohortData2") ## TODO: re-enable check once fixed above
    #}
  }
  return(cohortData)
})

#' calculateAgeMortality
#'
#' TODO: description and title needed
#'
#' @param cohortData \code{data.table} TODO: description needed
#' @param stage TODO: description needed
#' @param spinupMortalityfraction TODO: description needed
#'
#' @return updated cohort \code{data.table}
#'
#' @export
#' @importFrom data.table setkey
calculateAgeMortality <- function(cohortData, stage = "nonSpinup", spinupMortalityfraction) {
  # for age-related mortality calculation
  if (stage == "spinup") {
    cohortData[age > 0, mAge := B*(exp((age) / longevity*mortalityshape) / exp(mortalityshape))]
    cohortData[age > 0, mAge := mAge + B*spinupMortalityfraction]
    cohortData[age > 0, mAge := pmin(B, mAge)]
  } else {
    set(cohortData, NULL, "mAge",
        cohortData$B * (exp((cohortData$age) / cohortData$longevity * cohortData$mortalityshape) /
                          exp(cohortData$mortalityshape)))
    set(cohortData, NULL, "mAge",
        pmin(cohortData$B,cohortData$mAge))
  }
  return(cohortData)
}

#' calculateANPP
#'
#' TODO: description and title needed
#'
#' @param cohortData \code{data.table} TODO: description needed
#' @param stage TODO: description needed
#'
#' @return updated cohort \code{data.table}
#'
#' @export
#' @importFrom data.table set
calculateANPP <- compiler::cmpfun(function(cohortData, stage = "nonSpinup") {
  if (stage == "spinup") {
    cohortData[age > 0, aNPPAct := maxANPP * exp(1) * (bAP^growthcurve) *
                 exp(-(bAP^growthcurve)) * bPM]
    cohortData[age > 0, aNPPAct := pmin(maxANPP * bPM, aNPPAct)]
  } else {
    aNPPAct <- cohortData$maxANPP * exp(1) * (cohortData$bAP^cohortData$growthcurve) *
      exp(-(cohortData$bAP^cohortData$growthcurve)) * cohortData$bPM
    set(cohortData, NULL, "aNPPAct",
        pmin(cohortData$maxANPP*cohortData$bPM, aNPPAct))
  }
  return(cohortData)
})

#' calculateGrowthMortality
#'
#' TODO: description and title needed
#'
#' @param cohortData \code{data.table} TODO: description needed
#' @param stage TODO: description needed
#'
#' @return updated cohort \code{data.table}
#'
#' @export
#' @importFrom data.table set
#' @importFrom fpCompare %>>% %<=%
calculateGrowthMortality <- compiler::cmpfun(function(cohortData, stage = "nonSpinup") {
  if (stage == "spinup") {
    cohortData[age > 0 & bAP %>>% 1.0, mBio := maxANPP*bPM]
    cohortData[age > 0 & bAP %<=% 1.0, mBio := maxANPP*(2*bAP) / (1 + bAP)*bPM]
    cohortData[age > 0, mBio := pmin(B, mBio)]
    cohortData[age > 0, mBio := pmin(maxANPP*bPM, mBio)]
  } else {
    cohortData[bAP %>>% 1.0, mBio := maxANPP*bPM]
    cohortData[bAP %<=% 1.0, mBio := maxANPP*(2*bAP)/(1 + bAP)*bPM]
    set(cohortData, NULL, "mBio",
        pmin(cohortData$B, cohortData$mBio))
    set(cohortData, NULL, "mBio",
        pmin(cohortData$maxANPP*cohortData$bPM, cohortData$mBio))
  }
  return(cohortData)
})

#' calculateCompetition
#'
#' TODO: description and title needed
#'
#' @param cohortData \code{data.table} TODO: description needed
#' @param stage TODO: description needed
#'
#' @return updated cohort \code{data.table}
#'
#' @export
#' @importFrom data.table key setkeyv
calculateCompetition <- compiler::cmpfun(function(cohortData, stage = "nonSpinup") {
  # two competition indics are calculated bAP and bPM
  if (stage == "spinup") {
    cohortData[age > 0, bPot := pmax(1, maxB - sumB + B)]
    cohortData[age > 0, bAP := B/bPot]
    set(cohortData, NULL, "bPot", NULL)
    cohortData[, cMultiplier := pmax(as.numeric(B^0.95), 1)]
    cohortData[age > 0, cMultTotal := sum(cMultiplier), by = pixelGroup]
    cohortData[age > 0, bPM := cMultiplier / cMultTotal]
    set(cohortData, NULL, c("cMultiplier", "cMultTotal"), NULL)
  } else {
    set(cohortData, NULL, "bPot", pmax(1, cohortData$maxB - cohortData$sumB + cohortData$B))  ## differs from manual, follows source code
    set(cohortData, NULL, "bAP", cohortData$B/cohortData$bPot)
    set(cohortData, NULL, "bPot", NULL)
    set(cohortData, NULL, "cMultiplier", pmax(as.numeric(cohortData$B^0.95), 1))

    # These 2 lines are 5x slower compared to replacement 6 lines below -- Eliot June 2, 2019
    if (FALSE) {
      cohortData[, cMultTotal := sum(cMultiplier), by = pixelGroup]
      set(cohortData, NULL, "bPM", cohortData$cMultiplier / cohortData$cMultTotal)
    }

    # Faster replacement
    oldKey <- checkAndChangeKey(cohortData, "pixelGroup")
    cMultTotalTmp <- cohortData[, list(N = .N, Sum = sum(cMultiplier)), by = pixelGroup]
    cMultTotal <- rep.int(cMultTotalTmp$Sum, cMultTotalTmp$N)
    set(cohortData, NULL, "bPM", cohortData$cMultiplier / cMultTotal)
    if (!is.null(oldKey))
      setkeyv(cohortData, oldKey)


    set(cohortData, NULL, c("cMultiplier"), NULL)
  }
  return(cohortData)
})

checkAndChangeKey <- function(obj, key) {
  oldKey <- key(obj)
  oldKeyWasFine <- !identical(oldKey, key)
  returnKey <- if (oldKeyWasFine) {
    setkeyv(obj, key)
    oldKey
  } else {
    NULL
  }
  returnKey
}

maxRowsDT <- function(maxLen, maxMem) {
  am <- suppressWarnings(availableMemory())
  if (!is.null(am)) {
    maxMemAdj <- min(as.numeric(availableMemory()) / 1e9, maxMem) ## memory (GB) avail.
    maxLenAdj <- try(as.integer(log(maxMemAdj + 2)^5 * 1e4), silent = TRUE)
    if (is.numeric(maxLenAdj))
      if (maxLenAdj > 1e5)
        maxLen <- maxLenAdj
  }
  return(maxLen)
}
