defineModule(sim, list(
  name = "Biomass_borealDataPrep",
  description = "A data preparation module for parameterizing Biomass_core from open data sources, within the Boreal forest of Canada",
  keywords = c("LandWeb", "Biomass_core"),
  authors = c(
    person("Yong", "Luo", email = "yong.luo@canada.ca", role = c("aut")),
    person(c("Eliot", "J", "B"), "McIntire", email = "eliot.mcintire@canada.ca", role = c("aut", "cre")),
    person(c("Ceres"), "Barros", email = "cbarros@mail.ubc.ca", role = c("ctb")),
    person(c("Alex", "M."), "Chubaty", email = "achubaty@friresearch.ca", role = c("ctb"))
  ),
  childModules = character(0),
  version = list(SpaDES.core = "0.2.3.9009", Biomass_borealDataPrep = numeric_version("1.4.0.9000"),
                 LandR = "0.0.3.9000"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "Biomass_borealDataPrep.Rmd"),
  reqdPkgs = list("RCurl", "XML", "crayon", "data.table", "dplyr",
                  "fasterize", "plyr", "raster", "sp", "sf",
                  "achubaty/amc@development",
                  "PredictiveEcology/LandR@development",
                  "PredictiveEcology/pemisc@development",
                  "SpaDES.tools"),
  parameters = rbind(
    defineParameter("biomassModel", "call",
                    quote(lme4::lmer(B ~ logAge * speciesCode + cover * speciesCode +
                                       (logAge + cover + speciesCode | ecoregionGroup))),
                    NA, NA,
                    paste("Model and formula for estimating biomass (B) from ecoregionGroup",
                          "(currently ecoDistrict * LandCoverClass), speciesCode,",
                          "logAge (gives a downward curving relationship), and cover.",
                          "Defaults to a LMEM, which can be slow if dealing with very large datasets",
                          "(e.g. 36 000 points take 20min).",
                          "For faster fitting try P(sim)$subsetDataBiomassModel == TRUE, or",
                          "quote(RcppArmadillo::fastLm(formula = B ~ logAge * speciesCode * ecoregionGroup",
                          "+ cover * speciesCode * ecoregionGroup)).",
                          "A custom model call can also be provided, as long as the 'data' argument",
                          "is NOT included.")),
    defineParameter("coverModel", "call",
                    quote(lme4::glmer(cbind(coverPres, coverNum) ~ speciesCode + (1 | ecoregionGroup),
                                      family = binomial)),
                    NA, NA,
                    paste("Model and formula used for estimating cover from ecoregion and speciesCode",
                          "and potentially others. Defaults to a GLMEM if there are > 1 grouping levels.",
                          "A custom model call can also be provided, as long as the 'data' argument is NOT included")),
    defineParameter("forestedLCCClasses", "numeric", c(1:15, 20, 32, 34:35), 0, 39,
                    paste("The classes in the rstLCC layer that are 'treed' and will therefore be run in Biomass_core.",
                          "Defaults to forested classes in LCC2005 map.")),
    defineParameter("LCCClassesToReplaceNN", "numeric", 34:35, NA, NA,
                    paste("This will replace these classes on the landscape with the closest forest class P(sim)$forestedLCCClasses.",
                          "If the user is using the default 2005 data product for rstLCC, then users may wish to",
                          "include 36 (cities -- if running a historic range of variation project), and 34:35 (burns)",
                          "Since this is about estimating parameters for growth, it doesn't make any sense to have",
                          "unique estimates for transient classes in most cases")),
    defineParameter("omitNonTreedPixels", "logical", TRUE, FALSE, TRUE,
                    "Should this module use only treed pixels, as identified by P(sim)$forestedLCCClasses?"),
    defineParameter("pixelGroupAgeClass", "numeric", params(sim)$Biomass_borealDataPrep$successionTimestep, NA, NA,
                    "When assigning pixelGroup membership, this defines the resolution of ages that will be considered 'the same pixelGroup', e.g., if it is 10, then 6 and 14 will be the same"),
    defineParameter("pixelGroupBiomassClass", "numeric", 100, NA, NA,
                    "When assigning pixelGroup membership, this defines the resolution of biomass that will be considered 'the same pixelGroup', e.g., if it is 100, then 5160 and 5240 will be the same"),
    defineParameter("speciesUpdateFunction", "list",
                    list(quote(LandR::speciesTableUpdate(sim$species, sim$speciesTable, sim$sppEquiv, P(sim)$sppEquivCol))),
                    NA, NA,
                    paste("Unnamed list of quoted functions that updates species table to customize values.",
                          "Default should always come first.")),
    defineParameter("sppEquivCol", "character", "Boreal", NA, NA,
                    "The column in sim$specieEquivalency data.table to use as a naming convention"),
    defineParameter("subsetDataAgeModel", "numeric", NULL, NA, NA,
                    "the number of samples to use when subsampling the biomass data model; if TRUE, uses 50"),
    defineParameter("subsetDataBiomassModel", "numeric", NULL, NA, NA,
                    "the number of samples to use when subsampling the biomass data model; if TRUE, uses 50"),
    defineParameter("successionTimestep", "numeric", 10, NA, NA, "defines the simulation time step, default is 10 years"),
    defineParameter("useCloudCacheForStats", "logical", TRUE, NA, NA,
                    paste("Some of the statistical models take long (at least 30 minutes, likely longer).",
                          "If this is TRUE, then it will try to get previous cached runs from googledrive")),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA,
                    "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events"),
    defineParameter(".useCache", "character", c(".inputObjects", "init"), NA, NA,
                    desc = "Internal. Can be names of events or the whole module name; these will be cached by SpaDES")
  ),
  inputObjects = bind_rows(
    expectsInput("cloudFolderID", "character",
                 "The google drive location where cloudCache will store large statistical objects"),
    expectsInput("columnsForPixelGroups", "character",
                 "The names of the columns in cohortData that define unique pixelGroups. Default is c('ecoregionGroup', 'speciesCode', 'age', 'B') "),
    expectsInput("ecoDistrict", "SpatialPolygonsDataFrame",
                 desc = "ecodistricts in study area, default is Canada national ecodistricts",
                 sourceURL = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/district/ecodistrict_shp.zip"),
    expectsInput("rstLCC", "RasterLayer",
                 desc = paste("A land classification map in study area. It must be 'corrected', in the sense that:\n",
                              "1) Every class must not conflict with any other map in this module\n",
                              "    (e.g., speciesLayers should not have data in LCC classes that are non-treed);\n",
                              "2) It can have treed and non-treed classes. The non-treed will be removed within this\n",
                              "    module if P(sim)$omitNonTreedPixels is TRUE;\n",
                              "3) It can have transient pixels, such as 'young fire'. These will be converted to a\n",
                              "    the nearest non-transient class, probabilistically if there is more than 1 nearest\n",
                              "    neighbour class, based on P(sim)$LCCClassesToReplaceNN.\n",
                              "The default layer used, if not supplied, is Canada national land classification in 2005.",
                              " The metadata (res, proj, ext, origin) need to match rasterToMatchLarge."),
                 sourceURL = "https://drive.google.com/file/d/1g9jr0VrQxqxGjZ4ckF6ZkSMP-zuYzHQC/view?usp=sharing"),
    expectsInput("rasterToMatch", "RasterLayer",
                 desc = "a raster of the studyArea in the same resolution and projection as rawBiomassMap",
                 sourceURL = NA),
    expectsInput("rasterToMatchLarge", "RasterLayer",
                 desc = paste("A raster of the studyAreaLarge in the same resolution and projection as rawBiomassMap.",
                              "The metadata (res, proj, ext, origin) need to match rasterToMatchLarge."),
                 sourceURL = NA),
    expectsInput("rawBiomassMap", "RasterLayer",
                 desc = paste("total biomass raster layer in study area. Defaults to the Canadian Forestry",
                              "Service, National Forest Inventory, kNN-derived total aboveground biomass map",
                              "from 2001. See https://open.canada.ca/data/en/dataset/ec9e2659-1c29-4ddb-87a2-6aced147a990",
                              "for metadata"),
                 sourceURL = paste0("http://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/",
                                    "canada-forests-attributes_attributs-forests-canada/",
                                    "2001-attributes_attributs-2001/",
                                    "NFI_MODIS250m_2001_kNN_Structure_Biomass_TotalLiveAboveGround_v1.tif")),
    expectsInput("speciesLayers", "RasterStack",
                 desc = paste("cover percentage raster layers by species in Canada species map.",
                              "Defaults to the Canadian Forestry Service, National Forest Inventory,",
                              "kNN-derived species cover maps from 2001 using a cover threshold of 10 -",
                              "see https://open.canada.ca/data/en/dataset/ec9e2659-1c29-4ddb-87a2-6aced147a990 for metadata"),
                 sourceURL = paste0("http://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/",
                                    "canada-forests-attributes_attributs-forests-canada/2001-attributes_attributs-2001/")),
    expectsInput("speciesTable", "data.table",
                 desc = "species attributes table, default is from Dominic Cyr and Yan Boulanger's project",
                 sourceURL = "https://raw.githubusercontent.com/dcyr/LANDIS-II_IA_generalUseFiles/master/speciesTraits.csv"),
    expectsInput("sppColorVect", "character",
                 desc = "named character vector of hex colour codes corresponding to each species",
                 sourceURL = ""),
    expectsInput("sppEquiv", "data.table",
                 desc = "table of species equivalencies. See LandR::sppEquivalencies_CA.",
                 sourceURL = ""),
    expectsInput("sppNameVector", "character",
                 desc = "an optional vector of species names to be pulled from sppEquiv. If not provided, then species will be taken from the entire P(sim)$sppEquivCol in sppEquiv. See LandR::sppEquivalencies_CA.",
                 sourceURL = ""),
    expectsInput("standAgeMap", "RasterLayer",
                 desc =  paste("stand age map in study area.",
                               "Defaults to the Canadian Forestry Service, National Forest Inventory,",
                               "kNN-derived biomass map from 2001 -",
                               "see https://open.canada.ca/data/en/dataset/ec9e2659-1c29-4ddb-87a2-6aced147a990 for metadata"),
                 sourceURL = paste0("http://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/",
                                    "canada-forests-attributes_attributs-forests-canada/",
                                    "2001-attributes_attributs-2001/",
                                    "NFI_MODIS250m_2001_kNN_Structure_Stand_Age_v1.tif")),
    expectsInput("studyArea", "SpatialPolygonsDataFrame",
                 desc = paste("Polygon to use as the study area.",
                              "Defaults to  an area in Southwestern Alberta, Canada."),
                 sourceURL = ""),
    expectsInput("studyAreaLarge", "SpatialPolygonsDataFrame",
                 desc = paste("multipolygon (larger area than studyArea) used for parameter estimation,",
                              "with attribute LTHFC describing the fire return interval.",
                              "Defaults to a square shapefile in Southwestern Alberta, Canada."),
                 sourceURL = "")
  ),
  outputObjects = bind_rows(
    createsOutput("biomassMap", "RasterLayer",
                  desc = paste("total biomass raster layer in study area,",
                               "filtered for pixels covered by cohortData")),
    createsOutput("cohortData", "data.table",
                  desc = paste("initial community table, created from available biomass,",
                               "age and species cover data, as well as eco zonation information")),
    createsOutput("ecoDistrict", "", desc = ""), ## TODO: description and type needed
    createsOutput("ecoregion", "data.table",
                  desc = "ecoregion look up table"),
    createsOutput("ecoregionMap", "RasterLayer",
                  desc = "ecoregion map that has mapcodes match ecoregion table and speciesEcoregion table"),
    createsOutput("pixelGroupMap", "RasterLayer",
                  desc = "initial community map that has mapcodes match initial community table"),
    createsOutput("minRelativeB", "data.frame",
                  desc = "define the cut points to classify stand shadeness"),
    createsOutput("rawBiomassMap", "RasterLayer",
                  desc = paste("total biomass raster layer in study area. Defaults to the Canadian Forestry",
                               "Service, National Forest Inventory, kNN-derived total aboveground biomass map",
                               "from 2001. See https://open.canada.ca/data/en/dataset/ec9e2659-1c29-4ddb-87a2-6aced147a990",
                               "for metadata")),
    createsOutput("species", "data.table",
                  desc = "a table that has species traits such as longevity..."),
    createsOutput("speciesEcoregion", "data.table",
                  desc = "define the maxANPP, maxB and establishprob change with both ecoregion and simulation time"),
    createsOutput("studyArea", "",
                  desc = paste("Polygon to use as the study area.",
                               "Defaults to  an area in Southwestern Alberta, Canada.")),
    createsOutput("sufficientLight", "data.frame",
                  desc = "define how the species with different shade tolerance respond to stand shadeness")
    # createsOutput("speciesEstablishmentProbMap", "RasterStack",
    #               paste("Species establishment probability as a map, ",
    #                     "by species. This is written to disk to save RAM space")),
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.Biomass_borealDataPrep <- function(sim, eventTime, eventType, debug = FALSE) {
  if (eventType == "init") {
    sim <- createBiomass_coreInputs(sim)

    # schedule future event(s)
    sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "Biomass_borealDataPrep", "save")
  } else if (eventType == "save") {
    sim <- Save(sim)
  } else {
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  }
  return(invisible(sim))
}

createBiomass_coreInputs <- function(sim) {
  # # ! ----- EDIT BELOW ----- ! #
  if (is.null(P(sim)$pixelGroupAgeClass))
    params(sim)[[currentModule(sim)]]$pixelGroupAgeClass <- P(sim)$successionTimestep

  cacheTags <- c(currentModule(sim), "init")

  message(blue("Starting to createBiomass_coreInputs in Biomass_borealDataPrep: ", Sys.time()))
  if (is.null(sim$speciesLayers))
    stop(red(paste("'speciesLayers' are missing in Biomass_borealDataPrep init event.\n",
                   "This is likely due to the module producing 'speciesLayers' being scheduled after Biomass_borealDataPrep.\n",
                   "Please check module order.")))
  sim$ecoDistrict <- spTransform(sim$ecoDistrict, crs(sim$speciesLayers))

  sim$standAgeMap <- round(sim$standAgeMap / 20, 0) * 20 # use 20-year bins (#103)
  sim$standAgeMap[] <- asInteger(sim$standAgeMap[])

  ################################################################
  ## species traits inputs
  ################################################################
  message(blue("Prepare 'species' table, i.e., species level traits", Sys.time()))
  sim$species <- prepSpeciesTable(speciesTable = sim$speciesTable,
                                  speciesLayers = sim$speciesLayers,
                                  sppEquiv = sim$sppEquiv[get(P(sim)$sppEquivCol) %in%
                                                            names(sim$speciesLayers)],
                                  sppEquivCol = P(sim)$sppEquivCol)

  ### override species table values ##############################
  defaultQuote <- quote(LandR::speciesTableUpdate(sim$species, sim$speciesTable,
                                                  sim$sppEquiv, P(sim)$sppEquivCol))
  if (P(sim)$speciesUpdateFunction[[1]] != defaultQuote) {
    stop("Make sure that the first entry in speciesUpdateFunction is the default expression")
  }

  for (fn in P(sim)$speciesUpdateFunction) {
    if (is(fn, "call")) {
      sim$species <- eval(fn)
    } else {
      stop("speciesUpdateFunction should be a list of functions.")
    }
  }

  if (getOption("LandR.verbose") > 0) {
    message("Adjusting species-level traits, part 2, for LandWeb")
    print(sim$species)
  }

  ## check that all species have trait values.
  missingTraits <- setdiff(names(sim$speciesLayers), sim$species$species)
  if (length(missingTraits) == length(names(sim$speciesLayers))) {
    stop("No trait values where found for ", paste(missingTraits, collapse = ", "), ".\n",
         "Please check the species list and traits table")
  } else if (length(missingTraits))
    warning("No trait values where found for ", paste(missingTraits, collapse = ", "), ".\n",
            "Please check the species list and traits table")

  ### make table of light shade tolerance  #######################
  sim$sufficientLight <- data.frame(speciesshadetolerance = 1:5,
                                    X0 = 1,
                                    X1 = c(0.5, rep(1, 4)),
                                    X2 = c(0, 0.5, rep(1, 3)),
                                    X3 = c(rep(0, 2), 0.5, rep(1, 2)),
                                    X4 = c(rep(0, 3), 0.5, 1),
                                    X5 = c(rep(0, 4), 1))

  ################################################################
  ## initialEcoregionMap
  ################################################################
  if (!identical(crs(sim$studyArea), crs(sim$rasterToMatch))) {
    warning(paste0("studyArea and rasterToMatch projections differ.\n",
                   "studyArea will be projected to match rasterToMatch"))
    sim$studyArea <- spTransform(sim$studyArea, crs(sim$rasterToMatch))
    sim$studyArea <- fixErrors(sim$studyArea)
  }

  if (!identical(crs(sim$studyAreaLarge), crs(sim$rasterToMatchLarge))) {
    warning(paste0("studyAreaLarge and rasterToMatchLarge projections differ.\n",
                   "studyAreaLarge will be projected to match rasterToMatchLarge"))
    sim$studyAreaLarge <- spTransform(sim$studyAreaLarge, crs(sim$rasterToMatchLarge))
    sim$studyAreaLarge <- fixErrors(sim$studyAreaLarge)
  }

  sim$ecoDistrict <- fixErrors(sim$ecoDistrict)

  ecoregionMap <- Cache(postProcess,
                        x = sim$ecoDistrict,
                        studyArea = sim$studyAreaLarge,
                        filename2 = NULL,
                        userTags = c(cacheTags, "ecoregionMapLarge"),
                        omitArgs = c("userTags"))

  ecoregionMapSF <- sf::st_as_sf(ecoregionMap)
  if (is(ecoregionMapSF$ECODISTRIC, "character"))
    ecoregionMapSF$ECODISTRIC <- as.numeric(ecoregionMapSF$ECODISTRIC)
  rstEcoregionMap <- fasterize::fasterize(ecoregionMapSF, raster = sim$rasterToMatchLarge,
                                          field = "ECODISTRIC")
  ecoregionstatus <- data.table(active = "yes", ecoregion = 1:1031)
  rstLCCAdj <- sim$rstLCC

  ## Clean pixels for veg. succession model
  ## remove pixes with no spp data
  pixelsToRm <- is.na(sim$speciesLayers[[1]][])

  ## remove non-forested if asked by user
  if (P(sim)$omitNonTreedPixels) {
    if (is.null(P(sim)$forestedLCCClasses))
      stop("No P(sim)$forestedLCCClasses provided, but P(sim)$omitNonTreedPixels is TRUE.
           \nPlease provide a vector of forested classes in P(sim)$forestedLCCClasses")

    lccPixelsRemoveTF <- !(sim$rstLCC[] %in% P(sim)$forestedLCCClasses)
    pixelsToRm <- lccPixelsRemoveTF | pixelsToRm
  }

  rstLCCAdj[pixelsToRm] <- NA
  rstEcoregionMap[pixelsToRm] <- NA

  ## TODO: clean up - not the most effient function (maybe contains redundancies). Producing a non-used object
  message(blue("Make initial ecoregionGroups ", Sys.time()))
  assertthat::assert_that(isTRUE(compareRaster(rstEcoregionMap, rstLCCAdj,
                                               res = TRUE, orig = TRUE, stopiffalse = FALSE)))
  ecoregionFiles <- Cache(ecoregionProducer,
                          ecoregionMaps = list(rstEcoregionMap, rstLCCAdj),
                          ecoregionName = "ECODISTRIC",
                          ecoregionActiveStatus = ecoregionstatus,
                          rasterToMatch = sim$rasterToMatchLarge,
                          userTags = c(cacheTags, "ecoregionFiles", "stable"),
                          omitArgs = c("userTags"))

  ################################################################
  ## put together pixelTable object
  ################################################################
  #  Round age to pixelGroupAgeClass
  pixelTable <- Cache(makePixelTable,
                      speciesLayers = sim$speciesLayers,
                      species = sim$species,
                      standAgeMap = sim$standAgeMap,
                      ecoregionFiles = ecoregionFiles,
                      biomassMap = sim$rawBiomassMap,
                      rasterToMatch = sim$rasterToMatchLarge,
                      rstLCC = rstLCCAdj,
                      pixelGroupAgeClass = P(sim)$pixelGroupAgeClass,
                      userTags = c(cacheTags, "pixelTable"),
                      omitArgs = c("userTags"))

  #######################################################
  # Make the initial pixelCohortData table
  #######################################################
  coverColNames <- paste0("cover.", sim$species$species)
  pixelCohortData <- Cache(makeAndCleanInitialCohortData, pixelTable,
                           sppColumns = coverColNames,
                           pixelGroupBiomassClass = P(sim)$pixelGroupBiomassClass,
                           doSubset = P(sim)$subsetDataAgeModel,
                           userTags = c(cacheTags, "pixelCohortData"),
                           omitArgs = c("userTags"))

  #######################################################
  # replace 34 and 35 and 36 values -- burns and cities -- to a neighbour class *that exists*
  #######################################################
  uwc <- P(sim)$LCCClassesToReplaceNN

  message("Replace ", paste(uwc, collapse = ", "),
          " values -- ", "burns"[any(uwc %in% 34:35)], " and cities"[any(uwc %in% 36)],
          " -- to a neighbour class *that exists*")

  rmZeroBiomassQuote <- quote(B > 0)
  # availableCombinations <- unique(pixelCohortData[eval(rmZeroBiomassQuote),
  #                                                 .(speciesCode, initialEcoregionCode, pixelIndex)])
  availableCombinations <- unique(pixelCohortData[, .(speciesCode, initialEcoregionCode, pixelIndex)])
  newLCCClasses <- Cache(convertUnwantedLCC,
                         classesToReplace = P(sim)$LCCClassesToReplaceNN,
                         rstLCC = rstLCCAdj,
                         availableERC_by_Sp = availableCombinations,
                         userTags = c(cacheTags, "newLCCClasses", "stable"),
                         omitArgs = c("userTags"))

  ## split pixelCohortData into 2 parts -- one with the former 34:36 pixels, one without
  #    The one without 34:36 can be used for statistical estimation, but not the one with
  cohortData34to36 <- pixelCohortData[pixelIndex %in% newLCCClasses$pixelIndex]
  cohortData34to36 <- merge(newLCCClasses, cohortData34to36, all.x = TRUE,
                            all.y = FALSE, by = "pixelIndex")
  cohortDataNo34to36 <- pixelCohortData[!pixelIndex %in% newLCCClasses$pixelIndex]
  setnames(cohortDataNo34to36, "initialEcoregionCode", "ecoregionGroup")
  cohortDataNo34to36NoBiomass <- cohortDataNo34to36[eval(rmZeroBiomassQuote),
                                                    .(B, logAge, speciesCode, ecoregionGroup, lcc, cover)]

  ## make sure ecoregionGroups match
  assert1(cohortData34to36, pixelCohortData, rmZeroBiomassQuote)

  ##############################################################
  # Statistical estimation of establishprob, maxB and maxANPP
  ##############################################################
  cohortDataShort <- cohortDataNo34to36[, list(coverNum = .N,
                                               coverPres = sum(cover > 0)),
                                        by = c("ecoregionGroup", "speciesCode")]
  cohortDataShortNoCover <- cohortDataShort[coverPres == 0] #
  cohortDataShort <- cohortDataShort[coverPres > 0] # remove places where there is 0 cover
  # will be added back as establishprob = 0
  message(blue("Estimating Species Establishment Probability using P(sim)$coverModel, which is\n",
               magenta(paste0(format(P(sim)$coverModel, appendLF = FALSE), collapse = ""))))

  # for backwards compatibility -- change from parameter to object
  if (is.null(sim$cloudFolderID))
    if (!is.null(P(sim)$cloudFolderID))
      sim$cloudFolderID <- P(sim)$cloudFolderID

  useCloud <- if (!is.null(sim$cloudFolderID)) {
    (getOption("reproducible.useCache", FALSE) && P(sim)$useCloudCacheForStats)
  } else {
    FALSE
  }

  modelCover <- Cache(statsModel,
                      modelFn = P(sim)$coverModel,
                      uniqueEcoregionGroup = .sortDotsUnderscoreFirst(unique(cohortDataShort$ecoregionGroup)),
                      sumResponse = sum(cohortDataShort$coverPres, cohortDataShort$coverNum, na.rm = TRUE),
                      .specialData = cohortDataShort,
                      useCloud = useCloud,
                      cloudFolderID = sim$cloudFolderID,
                      showSimilar = getOption("reproducible.showSimilar", FALSE),
                      userTags = c(cacheTags, "modelCover"),
                      omitArgs = c("userTags", "showSimilar", ".specialData", "useCloud", "cloudFolderID"))
  message(blue("  The rsquared is: "))
  print(modelCover$rsq)

  ## For biomass
  ### Subsample cases where there are more than 50 points in an ecoregionGroup * speciesCode
  cohortDataNo34to36NoBiomass <- subsetDT(cohortDataNo34to36NoBiomass,
                                          by = c("ecoregionGroup", "speciesCode"),
                                          doSubset = P(sim)$subsetDataBiomassModel)

  ### For Cache -- doesn't need to cache all columns in the data.table -- only the ones in the model
  ### force parameter values to avoid more checks
  message(blue("Estimating biomass using P(sim)$biomassModel as:\n"),
          magenta(paste0(format(P(sim)$biomassModel, appendLF = FALSE), collapse = "")))
  modelBiomass <- Cache(statsModel,
                        modelFn = P(sim)$biomassModel,
                        uniqueEcoregionGroup = .sortDotsUnderscoreFirst(unique(cohortDataNo34to36NoBiomass$ecoregionGroup)),
                        sumResponse = sum(cohortDataShort$B, na.rm = TRUE),
                        .specialData = cohortDataNo34to36NoBiomass,
                        useCloud = useCloud,
                        cloudFolderID = sim$cloudFolderID,
                        showSimilar = getOption("reproducible.showSimilar", FALSE),
                        userTags = c(cacheTags, "modelBiomass"),
                        omitArgs = c("userTags", "showSimilar", ".specialData", "useCloud", "cloudFolderID"))

  message(blue("  The rsquared is: "))
  print(modelBiomass$rsq)

  ########################################################################
  # create speciesEcoregion -- a single line for each combination of ecoregionGroup & speciesCode
  #   doesn't include combinations with B = 0 because those places can't have the species/ecoregion combo
  ########################################################################
  message(blue("Create speciesEcoregion using modelCover and modelBiomass to estimate species traits"))
  speciesEcoregion <- makeSpeciesEcoregion(cohortDataNoBiomass = cohortDataNo34to36NoBiomass,
                                           cohortDataShort = cohortDataShort,
                                           cohortDataShortNoCover = cohortDataShortNoCover,
                                           species = sim$species,
                                           modelCover = modelCover,
                                           modelBiomass = modelBiomass,
                                           successionTimestep = P(sim)$successionTimestep,
                                           currentYear = time(sim))

  #######################################
  if (!is.na(P(sim)$.plotInitialTime)) {
    uniqueSpeciesNames <- as.character(unique(speciesEcoregion$speciesCode))
    names(uniqueSpeciesNames) <- uniqueSpeciesNames
    speciesEcoregionTable2 <- copy(speciesEcoregion)
    speciesEcoregionTable2[, ecoregionInt := as.integer(ecoregionGroup)]
    maxB <- stack(lapply(uniqueSpeciesNames, function(sp) {
      rasterizeReduced(speciesEcoregionTable2[speciesCode == sp], ecoregionFiles$ecoregionMap,
                       "maxB", "ecoregionInt")
    }))
    curDev <- dev.cur()
    quickPlot::dev(6, width = 18, height = 10)
    Plot(maxB, legendRange = c(0, max(maxValue(maxB))))
    quickPlot::dev(curDev)
  }

  if (ncell(sim$rasterToMatchLarge) > 3e6) .gc()

  ########################################################################
  # Create initial communities, i.e., pixelGroups
  ########################################################################
  # Rejoin back the pixels that were 34 and 35
  pixelCohortData <- rbindlist(list(cohortData34to36, cohortDataNo34to36),
                               use.names = TRUE, fill = TRUE)

  ########################################################################
  # "Downsize" to studyArea after estimating parameters on studyAreaLarge
  ########################################################################
  ## 1. Subset pixels (IDs) on rasterToMatchLarge, using rasterToMatch
  ## 2. Subset data.tables using the pixel IDs / ecoregion/species combinations
  ##    that are common across the two rasters
  ## 3. Re-do pixel ID numbering so that it matches the final rasterToMatch
  ## Note: if SA and SALarge are the same, no subsetting will take place.

  if (!identical(extent(sim$rasterToMatch), extent(sim$rasterToMatchLarge))) {
    message(blue("Subsetting to studyArea"))
    rasterToMatchLarge <- sim$rasterToMatchLarge
    rasterToMatchLarge <- setValues(rasterToMatchLarge, seq(ncell(rasterToMatchLarge)))
    rasterToMatchLarge <- Cache(postProcess,
                                x = rasterToMatchLarge,
                                rasterToMatch = sim$rasterToMatch,
                                maskWithRTM = TRUE,
                                filename2 = NULL,
                                userTags = c(cacheTags, "rasterToMatchLarge"),
                                omitArgs = c("userTags"))

    if (!compareRaster(rasterToMatchLarge, sim$rasterToMatch,
                       orig = TRUE, res = TRUE,
                       stopiffalse = FALSE))
      stop("Downsizing to rasterToMatch after estimating parameters didn't work.
           Please debug Biomass_borealDataPrep::createBiomass_coreInputs()")

    ## subset pixels that are in studyArea/rasterToMatch only
    pixToKeep <- na.omit(getValues(rasterToMatchLarge))
    pixelCohortData <- pixelCohortData[pixelIndex %in% pixToKeep]

    # re-do pixelIndex (it now needs to match rasterToMatch)
    newPixelIndexDT <- data.table(pixelIndex = getValues(rasterToMatchLarge),
                                  newPixelIndex = as.integer(1:ncell(rasterToMatchLarge)))

    pixelCohortData <- newPixelIndexDT[pixelCohortData, on = "pixelIndex"]
    pixelCohortData[, pixelIndex := NULL]
    setnames(pixelCohortData, old = "newPixelIndex", new = "pixelIndex")
    rm(rasterToMatchLarge)
  }

  if (ncell(sim$rasterToMatch) > 3e6) .gc()

  ## subset ecoregionFiles$ecoregionMap to smaller area.
  ecoregionFiles$ecoregionMap <- Cache(postProcess,
                                       x = ecoregionFiles$ecoregionMap,
                                       rasterToMatch = sim$rasterToMatch,
                                       maskWithRTM = TRUE,
                                       filename2 = NULL,
                                       userTags = c(cacheTags, "ecoregionMap"),
                                       omitArgs = c("userTags"))

  ## make cohortDataFiles: pixelCohortData (rm unnecessary cols, subset pixels with B>0,
  ## generate pixelGroups, add ecoregionGroup and totalBiomass) and cohortData
  cohortDataFiles <- makeCohortDataFiles(pixelCohortData, columnsForPixelGroups, speciesEcoregion)
  sim$cohortData <- cohortDataFiles$cohortData
  pixelCohortData <- cohortDataFiles$pixelCohortData
  rm(cohortDataFiles)

  ## make a table of available active and inactive (no biomass) ecoregions
  sim$ecoregion <- makeEcoregionDT(pixelCohortData, speciesEcoregion)

  ## make biomassMap, ecoregionMap, minRelativeB, pixelGroupMap
  sim$biomassMap <- makeBiomassMap(pixelCohortData, sim$rasterToMatch)
  sim$ecoregionMap <- makeEcoregionMap(ecoregionFiles, pixelCohortData)
  sim$minRelativeB <- makeMinRelativeB(pixelCohortData)
  sim$pixelGroupMap <- makePixelGroupMap(pixelCohortData, sim$rasterToMatch)

  ## rm ecoregions that may not be present in rasterToMatch
  ## make ecoregionGroup a factor and export speciesEcoregion to sim
  speciesEcoregion <- speciesEcoregion[ecoregionGroup %in% pixelCohortData$ecoregionGroup]
  speciesEcoregion[, ecoregionGroup := factor(as.character(ecoregionGroup))]
  sim$speciesEcoregion <- speciesEcoregion

  ## write species layers to disk
  sim$speciesLayers <- lapply(seq(numLayers(sim$speciesLayers)), function(x) {
    writeRaster(sim$speciesLayers[[x]],
                file.path(outputPath(sim), paste0(names(sim$speciesLayers)[x], ".tif")),
                datatype = "INT2U", overwrite = TRUE)
  }) %>% raster::stack()

  ## do assertions
  message(blue("Create pixelGroups based on: ", paste(columnsForPixelGroups, collapse = ", "),
               "\n  Resulted in", magenta(length(unique(sim$cohortData$pixelGroup))),
               "unique pixelGroup values"))
  LandR::assertERGs(sim$ecoregionMap, cohortData = sim$cohortData,
                    speciesEcoregion = sim$speciesEcoregion,
                    minRelativeB = sim$minRelativeB)

  LandR::assertCohortData(sim$cohortData, sim$pixelGroupMap)

  message("Done Biomass_borealDataPrep: ", Sys.time())
  return(invisible(sim))
}

Save <- function(sim) {
  sim <- saveFiles(sim)
  return(invisible(sim))
}

## see other helper functions in R/ subdirectory

.inputObjects <- function(sim) {
  cacheTags <- c(currentModule(sim), "function:.inputObjects")
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # 1. test if all input objects are already present (e.g., from inputs, objects or another module)
  a <- depends(sim)
  whThisMod <- which(unlist(lapply(a@dependencies, function(x) x@name)) == "Biomass_borealDataPrep")
  objNames <- a@dependencies[[whThisMod]]@inputObjects$objectName
  objExists <- !unlist(lapply(objNames, function(x) is.null(sim[[x]])))
  names(objExists) <- objNames

  # Filenames
  ecoregionFilename <-   file.path(dPath, "ecoregions.shp")
  ecodistrictFilename <- file.path(dPath, "ecodistricts.shp")
  ecozoneFilename <-   file.path(dPath, "ecozones.shp")
  lcc2005Filename <- file.path(dPath, "LCC2005_V1_4a.tif")

  # Also extract
  fexts <- c("dbf", "prj", "sbn", "sbx", "shx")
  ecoregionAE <- basename(paste0(tools::file_path_sans_ext(ecoregionFilename), ".", fexts))
  ecodistrictAE <- basename(paste0(tools::file_path_sans_ext(ecodistrictFilename), ".", fexts))
  ecozoneAE <- basename(paste0(tools::file_path_sans_ext(ecozoneFilename), ".", fexts))

  ## Study area(s) ------------------------------------------------
  if (!suppliedElsewhere("studyArea", sim)) {
    message("'studyArea' was not provided by user. Using a polygon (6250000 m^2) in southwestern Alberta, Canada")
    sim$studyArea <- randomStudyArea(seed = 1234, size = (250^2)*100)
  }

  if (!suppliedElsewhere("studyAreaLarge", sim)) {
    message("'studyAreaLarge' was not provided by user. Using the same as 'studyArea'")
    sim <- objectSynonyms(sim, list(c("studyAreaLarge", "studyArea")))
  }

  if (!identical(crs(sim$studyArea), crs(sim$studyAreaLarge))) {
    warning("studyArea and studyAreaLarge have different projections.\n
            studyAreaLarge will be projected to match crs(studyArea)")
    sim$studyAreaLarge <- spTransform(sim$studyAreaLarge, crs(sim$studyArea))
  }

  ## check whether SA is within SALarge
  ## convert to temp sf objects
  studyArea <- st_as_sf(sim$studyArea)
  studyAreaLarge <- st_as_sf(sim$studyAreaLarge)

  #this is necessary if studyArea and studyAreaLarge are multipolygon objects
  if (nrow(studyArea) > 1) {
    studyArea <- st_union(studyArea) %>%
      st_as_sf(.)
  }

  if (nrow(studyAreaLarge) > 1) {
    studyAreaLarge <- st_union(studyArea) %>%
      st_as_sf(.)
  }

  if (length(st_within(studyArea, studyAreaLarge))[[1]] == 0)
    stop("studyArea is not fully within studyAreaLarge.
           Please check the aligment, projection and shapes of these polygons")
  rm(studyArea, studyAreaLarge)

  ## Raster(s) to match ------------------------------------------------
  needRTM <- FALSE
  if (is.null(sim$rasterToMatch) || is.null(sim$rasterToMatchLarge)) {
    if (!suppliedElsewhere("rasterToMatch", sim) ||
        !suppliedElsewhere("rasterToMatchLarge", sim)) {      ## if one is not provided, re do both (safer?)
      needRTM <- TRUE
      message("There is no rasterToMatch/rasterToMatchLarge supplied; will attempt to use rawBiomassMap")
    } else {
      stop("rasterToMatch/rasterToMatchLarge is going to be supplied, but ", currentModule(sim), " requires it ",
           "as part of its .inputObjects. Please make it accessible to ", currentModule(sim),
           " in the .inputObjects by passing it in as an object in simInit(objects = list(rasterToMatch = aRaster)",
           " or in a module that gets loaded prior to ", currentModule(sim))
    }
  }

  if (!suppliedElsewhere("rawBiomassMap", sim) || needRTM) {
    sim$rawBiomassMap <- Cache(prepInputs,
                               url = extractURL("rawBiomassMap"),
                               destinationPath = dPath,
                               studyArea = sim$studyAreaLarge,   ## Ceres: makePixel table needs same no. pixels for this, RTM rawBiomassMap, LCC.. etc
                               rasterToMatch = if (!needRTM) sim$rasterToMatchLarge else NULL,
                               maskWithRTM = if (!needRTM) TRUE else FALSE,
                               useSAcrs = FALSE,     ## never use SA CRS
                               method = "bilinear",
                               datatype = "INT2U",
                               filename2 = TRUE, overwrite = TRUE,
                               userTags = c(cacheTags, "rawBiomassMap"),
                               omitArgs = c("destinationPath", "targetFile", "userTags", "stable"))
  }
  if (needRTM) {
    ## if we need rasterToMatch/rasterToMatchLarge, that means a) we don't have it, but b) we will have rawBiomassMap
    ## even if one of the rasterToMatch is present re-do both.

    if (is.null(sim$rasterToMatch) != is.null(sim$rasterToMatchLarge))
      warning(paste0("One of rasterToMatch/rasterToMatchLarge is missing. Both will be created \n",
                     "from rawBiomassMap and studyArea/studyAreaLarge.\n
              If this is wrong, provide both rasters"))

    sim$rasterToMatchLarge <- sim$rawBiomassMap
    RTMvals <- getValues(sim$rasterToMatchLarge)
    sim$rasterToMatchLarge[!is.na(RTMvals)] <- 1

    sim$rasterToMatchLarge <- Cache(writeOutputs, sim$rasterToMatchLarge,
                                    filename2 = file.path(cachePath(sim), "rasters", "rasterToMatchLarge.tif"),
                                    datatype = "INT2U", overwrite = TRUE,
                                    userTags = c(cacheTags, "rasterToMatchLarge"),
                                    omitArgs = c("userTags"))

    sim$rasterToMatch <- Cache(postProcess,
                               x = sim$rawBiomassMap,
                               studyArea = sim$studyArea,
                               rasterToMatch = sim$rasterToMatchLarge,
                               useSAcrs = FALSE,
                               maskWithRTM = FALSE,   ## mask with SA
                               method = "bilinear",
                               datatype = "INT2U",
                               filename2 = file.path(cachePath(sim), "rasterToMatch.tif"),
                               overwrite = TRUE,
                               userTags = c(cacheTags, "rasterToMatch"),
                               omitArgs = c("destinationPath", "targetFile", "userTags", "stable"))

    ## covert to 'mask'
    RTMvals <- getValues(sim$rasterToMatch)
    sim$rasterToMatch[!is.na(RTMvals)] <- 1
  }

  # if (ncell(sim$rasterToMatch) < 1e4)
  # stop("sim$rasterToMatch is too small, it should have more than 10,000 pixels")

  ## TODO: KEEP THIS HERE OR ONLY INIT?
  if (!identical(crs(sim$studyArea), crs(sim$rasterToMatch))) {
    warning(paste0("studyArea and rasterToMatch projections differ.\n",
                   "studyArea will be projected to match rasterToMatch"))
    sim$studyArea <- spTransform(sim$studyArea, crs(sim$rasterToMatch))
    sim$studyArea <- fixErrors(sim$studyArea)
  }

  if (!identical(crs(sim$studyAreaLarge), crs(sim$rasterToMatchLarge))) {
    warning(paste0("studyAreaLarge and rasterToMatchLarge projections differ.\n",
                   "studyAreaLarge will be projected to match rasterToMatchLarge"))
    sim$studyAreaLarge <- spTransform(sim$studyAreaLarge, crs(sim$rasterToMatchLarge))
    sim$studyAreaLarge <- fixErrors(sim$studyAreaLarge)
  }

  ## Land cover raster ------------------------------------------------
  if (!suppliedElsewhere("rstLCC", sim)) {
    sim$rstLCC <- Cache(prepInputs,
                        targetFile = lcc2005Filename,
                        archive = asPath("LandCoverOfCanada2005_V1_4.zip"),
                        url = extractURL("rstLCC"),
                        destinationPath = dPath,
                        studyArea = sim$studyAreaLarge,   ## Ceres: makePixel table needs same no. pixels for this, RTM rawBiomassMap, LCC.. etc
                        # studyArea = sim$studyArea,
                        rasterToMatch = sim$rasterToMatchLarge,
                        # rasterToMatch = sim$rasterToMatch,
                        maskWithRTM = TRUE,
                        method = "bilinear",
                        datatype = "INT2U",
                        filename2 = TRUE, overwrite = TRUE,
                        userTags = c("prepInputsrstLCC_rtm", currentModule(sim)), # use at least 1 unique userTag
                        omitArgs = c("destinationPath", "targetFile", "userTags"))

    if (!identical(projection(sim$rstLCC),
                   projection(sim$rasterToMatchLarge)))
      projection(sim$rstLCC) <- projection(sim$rasterToMatchLarge) ## Ceres: this shouldn't be necessary anymore
  }

  ## Ecodistrict ------------------------------------------------
  if (!suppliedElsewhere("ecoDistrict", sim)) {
    sim$ecoDistrict <- Cache(prepInputs,
                             targetFile = asPath(ecodistrictFilename),
                             archive = asPath("ecodistrict_shp.zip"),
                             url = extractURL("ecoDistrict"),
                             alsoExtract = ecodistrictAE,
                             destinationPath = dPath,
                             studyArea = sim$studyAreaLarge,   ## Ceres: makePixel table needs same no. pixels for this, RTM rawBiomassMap, LCC.. etc
                             overwrite = TRUE,
                             useSAcrs = TRUE, # this is required to make ecoZone be in CRS of studyArea
                             fun = "raster::shapefile",
                             #filename2 = TRUE,
                             userTags = c("prepInputsEcoDistrict_SA", currentModule(sim), cacheTags), # use at least 1 unique userTag
                             omitArgs = c("destinationPath", "targetFile", "overwrite", "alsoExtract", "userTags"))
  }

  ## Stand age map ------------------------------------------------
  if (!suppliedElsewhere("standAgeMap", sim)) {
    
    sim$standAgeMap <- Cache(prepInputs,
                             destinationPath = dPath,
                             url = extractURL("standAgeMap"),
                             fun = "raster::raster",
                             studyArea = sim$studyAreaLarge,   ## Ceres: makePixel table needs same no. pixels for this, RTM rawBiomassMap, LCC.. etc
                             rasterToMatch = sim$rasterToMatchLarge,
                             # rasterToMatch = sim$rasterToMatch,
                             maskWithRTM = TRUE,
                             method = "bilinear",
                             datatype = "INT2U",
                             filename2 = NULL,
                             overwrite = TRUE,
                             userTags = c("prepInputsStandAge_rtm", currentModule(sim), cacheTags), # use at least 1 unique userTag
                             omitArgs = c("destinationPath", "targetFile", "overwrite", "alsoExtract", "userTags"))
    sim$standAgeMap[] <- asInteger(sim$standAgeMap[])
  }

  ## Species equivalencies table -------------------------------------------
  
  if (!suppliedElsewhere("sppEquiv", sim)) {
    if (!is.null(sim$sppColorVect))
      message("No 'sppColorVect' provided; using default colour palette: Accent")
    
    data("sppEquivalencies_CA", package = "LandR", envir = environment())
    sim$sppEquiv <- as.data.table(sppEquivalencies_CA)
    ## By default, Abies_las is renamed to Abies_sp
    sim$sppEquiv[KNN == "Abie_Las", LandR := "Abie_sp"]

    ## check spp column to use
    if (P(sim)$sppEquivCol == "Boreal") {
      message(paste("There is no 'sppEquiv' table supplied;",
                    "will attempt to use species listed under 'Boreal'",
                    "in the 'LandR::sppEquivalencies_CA' table"))
    } else {
      if (grepl(P(sim)$sppEquivCol, names(sim$sppEquiv))) {
        message(paste("There is no 'sppEquiv' table supplied,",
                      "will attempt to use species listed under", P(sim)$sppEquivCol,
                      "in the 'LandR::sppEquivalencies_CA' table"))
      } else {
        stop("You changed 'sppEquivCol' without providing 'sppEquiv',",
             "and the column name can't be found in the default table ('LandR::sppEquivalencies_CA').",
             "Please provide conforming 'sppEquivCol', 'sppEquiv' and 'sppColorVect'")
      }
    }

    ## remove empty lines/NAs
    sim$sppEquiv <- sim$sppEquiv[!"", on = P(sim)$sppEquivCol]
    sim$sppEquiv <- na.omit(sim$sppEquiv, P(sim)$sppEquivCol)

    ## add default colors for species used in model
    sim$sppColorVect <- sppColors(sim$sppEquiv, P(sim)$sppEquivCol,
                                  newVals = "Mixed", palette = "Accent")
  } else {
    if (is.null(sim$sppColorVect)) {
      ## add default colors for species used in model
      sim$sppColorVect <- sppColors(sim$sppEquiv, P(sim)$sppEquivCol,
                                    newVals = "Mixed", palette = "Accent")
      message("No 'sppColorVect' provided; using default colour palette: Accent")
    }
  }

  ## Species raster layers -------------------------------------------
  if (!suppliedElsewhere("speciesLayers", sim)) {
    #opts <- options(reproducible.useCache = "overwrite")
    sim$speciesLayers <- Cache(loadkNNSpeciesLayers,
                               dPath = dPath,
                               rasterToMatch = sim$rasterToMatchLarge,
                               # rasterToMatch = sim$rasterToMatch,
                               studyArea = sim$studyAreaLarge,   ## Ceres: makePixel table needs same no. pixels for this, RTM rawBiomassMap, LCC.. etc
                               sppEquiv = sim$sppEquiv,
                               knnNamesCol = "KNN",
                               sppNameVector = sim$sppNameVector,
                               sppEquivCol = P(sim)$sppEquivCol,
                               thresh = 10,
                               url = extractURL("speciesLayers"),
                               userTags = c(cacheTags, "speciesLayers"),
                               omitArgs = c("userTags"))
  }

  # 3. species maps
  if (!suppliedElsewhere("speciesTable", sim)) {
    sim$speciesTable <- getSpeciesTable(dPath = dPath,
                                        cacheTags = c(cacheTags, "speciesTable"))
  }

  if (!suppliedElsewhere("columnsForPixelGroups", sim)) {
    sim$columnsForPixelGroups <- LandR::columnsForPixelGroups
  }

  return(invisible(sim))
}
