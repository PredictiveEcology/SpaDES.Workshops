defineModule(sim, list(
  name = "andisonDriver_dataPrep",
  description = "Ensures studyArea built correctly for andisonDriver module.",
  keywords = c("fire"),
  authors = c(
    person(c("Steven", "G"), "Cumming", email = "stevec@sbf.ulaval.ca", role = c("aut", "cre")),
    person(c("Ian", "M", "S"), "Eddy", email = "ianmseddy@gmail.com", role = c("aut")),
    person(c("Alex", "M"), "Chubaty", email = "alex.chubaty@gmail.com", role = c("ctb"))
  ),
  childModules = character(),
  version = numeric_version("0.1.0"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list(),
  documentation = list("README.txt", "andisonDriver_dataPrep.Rmd"),
  reqdPkgs = list("stats"),
  parameters = rbind(
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter("minFRI", "numeric", 40, NA, NA, desc = "minimum fire return interval to consider")
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "AndisonFRI", objectClass = "SpatialPolygonsDataFrame", desc = "Dave's FRI map",
                 sourceURL = "https://drive.google.com/file/d/1JptU0R7qsHOEAEkxybx5MGg650KC98c6/view?usp=sharing"),
    expectsInput(objectName = "studyArea0", objectClass = "SpatialPolygonsDataFrame",
                 desc = "study area template",
                 sourceURL = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/district/ecodistrict_shp.zip"),
    expectsInput(objectName = "rasterToMatch", objectClass = "rasterLayer",
                  desc = "template raster for raster GIS operations. Must be supplied by user")
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "studyArea", objectClass = "SpatialPolygonsDataFrame",
                  desc = "Updated study area with 'LTHFC' and 'PolyID' fields.")
  )
))

## event types
#   - type `init` is required for initilization

doEvent.andisonDriver_dataPrep <- function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,
    init = {
      sim <- Init(sim)
    },
    warning(paste("Undefined event type: '", events(sim)[1, "eventType", with = FALSE],
                  "' in module '", events(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

Init <- function(sim) {
<<<<<<< Updated upstream

=======
  #browser()
  ## do studyArea*AndisonFRI map intersection and add the PolyID field
  sim$studyArea <- Cache(crop, sim$AndisonFRI, y = sim$studyArea0)
>>>>>>> Stashed changes
  sim$studyArea$PolyID <- row.names(sim$studyArea)


  return(invisible(sim))
}

.inputObjects <- function(sim) {
  #browser()
  dPath <- dataPath(sim)
  cacheTags = c(currentModule(sim), "function:.inputObjects")

  if (!suppliedElsewhere("studyArea", sim)) {
    message("study area not supplied. Using Ecodistrict 348")

    #source shapefile from ecodistict in input folder. Use ecodistrict 348

    SA <- Cache(prepInputs,
                url = extractURL(objectName = "studyArea"),
                archive = "ecodistrict_shp.zip",
                filename2 = TRUE,
                userTags = c(cacheTags, "studyArea"),
                destinationPath = file.path(dPath, "ecodistricts_shp", "Ecodistricts"))

    SA <- SA[SA$ECODISTRIC == 348, ]
    sim$studyArea <- SA
  }

  if (!suppliedElsewhere("AndisonFRI", sim)) {
    AndisonFRI <- Cache(prepInputs,
                        url = extractURL(objectName = "AndisonFRI", sim),
                        destinationPath = dataPath(sim),
                        userTags = "AndisonFRI",
                        filename2 = TRUE)

    # check for duplicated long-term historic fire cycles
    b <- duplicated(AndisonFRI$LTHFC)

    if (any(b)) {
      sim$AndisonFRI <- Cache(raster::aggregate,
                          AndisonFRI[AndisonFRI$LTHFC > P(sim)$minFRI, ],
                          by = "LTHFC",
                          dissolve = TRUE)
    }
  }

  if (!suppliedElsewhere("rasterToMatch", sim)) {
    rasterToMatch <- pemisc::prepInputsLCC(year = 2005,
                                           destinationPath = dPath,
                                           studyArea = sim$studyArea,
                                           filename2 = TRUE,
                                           overwrite = TRUE,
                                           useSAcrs = TRUE)
    ###TODO: this shoudl realy be revisited. Basically this whole concept of AndisonFRI replacing studyArea in .inputObjects doesn't work
    # if we need to supply special files as studyArea, they need to be supplied by user, not defaults. Anything else introduces
    #the potential for crs mismatches that break down the line due to vector-raster functions requiring same crs
  }


  return(invisible(sim))
}
