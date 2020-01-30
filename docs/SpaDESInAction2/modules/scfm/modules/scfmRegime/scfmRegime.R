defineModule(sim, list(
  name = "scfmRegime",
  description = "estimates fire regime parameters for BEACONs a la Steve's method",
  keywords = c("fire regime", "BEACONs"),
  authors = c(person("Steve", "Cumming", email = "stevec@sbf.ulaval.ca", role = c("aut")),
              person("Ian", "Eddy", email = "ian.eddy@canada.ca", role = c("aut"))),
  childModules = character(),
  version = numeric_version("0.1.0"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list(),
  documentation = list("README.txt", "scfmRegime.Rmd"),
  reqdPkgs = list("raster", "reproducible", "rgdal", "sp"),
  parameters = rbind(
    defineParameter("empiricalMaxSizeFactor", "numeric", 1.2, 1, 10, "scale xMax by this is HD estimator fails "),
    defineParameter("fireCause", "character", c("L"), NA_character_, NA_character_, "subset of c(H,H-PB,L,Re,U)"),
    defineParameter("fireEpoch", "numeric", c(1971,2000), NA, NA, "start of normal period"),
    defineParameter(".useCache", "character", c(".inputObjects", "init"), NA, NA,
                    desc = "Internal. Can be names of events or the whole module name; these will be cached by SpaDES")
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "firePoints", objectClass = "SpatialPointsDataFrame",
                 desc = "Historical fire data in point form. Must contain fields 'CAUSE', 'YEAR', and 'SIZE_HA'",
                 sourceURL = "http://cwfis.cfs.nrcan.gc.ca/downloads/nfdb/fire_pnt/current_version/NFDB_point.zip"),
    expectsInput(objectName = "flammableMap", objectClass = "RasterLayer", desc = "binary map of landscape flammbility"),
    expectsInput(objectName = "landscapeAttr", objectClass = "list",
                 desc = "contains landscape attributes for each polygon"),
    expectsInput(objectName = "studyArea", objectClass = "SpatialPolygonsDataFrame", desc = "",
                 sourceURL = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/district/ecodistrict_shp.zip"),
    expectsInput(objectName = "rasterToMatch", objectClass = "RasterLayer",
                 desc = "template raster for raster GIS operations. Must be supplied by user with same CRS as studyArea"),
    expectsInput(objectName = 'fireRegimePolys', objectClass = "RasterLayer",
                 desc = "Areas to calibrate individual fire regime parameters. Defaults to ecoregions")
  ),
  outputObjects = bind_rows(
   createsOutput(objectName = "scfmRegimePars", objectClass = "list", desc =  "Fire regime parameters for each polygon"),
   createsOutput(objectName = "firePoints", objectClass = "SpatialPointsDataFrame",
                 desc = "Fire locations. Points outside studyArea are removed")
  )
))


## event types
#   - type `init` is required for initiliazation

doEvent.scfmRegime = function(sim, eventTime, eventType, debug = FALSE) {
  if (eventType == "init") {
    Init(sim)
  } else {
    warning(paste("Undefined event type: '", events(sim)[1, "eventType", with = FALSE],
                  "' in module '", events(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
    }
  return(invisible(sim))
}

Init <- function(sim) {

  tmp <- sim$firePoints
  if (length(sim$firePoints) == 0) {
    stop("there are no fires in your studyArea. Consider expanding the study Area")
  }
  #extract and validate fireCause spec

  fc <- P(sim)$fireCause
  #should verify CAUSE is a column in the table...
  if (is.factor(tmp$CAUSE)){
    causeSet <- levels(tmp$CAUSE)}
    else {
    causeSet <- unique(tmp$CAUSE)
  }
  if (any(!(fc %in% causeSet))) {
    notPresent <- fc[!fc %in% causeSet]
    warning("this firecause is not present: ", notPresent)
  }
  tmp <- subset(tmp, CAUSE %in% fc)

  #extract and validate fireEpoch
  epoch <- P(sim)$fireEpoch
  if (length(epoch) != 2 ||
      !is.numeric(epoch) || any(!is.finite(epoch)) || epoch[1] > epoch[2])
    stop("illegal fireEpoch: ", epoch)
  tmp <- subset(tmp, YEAR >= epoch[1] & YEAR <= epoch[2])

  epochLength <- as.numeric(epoch[2] - epoch[1] + 1)

  # Assign polygon label to SpatialPoints of fires object
  #should be specify the name of polygon layer? what if it PROVINCE or ECODISTRICT
  #tmp[["ECOREGION"]] <- sp::over(tmp, sim$studyArea[, "ECOREGION"])
  frpl <- sim$fireRegimePolys$PolyID
  if (is.null(frpl)) 
    sim$fireRegimePolys$PolyID <- sim$fireRegimePolys[[grep("REGION", names(sim$fireRegimePolys), value = TRUE)[1]]]
  tmp$PolyID <- sp::over(tmp, sim$fireRegimePolys)$PolyID #gives studyArea row name to point
  # tmp$PolyID <- tmp$PolyID$PolyID

  tmp <- tmp[!is.na(tmp$PolyID),] #have to remove NA points
  sim$firePoints <- tmp

  firePolys <- unlist(sim$firePoints)

  #this function estimates the ignition probability and escape probability based on NFDB
  scfmRegimePars <- lapply(names(sim$landscapeAttr), FUN = calcZonalRegimePars,
                           firePolys = firePolys, landscapeAttr = sim$landscapeAttr,
                           firePoints = sim$firePoints, epochLength = epochLength,
                           maxSizeFactor = P(sim)$empiricalMaxSizeFactor)

  names(scfmRegimePars) <- names(sim$landscapeAttr)

  nullIdx <- sapply(scfmRegimePars, is.null)
  if (any(nullIdx)){
    scfmRegimePars <- scfmRegimePars[-which(nullIdx)]
  }
  sim$scfmRegimePars <- scfmRegimePars

  return(invisible(sim))
}


.inputObjects <- function(sim) {
  dPath <- dataPath(sim)
  cacheTags = c(currentModule(sim), "function:.inputObjects")

  if (!suppliedElsewhere("studyArea", sim)) {

  }
  if (!suppliedElsewhere("fireRegimePolys", sim)) {
    message("fireRegimePolys not supplied. Using default ecoregions of Canada")

    sim$fireRegimePolys <- prepInputs(url = extractURL("fireRegimePolys", sim),
                                      destinationPath = dPath,
                                      studyArea = sim$studyArea,
                                      rasterToMatch = sim$rasterToMatch,
                                      filename2 = TRUE,
                                      overwrite = TRUE,
                                      userTags = c("cacheTags", "fireRegimePolys"))
  }
  #this module has many dependencies that aren't sourced in .inputObjects
  #this workaround prevents checksums updating due to daily name change of NFDB files
  if (!suppliedElsewhere("firePoints", sim)) {

    NFDB_pointPath <- file.path(dataPath(sim), "NFDB_point")
    checkPath(NFDB_pointPath, create = TRUE)
    a <- Checksums(NFDB_pointPath, checksumFile = file.path(dataPath(sim), "CHECKSUMS.txt"))
    whRowIsShp <- grep("NFDB_point.*shp$", a$expectedFile)
    whIsOK <- which(a$result[whRowIsShp] == "OK")

    #I don't know why the checksums are not 'OK' for more recent downloads - I've rewritten them. No dice.

    needNewDownload <- TRUE
    if (any(whIsOK)) {
      dateOfFile <- gsub("NFDB_point_|\\.shp", "", a[whRowIsShp[whIsOK], "expectedFile"])
      if ((as.Date(dateOfFile, format = "%Y%m%d") + dyear(1)) > Sys.Date()) {
        # can change dyear(...) to whatever... e.g., dyear(0.5) would be 6 months
        needNewDownload <- FALSE
      }
    }
    if (needNewDownload) {
      print("downloading NFDB")# put prepInputs here
      sim$firePoints <- Cache(prepInputs, url = extractURL(objectName = "firePoints"),
                              studyArea = sim$studyArea, fun = "sf::st_read",
                              destination = dPath, overwrite = TRUE,
                              purge = 7, rasterToMatch = sim$rasterToMatch,
                              omitArgs = c("dPath", "overwrite", "purge"))
      sim$firePoints <- as(sim$firePoints, "Spatial")
    } else {
      NFDBs <- grep(list.files(dPath), pattern = "^NFDB", value = TRUE)
      shps <- grep(list.files(dPath), pattern = ".shp$", value = TRUE)
      aFile <- NFDBs[NFDBs %in% shps][1] #in case there are multiple files
      firePoints <- Cache(shapefile, file.path(dPath, aFile))
      sim$firePoints <- Cache(postProcess, x = firePoints,
                              studyArea = sim$studyArea, filename2 = NULL,
                              rasterToMatch = sim$rasterToMatch,
                              userTags = c("cacheTags", "NFDB"))

    }
  }
  if (!identicalCRS(sim$firePoints, sim$fireRegimePolys)) {
    sim$firePoints <- spTransform(sim$firePoints, crs(sim$fireRegimePolys))
  }
  return(invisible(sim))
}
