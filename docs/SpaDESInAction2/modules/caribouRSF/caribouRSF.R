defineModule(sim, list(
  name = "caribouRSF",
  description = paste0("Module to simulate caribou resource selection", "
                       based on the Taiga plains RSF model published by ECCC 2012"),
  keywords = c("Caribou", "population", "lambda"),
  authors = c(person("Tati", "Micheletti", email = "tati.micheletti@gmail.com", role = c("aut", "cre")),
              person("Frances", "Stewart", email = "frances.stewart@canada.ca", role = c("aut", "cre")),
              person("Eliot", "McIntire", email = "Eliot.McIntire@canada.ca", role = c("aut", "cre"))),
  childModules = character(0),
  version = list(SpaDES.core = "0.2.5", caribouRSF = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "caribouRSF.Rmd"),
  reqdPkgs = list("data.table", "ggplot2", "PredictiveEcology/pemisc", "tati-micheletti/usefun"), 
  parameters = rbind(
    defineParameter("predictLastYear", "logical", TRUE, NA, NA, paste0("If last year of simulation is not multiple of",
                    " predictionInterval, should it predict for the last year too?")),
    defineParameter(".useCache", "logical", FALSE, NA, NA, "Should this entire module be run with caching activated?"),
    defineParameter("modelType", "character", "TaigaPlains", NA, NA, "Should this entire module be run with caching activated?"),
    defineParameter("meanFire", "numeric", 30.75, NA, NA, "Mean cummulative fire from ECCC Scientific report 2011"),
    defineParameter("sdFire", "numeric", 10.6, NA, NA, "SD cummulative fire from ECCC Scientific report 2011"),
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA, "inital plot time"),
    defineParameter(".plotTimeInterval", "numeric", 10, NA, NA, "Interval of plotting time"),
    defineParameter(".useDummyData", "logical", FALSE, NA, NA, "Should use dummy data? Automatically set"),
    defineParameter("recoveryTime", "numeric", 40, NA, NA, "Time to recover the forest enough for caribou"),
    defineParameter("predictionInterval", "numeric", 10, NA, NA, "Time between predictions"),
    defineParameter(name = "baseLayer", class = "numeric", default = 2005, min = NA, max = NA, 
                    desc = "Which layer should be used? LCC05 or LCC10?"),
    defineParameter(name = "decidousSp", class = "character", default = c("Betu_Pap", "Popu_Tre", "Popu_Bal"), 
                    min = NA, max = NA, desc = "Deciduous species to be considered for caribou"),
    defineParameter(name = "oldBurnTime", class = "numeric", default = 40, 
                    min = NA, max = NA, desc = "Threshold for oldBurn/newBurn. Max oldburn + 20"),
    defineParameter(".useCache", "character", c(".inputObjects", "init"), NA, NA,
                    desc = "Internal. Can be names of events or the whole module name; these will be cached by SpaDES")
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "waterRaster", objectClass = "RasterLayer",
                 desc = "Wetland raster for excluding water from anthropogenic layer",
                 sourceURL = NA),
    expectsInput(objectName = "pixelGroupMap", objectClass = "RasterLayer",
                 desc = paste0("Map of groups of pixels that share the same info from cohortData (sp, age, biomass, etc).",
                               "Here is mainly used to determine old and recent burns based on tree age,",
                               " and if deciduous by species")),
    expectsInput(objectName = "cohortData", objectClass = "data.table",
                 desc = paste0("data.table with information by pixel group of sp, age, biomass, etc")),
    expectsInput(objectName = "roadDensity", objectClass = "RasterLayer",
                 desc = paste0("Layer that maps a 10km buffer on each road.", 
                               "This layer is static if no modules are forecasting anthropogenic disturbances"), 
                 sourceURL = "https://drive.google.com/open?id=1C0Y0z1cgQKwa3_-X2qWrhNIzEHIl9m5e"),
    expectsInput(objectName = "modelsToUse", objectClass = "character", 
                 desc = "Which models from ECCC to be used? National or regional?", 
                 sourceURL = NA),
    expectsInput(objectName = "anthropogenicLayer", objectClass = "RasterLayer", 
                 desc = "Raster with road buffered disturbances", 
                 sourceURL = "https://drive.google.com/open?id=1zj7zo8NBNhxxHMUL4ZnKTaP3niuQEI1m"),
    expectsInput(objectName = "Elevation", objectClass = "RasterLayer", 
                 desc = "Raster with elevation values", 
                 sourceURL = "https://drive.google.com/open?id=1SKnXVqUD10_VdemQaPaz9MrWiNZzK7VY"),
    expectsInput(objectName = "Vrug", objectClass = "RasterLayer", 
                 desc = "Raster with elevation values", 
                 sourceURL = "https://drive.google.com/open?id=16u07GpGQbBd5Yh8xPZ_xLiUo31OF0uDP"),
    expectsInput(objectName = "provincesToModel", objectClass = "character", 
                 desc = "Which province caribou data should be used for the module?"),
    expectsInput(objectName = "caribouCoefTableRSF", objectClass = "data.table", 
                 desc = "Published caribou coefficients", 
                 sourceURL = "https://drive.google.com/open?id=16bgCDuQaxrQakKs2RL-eU2iFAhWylcRu"),
    expectsInput(objectName = "LCC05", objectClass = "RasterLayer", 
                 desc = "This will give is both shrub and herb layers", 
                 sourceURL = ""),
    expectsInput(objectName = "fixedLayers", objectClass = "character", 
                 desc = "Fixed layers for the Caribou RSF model, currently: Elevation, Vrug, Peatland, NDVI, Shrub, Herb", 
                 sourceURL = ""),
    expectsInput(objectName = "simulLayers", objectClass = "character", 
                 desc = "Possibly simulated layers for the Caribou RSF model, currently: RoadDensity, Conifer, Deciduous, Wetland, Water, RecentBurn, OldBurn", 
                 sourceURL = ""),
    expectsInput(objectName = "studyArea", objectClass = "SpatialPolygonDataFrame", 
                 desc = "Study area for the prediction. Currently only available for NWT", 
                 sourceURL = "https://drive.google.com/open?id=1LUxoY2-pgkCmmNH5goagBp3IMpj6YrdU"),
    expectsInput(objectName = "rasterToMatch", objectClass = "RasterLayer",
                 desc = "All spatial outputs will be reprojected and resampled to it", 
                 sourceURL = "https://drive.google.com/open?id=1fo08FMACr_aTV03lteQ7KsaoN9xGx1Df"),
    expectsInput(objectName = "reclassLCC05", objectClass = "data.table",
                 desc = "Table 41 from ECCC report converting LCC05 classes", 
                 sourceURL = "https://drive.google.com/open?id=1pMfkIoqFoxwICMlend_mNuwNMGA5_6Fr"),
    expectsInput(objectName = "caribouArea1", objectClass = "SpatialPolygonsDataFrame",
                 desc = "Study area to predict caribou population to (NWT_Regions_2015_LCs_DC_SS)",
                 sourceURL = "https://drive.google.com/open?id=1Qbt2pOvC8lGg25zhfMWcc3p6q3fZtBtO"),
    expectsInput(objectName = "Edehzhie", objectClass = "SpatialPolygonsDataFrame",
                 desc = "Study area to predict caribou pospulation to",
                 sourceURL = "https://drive.google.com/open?id=1VP91AyIeGCFwJS9oPSEno4_SbtJQJMh7"),
    expectsInput(objectName = "caribouArea2", objectClass = "SpatialPolygonsDataFrame",
                 desc = "Study area to predict caribou population to (NT1_BOCA_spatial_units_for_landscape)",
                 sourceURL = "https://drive.google.com/open?id=1Vqny_ZMoksAjji4upnr3OiJl2laGeBGV"),
    expectsInput(objectName = "currentPop", objectClass = "numeric", 
                 desc = "Caribou population size in the study area. Is updated every time step",
                 sourceURL = NA)
  ), 
  outputObjects = bind_rows(
    createsOutput(objectName = "caribouModelsRSF", objectClass = "list", 
                  desc = "List with model equations. Default is TaigaPlains (ECCC 2011, Table 46)."),
    createsOutput(objectName = "predictedPresenceProbability", objectClass = "list", 
                  desc = "List of rasters per year, indicating the probability of presence of Caribous"),
    createsOutput(objectName = "modLayers", objectClass = "RasterStack", 
                  desc = "Stack of all dynamic layers: oldBurn, newBurn, biomassMap, roadDensity, waterRaster"),
    createsOutput(objectName = "listSACaribou", objectClass = "list", 
                  desc = paste0("List of caribou areas to predict for",
                                " Currently only takes 3 shapefiles"))
  )
))

doEvent.caribouRSF = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      
      sim$listSACaribou = list(sim$caribouArea1, sim$caribouArea2, sim$Edehzhie)
      names(sim$listSACaribou) <- c("caribouArea1", "caribouArea2", "Edehzhie")
      
                               
      # schedule future event(s)
      sim <- scheduleEvent(sim, start(sim), "caribouRSF", "makingModel")
      sim <- scheduleEvent(sim, start(sim), "caribouRSF", "gettingData")
      sim <- scheduleEvent(sim, start(sim), "caribouRSF", "lookingForCaribou")
      sim <- scheduleEvent(sim, end(sim), "caribouRSF", "plot", eventPriority = .last()) #P(sim)$.plotInitialTime
    },
    makingModel = {
      # Prepare the Equation
      sim$caribouModelsRSF <- usefun::createModels(caribouCoefTable = sim$caribouCoefTableRSF, 
                                        modelsToUse = P(sim)$modelType)
    },
    gettingData = {
      Require("magrittr")
      mod$cohortData <- usefun::createModObject(data = "cohortData", sim = sim, 
                                        pathInput = inputPath(sim), currentTime = time(sim))
      mod$pixelGroupMap <- usefun::createModObject(data = "pixelGroupMap", sim = sim, 
                                           pathInput = inputPath(sim), currentTime = time(sim))

      if (any(is.null(mod$pixelGroupMap), is.null(mod$cohortData))) {
        params(sim)$.useDummyData <- TRUE
      }
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$predictionInterval, "caribouRSF", "gettingData")
      if (P(sim)$predictLastYear){
        if (all(time(sim) == start(sim), (end(sim)-start(sim)) != 0))
          sim <- scheduleEvent(sim, end(sim), "caribouRSF", "gettingData")
      }
      
    },
    lookingForCaribou = {
      if (isTRUE(P(sim)$.useDummyData)){
        stop("This module does not work without data. Please provide the necessary layers")
      } else {
        if (is.null(sim$modLayers)){
          sim$modLayers <- list()
        }

        caribouDynCovs <- sim$caribouCoefTableRSF[ModelNum == sim$modelsToUse, ][!is.na(Value), Coefficient]
        
        sim$modLayers[[paste0("Year", time(sim))]] <- usefun::getLayers(currentTime = time(sim),
                                           startTime = start(sim),
                                           endTime = end(sim),
                                           cohortData = mod$cohortData, # Has age info per pixel group
                                           pixelGroupMap = mod$pixelGroupMap,
                                           recoveryTime = P(sim)$recoveryTime,
                                           listSACaribou = sim$listSACaribou,
                                           anthropogenicLayer = sim$anthropogenicLayer,
                                           roadDensity = sim$roadDensity,
                                           waterRaster = sim$waterRaster,
                                           isRSF = TRUE,
                                           decidousSp = P(sim)$decidousSp,
                                           oldBurnTime = P(sim)$oldBurnTime,
                                        elevation = sim$Elevation,
                                        vrug = sim$Vrug,
                                        LCC05 = sim$LCC05,
                                   reclassLCC05 = sim$reclassLCC05,
                                   rasterToMatch = sim$rasterToMatch)
      }
      fls <- tryCatch({usefun::grepMulti(x = list.files(outputPath(sim)), patterns = c("relativeSelection", time(sim)))}, error = function(e){
        return(NULL)
      })
      if (length(fls) > 0) { 
        # THIS SOLUTION PROBABLY DOESN'T WORK FOR WHEN WE ADD ANOTHER MODEL! WILL HAVE TO BE FIXED BY THEN!!!
        sim$predictedPresenceProbability[[paste0("Year", time(sim))]] <- list(lapply(file.path(outputPath(sim), fls), FUN = raster))
        names(sim$predictedPresenceProbability[[paste0("Year", time(sim))]]) <- sim$modelsToUse
        names(sim$predictedPresenceProbability[[paste0("Year", time(sim))]][[sim$modelsToUse]]) <- c("relativeSelection", "relativeSelectionUncertain")
        
      } else {
        
        sim$predictedPresenceProbability[[paste0("Year", time(sim))]] <- RSFModel(caribouModelsRSF = sim$caribouModelsRSF,
                                                                                  modLayers = sim$modLayers[[paste0("Year", time(sim))]],
                                                                                  currentTime = time(sim),
                                                                                  pathData = dataPath(sim),
                                                                                  modelType = P(sim)$modelType,
                                                                                  pathOut = outputPath(sim))
              }

      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$predictionInterval, "caribouRSF", "lookingForCaribou")
      if (P(sim)$predictLastYear){
        if (all(time(sim) == start(sim), (end(sim)-start(sim)) != 0))
          sim <- scheduleEvent(sim, end(sim), "caribouRSF", "lookingForCaribou")
      }
      
    },
    plot = {
      caribouResourceSelection <- sim$predictedPresenceProbability[[paste0("Year", time(sim))]][["TaigaPlains"]][["relativeSelection"]]
      Plot(caribouResourceSelection, 
           title = "Caribou resource selection")

      # schedule future event(s)
      if (time(sim) != end(sim))
        sim <- scheduleEvent(sim, end(sim), "caribouRSF", "plot", eventPriority = .last())
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {

  cloudFolderID <- "https://drive.google.com/open?id=1PoEkOkg_ixnAdDqqTQcun77nUvkEHDc0"
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")
  
  cloudFolderID <- "https://drive.google.com/open?id=1PoEkOkg_ixnAdDqqTQcun77nUvkEHDc0"

  if (!suppliedElsewhere("provinces", sim)){
    sim$provinces <- "NWT"
  }
  if (!suppliedElsewhere("caribouCoefTableRSF", sim)){
    sim$caribouCoefTableRSF <- prepInputs(targetFile = "caribouRSF_ModelCoefficents.csv", 
                                          url = extractURL("caribouCoefTableRSF"),
                                          destinationPath = dataPath(sim), fun = "data.table::fread", 
                                          omitArgs = "destinationPath", overwrite = TRUE)
  }

  if (!suppliedElsewhere(object = "studyArea", sim = sim)){
    sim$studyArea <- Cache(prepInputs,
                           url = extractURL("studyArea"),
                           destinationPath = dataPath(sim),
                           cloudFolderID = sim$cloudFolderID,
                           omitArgs = c("destinationPath", "cloudFolderID"))
  }
  
  if (!suppliedElsewhere(object = "rasterToMatch", sim = sim)){
    sim$rasterToMatch <- Cache(prepInputs, url = extractURL("rasterToMatch"), 
                               studyArea = sim$studyArea,
                               targetFile = "RTM.tif", destinationPath = dataPath(sim), 
                               # useCloud = P(sim)$.useCloud,
                               # cloudFolderID = sim$cloudFolderID, 
                               overwrite = TRUE, filename2 = NULL,
                               omitArgs = c("destinationPath", "cloudFolderID", "useCloud", "overwrite", "filename2"))
  }
  
  if (!suppliedElsewhere("adultFemaleSurv", sim)){
    message(crayon::yellow(paste0("No LPU specific values for the female survival is available for NWT.", 
                                  "\nUsing national ECCC value of 0.85.")))
    sim$adultFemaleSurv <- 0.85
  }
  
  if (!suppliedElsewhere("waterRaster", sim)){
    wetlandRaster <- Cache(prepInputsLayers_DUCKS, destinationPath = dataPath(sim), 
                           studyArea = sim$studyArea, 
                           userTags = "objectName:wetlandRaster")
    sim$waterRaster <- Cache(classifyWetlands, LCC = P(sim)$baseLayer,
                             rasterToMatch = sim$rasterToMatch,
                             wetLayerInput = wetlandRaster,
                             pathData = dataPath(sim),
                             studyArea = sim$studyArea,
                             userTags = c("objectName:wetLCC"))
    
    waterVals <- raster::getValues(sim$waterRaster) # Uplands = 3, Water = 1, Wetlands = 2, so 2 and 3 to NA
    waterVals[!is.na(waterVals) & waterVals != 1] <- 0
    sim$waterRaster <- raster::setValues(sim$waterRaster, waterVals)

  }

  if (!suppliedElsewhere("anthropogenicLayer", sim)){
    sim$anthropogenicLayer <- prepInputs(targetFile = "bufferMap_v0.1.0_m_r500_t0_anthrDisturb.grd",
                                         archive = "bufferMap_v0.1.0_m_r500_t0_anthrDisturb.zip",
                                         alsoExtract = "similar",
                                         url = "https://drive.google.com/open?id=1GhnIjmKsZ3JoxTjefeeBUb02iiEcV_qD",
                                         destinationPath = dataPath(sim), studyArea = sim$studyArea,
                                         overwrite = TRUE, 
                                         rasterToMatch = sim$rasterToMatch)

  }
  
  if (!suppliedElsewhere("roadDensity", sim)){
    sim$roadDensity <- prepInputs(targetFile = "roadDensity_BCR6_NWT_t0.tif",
                                         url = extractURL("roadDensity"),
                                         destinationPath = dataPath(sim), 
                                         studyArea = sim$studyArea,
                                         overwrite = TRUE, 
                                         rasterToMatch = sim$rasterToMatch)
  }
  
  if (!suppliedElsewhere("modelsToUse", sim)){
    sim$modelsToUse <- "TaigaPlains"
  }
  if (!suppliedElsewhere("LCC05", sim)){
    sim$LCC05 <- LandR::prepInputsLCC(destinationPath = dataPath(sim),
                                      studyArea = sim$studyArea,
                                      rasterToMatch = sim$rasterToMatch)
  }
  if (!suppliedElsewhere("Elevation", sim)){
    sim$Elevation <- prepInputs(targetFile = "nadem100laz_BCR6_NWT.tif", 
                                   url = extractURL("Elevation"),
                                   destinationPath = dataPath(sim), studyArea = sim$studyArea,
                                   overwrite = TRUE, fun = "raster::stack",
                                   rasterToMatch = sim$rasterToMatch)

    
  }
  if (!suppliedElsewhere("Vrug", sim)){
    
    sim$Vrug <- prepInputs(archive = "vrug_bcr6.zip",
                           targetFile = "vrug_bcr6.tif",
                                url = extractURL("Vrug"),
                                destinationPath = dataPath(sim), studyArea = sim$studyArea,
                                overwrite = TRUE, 
                                rasterToMatch = sim$rasterToMatch)

  }
  if (!suppliedElsewhere("reclassLCC05", sim)){
    
    sim$reclassLCC05 <- prepInputs(targetFile = "Table41_ConvertLCC05.csv",
                           url = extractURL("reclassLCC05"),
                           destinationPath = dataPath(sim),
                           overwrite = TRUE, fun = "data.table::fread")
  }
  
  if (!suppliedElsewhere(object = "caribouArea2", sim = sim)){
    sim$caribouArea2 <- Cache(prepInputs, url = extractURL("caribouArea2"),
                              targetFile = "NT1_BOCA_spatial_units_for_landscape_projections.shp",
                              destinationPath = dataPath(sim), filename2 = "caribouArea2")
  }
  if (!suppliedElsewhere("caribouArea1", sim)){
    sim$caribouArea1 <- Cache(prepInputs, url = extractURL("caribouArea1"),
                              targetFile = "NWT_Regions_2015_LCs_DC_SS_combined_NT1_clip_inc_Yukon.shp",
                              destinationPath = dataPath(sim), filename2 = "caribouArea1")
  }
  
  if (!suppliedElsewhere("Edehzhie", sim)){
    sim$Edehzhie <- Cache(prepInputs, targetFile = "Edehzhie.shp",
                          archive = "Edehzhie.zip",
                          alsoExtract = "similar",
                          url = extractURL("Edehzhie"), studyArea = sim$studyArea,
                          destinationPath = dataPath(sim), filename2 = NULL,
                          rasterToMatch = sim$rasterToMatch)
    sim$Edehzhie$Name <- sim$Edehzhie$NAME_1
  }
  
  if (!suppliedElsewhere("forestOnly", sim = sim, where = "sim")){
    
    forestClasses <- c(1:15, 34:35)
    sim$forestOnly <- sim$rasterToMatch
    sim$forestOnly[!sim$LCC05[] %in% forestClasses] <- NA
  }

  return(invisible(sim))
}