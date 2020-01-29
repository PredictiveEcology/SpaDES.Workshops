generateRSFRas <- function(modelType, templateRas, currentTime, 
                           responseTable, column, rasName, pathOut){
  rasList <- lapply(X = seq_len(length(rasName)), FUN = function(r){
    
    ras <- raster::setValues(x = raster(templateRas[[paste0("Year", currentTime)]][[1]]), 
                             values = responseTable[, get(column[r])])
    names(ras) <- paste0(rasName[r], modelType, "_Year", currentTime)
    ras[] <- ras[]
    WaterRas <- templateRas[[paste0("Year", currentTime)]][["Water"]]
    WaterRas[WaterRas == 1] <- NA
    ras <- postProcess(x = ras, rasterToMatch = WaterRas, maskWithRTM = TRUE, 
                       filename2 = NULL, destinationPath = tempdir())
    writeRaster(x = ras, filename = file.path(pathOut, paste0(rasName[r], modelType, "_Year", currentTime)),
                format = "GTiff")
    return(ras)
  })
  names(rasList) <- rasName
  return(rasList)
}