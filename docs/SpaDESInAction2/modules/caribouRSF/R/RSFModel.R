RSFModel <- function(caribouModelsRSF,
                     modLayers,
                     currentTime,
                     pathData,
                     modelType,
                     pathOut){

  message("Forecasting caribou habitat from RSF ...")
  yearPrediction <- lapply(X = names(modLayers), FUN = function(yrs) {
    modsPred <- lapply(X = names(caribouModelsRSF), FUN = function(modelType) {
      coeffTable <- data.table::data.table(raster::getValues(modLayers[[yrs]]))

coeffTable <- generateRSFPredictions(coeffTable = coeffTable,
                    resultCol = c("meanResponse","sdResponse"),
                    caribouModelsRSF = caribouModelsRSF,
                    modelType = modelType)

predAndUncertain <- generateRSFRas(modelType = modelType, 
                            templateRas = modLayers, 
                            currentTime = currentTime, 
                            responseTable = coeffTable, 
                            column = c("meanResponse", "sdResponse"),
                            rasName = c("relativeSelection", "relativeSelectionUncertain"),
                            pathOut = pathOut)
return(predAndUncertain)

    })
    names(modsPred) <- paste0(modelType)
    return(modsPred)
  })
  names(yearPrediction) <- paste0(names(modLayers))
  return(yearPrediction[[paste0("Year", currentTime)]])
}