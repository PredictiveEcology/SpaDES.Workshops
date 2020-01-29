generateRSFPredictions <- function(coeffTable, resultCol, caribouModelsRSF,
                                   modelType){
  if (length(modelType)>1)
    stop("For now, RSF can't predict with more than one model type at a time") # [ FIX ] make it possible to run more than 1 model at a time
  
  caribMod <- caribouModelsRSF[[modelType]]
  
  coeffTable[, (resultCol) := rbindlist(Map(calculateRSF,
                                            Elevation = Elevation,
                                            Vrug = Vrug,
                                            RoadDensity = RoadDensity,
                                            Deciduous = Deciduous,
                                            Shrub = Shrub,
                                            Herb = Herb,
                                            Water = Water,
                                            RecentBurn = RecentBurn,
                                            OldBurn = OldBurn,
                                            caribouModelsRSF = caribMod)
  )]
  return(coeffTable)
}