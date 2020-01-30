calculateRSF <- function(Elevation, 
                         Vrug,
                         Shrub, 
                         Herb, 
                         RoadDensity, 
                         Deciduous, 
                         Water, 
                         RecentBurn,
                         OldBurn,
                         caribouModelsRSF){
  
  if (is.na(Deciduous)) return(list(meanResponse = NA, sdResponse = NA))
    resp <- eval(parse(text = caribouModelsRSF))
    return(list(meanResponse = mean(resp), sdResponse = sd(resp)))
}
