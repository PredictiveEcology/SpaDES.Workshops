meanRSFtime <- function(predictedPresenceProbability, 
                        scenario,
                        initialTime){ 
  # scenario == "noCS" | "CS"
  fullTable <- lapply(predictedPresenceProbability, FUN = function(year){
    modTable <- lapply(year, FUN = function(mod){
meanAndUnc <- lapply(mod, function(eachRas){
  average <- median(eachRas[], na.rm = TRUE)
  rasType <- ifelse(grepl(names(eachRas), pattern = "Uncertain"), "SD", "AVERAGE")
  yr <- as.numeric(substrBoth(string = names(eachRas), howManyCharacters = nchar(initialTime)))
  dt <- data.table::data.table(average = average, year = yr, scenario = scenario)
  return(dt)
})
  dt <- meanAndUnc[[1]]
  dt$SD <-  meanAndUnc[[2]][["average"]]
  dt$IC <- dt$SD*1.96
return(dt)
    })
    return(rbindlist(modTable))
  })
  fullTable <- rbindlist(fullTable)
return(fullTable)
}
