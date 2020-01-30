test_that("test growth and mortality at main simulation stage",{
  library(SpaDES)
  library(data.table)
  library(raster)
  # define the module and path
  module <- list("Biomass_core")
  path <- list(modulePath="~/GitHub/nrv-succession/code blitz succession/Module_Biomass_core",
               outputPath="~/output")
  parameters <- list(.progress=list(type="graphical", interval=1),
                     .globals=list(verbose=FALSE),
                     Biomass_core=list( .saveInitialTime=NA))

  pixelGroupMap <- raster(xmn=50,xmx=50+1*100,
                          ymn=50,ymx=50+1*100,
                          res=c(100,100), val=11)
  speciesEcoregion <- data.table(year=seq(0,500,by=10), ecoregion="eco1",species="tsugcana",
                                 establishprob=0.22,maxANPP=1096,maxB=32880,speciesCode=1,
                                 ecoregionGroup=1)
  successionTimestep <- 1
  species <- read.csv("~/GitHub/nrv-succession/code blitz succession/modeltesting-data/species.csv",
                      header=T,stringsAsFactor=FALSE)
  species <- data.table(species)[species=="tsugcana",][,':='(sexualmature=longevity,
                                                             speciesCode=1)]
  ecoregion <- read.csv("~/GitHub/nrv-succession/code blitz succession/modeltesting-data/ecoregion.csv",
                      header=T,stringsAsFactor=FALSE)
  cohortData <- data.table(pixelGroup = 11, ecoregionGroup = 1L,
                           speciesCode = 1, age = 1, B = 1096L,
                           mortality = 0, aNPPAct = 0)
  lastReg <- 999
  calibrate <- TRUE
  objects <- list("pixelGroupMap"=pixelGroupMap,
                  "speciesEcoregion"=speciesEcoregion,
                  "species"=species,
                  "successionTimestep"=successionTimestep,
                  "cohortData"=cohortData,
                  "ecoregion"=ecoregion,
                  "lastReg"=lastReg,
                  "calibrate" = calibrate)
  mySim <- simInit(times=list(start=1, end=2),
                   params=parameters,
                   modules=module,
                   objects=objects,
                   paths=path)
  output <- data.table(Year=0,Biomass=1096)
  for(i in 2:493){
    if(exists("Biomass_coreMortalityAndGrowth")){
      simOutput <- Biomass_coreMortalityAndGrowth(mySim)
    } else {
      simOutput <- mySim$Biomass_coreMortalityAndGrowth(mySim)
    }
    cohortData <- simOutput$cohortData
    if(i == 2 | i == 3){
      expect_is(cohortData,"data.table")
      expect_equal(names(cohortData),
                   c("pixelGroup", "ecoregionGroup", "speciesCode", "age",
                     "B", "mortality", "aNPPAct"))
    }
    output_added <- cohortData[,.(Year=time(mySim),Biomass=B)]
    output <- rbind(output,output_added)
    objects <- list("pixelGroupMap"=pixelGroupMap,
                    "speciesEcoregion"=speciesEcoregion,
                    "species"=species,
                    "successionTimestep"=successionTimestep,
                    "cohortData"=cohortData,
                    "ecoregion"=ecoregion,
                    "lastReg"=lastReg,
                    "calibrate" = calibrate)
    mySim <- simInit(times=list(start=i, end=i+1),
                     params=parameters,
                     modules=module,
                     objects=objects,
                     paths=path)
  }

  output_compared <- data.table(Year=0:492,
                                Biomass=c(1096, 1855, 2629, 3397, 4149, 4881, 5591, 6278, 6943, 7585,
                                          8206, 8806, 9386, 9947, 10489, 11014, 11522, 12014, 12491, 12953,
                                          13401, 13836, 14258, 14667, 15065, 15451, 15826, 16191, 16546,
                                          16891, 17227, 17554, 17872, 18182, 18484, 18778, 19065, 19344,
                                          19616, 19882, 20141, 20394, 20641, 20882, 21117, 21346, 21570,
                                          21789, 22003, 22212, 22416, 22615, 22810, 23000, 23186, 23368,
                                          23546, 23720, 23890, 24056, 24218, 24377, 24532, 24684, 24833,
                                          24978, 25120, 25259, 25395, 25528, 25659, 25787, 25912, 26034,
                                          26154, 26271, 26386, 26498, 26608, 26716, 26821, 26924, 27025,
                                          27124, 27221, 27316, 27409, 27500, 27589, 27676, 27761, 27844,
                                          27925, 28005, 28083, 28159, 28234, 28307, 28378, 28448, 28516,
                                          28583, 28648, 28712, 28774, 28835, 28895, 28953, 29010, 29066,
                                          29120, 29173, 29225, 29276, 29325, 29373, 29420, 29466, 29511,
                                          29555, 29597, 29638, 29678, 29717, 29755, 29792, 29828, 29863,
                                          29897, 29930, 29962, 29993, 30023, 30052, 30080, 30107, 30133,
                                          30158, 30183, 30207, 30230, 30252, 30273, 30293, 30312, 30330,
                                          30347, 30364, 30380, 30395, 30409, 30422, 30434, 30446, 30457,
                                          30467, 30476, 30484, 30491, 30498, 30504, 30509, 30513, 30516,
                                          30519, 30521, 30522, 30522, 30522, 30522, 30521, 30519, 30516,
                                          30512, 30507, 30502, 30496, 30489, 30481, 30472, 30462, 30451,
                                          30440, 30428, 30415, 30401, 30386, 30370, 30353, 30335, 30317,
                                          30298, 30278, 30257, 30235, 30212, 30188, 30163, 30137, 30110,
                                          30082, 30053, 30023, 29992, 29960, 29927, 29893, 29858, 29822,
                                          29785, 29747, 29708, 29668, 29627, 29585, 29542, 29498, 29453,
                                          29407, 29359, 29310, 29260, 29209, 29157, 29104, 29049, 28993,
                                          28936, 28878, 28819, 28758, 28696, 28633, 28568, 28502, 28435,
                                          28367, 28297, 28226, 28153, 28079, 28004, 27927, 27849, 27770,
                                          27689, 27607, 27523, 27438, 27351, 27263, 27173, 27082, 26990,
                                          26896, 26801, 26704, 26605, 26505, 26403, 26300, 26195, 26089,
                                          25981, 25872, 25761, 25649, 25535, 25420, 25303, 25184, 25064,
                                          24942, 24819, 24694, 24568, 24440, 24311, 24180, 24048, 23914,
                                          23779, 23642, 23504, 23364, 23223, 23080, 22936, 22790, 22643,
                                          22495, 22345, 22194, 22042, 21888, 21733, 21577, 21419, 21260,
                                          21100, 20939, 20777, 20614, 20450, 20285, 20119, 19952, 19784,
                                          19615, 19445, 19274, 19102, 18930, 18757, 18583, 18409, 18234,
                                          18059, 17883, 17707, 17530, 17353, 17176, 16998, 16820, 16642,
                                          16464, 16286, 16108, 15930, 15752, 15574, 15396, 15218, 15041,
                                          14864, 14687, 14511, 14335, 14160, 13985, 13811, 13638, 13465,
                                          13293, 13122, 12951, 12781, 12602, 12415, 12221, 12020, 11814,
                                          11604, 11390, 11173, 10954, 10733, 10512, 10290, 10068, 9847,
                                          9628, 9410, 9194, 8981, 8770, 8562, 8358, 8157, 7959, 7765, 7575,
                                          7389, 7207, 7029, 6855, 6685, 6519, 6357, 6199, 6045, 5894, 5747,
                                          5604, 5465, 5329, 5197, 5069, 4944, 4822, 4703, 4587, 4475, 4365,
                                          4258, 4154, 4053, 3954, 3858, 3764, 3673, 3584, 3497, 3413, 3331,
                                          3251, 3172, 3096, 3021, 2948, 2877, 2808, 2741, 2675, 2611, 2549,
                                          2488, 2429, 2371, 2314, 2259, 2205, 2152, 2101, 2051, 2002, 1954,
                                          1908, 1863, 1818, 1775, 1733, 1692, 1652, 1612, 1574, 1536, 1500,
                                          1464, 1429, 1395, 1362, 1330, 1298, 1267, 1237, 1208, 1179, 1151,
                                          1124, 1097, 1071, 1045, 1020, 996, 973, 950, 927, 905, 883, 862,
                                          842, 822, 802, 783, 764, 746, 728, 711, 694, 677, 661, 645, 630,
                                          615, 600, 586, 572, 558, 545, 532, 519, 507, 494, 483, 471, 460,
                                          449, 438, 427, 417, 407))
  expect_equal(output,output_compared)
  rm(mySim,output,output_compared,cohortData,successionTimestep)



  successionTimestep <- 4

  cohortData <- data.table(pixelGroup = 11, ecoregionGroup = 1L,
                           speciesCode = 1, age = 4, B = 4141,
                           mortality = 0, aNPPAct = 0)
  lastReg <- 999
  objects <- list("pixelGroupMap"=pixelGroupMap,
                  "speciesEcoregion"=speciesEcoregion,
                  "species"=species,
                  "successionTimestep"=successionTimestep,
                  "cohortData"=cohortData,
                  "ecoregion"=ecoregion,
                  "lastReg"=lastReg,
                  "calibrate" = calibrate)
  mySim <- simInit(times=list(start=0, end=5),
                   params=parameters,
                   modules=module,
                   objects=objects,
                   paths=path)
  output <- data.table(Year=0,Biomass=4141)
  for(i in 1:492){
    if(exists("Biomass_coreMortalityAndGrowth")){
      simOutput <- Biomass_coreMortalityAndGrowth(mySim)
    } else {
      simOutput <- mySim$Biomass_coreMortalityAndGrowth(mySim)
    }
    cohortData <- simOutput$cohortData
    if(i == 2 | i == 3){
      expect_is(cohortData,"data.table")
      expect_equal(names(cohortData),
                   c("pixelGroup", "ecoregionGroup", "speciesCode", "age",
                     "B", "mortality", "aNPPAct"))
    }
    if((time(mySim)+1)/4==as.integer((time(mySim)+1)/4)){
      output_added <- cohortData[,.(Year=time(mySim)+1,Biomass=B)]
      output <- rbind(output,output_added)
    }
    objects <- list("pixelGroupMap"=pixelGroupMap,
                    "speciesEcoregion"=speciesEcoregion,
                    "species"=species,
                    "successionTimestep"=successionTimestep,
                    "cohortData"=cohortData,
                    "ecoregion"=ecoregion,
                    "lastReg"=lastReg,
                    "calibrate" = calibrate)
    mySim <- simInit(times=list(start=i, end=i+1),
                     params=parameters,
                     modules=module,
                     objects=objects,
                     paths=path)
  }

  output_compared <- data.table(Year=seq(0,492,by=4),
                                Biomass=c(4141, 6936, 9380, 11516, 13395, 15060, 16541, 17867, 19060,
                                          20138, 21114, 22001, 22808, 23544, 24217, 24832, 25395, 25912,
                                          26386, 26821, 27221, 27589, 27926, 28235, 28518, 28777, 29013,
                                          29228, 29424, 29602, 29761, 29903, 30029, 30141, 30239, 30321,
                                          30391, 30447, 30489, 30519, 30536, 30540, 30535, 30517, 30486,
                                          30440, 30382, 30308, 30222, 30120, 30002, 29868, 29718, 29552,
                                          29368, 29166, 28944, 28703, 28442, 28159, 27855, 27528, 27178,
                                          26804, 26406, 25984, 25538, 25066, 24570, 24050, 23505, 22937,
                                          22346, 21733, 21100, 20450, 19784, 19102, 18409, 17707, 16998,
                                          16286, 15574, 14864, 14160, 13465, 12781, 12020, 11173, 10290,
                                          9410, 8562, 7765, 7029, 6357, 5747, 5197, 4703, 4258, 3858, 3497,
                                          3172, 2877, 2611, 2371, 2152, 1954, 1775, 1612, 1464, 1330, 1208,
                                          1097, 996, 905, 822, 746, 677, 615, 558, 507, 460, 417, 378))

  expect_equal(output,output_compared)
})
