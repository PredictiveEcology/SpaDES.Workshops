regionMap <- Cache(reproducible::prepInputs, url = "https://drive.google.com/open?id=1WhL-DxrByCbzAj8A7eRx3Y1FVujtGmtN",
                   targetFile = "LCC2005_V1_4a_BCR6_NWT.tif", 
                   destinationPath = getPaths()$inputPath, 
                   fun = "raster::raster", userTags = "objectName:regionMap")
polyMatrix <- matrix(c(-118.269387, 61.783558), ncol = 2)
areaSize <- 30000000000
studyAreaOriginal <- SpaDES.tools::randomPolygon(x = polyMatrix, hectares = areaSize) # Create Random polygon in the NWT
# Reproject study area to match base raster
studyAreaOriginal <- projectInputs(x = studyAreaOriginal, targetCRS = crs(regionMap))
sA_df <- as.data.frame(sapply(slot(studyAreaOriginal, "polygons"), function(x) slot(x, "ID"))) 
row.names(sA_df) <- sapply(slot(studyAreaOriginal, "polygons"), function(x) slot(x, "ID"))
studyArea_sp <- SpatialPolygonsDataFrame(studyAreaOriginal, data = sA_df)
names(studyArea_sp) <- "studyArea"
saveRDS(studyArea_sp, file.path(getPaths()$inputPath, "studyArea.rds"))

regionMapCropped <- Cache(reproducible::prepInputs, url = "https://drive.google.com/open?id=1WhL-DxrByCbzAj8A7eRx3Y1FVujtGmtN",
                          targetFile = "LCC2005_V1_4a_BCR6_NWT.tif", 
                          destinationPath = getPaths()$inputPath, 
                          studyArea = studyArea_sp,
                          fun = "raster::raster", userTags = c("objectName:regionMapCropped", "area:areaSize"),
                          overwrite = TRUE)

saveRDS(regionMapCropped, file.path(getPaths()$inputPath, "rasterToMatch.rds"))

RTM <- readRDS(file.path(getPaths()$inputPath, "rasterToMatch.rds"))
studyArea <- readRDS(file.path(getPaths()$inputPath, "studyArea.rds"))
clearPlot()
Plot(RTM)
Plot(studyArea)
