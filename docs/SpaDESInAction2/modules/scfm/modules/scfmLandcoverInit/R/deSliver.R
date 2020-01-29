#' A function for merging sliver polygons into non-sliver neighbours.
#' The threshold is applied to the area of the multipolygon object, not each
#' individual polygon. Non-sliver polygons keep their original attributesl
#' Inteded to be used when it is important to retain the original extent of an
#' area while removing sliver polygons
#' @keywords sliver polygons intersect
#' @param x A spatialPolygonsDataFrame or sf object
#' @param threshold the minimum area below which a polygon is considered a sliver
#' @return an object of class sf or Spatial with sliver polygons
#' merged to their nearest valid neighbour.
#'
#' @export
#' @importFrom sf st_area st_cast st_nearest_feature as_Spatial st_union st_as_sf
#' @importFrom raster bind
#' @importFrom rgeos gBuffer
#' @examples
#'deSliver(x = intersectedPolygons, threshold = 500)

deSliver <- function(x, threshold) {
  backToSf <- FALSE
  if (class(x)[1] == "sf") {
    backToSf <- TRUE
    x$tempArea <- as.numeric(st_area(x))
  } else {
    x<- st_as_sf(x)
    x$tempArea <- as.numeric(st_area(x))
  }

  #determine slivers by area
  xSlivers <- x[x$tempArea < threshold, ]
  xNotSlivers <- x[x$tempArea > threshold, ]
  if (nrow(xNotSlivers) < 1) {
    stop("Threshold exceeds the area of every polygon. Please select a smaller number")
  }

  #Split slivers from multipolygon, or nearest feature may be incorrect
  xSlivers <- suppressWarnings(st_cast(xSlivers, 'POLYGON'))

  #Find nearest non-sliver
  nearestFeature <- st_nearest_feature(xSlivers, xNotSlivers)

  #Merge each sliver polygon into nearest neighbour
  mergeSlivers <- lapply(
    unique(nearestFeature),
    FUN = function(i,
                   ns = xNotSlivers,
                   s = xSlivers,
                   nf = nearestFeature) {
      featurePolys <- nearestFeature == i
      xMerge <- s[featurePolys, ] %>%
        st_union(.)
      yMerge <- ns[i, ]
      #convert slivers back to multipolygon
      out <- sf::st_union(x = xMerge, y = yMerge) %>%
        as_Spatial(.)
      yMergeSpd <- sf::as_Spatial(yMerge)
      out <- SpatialPolygonsDataFrame(Sr = out,
                                      data = yMergeSpd@data,
                                      match.ID = FALSE)

      return(out)
    }
  )

  if (length(mergeSlivers) > 1) {
    m <- bind(mergeSlivers)
  } else {
    m <- mergeSlivers[[1]]
  }

  #Remove the temporary column
  m$tempArea <- NULL

  #remove self-intersecting geometries
  m <- gBuffer(spgeom = m, byid = TRUE, width = 0)

  if (backToSf) {
    m <- st_as_sf(m)
  }

  return(m)
}
