#' Ward Seed Dispersal kernel
#'
#' A probability distribution used in LANDIS-II.
#'
#' @export
#' @docType methods
#'
#' @author Eliot McIntire
#'
#' @name Ward
#' @rdname Ward
Ward <- expression(if (cellSize <= effDist) {
  ifelse(
    dis <= effDist,
    exp((dis - cellSize) * log(1 - k) / effDist) -
      exp(dis * log(1 - k) / effDist),
    (1 - k) * exp((dis - cellSize - effDist) * log(b) / maxDist) -
      (1 - k) * exp((dis - effDist) * log(b) / maxDist)
  )
} else {
  ifelse(
    dis <= cellSize,
    exp((dis - cellSize) * log(1 - k) / effDist) - (1 - k) *
      exp((dis - effDist) * log(b) / maxDist),
    (1 - k) * exp((dis - cellSize - effDist) * log(b) / maxDist) -
      (1 - k) * exp((dis - effDist) * log(b) / maxDist)
  )
})

#' WardFast Seed Dispersal kernel
#'
#' A probability distribution used in LANDIS-II.
#'
#' @export
#' @docType methods
#'
#' @author Eliot McIntire
#'
#' @name WardFast
#' @rdname WardFast
WardFast <- expression(ifelse(cellSize <= effDist, {
  ifelse(
    dis <= effDist,
    exp((dis - cellSize) * log(1 - k) / effDist) -
      exp(dis * log(1 - k) / effDist),
    (1 - k) * exp((dis - cellSize - effDist) * log(b) / maxDist) -
      (1 - k) * exp((dis - effDist) * log(b) / maxDist)
  )
} , {
  ifelse(
    dis <= cellSize,
    exp((dis - cellSize) * log(1 - k) / effDist) - (1 - k) *
      exp((dis - effDist) * log(b) / maxDist),
    (1 - k) * exp((dis - cellSize - effDist) * log(b) / maxDist) -
      (1 - k) * exp((dis - effDist) * log(b) / maxDist)
  )
}))

#' Simulate a LANDIS-II dispersal process on a landscape.
#'
#' Simulate seed dispersal using user defined function. This is a "receiving
#' pixel" focused dispersal approach. It is the "potentially receiving" cell
#' that looks around itself for potential seed sources. If it finds a single
#' seed source, that passes the probability function described by the
#' dispersalFn, then the cluster ends and the receiving cell index is returned
#' as part of a vector of indices of all successfully cells that received seeds.
#' This function can therefore only be used for a relatively specific situation
#' where there is a yes/no returned for each potential receiving cell, i.e., not
#' abundance. This function is also not cumulative, i.e,. there is no higher
#' abundance of seeds received if a receiving cell has lots of seed sources
#' around it vs. a single seed source. The difference will come with a higher
#' probability of successfully receiving a "seed".
#'
#' \code{dispersalFn} must be an expression that returns a probability
#' distribution. Because it is a dispersal kernal, it must be a probability
#' distribution. The expression that can take an argument named "dis" (without
#' quotes) as this will be calculated internally and represents the distance
#' from the initial (receiving) pixel and all active pixels within that cluster
#' of active pixels. \code{SpaDES} includes the \code{\link{Ward}} kernel as
#' defined in the LANDIS-II documentation.
#'
#' @param sim A simList object
#'
#' @param dtSrc data.table
#'
#' @param dtRcv data.table
#'
#' @param pixelGroupMap map
#'
#' @param species Landis object from initial species file
#'
#' @param dispersalFn  An expression that can take a "dis" argument. See details. Default is "Ward"
#'
#' @param plot.it  If TRUE, then plot the raster at every iteraction, so one can watch the
#' LANDISDisp event grow.
#' @param effDist Landis species- and ecoregion-specific effective distance parameter
#'
#' @param maxDist  Landis species- and ecoregion-specific effective distance parameter
#'
#' @param b  Landis ward seed dispersal calibration coefficient (set to 0.01 in Landis)
#'
#' @param k  Landis ward seed dispersal the probability that seed will disperse within
#' the effective distance (eg., 0.95)
#'
#' @param maxPotentialsLength numeric, number of unique pixels to treat simultaneously. Smaller reduces memory use.
#' @param successionTimestep integer. The time in timeunits between succession (i.e., dispersal) events.
#'
#' @param verbose Logical. Whether a somewhat verbose output to screen occurs. For debugging.
#'
#' @param useParallel ANY. if the class of this arguement is logical, whether the function creates the cluster based on TRue and FALSE.
#'                         if the class of this arguement is cluster, the function will use this cluster to implement the parallel computation.
#'
#' @param ...   Additional parameters. Currently none
#'
#' @return A numeric vector of raster pixel indices, in the same resolution and extent as
#' \code{seedSrc} raster.
#'
#' @import data.table
#' @import dplyr
#' @import raster
#' @importFrom R.utils intToBin
#' @export
#' @docType methods
#'
#' @author Eliot McIntire
#'
#' @name LANDISDisp
#' @rdname LANDISDisp
#'
#' @examples
#' library(raster)
#' library(SpaDES.tools)
#'
#' # Make random forest cover map
#' a <- raster(extent(0, 1e4, 0, 1e4), res = 100)
#' hab <- gaussMap(a, speedup = 1) # if raster is large (>1e6 pixels), use speedup>1
#' names(hab) <- "hab"
#'
#' seedSrc <- hab > 5
#' setColors(seedSrc, 1) <- c("white", "black")
#'
#' seedRcv <- hab > 5
#' system.time(seeds <- LANDISDisp(seedSrc, seedRcv = seedRcv, maxDist = 250, plot.it = TRUE))
#' seedRcvRaster <- raster(seedSrc)
#' if (length(seeds) > 0) {
#'   seedRcvRaster[seeds] <- 1
#'   Plot(seedRcvRaster, cols = "black")
#' }
#'
LANDISDisp <- compiler::cmpfun(function(sim, dtSrc, dtRcv, pixelGroupMap, species,
                                        dispersalFn = WardFast, b = 0.01, k = 0.95, plot.it = FALSE,
                                        maxPotentialsLength = 1e5, successionTimestep,
                                        verbose = getOption("LandR.verbose", TRUE),
                                        useParallel, ...) {
    cellSize <- res(pixelGroupMap) %>% unique()
    if (length(cellSize) > 1) {
      ## check for equal cell sizes that "aren't" due to floating point error
      res <-
        vapply(cellSize, function(x)
          isTRUE(all.equal(x, cellSize[1])), logical(1))
      if (all(res))
        cellSize <- cellSize[1]
    }
    seedsReceived <- raster(pixelGroupMap)
    seedsReceived[] <- 0L

    # NOTE new as.integer for speciesCode -- it is now a factor
    sc <- species[, list(
      speciesCode = as.integer(speciesCode),
      effDist = seeddistance_eff,
      maxDist = seeddistance_max
    )]
    # sc <- species %>%
    #   dplyr::select(speciesCode, seeddistance_eff, seeddistance_max) %>%
    #   rename(effDist = seeddistance_eff, maxDist = seeddistance_max) %>%
    #   mutate(speciesCode = as.integer(speciesCode)) %>%
    #   data.table()
    dtSrc[, speciesCode := as.integer(speciesCode)]
    dtRcv[, speciesCode := as.integer(speciesCode)]

    setkey(sc, speciesCode)
    setkey(dtSrc, speciesCode)
    setkey(dtRcv, speciesCode)

    # speciesSrcPool <- sc[dtSrc] %>%
    #   group_by(pixelGroup) %>%
    #   summarise(speciesSrcPool = sum(2 ^ speciesCode)) %>%
    #   data.table(key = "pixelGroup") %>%
    #   na.omit()

    speciesSrcPool <-
      sc[dtSrc][, list(speciesSrcPool = sum(2 ^ speciesCode)), by = "pixelGroup"]
    setkeyv(speciesSrcPool, "pixelGroup")
    speciesSrcPool <- na.omit(speciesSrcPool)

    # speciesRcvPool <- sc[dtRcv] %>%
    #   group_by(pixelGroup) %>%
    #   summarise(speciesRcvPool = sum(2 ^ speciesCode)) %>%
    #   data.table(key = "pixelGroup") %>%
    #   na.omit()

    speciesRcvPool <-
      sc[dtRcv][, list(speciesRcvPool = sum(2 ^ speciesCode)), by = "pixelGroup"]
    setkeyv(speciesRcvPool, "pixelGroup")
    speciesRcvPool <- na.omit(speciesRcvPool)

    setkey(sc, speciesCode)
    spPool <- merge(speciesRcvPool, speciesSrcPool, all = TRUE)
    seedSourceMaps <- rasterizeReduced(
      spPool,
      fullRaster = pixelGroupMap,
      mapcode = "pixelGroup",
      newRasterCols = c("speciesSrcPool", "speciesRcvPool")
    )
    # seedSourceMap <- rasterizeReduced(speciesSrcPool, fullRaster = pixelGroupMap,
    #                                   mapcode = "pixelGroup", newRasterCols = "speciesSrcPool")
    # seedReceiveMap <- rasterizeReduced(speciesRcvPool, fullRaster = pixelGroupMap,
    #                                    mapcode = "pixelGroup", newRasterCols = "speciesRcvPool")
    seedSourceMaps <-
      lapply(seedSourceMaps, function(x)
        setValues(x, as.integer(x[])))

    seedRcvOrig <- which(!is.na(seedSourceMaps$speciesRcvPool[]))
    seedSrcOrig <- which(seedSourceMaps$speciesSrcPool[] > 0)

    xysAll <-
      xyFromCell(seedSourceMaps$speciesSrcPool,
                 1:ncell(seedSourceMaps$speciesSrcPool))

    if (is.null(seedRcvOrig))  {
      # start it in the centre cell
      activeCell <- (nrow(seedSrcOrig) / 2L + 0.5) * ncol(seedSrcOrig)
    }
    if (length(cellSize) > 1)
      stop("pixelGroupMap resolution must be same in x and y dimension")
    ### should sanity check map extents
    if (plot.it) {
      wardSeedDispersalHab1 <- raster(seedSourceMaps$speciesSrcPool)
      wardSeedDispersalHab1[] <- NA
      Plot(wardSeedDispersalHab1, new = TRUE)
    }

    seedSourceMaps$speciesSrcPool <-
      NULL # don't need once xysAll are gotten

    lociReturn <- data.table(fromInit = seedRcvOrig, key = "fromInit")

    #            scFull <- sc[speciesComm(getValues(seedSourceMap))] %>%
    #              setkey(.,pixelIndex)
    potentialsOrig <- data.table(
      "fromInit" = seedRcvOrig,
      "RcvCommunity" = seedSourceMaps$speciesRcvPool[][seedRcvOrig],
      key = "fromInit"
    )
    potentialsOrig[, from := fromInit]

    #maxPotentialsLength <- 3e3
    nPotentials <- length(seedRcvOrig)

    if (nPotentials > maxPotentialsLength) {
      subSamp <- ceiling(nPotentials / maxPotentialsLength)
    } else {
      subSamp <- 1
    }
    ultimateMaxDist <- max(dtRcv$seeddistance_max)
    splitFactor <-
      sort(rep(seq_len(subSamp), length.out = nPotentials))
    subSampList <- purrr::transpose(list(
      activeCell = split(seedRcvOrig, splitFactor),
      potentials = split(potentialsOrig, splitFactor)
    ))

    if (verbose > 0)
      message("  Seed dispersal: starting ", length(subSampList), " groups")
    if (is.logical(useParallel) | is.numeric(useParallel)) {
      if (isTRUE(useParallel)) {
        numCores <- min(length(subSampList), parallel::detectCores() - 1)
        if (Sys.info()[["sysname"]] == "Windows") {
          cl <- parallel::makeCluster(numCores)
          allarguementsInMainFunc <- formalArgs(seedDispInnerFn)
          parallel::clusterExport(
            cl,
            c(
              "seedDispInnerFn",
              "data.table",
              "%>%",
              "setkey",
              "adj",
              "%<=%",
              "set",
              "rbindlist",
              "%<<%",
              allarguementsInMainFunc[!(allarguementsInMainFunc %in% c("activeCell", "potentials", "n"))]
            ),
            envir = environment()
          )
        } else {
          cl <- parallel::makeForkCluster(numCores)
        }
        if (verbose > 0)
          message("Running seed dispersal in parallel on ",
                  length(cl),
                  " clusters")
        seedsArrived <- parallel::parLapply(cl, subSampList,
                                            function(y)
                                              seedDispInnerFn(
                                                activeCell = y[[1]],
                                                potentials = y[[2]],
                                                n = cellSize,
                                                speciesRcvPool,
                                                sc,
                                                pixelGroupMap,
                                                ultimateMaxDist,
                                                cellSize,
                                                xysAll,
                                                dtSrc,
                                                dispersalFn,
                                                k,
                                                b,
                                                lociReturn,
                                                speciesComm,
                                                pointDistance,
                                                successionTimestep = successionTimestep
                                              )) %>%
          rbindlist()
        parallel::stopCluster(cl)
      } else {
        allSeedsArrived <- list()
        if (verbose > 0)
          message("  Seed dispersal should be using more than 100% CPU because of data.table openMP use")

        for (y in seq_along(subSampList)) {
          curThreads <- getDTthreads()
          if (useParallel != curThreads) {
            a <- if (isFALSE(useParallel))
              data.table::setDTthreads(1)
            else {
              if (isTRUE(useParallel))
                data.table::setDTthreads(0)   ## use all threads
              else
                data.table::setDTthreads(useParallel)
            }
            on.exit(data.table::setDTthreads(a), add = TRUE)
          }
          allSeedsArrived[[y]] <- seedDispInnerFn(
            activeCell = subSampList[[y]][[1]],
            potentials = subSampList[[y]][[2]],
            n = cellSize,
            speciesRcvPool,
            sc,
            pixelGroupMap,
            ultimateMaxDist,
            cellSize,
            xysAll,
            dtSrc,
            dispersalFn,
            k,
            b,
            lociReturn,
            speciesComm,
            pointDistance,
            successionTimestep = successionTimestep
          )
        }
        if (verbose > 0)
          message("    End of using more than 100% CPU because of data.table openMP use")
        seedsArrived <- rbindlist(allSeedsArrived)
      }
    } else if (is(useParallel, "cluster")) {
      if (!all(unlist(lapply(useParallel, function(x)
        is(x, "forknode"))))) {
        allarguementsInMainFunc <- formalArgs(seedDispInnerFn)
        parallel::clusterExport(
          useParallel,
          c(
            "seedDispInnerFn",
            "data.table",
            "%>%",
            "setkey",
            "adj",
            "%<=%",
            "set",
            "rbindlist",
            "%<<%",
            allarguementsInMainFunc[!(allarguementsInMainFunc %in%
                                        c("activeCell", "potentials", "n"))]
          ),
          envir = environment()
        )
      }
      if (verbose > 0)
        message("Running seed dispersal in parallel on ",
                length(useParallel),
                " clusters")
      reqdPkgs <-
        unlist(depends(sim)@dependencies[[current(sim)[["moduleName"]]]]@reqdPkgs)
      parallel::clusterExport(useParallel, c("reqdPkgs"), envir = environment())
      clusterEvalQ(cl = useParallel, {
        SpaDES.core:::.modifySearchPath(reqdPkgs, removeOthers = FALSE)
      })
      seedsArrived <- parallel::parLapplyLB(useParallel, subSampList,
                                            function(y)
                                              seedDispInnerFn(
                                                activeCell = y[[1]],
                                                potentials = y[[2]],
                                                n = cellSize,
                                                speciesRcvPool,
                                                sc,
                                                seedSourceMaps$speciesRcvPool,
                                                ultimateMaxDist,
                                                cellSize,
                                                xysAll,
                                                dtSrc,
                                                dispersalFn,
                                                k,
                                                b,
                                                lociReturn,
                                                speciesComm,
                                                pointDistance,
                                                successionTimestep = successionTimestep
                                              )) %>%
        rbindlist()
    } else {
      stop(
        "Please specify the useParallel argument correctly. ",
        "Currently, it takes either numeric, logical or cluster object"
      )
    }

    # COnvert speciesCode back to factor using original species object from top of this fn
    seedsArrived[, speciesCode := species$speciesCode[speciesCode]]
    setnames(seedsArrived, "fromInit", "pixelIndex")
    return(seedsArrived)
  })

speciesComm <- function(num, sc) {
  indices <- lapply(strsplit(R.utils::intToBin(num), split = ""), function(x) {
    rev(as.logical(as.numeric(x)))
  })

  speciesCode <- lapply(indices, function(x) (seq_len(length(x)) - 1)[x])

  data.table(RcvCommunity = as.integer(rep(num, sapply(speciesCode, length))),
             speciesCode = unlist(speciesCode),
             key = "speciesCode")[!is.na(speciesCode)] %>%
    sc[.]
}

ringCells <- compiler::cmpfun(function(x, index, minDist, maxDist) {
  ras <- raster(x)
  ras[index] <- 1
  xOuter <- buffer(ras, width = maxDist)
  xInner <- buffer(ras, width = minDist)
  xOuter[xInner == 1] <- NA
  return(Which(xOuter == 1, cells = TRUE))

  #  xOuter[xInner==1] <- NA
  #  return(xOuter)
})

# ringWeight <- function(x, minDist, maxDist) {
#   b = focalWeight(x, minDist, "circle")
#   dis = focalWeight(x, maxDist, "circle")
#   colsRmv <- (ncol(dis)-ncol(b))/2
#   indices <- (1:ncol(dis))[-c((1:colsRmv),(ncol(dis)-colsRmv+1))]
#   keep <- expand.grid(indices, indices) %>% as.matrix
#   dis[keep] <- pmax(0,dis[keep]-b)
#
#   aRas <- raster(dis, xmn=0, ymn=0, xmx=ncol(dis)*res(x)[1], ymx=ncol(dis)*res(x)[1])
#
#   p1 <- xyFromCell(aRas,ceiling(ncell(dis)/2))
#   p2 <- xyFromCell(aRas,Which(aRas>0,cell=TRUE))
#   dis[dis>0] <- pointDistance(p1,p2,lonlat=FALSE)
#   dis[dis==0] <- NA
#   return(dis)
# }

WardEqn <- compiler::cmpfun(function(dis, cellSize, effDist, maxDist, k, b) {
  if (cellSize %<=% effDist) {
    ifelse(
      dis %<=% effDist,
      exp((dis - cellSize) * log(1 - k) / effDist) -
        exp(dis * log(1 - k) / effDist),
      (1 - k) * exp((dis - cellSize - effDist) * log(b) / maxDist) -
        (1 - k) * exp((dis - effDist) * log(b) / maxDist)
    )
  } else {
    ifelse(
      dis %<=% cellSize,
      exp((dis - cellSize) * log(1 - k) / effDist) - (1 - k) *
        exp((dis - effDist) * log(b) / maxDist),
      (1 - k) * exp((dis - cellSize - effDist) * log(b) / maxDist) -
        (1 - k) * exp((dis - effDist) * log(b) / maxDist)
    )
  }
})

# WardFn <- function(dis, maxDist=3000, effDist=30, ...) {
#   #effDist = 100
#   #maxDist = 150
#   dis[dis==0] <- NA
#   dis <- as.numeric( na.omit(dis))
#   cellSize = 100
#   k = 0.95
#   b = 0.01
#   nr <- length(dis)
#   if(nr>0) {
#     e = runif(nr)<WardEqn(dis=dis, cellSize=100, effDist=effDist, maxDist=maxDist, k=0.95, b=0.01)
#   } else {
#     e <- FALSE
#   }
#   return(any(e))
# }
#
#
# #effDist=species[species==speciesCode,seeddistance_eff]
#
# #seedSourceMaps$speciesRcvPool[seedSourceMaps$speciesRcvPool==0] <- NA
#
# seedSourceMaps$speciesRcvPool = pixelGroupMap %in% seedReceive[species==speciesCode]$pixelGroup
# seedSourceMap[is.na(seedSourceMap)] <- 0
# Plot(seedSourceMap, cols=c("white","light grey"), new=TRUE)
#
# seedReceiveMap2 <- raster(seedSourceMaps$speciesRcvPool)
# seedReceiveMap2[] <- 0
#
# st <- system.time({
# for(i in 1:30*100) {
#   a  <-  focal(seedSourceMap, w=ringWeight(seedSourceMap,i-100,i), fun=WardFn, pad=TRUE, padValue=0)
#   a[seedSourceMaps$speciesRcvPool==0] <- 0
# #  Plot(a)
#   seedSourceMaps$speciesRcvPool <- seedSourceMaps$speciesRcvPool*(a==0)
#   seedReceiveMap2[a==1] <- seedReceiveMap2[a==1] + i
# }
# })
#
# Plot(seedReceiveMap2)
# seedSourceMaps$speciesRcvPool = pixelGroupMap %in% seedReceive[species==speciesCode]$pixelGroup
# Plot(seedReceiveMap2, addTo="seedSourceMap", zero.color="#00000000")
# Plot(seedReceiveMap2, seedSourceMaps$speciesRcvPool)
# Plot(seedReceiveMap2, new=T)
# Plot(seedSourceMap, addTo="seedReceiveMap2", zero.color="#00000000", cols="#FFFFFF11")
#
# Plot(seedSourceMap, new=T, cols=c("white","black"))
# Plot(seedReceiveMap2, addTo="seedSourceMap", zero.color="#00000000", cols="#11FF1155")
# Plot(seedSourceMaps$speciesRcvPool, cols=c("white","black"))
# Plot(seedReceiveMap2, addTo="seedSourceMaps$speciesRcvPool", zero.color="#00000000", cols="#11881100")
#
# h <- raster(seedSourceMaps$speciesRcvPool)
# h[]=0
# h[seedingData[species=="querrubr",pixelIndex]] <- 1
# Plot(h)

#' @inheritParams LANDISDisp
seedDispInnerFn <- compiler::cmpfun(function(activeCell, potentials, n, speciesRcvPool, sc,
                                             pixelGroupMap, ultimateMaxDist, cellSize, xysAll,
                                             dtSrc, dispersalFn, k, b, lociReturn, speciesComm,
                                             pointDistance, successionTimestep,
                                             verbose = getOption("LandR.verbose", TRUE)) {
    if (verbose > 0)
      message("  Dispersal for pixels ", min(activeCell), " to ", max(activeCell))

    seedsArrived <- data.table(
      fromInit = integer(),
      speciesCode = integer(),
      key = c("fromInit", "speciesCode")
    )
    # Go to species level
    spRcvCommCodes <-
      speciesComm(unique(speciesRcvPool$speciesRcvPool), sc = sc)
    #spRcvCommCodes <- speciesComm(unique(potentials$RcvCommunity))
    setkey(spRcvCommCodes, RcvCommunity)
    setkey(potentials, RcvCommunity)

    # Make potentials have all Rcv pixels, with each species as unique line
    potentials = spRcvCommCodes[potentials, allow.cartesian = TRUE][, ':='(RcvCommunity = NULL)]
    setkey(potentials, "from")

    ## identify which are 8 neighbours from each "active cell";
    ## don't need internal sort because data.table will do in 2nd line
    adjCells <- adj(
      pixelGroupMap,
      unique(activeCell),
      directions = 8,
      pairs = TRUE,
      include = TRUE,
      sort = FALSE
    ) %>%
      data.table(key = c("from"))
    set(adjCells, NULL, "from", as.integer(adjCells$from))
    set(adjCells, NULL, "to", as.integer(adjCells$to))
    #adjCells[, ':='(from = as.integer(from), to = as.integer(to))]
    startTime <- Sys.time()
    ## while there are active cells and less than maxDistance:
    while (NROW(potentials) &&
           ((n - cellSize) %<=% ultimateMaxDist)) {
      if (n > cellSize) {
        potentials[, ':='(from = NULL)][, from := to][, ':='(to = NULL, dis = NULL)]
        setkey(potentials, "from")
      }
      ################ original
      # join these to the potentials object
      potentials <-
        potentials[adjCells, allow.cartesian = TRUE, nomatch = 0] %>%
        unique(by = c("fromInit", "to", "speciesCode"))
      # if (anyNA(potentials$fromInit)) {
      #   potentials <- potentials[!is.na(fromInit),]
      # }

      # because there will be duplicate "from - to" pairs, say from 2 different species, only calculate
      #   distance once, then re-join the shorter version back to longer version by species
      shortPotentials <- setkey(potentials, fromInit, to) %>%
        unique(., by = c("fromInit", "to")) %>%
        .[, list(fromInit, to)]
      set(shortPotentials, NULL, "dis", as.integer(pointDistance(xysAll[shortPotentials$fromInit,],
                                                                 xysAll[shortPotentials$to,],
                                                                 lonlat = FALSE)))

      # merge shorter object with no duplicate from-to pairs back with potentials,
      # which does have duplicate from-to pairs due to multiple species having same from-to pair
      if (n - cellSize == 0) {
        # the first loop incudes on site regeneration
        potentials <-
          shortPotentials[((n - cellSize) %<=% dis) & (dis %<=% n),][potentials, nomatch = 0][(dis %<=% maxDist),]
      } else {
        potentials <-
          shortPotentials[((n - cellSize) %<<% dis) & (dis %<=% n),][potentials, nomatch = 0][(dis %<=% maxDist),]
      }

      if (NROW(potentials) > 0) {
        dtSrcShort <- dtSrc[, list(pixelGroup, speciesCode)]
        set(potentials, NULL, "pixelGroup", pixelGroupMap[][potentials$to])
        setkey(dtSrcShort, speciesCode, pixelGroup)
        setkey(potentials, speciesCode, pixelGroup)

        potentialsWithSeedDT <- potentials[dtSrcShort, nomatch = 0]
        if (NROW(potentialsWithSeedDT) > 0) {
          set(potentialsWithSeedDT, NULL, "receivesSeeds", NA)
          nr <- NROW(potentialsWithSeedDT)

          # back to Ward
          # Don't include the ones that were already, calculate probability
          whDisZero <- which(potentialsWithSeedDT$dis == 0)
          if (length(whDisZero)) {
            potentialsWithSeedDT[whDisZero, dispersalProb := 1]
            potentialsWithSeedDT[-whDisZero, dispersalProb := eval(dispersalFn)]
          } else {
            potentialsWithSeedDT[, dispersalProb := eval(dispersalFn)]
          }

          # Eliot -- Jan 11, 2019 -- adjust based on successionTimestep --
          #  seeds can disperse every year in reality, their probabilities must be scaled
          #  to timestep
          potentialsWithSeedDT[, dispersalProb := 1 - (1 - dispersalProb) ^ successionTimestep]

          potentialsWithSeedDT <- potentialsWithSeedDT[, .(receivesSeeds = runif(nr) < dispersalProb,
                                                           fromInit,
                                                           speciesCode)]
          receivedSeeds <- potentialsWithSeedDT[, any(receivesSeeds), by = c("fromInit,speciesCode")]
          setkey(receivedSeeds, fromInit, speciesCode)

          #drop any that received seeds from potentials, as they are now in lociReturn
          if (NROW(receivedSeeds[V1 == TRUE]) > 0)  {
            seedsArrived <- rbindlist(list(seedsArrived,
                                           lociReturn[receivedSeeds[V1 == TRUE]][, V1 := NULL]))
            setkey(seedsArrived, fromInit, speciesCode)
            setkey(potentials, fromInit, speciesCode)
            potentials <- potentials[!receivedSeeds[V1 == TRUE]]
            #potentials <- potentials[!receivedSeeds[V1 == TRUE]]
          }
          #potentials <- specPotentials[, list(RcvCommunity = sum(2^speciesCode)), by = c("fromInit", "from", "to")]
        }
      }
      midTime <- Sys.time()
      if (verbose > 1)
        message("    Dispersal for ",
                n,
                " m from source. Elapsed time: ",
                midTime - startTime)
      n <- n + cellSize
      # refresh so that "to" cells become new "from" cells
      activeCell <- potentials[, to]
    }
    return(seedsArrived)
})
