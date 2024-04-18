# Helper functions used to thin out map data in the background of plots
# Helps reduce file size of created tikz/pdf files

# Returns a thinned version of map_data to keep tikz/pdf size small
thinned_map_data <- function(thinningRatio = 1, map, region = ".", exact = FALSE, ...){
  dmap <- map_data(map, region, exact, ...)
  if(thinningRatio < 1){
    dmap <- thinPoints(dmap, thinningRatio)
  }
  return(dmap)
}

thinPoints <- function(dmap, p, k=NULL){
  cols0 <- colnames(dmap)
  y <- dmap$y
  x <- dmap$x
  dmap <- computeCols(dmap)
  n <- nrow(dmap)
  if(is.null(k)){
    k <- floor((1-p) * n)
  }
  kVec <- c()
  kNew <- 1
  while(k > 0){
    kVec <- c(kVec, kNew)
    k <- k - kNew
    kNew <- min(2*kNew, k, 1000)
  }
  kVec <- sort(kVec, decreasing = TRUE)
  for(i in seq_along(kVec)){
    k <- kVec[i]
    ord <- order(dmap$stat)
    ind <- ord[1:k]
    dmap <- removePoints(dmap, ind)
  }
  dmap <- dmap[cols0]
  return(dmap)
}

computeCols <- function(dmap){
  x <- dmap$long
  y <- dmap$lat
  n <- length(x)
  coords <- cbind(x, y)
  diffs <- rbind(
    diff(coords),
    NA
  )
  lens <- sqrt(rowSums(diffs^2))
  diffs1 <- diffs / cbind(lens, lens)
  sprods <- rowSums(
    diffs1 * rbind(
      NA,
      diffs1[-n,]
    )
  )
  angles <- suppressWarnings(acos(sprods))
  stats <- angles * lens * c(NA, lens[-n])
  dmap$angle <- angles
  dmap$len <- lens
  dmap$stat <- stats
  return(dmap)
}

removePoints <- function(dmap, ind){
  dmap <- dmap[-ind,]
  dmap <- computeCols(dmap)
  return(dmap)
}
