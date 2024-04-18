
source('plotThinning.R')

# Wrapper around graphicalExtremes::plotDanube
plotDanube <- function(
  ...,
  hideXTicks = FALSE,
  hideYTicks = FALSE,
  returnGGPlot = FALSE
){
  ggp <- graphicalExtremes::plotDanube(..., returnGGPlot = TRUE)
  # Hide ticks labels:
  # print(c(hideXTicks, hideYTicks))
  if(hideXTicks >= 1){
    ggp <- ggp + theme(
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank()
    )
  }
  if(hideYTicks >= 1){
    ggp <- ggp + theme(
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank()
    )
  }
  if(returnGGPlot){
    return(ggp)
  }
  plot(ggp)
  invisible(NULL)
}


# Wrapper around graphicalExtremes::plotFlights
plotFlights <- function(
  ...,
  map = 'state',
  returnGGPlot = FALSE,
  thinningRatio = 1,
  hideXTicks = FALSE,
  hideYTicks = FALSE
){
  dmap <- thinned_map_data(thinningRatio, map)
  ggp <- graphicalExtremes::plotFlights(..., map = dmap, returnGGPlot = TRUE)
  if(hideXTicks){
    ggp <- ggp + theme(
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank()
    )
  }
  if(hideYTicks){
    ggp <- ggp + theme(
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank()
    )
  }
  if(returnGGPlot){
    return(ggp)
  }
  plot(ggp)
  invisible(NULL)
}

# Function to plot empirical vs fitted parameters
plotEmpVsFitted <- function(
  emp,
  fitted,
  minVal=NULL,
  maxVal=NULL,
  makeSquare=FALSE,
  upperTri=TRUE,
  color=NULL,
  returnGGPlot = FALSE,
  xLabel=TRUE,
  yLabel=TRUE,
  xTicks=xLabel,
  yTicks=yLabel
){
  if(upperTri && is.matrix(emp) && is.matrix(fitted)){
    emp <- emp[upper.tri(emp)]
    fitted <- fitted[upper.tri(fitted)]
    if(!is.null(color) && is.matrix(color)){
      color <- color[upper.tri(color)]
    }
  }
  # Compute min/max values from data if not specified:
  if(length(minVal) == 0){
    minVal <- c(min(emp), min(fitted))
  }
  if(length(maxVal) == 0){
    maxVal <- c(max(emp), max(fitted))
  }
  # Use same min/max values for both axes if square plot specified:
  if(makeSquare){
    minVal <- min(minVal)
    maxVal <- max(maxVal)
  }
  # Make sure that minVal/maxVal have exactly two entries:
  minVal <- c(minVal, minVal)[1:2]
  maxVal <- c(maxVal, maxVal)[1:2]
  # Convert minVal/maxVal to axis limits:
  xlim <- c(minVal[1], maxVal[1])
  ylim <- c(minVal[2], maxVal[2])
  # Assign axis labels
  if(xLabel){
    xlab <- 'Empirical'
  } else{
    xlab <- NULL
  }
  if(yLabel){
    ylab <- 'Fitted'
  } else{
    ylab <- NULL
  }
  # Actually create the plot
  ggp <- (
    ggplot()
    + geom_point(
      aes(
        x = emp,
        y = fitted,
        col = color
      ),
      na.rm = TRUE
    )
    + geom_abline(slope = 1, intercept = 0)
    + xlab(xlab)
    + ylab(ylab)
  )
  # Set axis limits if specified
  if(!is.null(xlim)){
    ggp <- ggp + xlim(xlim)
  }
  if(!is.null(ylim)){
    ggp <- ggp + ylim(ylim)
  }
  # Remove axis ticks if specified
  if(!xTicks){
    ggp <- ggp + theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
  }
  if(!yTicks){
    ggp <- ggp + theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )
  }
  # Make plot square
  ggp <- ggp + theme(aspect.ratio = 1)
  # Return/plot ggplot
  if(returnGGPlot){
    return(ggp)
  }
  plot(ggp)
}

# Function to plot convergence of Theta-entries to zero
makeThetaPlot <- function(
  rholist,
  Thetas,
  nEdges=NULL,
  maxFinal=1e-6,
  minMaximal=0,
  minMaximalRel=0,
  returnGGPlot=FALSE
){
  indValid <- sapply(Thetas, is.matrix)
  if(all(!indValid)){
    return(NULL)
  } else if(any(!indValid)){
    indLast <- match(FALSE, indValid) - 1
    Thetas <- Thetas[seq_len(indLast)]
    rholist <- rholist[seq_len(indLast)]
    if(!is.null(nEdges)){
      nEdges <- nEdges[seq_len(indLast)]
    }
  }
  nThetas <- length(Thetas)
  entries <- sapply(Thetas, function(P) P[upper.tri(P)])
  maxAbsEntries <- apply(entries, 1, function(x) max(abs(x)))
  lastEntries <- abs(entries[,nThetas])
  summedEntries <- apply(entries, 1, function(x) sum(abs(x)))
  inds <- (
    maxAbsEntries >= minMaximal
    & maxAbsEntries >= (minMaximalRel * max(maxAbsEntries))
    & lastEntries <= maxFinal
  )
  if(!any(inds)){
    return(NULL)
  }
  alphas <- (maxAbsEntries / max(maxAbsEntries))^2
  dfs <- lapply(which(inds), function(i){
    data.frame(
      rho = rholist,
      Theta = entries[i,],
      alpha = alphas[i],
      ind = as.character(i)
    )
  })
  df <- data.frame(Reduce(rbind, dfs))
  ggp <- (
    ggplot(df, aes(x=rho, y=Theta, group=ind, alpha=alpha))
    + geom_line()
    + coord_cartesian(
        xlim = spreadInterval(rholist, 1, 1.05),
        ylim = spreadInterval(entries[inds,], 1.1),
        expand = FALSE
      )
    + xlab('$\\rho$')
    + ylab('$\\Theta$')
    + theme(legend.position = 'none')
  )
  if(!is.null(nEdges)){
    ggp <- (
      ggp
      + scale_x_continuous(sec.axis = sec_axis(
        name = 'Edges',
        trans = function(x) x,
        breaks = rholist,
        labels = nEdges
      ))
      + theme(axis.text.x.top = element_text(angle=45))
    )
  }

  if(returnGGPlot){
    return(ggp)
  }
  plot(ggp)
  return(invisible(NULL))
}

# Utility function to make an interval wider around its centerpoint
spreadInterval <- function(interval, spreadByMin=1, spreadByMax=spreadByMin){
  interval <- range(interval)
  mid <- mean(interval)
  radius <- diff(interval)/2
  mid + radius * c(-spreadByMin, spreadByMax)
}


