

## Used to compute univariate shape parameters
## Needs to be called from/after main.R!


computeShapeParams <- function(mat){
  IATAs <- colnames(mat)
  nAirports <- ncol(mat)
  shapeParams <- numeric(nAirports)
  names(shapeParams) <- IATAs
  for(i in seq_len(nAirports)){
    cat(i, '/', nAirports, '...\n', sep = '')
    IATA <- IATAs[i]
    univData <- mat[,IATA]
    univData <- univData[!is.na(univData)]
    threshold <- quantile(univData, p)
    ret <- fevd(
      x = univData,
      type = 'GP',
      threshold = threshold
    )
    shapeParams[IATA] <- ret$results$par['shape']
  }
  return(shapeParams)
}

computeShapeParams <- myMemoise(computeShapeParams)

shapeParams <- computeShapeParams(mat_c)





# Plot marginals as color:
ggp <- (
  plotFlights(
    airports_sel = airports_sel_c,
    plotConnections = FALSE,
    vertexColors = shapeParams,
    map = 'state',
    thinningRatio = mapThinningRatio,
    returnGGPlot = TRUE,
    useAirportNFlights = TRUE,
    useConnectionNFlights = FALSE,
    useLatex = TRUE,
    minNFlights = minNFlights
  )
  + scale_colour_gradient2(
    # limits = c(-1, 1) * max(abs(shapeParams)),
    limits = c(-0.1, 0.1),
    low = '#00ffff',
    mid = '#000000',
    high = '#ff8800',
    midpoint = 0
  )
  + theme(legend.position = "none")
)

myTikz(
  'marginals',
  width=8,
  height=5,
  plot(ggp)
)




