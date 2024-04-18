
## Must be called from `../main.R`!
## Code used for the flights application
## Can take quite some time to run
tictoc::tic()

set.seed(1)

setTikzSubDir('application')

# Source other files:
source('makeMacroFile.R')
source('makeIataTable.R')
source('plotting.R')


# Files to store IATAs
TEX_NAME_CLUSTER_IATAS <- myPath(EXAMPLES_DIR, 'IATAS_cluster.tex')
TEX_NAME_ALL_IATAS <- myPath(EXAMPLES_DIR, 'IATAS_all.tex')

# Use fewer points to draw map (-> smaller .pdf files)
mapThinningRatio <- 0.3

# Parameter p for thresholding etc.:
p <- 0.95
p_c <- p # Use different p for clustering?
p_marginals <- p # Use different p for univariate marginals?

# Number of clusters for clustering:
nClusters <- 4

# Airport whose cluster to use:
IATA_choose_cluster <- 'HOU'

# Load data
cat('Loading data... ')
delays <- flights$delays[,,'arrivals'] + flights$delays[,,'departures']
connections <- flightCountMatrixToConnectionList(flights$flightCounts, directed = FALSE)
airports <- flights$airports
dates <- as.Date(rownames(flights$delays))
pairwiseFlightCounts <- apply(flights$flightCounts, c(1,2), sum)
airports$nFlights <- colSums(pairwiseFlightCounts) + rowSums(pairwiseFlightCounts)
cat('done.\n')


# Specify time range for train/test:
date0Est <- as.Date('2005-01-01')
date1Est <- as.Date('2010-12-31')
date0Val <- date1Est + 1
date1Val <- as.Date('2020-12-31')
date0All <- min(date0Est, date0Val)
date1All <- max(date1Est, date1Val)
datesEstInd <- (
  date0Est <= dates
  & dates <= date1Est
)
datesEst_sel <- dates[datesEstInd]
datesValInd <- (
  date0Val <= dates
  & dates <= date1Val
)
datesVal_sel <- dates[datesValInd]
if(any(datesEstInd & datesValInd)){
  stop('Estimation and validation dates must not intersect!')
}
datesAllInd <- datesEstInd | datesValInd
datesAll_sel <- dates[datesAllInd]

# Filter out routes with no regular flights (at least 1/month):
minMonthlyConnectionFlights <- 1
minNFlights <- length(dates)/30.5 * minMonthlyConnectionFlights

## Specify airports:
# Use only airports with least ... flights per year:
minYearlyAirportFlights <- 1000
ind <- airports$nFlights >= length(dates)/365.25 * minYearlyAirportFlights
# Only mainland US:
ind <- (
  ind
  & airports$Latitude > 24
  & airports$Latitude < 50
  & airports$Longitude < -60
  & airports$Longitude > -130
)
# Remove NAs
ind[is.na(ind)] <- FALSE
# Only <1000 NAs:
ind[ind] <- ind[ind] & (
  apply(is.na(delays[format(datesAll_sel),airports[ind,'IATA']]), 2, sum) < 1000
)


# Select airports/delays for clustering (..._c)
airports_sel_c <- airports[ind,]
IATAS_sel_c <- airports_sel_c$IATA
mat_c <- delays[format(datesAll_sel),IATAS_sel_c]

# Write IATAs used in clustering to .tex file
cat(makeIataTable(IATAS_sel_c), file=TEX_NAME_ALL_IATAS)

# Make sure enough/the right airports are included in clustering
if(!IATA_choose_cluster %in% IATAS_sel_c){
  stop('Chosen airport for cluster selection (', IATA_choose_cluster, ') not in selected airports!')
}
if(length(IATAS_sel_c) <= nClusters){
  stop('Number of clusters (', nClusters, ') must be smaller than number of selected airports (', length(IATAS_sel_c), ')!')
}


# Plot univariate marginals:
cat('Plotting marginals...\n')
source('marginals.R')


# Clustering to select airports
cat('Computing full chi/Gamma...\n')
# Due to the high number of NAs, we compute the chi for clustering based only on pairwise data
chi_emp_c <- emp_chi_pairwise(mat_c, p=p_c, verbose=TRUE)
dimnames(chi_emp_c) <- list(IATAS_sel_c, IATAS_sel_c)
diss <- (-chi_emp_c)

cat('Clustering & plotting...\n')
kmed <- pam(
  diss,
  diss = TRUE,
  k = nClusters,
  medoids = 'random',
  nstart = 10,
  variant = 'original'
)
ind_chosen_cluster <- kmed$clustering[IATA_choose_cluster]
IATA_chosen_cluster <- kmed$medoids[ind_chosen_cluster]
chosen_IATAS_c <- names(which(kmed$clustering == ind_chosen_cluster))

connections_sel_c <- connections %>% filter(
  departureAirport %in% IATAS_sel_c
  & arrivalAirport %in% IATAS_sel_c
  & nFlights >= minNFlights
)
connections_in_cluster <- connections_sel_c %>% filter(
  kmed$clustering[departureAirport] == kmed$clustering[arrivalAirport]
)


# Plot clusters:
nPlotCols <- 2
if(nClusters == 4){
  clusterOrder <- c(2, 4, 1, 3)
} else {
  clusterOrder <- seq_len(nClusters)
}
ggps <- lapply(seq_along(clusterOrder), function(j){
  (
    plotFlights(
      airports_sel = airports_sel_c,
      airportIndices = (kmed$clustering == clusterOrder[j]),
      map = 'state',
      thinningRatio = mapThinningRatio,
      returnGGPlot = TRUE,
      useAirportNFlights = TRUE,
      useConnectionNFlights = FALSE,
      minNFlights = minNFlights,
      useLatex = TRUE,
      hideXTicks = (!(j >= nClusters - nPlotCols)),
      hideYTicks = (!((j %% nPlotCols) != 1))
    )
    + theme(legend.position = "none")
  )
})
# Plot clusters:
ggp <- Reduce(`+`, ggps) + patchwork::plot_layout(ncol=nPlotCols)
myTikz(
  'clustering',
  width=8,
  height=5,
  plot(ggp)
)

## Other clustering plot(s):
# Specific to the realization used in the paper
nPlotCols <- 2
if(nClusters == 4){
  groups <- list(3, 1, 2, 4)
} else{
  groups <- list(2, 1, 4, c(3, 5, 6))
}
groups <- lapply(groups, function(v) v[v <= nClusters])
nGroups <- length(groups)

ggps2 <- lapply(seq_along(groups), function(j){
  (
    plotFlights(
      airports_sel = airports_sel_c,
      airportIndices = (kmed$clustering %in% groups[[j]]),
      connections_sel = connections_in_cluster,
      map = 'state',
      thinningRatio = mapThinningRatio,
      returnGGPlot = TRUE,
      useAirportNFlights = TRUE,
      useConnectionNFlights = FALSE,
      minNFlights = minNFlights,
      vertexShapes = kmed$clustering,
      useLatex = TRUE,
      hideXTicks = (!(j >= nGroups + 1 - nPlotCols))*2,
      hideYTicks = (!((j %% nPlotCols) == 1))*2
    )
    + theme(legend.position = "none")
  )
})
# Plot clusters:
ggp <- Reduce(`+`, ggps2) + patchwork::plot_layout(ncol=nPlotCols)
myTikz(
  'clustering2',
  width=8,
  height=5,
  plot(ggp)
)

# Actually select airports, connections, delays:
airports_sel <- airports[chosen_IATAS_c,]
IATAS_sel <- airports_sel$IATA

connections_sel <- (
  connections_sel_c
  %>% filter(
    departureAirport %in% IATAS_sel
    & arrivalAirport %in% IATAS_sel
  )
)

matEst0 <- delays[format(datesEst_sel),IATAS_sel]
matVal0 <- delays[format(datesVal_sel),IATAS_sel]

# Keep only rows without NAs
naRowsEst <- apply(is.na(matEst0), 1, any)
naRowsVal <- apply(is.na(matVal0), 1, any)

matEst <- matEst0[!naRowsEst,]
matVal <- matVal0[!naRowsVal,]

datesEst <- datesEst_sel[!naRowsEst]
datesVal <- datesVal_sel[!naRowsVal]

# Check that the dataset selection is the same as in the package
# Disable if clustering settings etc. are changed
if(TRUE){
  datesEst2 <- as.Date(graphicalExtremes::getFlightDelayData('dates', dateFilter = 'tcTrain'))
  datesVal2 <- as.Date(graphicalExtremes::getFlightDelayData('dates', dateFilter = 'tcTest'))
  matEst2 <- drop(graphicalExtremes::getFlightDelayData('delays', dateFilter = 'tcTrain', airportFilter = 'tcCluster'))
  matVal2 <- drop(graphicalExtremes::getFlightDelayData('delays', dateFilter = 'tcTest', airportFilter = 'tcCluster'))

  stopifnot(
    identical(datesEst, datesEst2),
    identical(datesVal, datesVal2),
    identical(matEst*1.0, matEst2), # matEst is integer, in the package it's float
    identical(matVal*1.0, matVal2) # matVal is integer, in the package it's float
  )
}


# Write airports to tex file
cat(makeIataTable(chosen_IATAS_c), file=TEX_NAME_CLUSTER_IATAS)


# Plot correlations inside/between clusters
cat('Plotting correlations inside/between clusters...\n')
source('correlations.R')

cat('Modelling...\n')
# create an `igraph::graph()` object for the flights connections
flight_edges <- connections_sel %>%
  select(arrivalAirport, departureAirport) %>%
  as.matrix()
flight_graph <- makeGraphWithVertexNames(IATAS_sel, flight_edges)
if(!igraph::is.connected(flight_graph)){
  warning('flight_graph is not connected! Adding edges to make it connected!')
  ccs <- igraph::components(flight_graph)
  cc0 <- ccs$membership[IATA_choose_cluster]
  disconnectedNodes <- names(which(ccs$membership != cc0))
  flight_graph = igraph::add.edges(flight_graph, cbind(IATA_choose_cluster, disconnectedNodes))
}

myTikz(
  'allFlights',
  width = 6,
  height = 3,
  plotFlights(
    chosen_IATAS_c,
    useAirportNFlights = TRUE,
    useConnectionNFlights = TRUE,
    useLatex = TRUE,
    minNFlights=minNFlights
  )
)


## Fit HR models
# Flight graph
Gamma_flight_graph <- fmpareto_graph_HR(
  data = matEst,
  graph = flight_graph,
  p = p,
  method = 'vario'
)
model_flight_graph <- list(
  Gamma = Gamma_flight_graph,
  graph = flight_graph,
  name = 'flightGraph',
  longName = 'Flight Graph',
  isFirst = TRUE,
  doPlot = TRUE
)

# Complete graph
Gamma_emp <- emp_vario(matEst, p=p)
model_complete <- list(
  isFirst = FALSE,
  graph = igraph::make_full_graph(length(chosen_IATAS_c)),
  Gamma = Gamma_emp,
  name = 'complete',
  longName = 'Complete Graph',
  doPlot = FALSE # Trivial graph structure not plotted
)

# EMST
model_tree <- emst(
  data = matEst,
  p = p,
  method = 'vario'
)
model_tree$name <- 'tree'
model_tree$longName <- 'Tree'
model_tree$isFirst <- FALSE
model_tree$doPlot <- TRUE

# eglearn
rholist <- seq(0, 0.5, 0.025)

cat('\nEGLearn (', length(rholist), '):\n')
models_eglearn <- eglearn(data = matEst, p = p, reg_method = "ns", rholist = rholist)

# Complete Gamma matrices according to eglearn graphs
completedGammas <- mclapply(mc.cores = N_CORES, seq_along(rholist), function(i){
  cat(i, '... ', sep='')
  if(!igraph::is.connected(models_eglearn$graph[[i]])){
    return(models_eglearn$Gamma[[i]])
  }
  Gamma1 <- complete_Gamma(
    Gamma_emp,
    models_eglearn$graph[[i]],
    N = 10000
  )
  if(!checkGraphicalGamma(Gamma1, models_eglearn$graph[[i]])){
    warning(paste('For index', i, 'the completed Gamma does not match the graph.'))
  }
  return(Gamma1)
})
models_eglearn$Gamma <- completedGammas
cat('\n')

# Compute test logliks for eglearn models
cat('Logliks and ICs (', length(rholist), '):\n')
logliksAndICs_eglearn_list <- mclapply(
  mc.cores = N_CORES,
  seq_along(rholist),
  function(i){
    tryCatch(error = function(...) c(NA, NA, NA), {
      cat(i, '... ', sep='')
      loglik_HR(
        data = matVal,
        p = p,
        Gamma = models_eglearn$Gamma[[i]],
        graph = models_eglearn$graph[[i]]
      )
    })
  }
)
logliksAndICs_eglearn <- do.call(cbind, logliksAndICs_eglearn_list)
cat('\n')

# Choose model that maximizes test loglik
ind <- which.max(logliksAndICs_eglearn['loglik',])  ## choose aic or bic or loglik

model_eglearn <- list(
  isFirst = FALSE,
  Gamma = models_eglearn$Gamma[[ind]],
  graph = models_eglearn$graph[[ind]],
  name = 'eglearn',
  longName = 'EGLearn',
  rho = rholist[ind],
  doPlot = TRUE
)


## Analyze/plot results

# List of considered models
models0 <- list(
  flight_graph = model_flight_graph,
  tree = model_tree,
  eglearn = model_eglearn,
  complete = model_complete
)

# Compute relevant quantities
cat('Summarize models (', length(models0), '):\n')
models <- mclapply(mc.cores = N_CORES, seq_along(models0), function(i){
  cat(i, '... ', sep='')
  model <- models0[[i]]
  tmp <- loglik_HR(
    matVal,
    p = p,
    graph = model$graph,
    Gamma = model$Gamma
  )
  model$loglik <- tmp['loglik']
  model$aic <- tmp['aic']
  model$bic <- tmp['bic']
  model$n <- nrow(matEst)
  model$k <- igraph::ecount(model$graph)
  model$chiFromGamma <- Gamma2chi(model$Gamma)
  return(model)
})
names(models) <- names(models0)
cat('\n')


# Select rholist entries to plot (here: all entries)
indPlot <- seq_along(rholist)

# Select edge counts to show on second x-axis (not all for readability)
indNEdges <- indPlot[seq(1, length(indPlot), 2)]
nEdgesEglearn <- sapply(models_eglearn$graph, igraph::ecount)

# Make loglik plot for eglearn models
cat('Plot: Loglik...\n')
plotInfo <- list(row='loglik', ylab='log-Likelihood')
myTikz(
  paste0('eglasso_rho_vs_', plotInfo$row),
  width = 3,
  plot(
    ggplot(
      mapping=aes(
        x = rholist[indPlot],
        y = logliksAndICs_eglearn[plotInfo$row,indPlot],
      )
    )
    + scale_x_continuous(sec.axis = sec_axis(
      name = 'Edges',
      trans = function(x) x,
      breaks = rholist[indNEdges],
      labels = nEdgesEglearn[indNEdges]
    ))
    + theme(axis.text.x.top = element_text(
        angle=45,
        margin = margin(b=3)
      ))
    + geom_point()
    + geom_line()
    + xlab('$\\rho$')
    + ylab(plotInfo$ylab)
    + theme(axis.title.y = element_text(
      margin = margin(r=8)
    ))
    + theme(aspect.ratio = 1) # Only for square plot
    + geom_hline(
      yintercept = models$flight_graph$loglik,
      linetype = 'dotted'
    )
    + geom_hline(
      yintercept = models$complete$loglik,
      linetype = 'dashed'
    )
  )
)

# Make Theta->0 plot for eglearn models
cat('Plot: Theta...\n')
Thetas <- lapply(
  models_eglearn$Gamma,
  function(Gamma){
    if(is.matrix(Gamma)){
      return(Gamma2Theta(Gamma))
    }
    return(NA)
  }
)
ggp <- makeThetaPlot(rholist, Thetas, returnGGPlot = TRUE)
ggp <- (
  ggp
  + scale_x_continuous(sec.axis = sec_axis(
    name = 'Edges',
    trans = function(x) x,
    breaks = rholist[indNEdges],
    labels = nEdgesEglearn[indNEdges]
  ))
  + theme(axis.text.x.top = element_text(
    angle=45,
    margin = margin(b=3)
  ))
  + theme(
    aspect.ratio = 1 # Only when plotting as square
  )
)
if(is.null(ggp)){
  warning('No lines for Theta plot!')
} else{
  myTikz(
    'ThetaConvergenceToZero',
    width = 3,
    plot(ggp)
  )
}


## Empirical vs implied chi (each based on Gamma)
cat('Plot: emp vs. fitted Gamma/Chi...\n')
chi_from_Gamma_emp <- graphicalExtremes::Gamma2chi(Gamma_emp)

ggp_all_empVsFitted_chi <- list()
ggp_all_empVsFitted_Gamma <- list()
ggp_all_graph <- list()

for(model in models){
  # Plot emp vs fitted Gamma with same scale
  model$empVsFitted_Gamma_plot <- plotEmpVsFitted(
    chi_from_Gamma_emp,
    model$chiFromGamma,
    yLabel = model$isFirst,
    yTicks = TRUE,
    makeSquare = TRUE,
    returnGGPlot = TRUE
  )
  myTikz(
    paste0('empVsFitted_Gamma_', model$name),
    width = 3 + 0.2 * model$isFirst,
    plot(model$empVsFitted_Gamma_plot)
  )
  model$graph_plot <- plotFlights(
    graph = model$graph,
    airportIndices = IATAS_sel,
    map = 'state',
    thinningRatio = mapThinningRatio,
    xyRatio = 1,
    clipMap = TRUE,
    hideYTicks = !model$isFirst,
    returnGGPlot = TRUE,
    useLatex = TRUE,
    edgeAlpha = 0.5
  )
  myTikz(
    paste0('graph_', model$name),
    width = 3 + 0.2 * model$isFirst,
    plot(model$graph_plot)
  )
  if(model$doPlot){
    ggp_all_empVsFitted_Gamma <- c(ggp_all_empVsFitted_Gamma, list(model$empVsFitted_Gamma_plot))
    ggp_all_graph <- c(ggp_all_graph, list(model$graph_plot))
  }
}

myTikz(
  'empVsFitted_Gamma_all',
  width = 8,
  height = 3,
  plot(Reduce('+', ggp_all_empVsFitted_Gamma))
)
myTikz(
  'graph_all',
  width = 9,
  height = 3,
  plot(Reduce('+', ggp_all_graph))
)


# Output results to .tex file:
cat('Make macro file...\n')
nClustersWord <- c('one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine')[nClusters]
if(is.na(nClustersWord)){
  nClustersWord <- as.character(nClusters)
}

makeMacroFile(
  file.path(EXAMPLES_DIR, 'applicationValues.tex'),
  list(
    UsedP = p,
    UsedPClustering = p_c,
    UsedPMarginals = p_marginals,
    DateEstStart = date0Est,
    DateEstEnd = date1Est,
    DateValStart = date0Val,
    DateValEnd = date1Val,
    YearStart = format(date0All, format='%Y'),
    YearEnd = format(date1All, format='%Y'),
    MinYearlyAirportFlights = minYearlyAirportFlights,
    MinMonthlyConnectionFlights = minMonthlyConnectionFlights,
    NDaysEst = nrow(matEst),
    NDaysVal = nrow(matVal),
    NYearsEst = round(as.numeric(date1Est - date0Est) / 365.25),
    NYearsVal = round(as.numeric(date1Val - date0Val) / 365.25),
    NYearsAll = round(diff(range(datesAll_sel)) / 365.25),
    NAirports = length(IATAS_sel),
    NFlights = sum(airports_sel$nFlights),
    NAirportsAll = ncol(delays),
    NAirportsFiltered = length(IATAS_sel_c),
    NAirportsCluster = length(IATAS_sel),
    NDaysAll = nrow(delays),
    NDaysFiltered = length(datesAll_sel),
    NClusters = nClusters,
    NClustersWord = nClustersWord,
    IatasClustering = paste0(IATAS_sel_c, collapse=', '),
    IatasChosenCluster = paste0(IATAS_sel, collapse=', '),
    RhoStar = model_eglearn$rho,
    RhoMin = min(rholist),
    RhoMax = max(rholist),
    NEdgesEglearn = igraph::ecount(model_eglearn$graph),
    NEdgesTree = igraph::ecount(model_tree$graph),
    NEdgesFlightGraph = igraph::ecount(model_flight_graph$graph),
    NEdgesComplete = igraph::ecount(model_complete$graph),
    TreeGraphLoglik = round(models$tree$loglik)
  )
)

tictoc::toc()
