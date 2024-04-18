
## Must be called from `../main.R`!
## Code used for the danube application
## Can take quite some time to run
tictoc::tic()

set.seed(1)

setTikzSubDir('danube')

# Source other files:
source('makeMacroFile.R')
source('makeIataTable.R')
source('plotting.R')


# Use fewer points to draw map (-> smaller .pdf files)
mapThinningRatio <- 1

# Parameter p for thresholding etc.:
p <- 0.90


# Specify time range for train/test:
year0Est <- 1960
year1Est <- 1985
year0Val <- year1Est + 1
year1Val <- 2010
rowYears <- as.numeric(rownames(danube$data_clustered))
yearsEstInd <- (
  year0Est <= rowYears
  & rowYears <= year1Est
)
yearsEst_sel <- seq(year0Est, year1Est) # (all years occur)
yearsValInd <- (
  year0Val <= rowYears
  & rowYears <= year1Val
)
yearsVal_sel <- seq(year0Val, year1Val)

# Specify stations to use (->all)
nStations <- nrow(danube$info)
indStations <- seq_len(nStations)


# Compute pairwise chis
pList0 <- sort(unique(c(
    seq(0.7, 1, 0.01),
    seq(0.99, 1, 0.002)
)))
pList0 <- pList0[pList0 != 1]

chiMatList0 <- lapply(pList0, function(p){
    emp_chi(danube$data_clustered, p)
})

# Keep only chi matrices without NAs (happens for p too large)
keepInds <- sapply(chiMatList0, function(M){
  !any(is.na(M))
})
pList <- pList0[keepInds]
chiMatList <- chiMatList0[keepInds]

# Plot behavior of chi for different p
chiMat <- sapply(chiMatList, function(M){
  M[upper.tri(M)]
})
df <- data.frame(
  p = as.vector(pList[col(chiMat)]),
  chi = as.vector(chiMat),
  ind = as.vector(row(chiMat))
)
ggp <- (
  ggplot2::ggplot(
    df,
    aes(x = p, y = chi, group = ind)
  )
  + geom_line(alpha=0.4)
  + xlab('$p$')
  + ylab('$\\hat\\chi(p)$')
)
myTikz(
  'pairwiseChi',
  width = 4,
  plot(ggp)
)

# Plot dataset overview
ggp <- plotDanube(
  useLatex=TRUE,
  xyRatio=4/3,
  clipMap=1.2,
  useStationVolume=TRUE,
  useConnectionVolume=TRUE,
  returnGGPlot = TRUE
) + theme(legend.position = "none")

myTikz(
  'flowGraph',
  width = 4,
  height = 3,
  plot(ggp)
)


# Train-test-split
matEst <- danube$data_clustered[yearsEstInd,]
matVal <- danube$data_clustered[yearsValInd,]



## Fit HR models

# Flow graph
flow_graph <- igraph::graph_from_edgelist(danube$flow_edges, directed = FALSE)
Gamma_flow_graph <- fmpareto_graph_HR(
  data = matEst,
  graph = flow_graph,
  p = p,
  method = 'vario'
)
model_flow_graph <- list(
  name = 'flowGraph',
  longName = 'Flow Graph',
  isFirst = TRUE,
  doPlot = TRUE,
  Gamma = Gamma_flow_graph,
  graph = flow_graph
)

# Complete graph
Gamma_emp <- emp_vario(matEst, p=p)
model_complete <- list(
  isFirst = FALSE,
  graph = igraph::make_full_graph(nStations),
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
rholist <- seq(0, 0.1, length.out=21)[-1]

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
    N = 100000
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
  flow_graph = model_flow_graph,
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
      yintercept = models$flow_graph$loglik,
      linetype = 'dotted'
    )
    # + geom_hline(
    #   yintercept = models$complete$loglik,
    #   linetype = 'dashed'
    # )
    + geom_hline(
      yintercept = models$tree$loglik,
      linetype = 'dotdash'
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

# Find limits for plots (if they should be the same for each):
allChis <- do.call('c', c(list(upper.tri.val(chi_from_Gamma_emp)), lapply(models, function(model){
  upper.tri.val(model$chiFromGamma)
})))
maxChi <- max(allChis, na.rm = TRUE)
minChi <- min(allChis, na.rm = TRUE)

ggp_all_empVsFitted_Gamma <- list()
ggp_all_graph <- list()

for(model in models){
  # Plot emp vs fitted Gamma with same scale
  model$empVsFitted_Gamma_plot <- plotEmpVsFitted(
    chi_from_Gamma_emp,
    model$chiFromGamma,
    yLabel = model$isFirst,
    minVal = minChi,
    maxVal = maxChi,
    returnGGPlot = TRUE
  )
  myTikz(
    paste0('empVsFitted_Gamma_', model$name),
    width = 3 + 0.2 * model$isFirst,
    plot(model$empVsFitted_Gamma_plot)
  )
  model$graph_plot <- plotDanube(
    graph = model$graph,
    # thinningRatio = mapThinningRatio,
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

makeMacroFile(
  file.path(EXAMPLES_DIR, 'danubeValues.tex'),
  prefix='dan',
  list(
    UsedP = p,
    YearAllStart = year0Est,
    YearAllEnd = year1Val,
    YearEstStart = year0Est,
    YearEstEnd = year1Est,
    YearValStart = year0Val,
    YearValEnd = year1Val,
    NObsAll = nrow(danube$data_clustered),
    NObsEst = nrow(matEst),
    NObsVal = nrow(matVal),
    NStations = nStations,
    RhoStar = model_eglearn$rho,
    RhoMin = min(rholist),
    RhoMax = max(rholist),
    NEdgesEglearn = igraph::ecount(model_eglearn$graph),
    NEdgesTree = igraph::ecount(model_tree$graph),
    NEdgesFlowGraph = igraph::ecount(model_flow_graph$graph),
    NEdgesComplete = igraph::ecount(model_complete$graph),
    CompleteGraphLoglik = round(models$complete$loglik),
    EglearnLoglik = round(models$eglearn$loglik),
    FlowGraphLoglik = round(models$flow_graph$loglik),
    TreeGraphLoglik = round(models$tree$loglik)
  )
)

tictoc::toc()
