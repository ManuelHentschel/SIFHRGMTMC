
d <- 10
d_env <- as.numeric(Sys.getenv('SIM_D'))
if(!is.na(d_env)){
  d <- d_env
  cat('Using d=', d, ' from env.\n')
}

n <- 200
n_env <- as.numeric(Sys.getenv('SIM_N'))
if(!is.na(n_env)){
  n <- n_env
  cat('Using n=', n, ' from env.\n')
}

nSimulations <- 20
nSimulations_env <- as.numeric(Sys.getenv('SIM_N_SIMULATIONS'))
if(!is.na(nSimulations_env)){
  nSimulations <- nSimulations_env
  cat('Using nSimulations=', nSimulations, ' from env.\n')
}

modelSeed <- 1
MAX_IT <- 100

timestamp <- format(Sys.time(), '%Y%m%d_%H%M%S')

packageInfo <- utils::packageDescription('graphicalExtremes')

sims <- list()

# Utility function for timing
myToc <- function(){
  t <- toc()
  as.numeric(t$toc - t$tic)
}

for(i in seq(nSimulations)){
  cat('\nSimulation', i, 'of', nSimulations, '...\n')
  # General setup
  seed <- i

  # set.seed(seed)
  if(is.null(modelSeed)){
    set.seed(seed)
  } else{
    set.seed(modelSeed)
  }
  m <- graphicalExtremes::generate_random_model(d, 'decomposable', cMax=4)

  set.seed(seed)
  data <- graphicalExtremes::rmpareto(n, 'HR', d, m$Gamma)

  graph <- m$graph

  rets <- list()
  simWrapper <- function(name, expr){
    cat(name, '...\n', sep='')
    tic()
    ret <- tryCatch(
      expr,
      error = function(e) {
        print(e)
        list(
          err = as.character(e)
        )
      }
    )
    ret$t <- myToc()
    ret$name <- name
    rets <<- c(rets, list(ret))
  }

  # Full Vario
  simWrapper(
    'full_vario',
    list(Gamma = graphicalExtremes::emp_vario(data))
  )

  # Graphical Vario
  simWrapper(
    'graphical_vario',
    list(Gamma = fmpareto_graph_HR(data, graph = m$graph, method = 'vario'))
  )

  # Full MLE
  simWrapper(
    'full_MLE',
    graphicalExtremes:::fmpareto_HR_MLE(
      data,
      maxit = MAX_IT,
      useTheta = TRUE
    )
  )

  # Graphical MLE simultaneous
  simWrapper(
    'graph_MLE_all',
    graphicalExtremes:::fmpareto_HR_MLE(
      data,
      graph = graph,
      maxit = MAX_IT,
      useTheta = TRUE
    )
  )

  # Graphical MLE cliquewise
  simWrapper(
    'graph_MLE_cli',
    list(Gamma = graphicalExtremes::fmpareto_graph_HR(
      data,
      graph,
      method = 'ML',
      handleCliques = 'average',
      maxit = MAX_IT
    ))
  )
  
  # Compute adjacency matrix for graph
  adj <- igraph::as_adjacency_matrix(graph, sparse = FALSE)

  # Add to results
  sim <- list(
    Gamma = m$Gamma,
    adjacencyMatrix = adj,
    n = n,
    d = d,
    seed = seed,
    modelSeed = modelSeed,
    rets = rets,
    timestamp = timestamp,
    packageInfo = packageInfo,
    sysInfo = Sys.info(),
    cpuInfo = benchmarkme::get_cpu(),
    ramInfo = benchmarkme::get_ram(),
    envInfo = Sys.getenv()
  )
  sims <- c(sims, list(sim))

  saveRDS(sims, paste0('temp.', i,'.sims_', timestamp, '.RDS'))
  if(i > 1){
    file.remove(paste0('temp.', i-1,'.sims_', timestamp, '.RDS'))
  }
}
cat('Done.\n')

saveRDS(sims, 'sims.RDS')

saveRDS(sims, paste0('sims_', timestamp, '.RDS'))

file.remove(paste0('temp.', nSimulations,'.sims_', timestamp, '.RDS'))

