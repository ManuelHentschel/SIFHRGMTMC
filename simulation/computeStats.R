
# List of nice method names to be used for captions etc.
niceNames <- list(
  full_vario = 'Full variogram',
  graphical_vario = 'Clique-wise variogram',
  full_MLE = 'Full MLE',
  graph_MLE_all = 'Graphical MLE',
  graph_MLE_cli = 'Clique-wise MLE'
)

computeStatsForRet <- function(ret, sim){
  err2 <- (ret$Gamma - sim$Gamma)^2
  niceName <- niceNames[[ret$name]]
  if(is.null(niceName)){
    niceName <- '???'
  }
  ret$niceName <- niceName
  ret$MSE <- mean(err2[upper.tri(err2)])
  ret$MSE_edges <- mean(graphicalExtremes:::getEdgeEntries(err2, sim$graph))
  ret$MSE_nonedges <- mean(graphicalExtremes:::getNonEdgeEntries(err2, sim$graph))
  return(ret)
}

computeStatsForSim <- function(sim){
  sim$graph <- igraph::graph_from_adjacency_matrix(
    sim$adjacencyMatrix,
    mode = 'undirected'
  )
  for(j in seq_along(sim$rets)){
    sim$rets[[j]] <- computeStatsForRet(sim$rets[[j]], sim)
  }
  names(sim$rets) <- sapply(sim$rets, function(ret) ret$name)
  return(sim)
}

computeStatsForSims <- function(sims){
  for(i in seq_along(sims)){
    sims[[i]] <- computeStatsForSim(sims[[i]])
  }
  return(sims)
}

getRetNames <- function(sims){
  allNames <- c()
  for(sim in sims){
    newNames <- sapply(sim$rets, function(ret){
      ret$name
    })
    allNames <- c(allNames, setdiff(newNames, allNames))
  }
  return(allNames)
}


getRetEntries <- function(sims, retName, returnColmeans=FALSE, fallbackToSim=TRUE, returnVector=FALSE){
  allNames <- getRetNames(sims)
  m <- matrix(NA, length(sims), length(allNames), dimnames = list(NULL, allNames))
  for(i in seq_along(sims)){
    for(ret in sims[[i]]$rets){
      val <- ret[[retName]]
      if(is.null(val)){
        val <- sims[[i]][[retName]]
      }
      if(!is.null(val)){
        m[i, ret$name] <- val
      }
    }
  }
  if(returnColmeans){
    return(colMeans(m))
  }
  if(returnVector){
    return(as.vector(m))
  }
  return(m)
}

getSimEntries <- function(sims, simName, simplifyIdentical=TRUE){
  m <- sapply(sims, function(sim) sim[[simName]])
  if(simplifyIdentical && length(unique(m)) == 1){
    return(m[[1]])
  }
  return(m)
}

graphToAdj <- function(sims){
  for(i in seq_along(sims)){
    graph <- igraph::upgrade_graph(sims[[i]]$graph)
    adjacencyMatrix <- igraph::as_adjacency_matrix(
      graph,
      sparse = FALSE
    )
    sims[[i]]$adjacencyMatrix <- adjacencyMatrix
    sims[[i]]$graph <- NULL
  }
  return(sims)
}

colMap <- list(
  method = 'name',
  niceMethod = 'niceName',
  MSE = 'MSE',
  MSE_edges = 'MSE_edges',
  MSE_nonedges = 'MSE_nonedges',
  time = 't',
  d = 'd',
  n = 'n',
  seed = 'seed',
  modelSeed = 'modelSeed'
)


fileNames <- dir(pattern = 'sims_.*\\.RDS')

# for(fileName in fileNames){
#   cat(fileName, '\n')
#   sims <- readRDS(fileName)
#   sims2 <- graphToAdj(sims)
#   saveRDS(sims2, fileName)
# }

df <- NULL
allSims <- list()
for(fileName in fileNames){
  cat(fileName, '\n')
  sims <- readRDS(fileName)
  sims <- computeStatsForSims(sims)

  allSims[[fileName]] <- sims

  cols <- lapply(colMap, getRetEntries, sims = sims, returnVector = TRUE)

  df_new <- data.frame(cols)
  df_new$fileName <- fileName

  if(is.null(df)){
    df <- df_new
  } else{
    df <- rbind(df, df_new)
  }
}


saveRDS(df, 'df.RDS')
saveRDS(allSims, 'allSims.RDS')
