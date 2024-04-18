
# Load packages
library(graphicalExtremes) # TODO: check required version
library(dplyr)
library(ggplot2)
library(igraph)
library(dplyr)
library(tidyr)
library(memoise)
library(cachem)
library(parallel)
library(cluster)
library(patchwork)
library(tikzDevice)
library(extRemes)
library(tictoc)
library(benchmarkme)
library(dplyr)
library(magrittr)
library(igraph)
library(xtable)

# Warn immediately, show traceback
options(warn = 1)
options(error = traceback)

# set seed
set.seed(1)

# For parallelization of some tasks
# N_CORES <- 16
N_CORES <- 1
if(Sys.info()['sysname'] == 'Windows'){
  N_CORES <- 1
}

# Set this to true to write to .pdf instead of .tikz
# Does not look as nice (e.g. due to tex code in labels), but is faster
if(is.null(globalenv()[['FORCE_PDF']])){
  FORCE_PDF <- FALSE
}


if(is.null(globalenv()[['DO_PLOT']])){
  DO_PLOT <- TRUE
}

# Get dir names
ROOT_DIR <- normalizePath('.', winslash = '/')

# make sub-dirs:
myPath <- function(...) {
  path <- normalizePath(file.path(...), mustWork = FALSE)
}
createIfNotExists <- function(path){
  dir.exists(path) || dir.create(path)
}
myDir <- function(...){
  path <- myPath(ROOT_DIR, ...)
  createIfNotExists(path)
  return(path)
}

TIKZ_DIR <- myDir('tikz')
APPLICATION_DIR <- myDir('application')
SCRIPTS_DIR <- file.path(ROOT_DIR, 'scripts')
EXAMPLES_DIR <- myDir('examples')
TEMP_DIR <- myDir('temp')
SIMUL_DIR <- myDir('simulation')
FIGURE_DIR <- myDir('figures')

tikzSubDir <- '.'
# Sets the subdirectory of TIKZ_DIR to be used by myTikz()
# Used to have subfolders for different applications, simulation
setTikzSubDir <- function(...){
  tikzSubDir <<- file.path(...)
  tikzDir <- file.path(TIKZ_DIR, tikzSubDir)
  createIfNotExists(tikzDir)
  return(tikzDir)
}
getTikzDir <- function(){
  tikzDir <- file.path(TIKZ_DIR, tikzSubDir)
  return(tikzDir)
}
# Used when `FORCE_PDF == TRUE`
getPdfDir <- function(){
  pdfDir <- file.path(FIGURE_DIR, tikzSubDir)
  createIfNotExists(pdfDir)
  return(pdfDir)
}


# Configure tikz
options(tikzMetricsDictionary = file.path(TEMP_DIR, 'tikz'))

theme_set(
  theme_bw()
  + theme(
    plot.background = element_blank(),
    legend.background = element_blank(),
    strip.background = element_rect(fill = "white")
  )
)


myTikz <- function(filename, expr, width=3, height=3, ext='.tex'){
  # If plotting is disabled, do nothing
  if(!DO_PLOT){
    return(NULL)
  }
  # If specified, make pdf instead of tikz
  if(FORCE_PDF){
    filename <- paste0(filename, '.pdf')
    filepath <- file.path(getPdfDir(), filename)
    pdf(filepath, width=width, height=height)
    tryCatch(eval(expr, envir = parent.frame()), finally = dev.off())
    return(invisible(NULL))
  }
  # Create tikz device
  filename <- paste0(filename, ext)
  tikzDevice::tikz(
    file.path(getTikzDir(), filename),
    width=width,
    height=height,
    timestamp = FALSE,
    verbose = TRUE
  )
  # Evaluate plot expression
  tryCatch(
    eval(expr, envir=parent.frame()),
    finally = dev.off()
  )
  invisible(NULL)
}

## Memoise (cache) expensive functions to make re-runs faster
# Custom hash function to handle graphs
myHash <- function(x){
  if(igraph::is_igraph(x)){
      x <- igraph::as_adjacency_matrix(x)
  } else if(is.list(x)){
      x <- lapply(x, myHash)
  }
  return(rlang::hash(x))
}

myCache <- cachem::cache_disk(
  myPath(TEMP_DIR, 'memoise_cache'),
  evict = 'fifo'
)

# Custom memoise function, using the hash/cache from above
myMemoise <- function(f){
  if('memoised' %in% class(f)){
    return(f)
  }
  return(memoise::memoise(
    f,
    hash = myHash,
    cache = myCache
  ))
}

# Wrapper around pam(), since the original is not compatible with memoise
pam <- function(x, diss, k, medoids, nstart, variant, ...){
  cluster::pam(
    x,
    diss = diss,
    k = k,
    medoids = medoids,
    nstart = nstart,
    variant = variant,
    ...
  )
}

# Memoise expensive functions:
complete_Gamma <- myMemoise(complete_Gamma)
loglik_HR <- myMemoise(loglik_HR)
emp_chi <- myMemoise(emp_chi)
emp_vario <- myMemoise(emp_vario)
eglearn <- myMemoise(eglearn)
emp_vario_pairwise <- myMemoise(emp_vario_pairwise)
emp_chi_pairwise <- myMemoise(emp_chi_pairwise)
fmpareto_graph_HR <- myMemoise(fmpareto_graph_HR)
pam <- myMemoise(pam)
fevd <- myMemoise(fevd)


## Some utility functions
# Check that Gamma has given graphical structure
checkGraphicalGamma <- function(Gamma, graph, tol=1e-5){
  Theta <- Gamma2Theta(Gamma)
  A <- 1 * (abs(Theta) > tol)
  diag(A) <- 0
  A1 <- igraph::as_adjacency_matrix(graph, sparse=FALSE)
  return(identical(A, A1))
}

# Create a graph with given vertex names
makeGraphWithVertexNames <- function(vertexNames, edges = c()){
  g <- igraph::make_empty_graph(length(vertexNames), directed = FALSE)
  igraph::V(g)$name <- vertexNames
  g <- igraph::add_edges(g, as.vector(t(edges)))
  if(any(igraph::as_adjacency_matrix(g) > 1)){
    stop('Duplicate edges specified!')
  }
  return(g)
}

# Get values in the upper triangle of a matrix
upper.tri.val <- function(m, diag = FALSE){
  m[upper.tri(m, diag)]
}


## Helper functions to write R matrices to .tex files

writeMatrixToFile <- function(fileName, m, ...){
  tex <- matrixToTex(m, ...)
  cat(tex, file = fileName)
}

matrixToTex <- function(
  m,
  tab=4,
  maxDigits=2,
  minDigits=0,
  tolDigits=6,
  groupDigits=c('no', 'col', 'row', 'all')[1],
  naString='\\\\mathrm{?}',
  prefix='',
  postfix=''
){

  digits <- mapply(findNumberOfDigits, m, minDigits = minDigits, maxDigits = maxDigits, tolDigits = tolDigits)
  # digits <- sapply(m, findNumberOfDigits, minDigits=minDigits, maxDigits=maxDigits, tolDigits=tolDigits)
  dim(digits) <- dim(m)

  if(groupDigits == 'all'){
    digits[,] <- max(digits)
  } else if(groupDigits == 'col'){
    colMax <- sapply(seq_len(ncol(m)), function(i){
      max(digits[,i])
    })
    digits <- t(digits)
    digits[,] <- colMax
    digits <- t(digits)
  } else if(groupDigits == 'row'){
    rowMax <- sapply(seq_len(ncol(m)), function(i){
      max(digits[,i])
    })
    digits[,] <- rowMax
  }

  mString <- mapply(function(x, d){
    sprintf(paste0('%.', d, 'f'), x)
  }, m, digits)
  mString <- paste0(prefix, mString, postfix)
  dim(mString) <- dim(m)

  if(is.numeric(tab)){
    tab <- paste0(rep(' ',tab), collapse='')
  }
  rows <- lapply(seq_len(nrow(m)), function(i) {
    paste(mString[i,], collapse=' & ')
  })
  body <- paste(tab, rows, collapse=' \\\\\n')
  body <- stringr::str_replace_all(body, 'NA', naString)
  return(paste(
    '\\begin{matrix}',
    body,
    '\\end{matrix}',
    sep='\n'
  ))
}

findNumberOfDigits <- function(x, minDigits=0, maxDigits=3, tolDigits=6){
  if(is.na(x)){
    return(0)
  }
  digits <- getDigits(x, maxDigits, tolDigits)
  if(digits[maxDigits+1] != 0){
    return(maxDigits)
  }
  suppressWarnings(
    d <- max(which(digits[1:maxDigits]>0))
  )
  return(max(d, minDigits))
}

floorTol <- function(x, tolDigits=6){
  f <- floor(x)
  if(abs(x - (f+1)) < 10**(-tolDigits)){
    f <- f+1
  }
  return(f)
}

getDigits <- function(x, maxDigits=3, tolDigits=6){
  digits <- integer(maxDigits + 1)
  x <- x - floorTol(x, tolDigits)
  if(abs(x) < 10**(-tolDigits)){
    return(digits)
  }
  for(i in seq_len(maxDigits)){
    x <- x*10
    digits[i] <- floorTol(x, tolDigits)
    x <- x - floorTol(x, tolDigits)
  }
  digits[maxDigits + 1] <- 1*(abs(x) > 10**(-tolDigits + maxDigits))
  return(digits)
}

