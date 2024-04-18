
## Generates some small example graphs (with matching Gamma matrices)
## Only one of the matrices is used in the paper, but most graphs

set.seed(1)

tol <- 1e-9
d <- 4

pos <- rbind(
  c(1,2),
  c(0,0),
  c(1,-2),
  c(2,0)
)

graphList <- list()

graphList[[1]] <- igraph::graph_from_edgelist(rbind(
  c(1,2),
  c(3,4)
), directed = FALSE)

graphList[[2]] <- igraph::graph_from_edgelist(rbind(
  c(1,2),
  c(2,3),
  c(2,4)
), directed = FALSE)

graphList[[3]] <- igraph::graph_from_edgelist(rbind(
  c(1,2),
  c(1,4),
  c(2,3),
  c(2,4)
), directed = FALSE)

graphList[[4]] <- igraph::graph_from_edgelist(rbind(
  c(1,2),
  c(1,4),
  c(2,3),
  c(2,4),
  c(3,4)
), directed = FALSE)

graphList[[5]] <- igraph::graph_from_edgelist(rbind(
  c(1,2),
  c(2,3),
  c(3,4),
  c(1,4)
), directed = FALSE)

Gamma0 <- graphicalExtremes::generate_random_integer_Gamma(d)

# Generate random integer Sigma:
is_sym_pos_def <- function(S, tol = 1e-9){
  max(abs(S - t(S))) <= tol && all(eigen(S, TRUE, TRUE)$values > 0)
}
generate_random_integer_Sigma <- function(d, b = 2, b_step = 1){
  repeat{
  B <- floor(b * (stats::runif(d^2) * 2 - 1))
  B <- matrix(B, d, d)
  S <- B %*% t(B)
  if (is_sym_pos_def(S)) {
    return(S)
  }
  b <- b + b_step
  }
}
Sigma <- generate_random_integer_Sigma(d)

ZeroMatrix <- matrix(0, d, d)

makeGamma <- function(g){
  if(igraph::is_connected(g)){
  graphicalExtremes::complete_Gamma(Gamma0, g)
  } else{
  NULL
  }
}

makeTheta <- function(G){
  if(is.null(G)){
  NULL
  } else{
  P <- graphicalExtremes::Gamma2Theta(G, k = NULL)
  P[abs(P)<tol] <- 0
  P
  }
}

GammaList <- lapply(graphList, makeGamma)
ThetaList <- lapply(GammaList, makeTheta)

makePartialMatrix <- function(M, A, removeDiag=FALSE){
  MPartial <- array(NA, dim=dim(M))
  MPartial[A] <- M[A]
  if(removeDiag){
  diag(MPartial) <- NA
  }
  return(MPartial)
}

for(i in seq_along(GammaList)){
  g <- graphList[[i]]
  tikz(
  file.path(TIKZ_DIR, sprintf('G_%02d_graph.tex', i)),
  width = 2,
  height = 2,
  timestamp = FALSE
  )
  par(mar = c(0,0,0,0))
  plot(
  g,
  layout = pos,
  # vertex.label = NA,
  vertex.label.cex=1.2,
  vertex.label.color='black',
  vertex.color=NA,
  edge.color='black',
  vertex.size=30,
  edge.width=3
  )
  dev.off()
  Adjacency <- igraph::as_adjacency_matrix(g, sparse=FALSE)
  A <- (Adjacency + diag(d)) == 1
  G <- GammaList[[i]]
  P <- ThetaList[[i]]

  PPartial <- makePartialMatrix(ZeroMatrix, !A, TRUE)
  SigmaPartial <- makePartialMatrix(Sigma, A)
  writeMatrixToFile(
  file.path(EXAMPLES_DIR, sprintf('Theta_Partial_%02d.tex', i)),
  PPartial
  )
  writeMatrixToFile(
  file.path(EXAMPLES_DIR, sprintf('Sigma_Partial_%02d.tex', i)),
  SigmaPartial
  )
  writeMatrixToFile(
  file.path(EXAMPLES_DIR, sprintf('Adjacency_%02d.tex', i)),
  Adjacency
  )
  if(!is.null(G)){
  GPartial <- makePartialMatrix(G, A)
  writeMatrixToFile(
    file.path(EXAMPLES_DIR, sprintf('Gamma_%02d.tex', i)),
    G
  )
  writeMatrixToFile(
    file.path(EXAMPLES_DIR, sprintf('Gamma_Partial_%02d.tex', i)),
    GPartial
  )
  writeMatrixToFile(
    file.path(EXAMPLES_DIR, sprintf('Theta_%02d.tex', i)),
    P
  )
  }
}
