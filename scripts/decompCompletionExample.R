
## Generates the decomposable completion example used in the paper

set.seed(1)

DIGITS = 2

edges0 <- rbind(
  c(1,2),
  c(1,3),
  c(2,3),
  c(3,4),
  c(1,4),
  c(4,5)
)
g0 <- igraph::graph_from_edgelist(edges0, directed = FALSE)

d <- igraph::vcount(g0)

G <- graphicalExtremes:::generate_random_graphical_Gamma(g0)
G <- round(G, DIGITS)
G <- graphicalExtremes::complete_Gamma(G, g0)

edges0 <- igraph::as_edgelist(g0)
cliques <- igraph::maximal.cliques(g0)
cliques <- graphicalExtremes:::order_cliques(cliques)
nCli <- length(cliques)

edgeList <- rep(NULL, length(cliques))
oldVertices <- c()
for(i in seq_len(nCli)){
  newVertices <- setdiff(cliques[[i]], oldVertices)
  newEdges <- cbind(
    rep(oldVertices, times=length(newVertices)),
    rep(newVertices, each=length(oldVertices))
  )
  edgeList[[i]] <- newEdges
  oldVertices <- c(oldVertices, newVertices)
}
edgeList[[1]] <- edges0

# remove duplicates from edges:
oldEdges <- matrix(0,0,2)
for(i in seq_along(edgeList)){
  newEdges <- t(apply(edgeList[[i]], 1, sort))
  allEdges <- rbind(oldEdges, newEdges)
  keep1 <- rep(c(FALSE, TRUE), c(nrow(oldEdges), nrow(newEdges)))
  keep2 <- !duplicated(allEdges)
  keep <- keep1 & keep2
  newEdges <- matrix(allEdges[keep,], ncol=2)
  edgeList[[i]] <- newEdges
  oldEdges <- rbind(oldEdges, newEdges)
}
allEdges <- oldEdges


loc <- igraph::layout_in_circle(g0)
# loc <- igraph::layout.auto(g0)

loc <- rbind(
  c(0, 2),
  c(-1, 1),
  c(0, 0),
  c(2, 0),
  c(2, 2)
) #*rep(c(2, 1), each = 5)

g1 <- igraph::make_empty_graph(igraph::vcount(g0), directed = FALSE)
for(edges in edgeList){
  g1 <- igraph::add.edges(g1, t(edges))
}

eTimes <- sapply(edgeList, nrow)
nEdges <- sum(eTimes)
for(i in seq_len(nCli)){
  nYesEdges <- sum(eTimes[1:i])
  nNoEdges <- nEdges - nYesEdges
  if(i == 1){
    nNewEdges <- 0
  } else {
    nNewEdges <- eTimes[i]
  }
  nOldEdges <- nYesEdges - nNewEdges
  nOriginalEdges <- eTimes[1]
  eTimesGrouped <- c(nOldEdges, nNewEdges, nNoEdges)
  eCols <- rep(c('black', 'black', 'black'), eTimesGrouped)
  eLty <- rep(c(1, 2, 0), eTimesGrouped)

  width = 2
  height = 1.3
  asp = (height - 0.1)/width

  tikz(
    file.path(TIKZ_DIR, sprintf('GD_%02d_graph.tex', i)),
    width = width,
    height = height,
    timestamp = FALSE
  )
  par(
    mar = c(0,0,0,0)
  )
  plot(
    g1,
    layout=loc,
    # vertex.label=NA,
    vertex.label.cex=1.2,
    vertex.label.color='black',
    vertex.color='white',
    vertex.size=30,
    edge.width=3,
    edge.color=eCols,
    edge.lty=eLty,
    asp = asp
    # rescale = FALSE
  )
  dev.off()

  prefix <- matrix('', d, d)
  postfix <- matrix('', d, d)
  newEdges <- rbind(edgeList[[i]], edgeList[[i]][,c(2,1)])
  if(i != 1){
    for(j in seq_len(nrow(newEdges))){
      edge <- newEdges[j,]
      prefix[edge[1], edge[2]] <- '\\underline{'
      postfix[edge[2], edge[1]] <- '}'
    }
  }
  G1 <- G
  for(j in seq_len(nNoEdges)+nYesEdges){
    edge <- oldEdges[j,]
    G1[edge[1], edge[2]] <- NA
    G1[edge[2], edge[1]] <- NA
  }
  minDigits <- matrix(DIGITS, d, d)
  diag(minDigits) <- 0
  maxDigits <- matrix(DIGITS, d, d)
  diag(maxDigits) <- 0
  writeMatrixToFile(
    file.path(EXAMPLES_DIR, sprintf('GD_%02d_matrix.tex', i)),
    G1,
    prefix = prefix,
    postfix = postfix,
    minDigits = minDigits,
    maxDigits = maxDigits
  )
}

