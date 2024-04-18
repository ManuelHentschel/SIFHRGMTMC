
## Generates the non-decomposable completion example used in the paper

set.seed(1)

DIGITS <- 2

d <- 5
N <- 59

g <- igraph::make_ring(d)
g <- igraph::add.edges(g, c(1,4))

gList <- list(
  igraph::add.edges(g, c(1,3)),
  igraph::add.edges(g, c(2,4))
)

G <- graphicalExtremes::generate_random_Gamma(d)
G <- round(G, DIGITS)
# G <- graphicalExtremes:::generate_random_integer_Gamma(d)

# g <- graphicalExtremes:::generate_random_connected_graph(d, m = d*4)

ret <- graphicalExtremes::complete_Gamma_general_demo(
  G,
  g,
  N=N-1,
  gList=gList
)

# This step requires a modified version of `graphicalExtremes`:
# GammaList <- ret$GammaList
GammaList <- lapply(ret$iterations, function(it) it$Gamma)
GammaList <- c(list(ret$Gamma0), GammaList)

A <- (igraph::as_adjacency_matrix(g, sparse = FALSE) == 1)
B <- (!A) & upper.tri(A)

m <- sum(B)


ThetaList <- lapply(GammaList, graphicalExtremes::Gamma2Theta, check = FALSE)

eVec <- numeric(N)
iVec <- numeric(N)
pMat <- matrix(0, m, N)
BB <- upper.tri(B) & !B
pMatBB <- matrix(0, sum(BB), N)
for(i in seq_along(ThetaList)){
  PB <- ThetaList[[i]][B]
  pMat[,i] <- PB
  pMatBB[,i] <- ThetaList[[i]][BB]
  iVec[i] <- which.max(abs(PB))
  eVec[i] <- abs(PB[iVec[i]])
}

# ord <- order(tabulate(iVec), decreasing = TRUE)
ord <- seq_len(m)

# plot(cbind(x, x, x), log(cbind(eVec, p1Vec, p2Vec)), type='l')

lineLabels <- outer(seq_len(d), seq_len(d), function(i, j){
  sprintf('$\\Theta_{%d,%d}$', i, j)
})

minPLog <- -16
pMat2 <- log10(abs(pMat))
# pMat2[pMat2 < minPLog] <- minPLog

lines <- lapply(seq_len(nrow(pMat)), function(i) {
  df <- data.frame(Theta = pMat2[i,])
  df[['Entry']] <- lineLabels[B][i]
  df[['iter']] <- seq_len(ncol(pMat))
  return(df)
})

df <- Reduce(rbind, lines)

ggp <- (
  ggplot(df)
  + geom_line(aes(
    x=iter,
    y = Theta,
    group = Entry,
    col = Entry
  ))
  + xlab('Iteration')
  + ylab('$\\log_{10} |\\Theta_{ij}|$')
  + theme(
    legend.position = c(0.8, 0.8),
    legend.title = element_blank()
  )
  + coord_cartesian(
    xlim = c(1, N),
    ylim = c(minPLog, max(4, max(pMat2))),
    expand = FALSE
  )
)

myTikz(
  'completionConvergence',
  width = 5,
  height = 3,
  plot(ggp)
)

prefix <- matrix('', d, d)
postfix <- matrix('', d, d)

C <- B | t(B)
prefix[C] <- '\\underline{'
postfix[C] <- '}'

minDigits <- matrix(DIGITS, d, d)
diag(minDigits) <- 0
maxDigits <- matrix(DIGITS, d, d)
diag(maxDigits) <- 0

writeMatrixToFile(
  file.path(EXAMPLES_DIR, 'completionConvergence.tex'),
  G,
  prefix=prefix,
  postfix=postfix,
  minDigits=minDigits,
  maxDigits=maxDigits
)

G2 <- GammaList[[N]]
writeMatrixToFile(
  file.path(EXAMPLES_DIR, 'completionConvergence2.tex'),
  G2,
  prefix=prefix,
  postfix=postfix,
  minDigits=minDigits,
  maxDigits=maxDigits
)

loc <- rbind(
  c(0, 2),
  c(0, 0),
  c(2, 0),
  c(2, 2),
  c(1, 4)
)
# plot(g, layout = loc)

myTikz(
  'completionConvergenceGraph',
  width = 2,
  height = 3,
  {
    mar <- par('mar')
    mar <- c(0, 0, 0, 0)
    # mar <- mar*2
    par(mar = mar)

    plot(
      g,
      vertex.size=30,
      # vertex.label = NA,
      vertex.label.cex=1.2,
      vertex.label.color='black',
      vertex.color='white',
      vertex.size=30,
      edge.width=3,
      edge.color='black',
      layout=loc
    )
  }
)



