

df <- readRDS('df.RDS')
allSims <- readRDS('allSims.RDS')

n_vals <- c(20, 50, 200, 500)
d_vals <- c(6, 10)
valCombinations <- expand.grid(n=n_vals, d=d_vals)

for(i in seq_len(nrow(valCombinations))){
  vals <- valCombinations[i,]
  n_filter <- vals$n
  d_filter <- vals$d
  df2 <- df %>% filter(
    modelSeed == 1,
    d == d_filter,
    n == n_filter
  )
  nRows <- nrow(df2)

  cat(i, ': n =', n_filter, ', d =', d_filter, ', nRows =', nRows, '\n')

  if(nRows == 0){
    cat('No data\n')
    # cat('Empty data frame for n =', n_filter, ', d =', d_filter, '!\n')
    next
  }

  methodNames <- unique(df2$niceMethod)

  ggp <- (
    ggplot(df2)
    + geom_point(
      aes(
        x = time,
        y = MSE,
        color = niceMethod
      )
    )
    + xlab('Time (seconds)')
    + ggtitle(paste0('$n=', n_filter, ', d=', d_filter, '$'))
    + labs(color = 'Method')
    + scale_color_discrete(breaks = methodNames)
  )

  if(d_filter >= 10){
    ggp <- ggp + scale_x_continuous(trans='log10')
  }

  myTikz(
    paste0('timeVsErrScatter_n', n_filter, '_d', d_filter),
    width = 6,
    plot(ggp)
  )

  df_summary <- (
    df2
    %>% group_by(Method = niceMethod)
    %>% summarise(
      Time = mean(time),
      MSE = mean(MSE) #,
      # 'MSE (edges)' = mean(MSE_edges),
      # 'MSE (nonedges)' = mean(MSE_nonedges)
    )
    %>% arrange(Time)
  )
  xt_summary <- xtable(df_summary, digits = -2, caption = paste0('Averages for $n=', n_filter, '$'))
  tableFileName <- file.path(EXAMPLES_DIR, paste0('simulation_methodSummary_n', n_filter, '_d', d_filter, '.tex'))

  print(
    xt_summary,
    comment = FALSE,
    only.contents = FALSE,
    floating = FALSE,
    include.rownames = FALSE,
    file = tableFileName
  )
}

cat('Plotting graph etc.\n')

for(d_filter in d_vals){
  # Extract graph from `allSims.RDS`
  dfGraph <- df %>% filter(
    modelSeed == 1,
    d == d_filter
  )
  if(nrow(dfGraph) == 0){
    cat('No graph for d =', d_filter, '\n')
    next
  }
  fileNameGraph <- dfGraph[1, 'fileName']
  sims <- allSims[[fileNameGraph]]
  graph <- sims[[1]]$graph
  Gamma <- sims[[1]]$Gamma
  Theta <- Gamma2Theta(Gamma)
  Theta[abs(Theta) < 1e-6] <- 0
  prefix <- matrix('', nrow(Theta), ncol(Theta))
  prefix[Theta > 0] <- '\\phantom{-}'

  writeMatrixToFile(
    file.path(EXAMPLES_DIR, paste0('simulation_Gamma_d', d_filter, '.tex')),
    Gamma
  )

  writeMatrixToFile(
    file.path(EXAMPLES_DIR, paste0('simulation_Theta_d', d_filter, '.tex')),
    Theta,
    prefix = prefix
  )

  # Plot graph
  myTikz(
    paste0('graph_d', d_filter),
    {
      par(mar = c(0,0,0,0))
      plot(
        graph,
        layout = layout_in_circle,
        vertex.label.cex=1.2,
        vertex.label.color='black',
        vertex.color=NA,
        edge.color='black',
        vertex.size=30,
        edge.width=3
      )
    }
  )
}
