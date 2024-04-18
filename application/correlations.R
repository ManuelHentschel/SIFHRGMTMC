
## Used to compute extremal chi for all airports
## Needs to be called from main.R!

chiMatList <- list(chi_emp_c)

ind_plot <- IATAS_sel_c

chiVals <- upper.tri.val(chi_emp_c[ind_plot, ind_plot])

allPairs <- upper.tri.val(outer(ind_plot, ind_plot, paste, sep='_'))
names(chiVals) <- allPairs

chosenPairs <- upper.tri.val(outer(IATAS_sel, IATAS_sel, paste, sep='_'))

clusterPairsList <- lapply(kmed$medoids, function(med){
  inds <- names(which(kmed$clustering == kmed$clustering[med]))
  upper.tri.val(outer(inds, inds, paste, sep='_'))
})
names(clusterPairsList) <- kmed$medoids
clusterPairs <- Reduce(c, clusterPairsList)


dfHist <- data.frame(
  chi = chiVals,
  inCluster = allPairs %in% clusterPairs,
  inTexasCluster = allPairs %in% chosenPairs
)

ggpHist <- (
  ggplot(dfHist)
  + geom_histogram(
    aes(x = chi, fill=inCluster),
    bins = 30,
    color = '#000000'
  )
  + labs(fill = 'In Some Cluster')
  + xlab(paste0('$\\widehat\\chi(p)$'))
  + ylab('Count')
  + theme(legend.position = "none")
  + scale_fill_manual(values=c('#999999', '#FFFFFF'))
)

myTikz(
  'chiEmp_hist',
  width = 8,
  plot(ggpHist)
)

