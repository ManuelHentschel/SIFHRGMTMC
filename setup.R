
## Helper script to (try to) install relevant packages
##
## Does not check versions.
## On some platforms, external dependencies etc. might need to be installed.
## Uses a local R library to avoid conflicts with already installed versions.


# To be sure that current version are installed,
# start with an empty library location.
LIB_PATH <- './tempRLib'
if(!dir.exists(LIB_PATH)) dir.create(LIB_PATH)
.libPaths(LIB_PATH)


# Required packages
packages <- c(
  'dplyr',
  'ggplot2',
  'igraph',
  'dplyr',
  'tidyr',
  'memoise',
  'cachem',
  'parallel',
  'cluster',
  'patchwork',
  'tikzDevice',
  'extRemes',
  'tictoc',
  'benchmarkme',
  'dplyr',
  'magrittr',
  'igraph',
  'xtable',
  'maps',
  'remotes' # To install the required graphicalExtremes version
)

# On windows, use binary packages (faster and we don't need the latest source versions)
if(Sys.info()['sysname'] == 'Windows'){
  options(pkgType = 'binary')
}

# Install missing packages
installed <- .packages(all = TRUE)
missing <- setdiff(packages, installed)
cat('Installing', length(missing), 'missing packages...\n')
for(pkg in missing){
  cat(pkg, '\n')
  install.packages(pkg)
}

# Install up-to-date graphicalExtremes
cat('Installing GitHub version of graphicalExtremes...\n')
remotes::install_github(
  'sebastian-engelke/graphicalExtremes@v0.3.2',
  dependencies = NA,
  upgrade = 'never'
)

# Make list of used packages
PKG_LIST <- 'packages.txt'
ip <- installed.packages()
cat('', file=PKG_LIST)
for(pkg in c(packages, 'graphicalExtremes')){
  cat('- ', pkg, ': v', ip[pkg, 'Version'], '\n', sep='', file=PKG_LIST, append=TRUE)
}
