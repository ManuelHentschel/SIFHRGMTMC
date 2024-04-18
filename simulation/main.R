
tictoc::tic()

setTikzSubDir('simulation')

REDO_SIMULATIONS <- isTRUE(globalenv()[['REDO_SIMULATIONS']])

## Takes very long
if(REDO_SIMULATIONS){
  # Delete all previous results
  rdsFiles <- dir(pattern = '\\.RDS$')
  cat('Removing present RDS files:\n')
  file.remove(rdsFiles)

  # Run first set of parameters
  Sys.setenv(SIM_D = 6, SIM_N = 20, SIM_N_SIMULATIONS = 20)
  source('simulate.R')

  # Run second set of parameters
  Sys.setenv(SIM_D = 10, SIM_N = 200, SIM_N_SIMULATIONS = 20)
  source('simulate.R')
}

# Analyse and plot results
source('computeStats.R')
source('makePlots.R')

tictoc::toc()
