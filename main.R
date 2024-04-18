
# # Uncomment to install dependencies (versions might be off, though)
# source('setup.R')

# Source shared prep file(s)
source('prep.R')
FORCE_PDF <- FALSE # Set to TRUE to produce .pdf plots instead of .tikz

# Run small sripts to generate plots/examples
source(file.path(SCRIPTS_DIR, 'main.R'), chdir = TRUE)

# Run flights application
source(file.path(APPLICATION_DIR, 'main_flights.R'), chdir = TRUE)

# Run danube application
source(file.path(APPLICATION_DIR, 'main_danube.R'), chdir = TRUE)

# Run simulation study
REDO_SIMULATIONS <- TRUE
source(file.path(SIMUL_DIR, 'main.R'), chdir = TRUE)

