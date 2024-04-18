
# Set wd to dir containing this script
setwd(SCRIPTS_DIR)

# Generate examples of graphs, (partial) matrices, etc.
source(file.path(SCRIPTS_DIR, 'generateExamples.R'), chdir = TRUE)

# Generate detailed example of decomposable completion algorithm
source(file.path(SCRIPTS_DIR, 'decompCompletionExample.R'), chdir=TRUE)

# Generate detailed example of general completion algorithm
source(file.path(SCRIPTS_DIR, 'generalCompletionExample.R'), chdir = TRUE)

