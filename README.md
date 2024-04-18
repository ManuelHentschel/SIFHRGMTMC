
## Reproducibility Materials

This repository contains the code used to reproduce the results from the paper
*"Statistical Inference for Hüsler–Reiss Graphical Models Through Matrix Completions"*.

The code contains four parts:

- scripts to recreate small figures and examples,
- the analysis of the flights dataset,
- the analysis of the Danube river dataset,
- the simulation study.

The file `main.R` will run each of these parts in order.
The file `prep.R` contains utility functions and configurations shared by all parts.
The file `setup.R` installs the R packages used into a local R library location.
Uncomment the corresponding line in `main.R` to run this script before the other files.

The four parts are independent of each other, to skip some of them,
simply comment out the corresponding line in `main.R`.
Reproducing the results of the simulation study takes by far the longest time (ca 9h).
The other scripts usually finish in less than 1h.

By default, tikz plots will be generated and placed in `./tikz/`.
In order to generate PDF plots, placed in `./pdf/`, set `FORCE_PDF <- TRUE` in `main.R`.

The datasets are shipped with the **GitHub version** (!) of the R-package `graphicalExtremes`,
which can be installed using `remotes::install_github()`.
