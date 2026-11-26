# GRASP

Graph-based Retention and Academic Success Prediction.

## R network pipeline

This repository implements the assigned **Phase 1, Phase 3, and Phase 4**
activities in R:

```r
install.packages(c("data.table", "igraph"))
source("R/01_profile_clean.R")
source("R/03_build_networks.R")
source("R/04_sna.R")
```

Run from the repository root. Phase 1 writes profiling, cleaned data, and the
network methodology to `outputs/`. Phase 3 writes node/edge CSV files and
GraphML files. Phase 4 writes structural statistics, centrality, communities,
and the SNA report.

The repository currently contains Git LFS pointers for `data/studentVle.csv`
and `data/vle.csv`; download the actual files before running the pipeline.
