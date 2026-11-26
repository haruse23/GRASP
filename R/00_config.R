# GRASP configuration ---------------------------------------------------------
# All dates in OULAD are relative to the start of a presentation.

options(stringsAsFactors = FALSE)

GRASP_ROOT <- normalizePath(".", winslash = "/", mustWork = FALSE)
DATA_DIR <- file.path(GRASP_ROOT, "data")
OUTPUT_DIR <- file.path(GRASP_ROOT, "outputs")
PROFILE_DIR <- file.path(OUTPUT_DIR, "profiling")
CLEAN_DIR <- file.path(OUTPUT_DIR, "clean")
NETWORK_DIR <- file.path(OUTPUT_DIR, "networks")
SNA_DIR <- file.path(OUTPUT_DIR, "sna")

dir.create(PROFILE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(CLEAN_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(NETWORK_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(SNA_DIR, recursive = TRUE, showWarnings = FALSE)

CUTOFFS <- c(day28 = 28L, day56 = 56L)
SNAPSHOT_WEEKS <- 0:8
SIMILARITY <- "cosine" # supported: cosine, jaccard
SIMILARITY_CUTOFF <- 0 # retain positive similarities

# The fourth network is the temporal student-resource network: the same
# bipartite relation exported separately for each weekly snapshot.
NETWORKS <- c(
  student_resource = "Student-resource bipartite",
  student_student = "Student-student projection",
  resource_resource = "Resource-resource projection",
  student_resource_temporal = "Temporal student-resource snapshots"
)

required_packages <- c("data.table", "igraph", "Matrix")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop(
    "Install required R packages first: install.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "), "))"
  )
}

source("R/00_helpers.R")
