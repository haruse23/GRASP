# Phase 3: construct networks, snapshots, and GraphML exports ---------------
source("R/00_config.R")
clean <- readRDS(file.path(CLEAN_DIR, "oulad_clean.rds"))
vle <- clean$vle
events <- clean$studentVle

events[, student_id := paste(code_module, code_presentation, id_student, sep = "_")]
events[, resource_id := paste(code_module, code_presentation, id_site, sep = "_")]
events[, week := pmax(0L, floor(date / 7))]

make_bipartite <- function(x, suffix) {
  edges <- x[, .(weight = sum(sum_click, na.rm = TRUE)),
             by = .(student_id, resource_id)]
  nodes <- data.table::rbindlist(list(
    x[, .(id = paste0("student:", unique(student_id)), type = "student")],
    x[, .(id = paste0("resource:", unique(resource_id)), type = "resource")]
  ), fill = TRUE)
  nodes <- unique(nodes)
  edges[, `:=`(
    from = paste0("student:", student_id),
    to = paste0("resource:", resource_id)
  )]
  edges[, c("student_id", "resource_id") := NULL]
  # igraph interprets the first two edge columns as source and target.
  edges <- edges[, .(from, to, weight)]
  data.table::fwrite(nodes, file.path(NETWORK_DIR, paste0("nodes_", suffix, ".csv")))
  data.table::fwrite(edges, file.path(NETWORK_DIR, paste0("edges_", suffix, ".csv")))
  write_graphml(nodes, edges, file.path(NETWORK_DIR, paste0(suffix, ".graphml")))
  invisible(list(nodes = nodes, edges = edges))
}

make_projection <- function(x, dimension = c("student", "resource"), suffix) {
  dimension <- match.arg(dimension)
  if (dimension == "student") {
    wide <- x[, .(weight = sum(sum_click)), by = .(student_id, resource_id)]
    ids <- unique(wide$student_id)
    cols <- unique(wide$resource_id)
    mat <- Matrix::sparseMatrix(
      i = match(wide$student_id, ids),
      j = match(wide$resource_id, cols),
      x = wide$weight,
      dims = c(length(ids), length(cols)),
      dimnames = list(ids, cols)
    )
  } else {
    wide <- x[, .(weight = sum(sum_click)), by = .(resource_id, student_id)]
    ids <- unique(wide$resource_id)
    cols <- unique(wide$student_id)
    mat <- Matrix::sparseMatrix(
      i = match(wide$resource_id, ids),
      j = match(wide$student_id, cols),
      x = wide$weight,
      dims = c(length(ids), length(cols)),
      dimnames = list(ids, cols)
    )
  }
  edges <- similarity_edges(mat, ids)
  nodes <- data.table::data.table(id = ids, type = dimension)
  data.table::fwrite(nodes, file.path(NETWORK_DIR, paste0("nodes_", suffix, ".csv")))
  data.table::fwrite(edges, file.path(NETWORK_DIR, paste0("edges_", suffix, ".csv")))
  write_graphml(nodes, edges, file.path(NETWORK_DIR, paste0(suffix, ".graphml")))
  invisible(list(nodes = nodes, edges = edges))
}

for (cut_name in names(CUTOFFS)) {
  x <- events[date <= CUTOFFS[[cut_name]]]
  make_bipartite(x, paste0("student_resource_", cut_name))
  make_projection(x, "student", paste0("student_student_", cut_name))
  make_projection(x, "resource", paste0("resource_resource_", cut_name))
}

for (wk in SNAPSHOT_WEEKS) {
  x <- events[week == wk]
  if (nrow(x)) make_bipartite(x, paste0("student_resource_week", wk))
}
message("Phase 3 complete. Network tables and GraphML files are in outputs/networks/.")
