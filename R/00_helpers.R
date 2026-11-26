# Shared helpers --------------------------------------------------------------

read_oulad <- function(name, data_dir = DATA_DIR) {
  path <- file.path(data_dir, name)
  if (!file.exists(path)) stop("Missing file: ", path)
  first_line <- readLines(path, n = 1L, warn = FALSE)
  if (startsWith(first_line, "version https://git-lfs.github.com")) {
    stop(
      name, " is a Git LFS pointer, not the data file. ",
      "Download the LFS object from GitHub before running the pipeline."
    )
  }
  data.table::fread(path, na.strings = c("", "NA", "NULL"))
}

profile_table <- function(x, name) {
  data.table::data.table(
    table = name,
    rows = nrow(x),
    columns = ncol(x),
    duplicate_rows = sum(duplicated(x)),
    missing_cells = sum(is.na(x)),
    missing_percent = round(100 * sum(is.na(x)) / max(1, length(x)), 4),
    types = paste(sprintf("%s:%s", names(x), vapply(x, typeof, character(1))),
                  collapse = "; ")
  )
}

safe_name <- function(x) gsub("[^A-Za-z0-9_-]", "_", x)

cosine_similarity <- function(x) {
  den <- sqrt(rowSums(x^2))
  out <- tcrossprod(x)
  out / outer(den, den, "*")
}

jaccard_similarity <- function(x) {
  binary <- (x > 0) * 1
  intersection <- tcrossprod(binary)
  totals <- rowSums(binary)
  union <- outer(totals, totals, "+") - intersection
  intersection / union
}

similarity_edges <- function(mat, ids, measure = SIMILARITY,
                             cutoff = SIMILARITY_CUTOFF) {
  sim <- switch(
    measure,
    cosine = cosine_similarity(mat),
    jaccard = jaccard_similarity(mat),
    stop("Unsupported similarity measure: ", measure)
  )
  diag(sim) <- 0
  ij <- which(upper.tri(sim) & is.finite(sim) & sim > cutoff, arr.ind = TRUE)
  if (!nrow(ij)) {
    return(data.table::data.table(from = character(), to = character(),
                                  weight = numeric(), similarity = character()))
  }
  data.table::data.table(
    from = ids[ij[, 1]], to = ids[ij[, 2]],
    weight = sim[ij], similarity = measure
  )
}

write_graphml <- function(nodes, edges, path, directed = FALSE) {
  g <- igraph::graph_from_data_frame(edges, directed = directed, vertices = nodes)
  igraph::write_graph(g, path, format = "graphml")
}
