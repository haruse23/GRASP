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
  if (is.null(dim(x))) {
    x <- Matrix::Matrix(x, nrow = length(x), ncol = 1L, sparse = TRUE)
  } else {
    x <- Matrix::Matrix(x, sparse = TRUE)
  }
  x <- methods::as(x, "dgCMatrix")
  sx <- Matrix::summary(x)
  den <- numeric(nrow(x))
  sums <- tapply(sx$x^2, sx$i, sum)
  den[as.integer(names(sums))] <- sums
  den <- sqrt(den)
  den[den == 0] <- 1
  normalized <- x
  normalized@x <- normalized@x / den[normalized@i + 1L]
  Matrix::drop0(crossprod(normalized))
}

jaccard_similarity <- function(x) {
  binary <- methods::as(x, "dgCMatrix")
  binary@x[] <- 1
  intersection <- Matrix::drop0(crossprod(binary))
  sb <- Matrix::summary(binary)
  totals <- tabulate(sb$i, nbins = nrow(binary))
  ij <- Matrix::summary(intersection)
  ij$x <- ij$x / (totals[ij$i] + totals[ij$j] - ij$x)
  Matrix::sparseMatrix(i = ij$i, j = ij$j, x = ij$x,
                       dims = dim(intersection))
}

similarity_edges <- function(mat, ids, measure = SIMILARITY,
                             cutoff = SIMILARITY_CUTOFF) {
  if (is.null(dim(mat))) {
    mat <- Matrix::Matrix(mat, nrow = length(ids), sparse = TRUE)
  }
  sim <- switch(
    measure,
    cosine = cosine_similarity(mat),
    jaccard = jaccard_similarity(mat),
    stop("Unsupported similarity measure: ", measure)
  )
  sim <- Matrix::drop0(sim)
  ij <- Matrix::summary(sim)
  ij <- ij[ij$i < ij$j & is.finite(ij$x) & ij$x > cutoff, , drop = FALSE]
  if (!nrow(ij)) {
    return(data.table::data.table(from = character(), to = character(),
                                  weight = numeric(), similarity = character()))
  }
  data.table::data.table(
    from = ids[ij$i], to = ids[ij$j],
    weight = ij$x, similarity = measure
  )
}

write_graphml <- function(nodes, edges, path, directed = FALSE) {
  nodes <- data.table::as.data.table(nodes)
  edges <- data.table::as.data.table(edges)
  nodes <- nodes[!is.na(id)]
  edges <- edges[stats::complete.cases(edges)]
  edges <- edges[!is.na(from) & !is.na(to)]
  nodes <- unique(nodes, by = "id")
  g <- igraph::graph_from_data_frame(edges, directed = directed, vertices = nodes)
  igraph::write_graph(g, path, format = "graphml")
}
