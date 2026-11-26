# Phase 4: structural statistics, centrality, communities, and plots --------
source("R/00_config.R")
PLOT_DIR <- file.path(SNA_DIR, "plots")
dir.create(PLOT_DIR, recursive = TRUE, showWarnings = FALSE)

graph_files <- list.files(NETWORK_DIR, pattern = "\\.graphml$", full.names = TRUE)

analyse_graph <- function(path) {
  g <- igraph::read_graph(path, format = "graphml")
  if (igraph::ecount(g)) E(g)$weight <- as.numeric(E(g)$weight)
  deg <- igraph::degree(g, mode = "all")
  btw <- igraph::betweenness(g, directed = FALSE, weights = NA, normalized = TRUE)
  clo <- igraph::closeness(g, normalized = TRUE, weights = NA)
  pr <- igraph::page_rank(g, directed = FALSE, weights = E(g)$weight)$vector
  communities <- if (vcount(g) > 2 && ecount(g) > 0) {
    igraph::cluster_louvain(igraph::as.undirected(g), weights = E(g)$weight)
  } else NULL
  comm_id <- if (is.null(communities)) rep(NA_integer_, vcount(g)) else
    igraph::membership(communities)[V(g)$name]
  nodes <- data.table::data.table(
    id = V(g)$name, degree = as.numeric(deg),
    betweenness = as.numeric(btw), closeness = as.numeric(clo),
    pagerank = as.numeric(pr), community = as.integer(comm_id)
  )
  stats <- data.table::data.table(
    network = basename(path), nodes = vcount(g), edges = ecount(g),
    density = igraph::edge_density(g, loops = FALSE),
    components = igraph::components(g)$no,
    isolated_nodes = sum(deg == 0),
    average_degree = mean(deg),
    clustering_coefficient = igraph::transitivity(g, type = "average",
                                                  isolates = "zero"),
    modularity = if (is.null(communities)) NA_real_ else
      igraph::modularity(communities)
  )
  data.table::fwrite(nodes, file.path(SNA_DIR,
             paste0(tools::file_path_sans_ext(basename(path)), "_centrality.csv")))
  # A lightweight reproducible overview plot for each graph.
  png(file.path(PLOT_DIR, paste0(tools::file_path_sans_ext(basename(path)), ".png")),
      width = 1400, height = 1000, res = 140)
  set.seed(42)
  if (vcount(g) > 0) {
    vertex_colors <- if ("type" %in% vertex_attr_names(g)) {
      ifelse(vertex_attr(g, "type") == "student", "#2C7FB8", "#F03B20")
    } else "#4D4D4D"
    plot(g, layout = layout_with_fr(g),
         vertex.color = vertex_colors, vertex.size = 4,
         vertex.label = NA, edge.width = 1,
         main = basename(path))
  } else {
    plot.new(); title(main = basename(path))
  }
  dev.off()
  stats
}

if (!length(graph_files)) stop("No GraphML files found. Run R/03_build_networks.R first.")
stats <- data.table::rbindlist(lapply(graph_files, analyse_graph), fill = TRUE)
data.table::fwrite(stats, file.path(SNA_DIR, "network_statistics.csv"))
writeLines(c(
  "# Phase 4 SNA report",
  "",
  "The CSV files in this folder contain structural statistics and node-level",
  "centrality for every exported network. Community labels use Louvain",
  "community detection; modularity and average clustering are reported per graph.",
  "",
  "Interpretation should distinguish weighted edge strength from unweighted",
  "topological measures. Closeness is calculated with unweighted shortest paths."
), file.path(SNA_DIR, "SNA_report.md"))
message("Phase 4 complete. Results are in outputs/sna/.")
