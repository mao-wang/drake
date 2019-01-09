#' @title Prune an igraph. Not a user-side function.
#' @export
#' @keywords internal
#' @description Similar to `igraph::subcomponent(mode = "in")`
#'   but able to support multiple destination vertices.
#'   For the internals of `drake` only.
#'   Not a user-side function.
#' @details Removes igraph attributes, so be careful.
#' @return An `igraph` object
#' @param graph An igraph object to be pruned.
#' @param to Character vector, of destination vertices.
#' @param jobs deprecated
#' @examples
#' # Not a user-side function.
prune_drake_graph <- function(
  graph, to = igraph::V(graph)$name, jobs = 1
) {
  if (!inherits(graph, "igraph")) {
    stop(
      "supplied graph must be an igraph object",
      call. = FALSE
    )
  }
  unlisted <- setdiff(to, V(graph)$name)
  if (length(unlisted)) {
    warning(
      "supplied targets not in the dependency graph:\n",
      multiline_message(unlisted),
      call. = FALSE
    )
    to <- setdiff(to, unlisted)
  }
  if (!length(to)) {
    warning(
      "cannot prune graph: no valid destination vertices supplied",
      call. = FALSE
    )
    return(graph)
  }
  edges <- igraph::as_long_data_frame(graph)
  edges <- weak_tibble(from = edges$from_name, to = edges$to_name)
  edges <- cdg_bfs_edges(edges, to)
  igraph::graph_from_data_frame(edges)
}

deps_graph <- function(targets, graph, reverse = FALSE) {
  if (!length(targets)) {
    return(character(0))
  }
  opt <- igraph::igraph_opt("return.vs.es")
  on.exit(igraph::igraph_options(return.vs.es = opt))
  igraph::igraph_options(return.vs.es = FALSE)
  index <- adjacent_vertices(
    graph = graph,
    v = targets,
    mode = ifelse(reverse, "out", "in")
  )
  index <- unlist(index)
  index <- unique(index)
  igraph::V(graph)$name[index + 1]
}

get_neighborhood <- function(graph, from, mode, order) {
  if (!length(order)) {
    order <- length(V(graph))
  }
  if (length(from)) {
    from <- sanitize_nodes(nodes = from, choices = V(graph)$name)
    egos <- igraph::make_ego_graph(
      graph = graph,
      order = order,
      nodes = from,
      mode = mode
    )
    subset <- lapply(
      X = egos,
      FUN = function(graph) {
      igraph::V(graph)$name
    })
    subset <- clean_dependency_list(subset)
    graph <- subset_graph(graph = graph, subset = subset)
  }
  graph
}

downstream_nodes <- function(from, graph, jobs) {
  if (!length(from)) {
    return(character(0))
  }
  out <- lightly_parallelize(
    X = from,
    FUN = function(node) {
      drake_subcomponent(graph, v = node, mode = "out")$name
    },
    jobs = jobs
  )
  clean_dependency_list(out)
}

leaf_nodes <- function(graph) {
  is_leaf <- igraph::degree(graph, mode = "in") == 0
  V(graph)[is_leaf]$name
}

subset_graph <- function(graph, subset) {
  if (!length(subset)) {
    return(igraph::make_empty_graph())
  }
  subset <- intersect(subset, V(graph)$name)
  igraph::induced_subgraph(graph = graph, vids = subset)
}

drake_subcomponent <- function(...) {
  opt <- igraph::igraph_opt("return.vs.es")
  on.exit(igraph::igraph_options(return.vs.es = opt))
  igraph::igraph_options(return.vs.es = TRUE)
  igraph::subcomponent(...)
}
