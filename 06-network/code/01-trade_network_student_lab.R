###
# 01 - Trade Networks: Student Lab
# Data Science for Economists - Networks application
# March 2026
###

# This lab starts from clean BACI product-level trade data and builds the
# network objects step by step.
#
# Learning goals:
# 1. Turn trade flows into an edge list and node list.
# 2. Compute density manually and compare it to igraph.
# 3. Compare three HS4 product-specific networks:
#    semiconductors, automobiles, and wheat.
# 4. Compute country centrality using unweighted and weighted measures.
# 5. Optional: compare observed hub structure with a simple random network.

if (!require("pacman")) install.packages("pacman")
library(pacman)

p_load(
  data.table,
  tidyverse,
  igraph,
  here
)

# =============================================================================
# STUDENT LAB VERSION
# =============================================================================

# How to use this file:
# - Work through the exercises in order.
# - Replace TODO(...) with your own code.
# - The 00-file only cleans BACI. You build the network here.

TODO <- function(...) {
  stop("Replace TODO(...) with your own code before running this exercise.",
       call. = FALSE)
}

# -----------------------------------------------------------------------------
# Setup
# -----------------------------------------------------------------------------

source_file <- here("06-network", "code", "00-build_trade_network_database.R")
if (!file.exists(source_file)) source_file <- "00-build_trade_network_database.R"
source(source_file)

dir.create(here("06-network", "output"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("06-network", "output", "figures"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("06-network", "output", "tables"), showWarnings = FALSE, recursive = TRUE)

clean_baci <- build_clean_baci(force_rebuild = FALSE)

glimpse(clean_baci)

# Interpretation:
# What are the nodes? What are the edges?

# -----------------------------------------------------------------------------
# Exercise 1. Create the country-country edge list
# -----------------------------------------------------------------------------

# We aggregate over products.
# One row should become one directed exporter-importer link.
# weight = total trade value over all products for that country pair.
# n_products = number of HS6 products traded on that link.

country_edges <- clean_baci |>
  group_by(TODO("exporter column"), TODO("importer column")) |>
  summarise(
    weight = TODO("sum trade_value"),
    n_products = TODO("number of distinct product_code"),
    .groups = "drop"
  ) |>
  arrange(TODO("descending weight"))

glimpse(country_edges)

# 1a. Show the 10 largest exporter-importer links by total trade value.
country_edges |>
  slice_head(TODO("top 10 rows")) |>
  print()

# Discussion:
# - What does an edge represent here?
# - Why is the network directed?

# -----------------------------------------------------------------------------
# Exercise 2. Create the node list
# -----------------------------------------------------------------------------

# Nodes are countries. A country is a node if it appears as exporter or importer.

exporter_vector <- TODO("take the exporter column from country_edges")
importer_vector <- TODO("take the importer column from country_edges")

country_nodes <- tibble(
  name = TODO("combine exporter_vector and importer_vector, keep unique values, and sort them")
)

glimpse(country_nodes)

n_nodes <- TODO("number of rows in country_nodes")
n_edges <- TODO("number of rows in country_edges")

cat("Number of nodes:", n_nodes, "\n")
cat("Number of directed edges:", n_edges, "\n")

# -----------------------------------------------------------------------------
# Exercise 3. Compute density manually
# -----------------------------------------------------------------------------

# This is a directed graph without self-loops.
# Maximum possible directed edges = n * (n - 1).

manual_density <- TODO("n_edges divided by maximum possible directed edges")

cat("Manual density:", round(manual_density, 4), "\n")

# Interpretation:
# Density is the share of all possible exporter-importer links that are observed.

# -----------------------------------------------------------------------------
# Exercise 4. Build the same graph with igraph
# -----------------------------------------------------------------------------

# Build a directed graph from the edge list and node list.

g_trade <- graph_from_data_frame(
  d = country_edges |>
    select(
      from = TODO("exporter column"),
      to = TODO("importer column"),
      weight,
      n_products
    ),
  vertices = country_nodes,
  directed = TODO("TRUE or FALSE?")
)

cat("igraph nodes:", TODO("count nodes in g_trade"), "\n")
cat("igraph edges:", TODO("count edges in g_trade"), "\n")
cat("Is directed:", is_directed(TODO("graph object")), "\n")

igraph_density <- edge_density(TODO("graph object"), loops = FALSE)

cat("Manual density:", round(manual_density, 4), "\n")
cat("igraph density:", round(igraph_density, 4), "\n")
cat("Are they equal?", all.equal(manual_density, igraph_density), "\n")

# Discussion:
# - Why do the manual and igraph densities match?
# - What would change if the network were undirected?

# -----------------------------------------------------------------------------
# Exercise 5. Basic distance and connectedness measures
# -----------------------------------------------------------------------------

# Important:
# g_trade has an edge attribute called weight.
# For topological/unweighted distances, we set weights = NA.

# practice with components 
g_toy <- graph_from_data_frame(tibble(from = c("A", "B", "E"), to = c("B", "C", "F")), directed = FALSE)
plot(g_toy, vertex.size = 30, main = "Two components: A-B-C and E-F")
toy_comp <- components(g_toy)
toy_comp$csize  # component sizes: 3 and 2

summarise_distances <- function(g, network_name) {
  
  comp <- components(g, mode = "weak")
  
  tibble(
    network = network_name,
    nodes = TODO("number of nodes"),
    edges = TODO("number of edges"),
    density = TODO("edge density with no loops"),
    largest_component_share = TODO("largest component size divided by number of nodes"),
    avg_path_length = mean_distance(
      g,
      directed = TRUE,
      unconnected = TRUE,
      weights = NA
    ),
    diameter = diameter(
      g,
      directed = TRUE,
      unconnected = TRUE,
      weights = NA
    )
  )
}

global_summary <- summarise_distances(g_trade, "All products")
print(global_summary)

# Discussion:
# - Is the aggregate trade network sparse or dense?
# - What does largest component share tell us?

# -----------------------------------------------------------------------------
# Exercise 6. Build three HS4 product-specific networks
# -----------------------------------------------------------------------------

# Product definitions:
# - Semiconductors: HS 8542, electronic integrated circuits
# - Automobiles:    HS 8703, motor cars and passenger vehicles
# - Wheat:          HS 1001, wheat and meslin

hs4_products <- tribble(
  ~network_name,      ~hs4,
  "Semiconductors",   "8542",
  "Automobiles",      "8703",
  "Wheat",            "1001"
)

make_hs4_network <- function(data, network_name, hs4_code) {
  
  product_data <- data |>
    mutate(hs4 = str_sub(product_code, 1, 4)) |>
    filter(TODO("keep only rows where hs4 equals hs4_code"))
  
  product_edges <- product_data |>
    group_by(TODO("exporter column"), TODO("importer column")) |>
    summarise(
      weight = TODO("sum trade_value"),
      n_products = TODO("number of distinct product_code"),
      .groups = "drop"
    ) |>
    arrange(desc(weight))
  
  product_nodes <- tibble(
    name = sort(unique(c(TODO("product exporters"), TODO("product importers"))))
  )
  
  g <- graph_from_data_frame(
    d = product_edges |>
      select(from = exp, to = imp, weight, n_products),
    vertices = product_nodes,
    directed = TRUE
  )
  
  list(
    name = network_name,
    hs4 = hs4_code,
    data = product_data,
    edges = product_edges,
    nodes = product_nodes,
    graph = g
  )
}

product_networks <- hs4_products |>
  mutate(
    network_object = map2(
      network_name,
      hs4,
      ~ make_hs4_network(
        data = clean_baci,
        network_name = .x,
        hs4_code = .y
      )
    )
  )

semiconductors <- product_networks$network_object[[1]]
automobiles    <- product_networks$network_object[[2]]
wheat          <- product_networks$network_object[[3]]

# 6a. Inspect the largest links for each product-specific network.
semiconductors$edges |> slice_head(n = 10) |> print()
automobiles$edges |> slice_head(n = 10) |> print()
wheat$edges |> slice_head(n = 10) |> print()

# -----------------------------------------------------------------------------
# Exercise 7. Compare product-specific networks
# -----------------------------------------------------------------------------

product_distance_summary <- bind_rows(
  summarise_distances(TODO("semiconductor graph"), "Semiconductors"),
  summarise_distances(TODO("automobile graph"), "Automobiles"),
  summarise_distances(TODO("wheat graph"), "Wheat")
)

print(product_distance_summary)

write_csv(
  product_distance_summary,
  here("06-network", "output", "tables", "product_network_distance_summary.csv")
)

# Discussion:
# - Which HS4 product network is densest?
# - Which has the shortest average path length?
# - Which has the largest diameter?
# - What does this tell us about product-specific trade networks?

# -----------------------------------------------------------------------------
# Exercise 8. Visualise the semiconductor network
# -----------------------------------------------------------------------------

# Full product-specific networks can still be dense.
# For a readable class figure, keep the strongest 60 directed links.

semiconductor_edges_top <- semiconductors$edges |>
  slice_max(TODO("trade value column"), n = 60, with_ties = FALSE)

semiconductor_nodes_top <- tibble(
  name = sort(unique(c(semiconductor_edges_top$exp, semiconductor_edges_top$imp)))
)

g_semiconductor_top <- graph_from_data_frame(
  d = semiconductor_edges_top |>
    select(from = exp, to = imp, weight, n_products),
  vertices = semiconductor_nodes_top,
  directed = TRUE
)

# Node size = total semiconductor trade strength within this top-link subnetwork.
V(g_semiconductor_top)$strength <- strength(
  g_semiconductor_top,
  mode = "all",
  weights = E(g_semiconductor_top)$weight
)

# These two lines only affect the plot.
# Larger nodes have more total semiconductor trade.
# Thicker arrows represent larger semiconductor trade flows.
V(g_semiconductor_top)$size <- 4 + 12 * (
  V(g_semiconductor_top)$strength / max(V(g_semiconductor_top)$strength)
)

E(g_semiconductor_top)$width <- 0.5 + 4 * (
  E(g_semiconductor_top)$weight / max(E(g_semiconductor_top)$weight)
)

set.seed(42)
plot(
  g_semiconductor_top,
  layout = layout_with_fr(g_semiconductor_top),
  vertex.label.cex = 0.7,
  vertex.label.color = "black",
  vertex.color = "lightblue",
  vertex.frame.color = "grey40",
  edge.arrow.size = 0.25,
  edge.curved = 0.15,
  main = "Semiconductor trade network: top 60 links"
)

png(
  filename = here("06-network", "output", "figures", "semiconductor_network_top60.png"),
  width = 1200,
  height = 900,
  res = 150
)
set.seed(42)
plot(
  g_semiconductor_top,
  layout = layout_with_fr(g_semiconductor_top),
  vertex.label.cex = 0.7,
  vertex.label.color = "black",
  vertex.color = "lightblue",
  vertex.frame.color = "grey40",
  edge.arrow.size = 0.25,
  edge.curved = 0.15,
  main = "Semiconductor trade network: top 60 links"
)
dev.off()

# -----------------------------------------------------------------------------
# Exercise 9. Unweighted centrality
# -----------------------------------------------------------------------------

# Unweighted centrality treats all trade links equally.
# Because the graph has a weight attribute, use weights = NA where relevant.

centrality_unweighted <- tibble(
  country = V(g_trade)$name,
  indegree = as.numeric(degree(TODO("graph object"), mode = TODO("in or out?"))),
  outdegree = as.numeric(degree(TODO("graph object"), mode = TODO("in or out?"))),
  closeness_out = as.numeric(closeness(
    TODO("graph object"),
    mode = "out",
    weights = NA,
    normalized = TRUE
  )),
  betweenness = as.numeric(betweenness(
    TODO("graph object"),
    directed = TRUE,
    weights = NA,
    normalized = TRUE
  )),
  eigenvector = as.numeric(eigen_centrality(
    TODO("graph object"),
    directed = TRUE,
    weights = NA
  )$vector)
)

# 9a. Top importers by number of trade partners.
centrality_unweighted |>
  arrange(TODO("descending indegree")) |>
  slice_head(n = 10) |>
  print()

# 9b. Top exporters by number of trade partners.
centrality_unweighted |>
  arrange(TODO("descending outdegree")) |>
  slice_head(n = 10) |>
  print()

# 9c. Top brokerage countries.
centrality_unweighted |>
  arrange(TODO("descending betweenness")) |>
  slice_head(n = 10) |>
  print()

# -----------------------------------------------------------------------------
# Exercise 10. Weighted centrality: strength
# -----------------------------------------------------------------------------

# Strength is weighted degree.
# In-strength = total imports.
# Out-strength = total exports.

centrality_weighted <- tibble(
  country = V(g_trade)$name,
  in_strength = as.numeric(strength(
    TODO("graph object"),
    mode = TODO("in or out?"),
    weights = TODO("edge weights")
  )),
  out_strength = as.numeric(strength(
    TODO("graph object"),
    mode = TODO("in or out?"),
    weights = TODO("edge weights")
  ))
)

# 10a. Top importers by trade volume.
centrality_weighted |>
  arrange(TODO("descending in_strength")) |>
  slice_head(n = 10) |>
  print()

# 10b. Top exporters by trade volume.
centrality_weighted |>
  arrange(TODO("descending out_strength")) |>
  slice_head(n = 10) |>
  print()

# -----------------------------------------------------------------------------
# Exercise 11. Compare weighted and unweighted centrality
# -----------------------------------------------------------------------------

centrality_all <- centrality_unweighted |>
  left_join(centrality_weighted, by = "country") |>
  mutate(
    rank_indegree = min_rank(desc(indegree)),
    rank_outdegree = min_rank(desc(outdegree)),
    rank_in_strength = min_rank(desc(in_strength)),
    rank_out_strength = min_rank(desc(out_strength)),
    rank_betweenness = min_rank(desc(betweenness))
  )

# 11a. Compare import-partner centrality and import-value centrality.
centrality_all |>
  select(country, indegree, in_strength, rank_indegree, rank_in_strength) |>
  arrange(TODO("rank by import value")) |>
  slice_head(n = 15) |>
  print()

# 11b. Compare export-partner centrality and export-value centrality.
centrality_all |>
  select(country, outdegree, out_strength, rank_outdegree, rank_out_strength) |>
  arrange(TODO("rank by export value")) |>
  slice_head(n = 15) |>
  print()

write_csv(
  centrality_all,
  here("06-network", "output", "tables", "country_centrality_all.csv")
)

# -----------------------------------------------------------------------------
# Exercise 12. Simple plots for class discussion
# -----------------------------------------------------------------------------

p_degree_distribution <- centrality_all |>
  ggplot(aes(x = TODO("indegree column"))) +
  geom_histogram(bins = 30) +
  labs(
    title = "In-degree distribution in the global trade network",
    x = "In-degree: number of origin countries",
    y = "Number of countries"
  ) +
  theme_minimal()

print(p_degree_distribution)

p_degree_strength <- centrality_all |>
  ggplot(aes(x = TODO("outdegree column"), y = TODO("out_strength column"))) +
  geom_point(alpha = 0.6) +
  scale_y_log10() +
  labs(
    title = "Out-degree vs out-strength",
    subtitle = "Many partners does not always mean large export volume",
    x = "Out-degree: number of destination countries",
    y = "Out-strength: total export value, log scale"
  ) +
  theme_minimal()

print(p_degree_strength)

# Final discussion prompts:
# 1. What is the difference between a trade link and a trade value?
# 2. Why is density different for directed and undirected networks?
# 3. Which HS4 product network looks most connected: semiconductors, autos, or wheat?
# 4. Which countries are central by partner count?
# 5. Which countries are central by trade volume?
# 6. Does the definition of centrality change the economic story?

# -----------------------------------------------------------------------------
# Optional Exercise. Null model: are product networks more hub-dominated than random?
# -----------------------------------------------------------------------------

# Question:
# Are the most central countries in each product network more central than we
# would expect in a random network with the same number of nodes and density?
#
# Null model:
# Erdős-Rényi directed random graph G(n, p)
# - keeps fixed: number of nodes and density
# - ignores: geography, country size, trade costs, specialization, GVC structure

run_degree_null <- function(g, network_name, n_sim = 100) {
  
  observed_density <- TODO("graph density with no loops")
  
  observed_stats <- tibble(
    statistic = c(
      "Max in-degree",
      "Max out-degree",
      "In-degree centralization",
      "Out-degree centralization"
    ),
    observed = c(
      TODO("maximum in-degree"),
      TODO("maximum out-degree"),
      TODO("in-degree centralization"),
      TODO("out-degree centralization")
    )
  )
  
  null_stats <- replicate(n_sim, {
    
    g_null <- sample_gnp(
      n = TODO("number of nodes"),
      p = TODO("observed density"),
      directed = TRUE,
      loops = FALSE
    )
    
    c(
      "Max in-degree" = max(degree(g_null, mode = "in")),
      "Max out-degree" = max(degree(g_null, mode = "out")),
      "In-degree centralization" =
        centr_degree(g_null, mode = "in", loops = FALSE, normalized = TRUE)$centralization,
      "Out-degree centralization" =
        centr_degree(g_null, mode = "out", loops = FALSE, normalized = TRUE)$centralization
    )
  })
  
  as_tibble(t(null_stats)) |>
    pivot_longer(
      cols = everything(),
      names_to = "statistic",
      values_to = "null_value"
    ) |>
    left_join(observed_stats, by = "statistic") |>
    group_by(statistic, observed) |>
    summarise(
      mean_null = mean(null_value, na.rm = TRUE),
      sd_null = sd(null_value, na.rm = TRUE),
      share_null_at_least_observed = mean(null_value >= observed, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      network = network_name,
      difference = observed - mean_null,
      ratio = observed / mean_null,
      z_score = (observed - mean_null) / sd_null,
      across(
        c(observed, mean_null, sd_null, difference, ratio, z_score),
        ~ round(.x, 3)
      )
    ) |>
    select(
      network,
      statistic,
      observed,
      mean_null,
      difference,
      ratio,
      z_score,
      share_null_at_least_observed
    ) |>
    arrange(network, statistic)
}

centrality_null_summary <- bind_rows(
  run_degree_null(TODO("semiconductor graph"), "Semiconductors"),
  run_degree_null(TODO("automobile graph"), "Automobiles"),
  run_degree_null(TODO("wheat graph"), "Wheat")
)

print(centrality_null_summary)

write_csv(
  centrality_null_summary,
  here("06-network", "output", "tables", "centrality_null_summary.csv")
)

# Interpretation:
# - If observed > mean_null, the real product network is more hub-dominated
#   than a random network with the same density.
# - If share_null_at_least_observed is close to 0, very few random networks
#   generate hubs as extreme as the observed network.
# - This tells us whether centrality concentration is surprising, not just
#   whether the network is dense or sparse.
