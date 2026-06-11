# install.packages(c("tidyverse", "tidygraph", "ggraph", "tidyquant"))

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(tidygraph)
library(ggraph)
library(igraph)

set.seed(406) # Carefully selected for elegant side-by-side gradual scaling symmetry

# ==========================================
# 1. GENERATE SPARE CORES & AUTOMATICALLY BRIDGE BY SCORE
# ==========================================
core_size <- 100

# Generate preferential attachment cores
g_core_A <- sample_pa(n = core_size, power = 1.2, m = 4, directed = FALSE)
g_core_B <- sample_pa(n = core_size, power = 1.2, m = 4, directed = FALSE)

df_edges_A <- as_tibble(as_data_frame(g_core_A, what = "edges"))
df_edges_B <- as_tibble(as_data_frame(g_core_B, what = "edges")) %>% 
  mutate(from = from + core_size, to = to + core_size) # Shift IDs for Cluster B

# --- AUTOMATED BRIDGING ENGINE ---
# Identify the top 10 most connected hub nodes from Cluster A
hubs_A <- tibble(node = 1:core_size, degree = degree(g_core_A)) %>% 
  arrange(desc(degree)) %>% 
  slice_head(n = 10) %>% 
  pull(node)

# Identify the top 10 most connected hub nodes from Cluster B (and shift their IDs)
hubs_B <- tibble(node = 1:core_size, degree = degree(g_core_B)) %>% 
  arrange(desc(degree)) %>% 
  slice_head(n = 10) %>% 
  mutate(node = node + core_size) %>% 
  pull(node)

# Create an expansive grid connection mapping between all top hubs (10 x 10 = 100 edges)
# Then sample a subset of them to keep it dense but organically irregular
bridge_edges <- expand_grid(from = hubs_A, to = hubs_B) %>% 
  slice_sample(prop = 0.45) # Retains roughly 45 highly structural cross-cluster links

# Setup Node Metadata base
df_nodes <- tibble(
  name = 1:(core_size * 2),
  community = rep(c("Cluster_A", "Cluster_B"), each = core_size)
)

# ==========================================
# 2. GENERATE THE PERIPHERAL "FAN" LEAF NODES
# ==========================================
n_leaves_per_cluster <- 85
total_cores <- core_size * 2

# Fan leaf nodes for Cluster A (attaching to nodes proportionally to their importance)
leaves_A_id <- (total_cores + 1):(total_cores + n_leaves_per_cluster)
edges_leaves_A <- tibble(
  from = sample(1:core_size, n_leaves_per_cluster, replace = TRUE, prob = (1:core_size)^(-0.5)),
  to = leaves_A_id
)

# Fan leaf nodes for Cluster B (attaching to nodes proportionally to their importance)
leaves_B_id <- (total_cores + n_leaves_per_cluster + 1):(total_cores + 2 * n_leaves_per_cluster)
edges_leaves_B <- tibble(
  from = sample((core_size + 1):total_cores, n_leaves_per_cluster, replace = TRUE, prob = (1:core_size)^(-0.5)),
  to = leaves_B_id
)

all_edges <- bind_rows(df_edges_A, df_edges_B, bridge_edges, edges_leaves_A, edges_leaves_B)
all_nodes <- bind_rows(
  df_nodes,
  tibble(name = leaves_A_id, community = "Cluster_A_Leaf"),
  tibble(name = leaves_B_id, community = "Cluster_B_Leaf")
)

# ==========================================
# 3. ASSEMBLE TIDYGRAPH SYSTEM
# ==========================================
graph <- tbl_graph(nodes = all_nodes, edges = all_edges, directed = FALSE) %>%
  activate(nodes) %>%
  mutate(component = group_components()) %>% 
  filter(component == 1) %>%
  mutate(centrality = centrality_degree()) # Continuous scale for gradual mapping

graph <- graph %>%
  activate(edges)  %>%
  mutate(weight = runif(with_graph(., graph_size()), 0.3, 1.0))

# ==========================================
# 4. BLACK CANVAS NEON VISUALIZATION
# ==========================================
neon_colors <- c(
  "Cluster_A"      = "#ff9800", # Bright Core Amber
  "Cluster_A_Leaf" = "#ff781e", # Fan Light Amber
  "Cluster_B"      = "#ff9800", # Bright Neon Pink Core
  "Cluster_B_Leaf" = "#ff781e"  # Vivid Coral/Vermillion Fan B
)

fan_network_plot <- ggraph(graph, layout = "stress") +
  
  # Ultra-thin filament connection lines
  geom_edge_link(aes(alpha = weight), color = "yellow", width = 0.2, show.legend = FALSE) +
  
  # 1. Base Node Layer: Now uses shape = 21 with black borders.
  # Size maps perfectly across the entire range continuously.
  geom_node_point(
    aes(fill = community, size = centrality), 
    shape = 21, 
    color = "#000000", 
    stroke = 0.35
  ) +
  
  # 2. Refined Inner Hubs: Highlights ONLY the absolute center-most structural anchors
  geom_node_point(
    aes(size = centrality), 
    fill = "whitesmoke", 
    shape = 21,
    color = "#000000",
    stroke = 0.5,
    alpha = 0.95,
    data = . %>% filter(centrality > quantile(centrality, 0.98))
  ) +
  
  # 3. Outer Edge Accent Layer: Delicate halo circles on the single-link leaf tips
  geom_node_point(
    aes(size = centrality),
    color = "#ffffff",
    shape = 1, 
    stroke = 0.18,
    alpha = 0.3,
    data = . %>% filter(centrality == 1)
  ) +
  
  scale_fill_manual(values = neon_colors) +
  # CRITICAL CHANGE: Sqrt transformation spreads out size values smoothly 
  # so nodes transition step-by-step from tiny up to large.
  scale_size_continuous(range = c(1.5, 5.0), trans = "sqrt", guide = "none") + 
  scale_edge_alpha_continuous(range = c(0.03, 0.22)) +
  
  theme_void() +
  theme(
    legend.position = "none",
    plot.margin = margin(10, 40, 10, 40), 
    background = element_rect(fill = "#000000", color = NA)
  )

# ==========================================
# 5. OVERWRITE LIVE IMAGE ASSET (WIDE LANDSCAPE)
# ==========================================
ggsave(
  filename = "../../public/images/dark_network.png", 
  plot = fan_network_plot, 
  width = 11, 
  height = 4.5, 
  dpi = 300, 
  bg = "#000000"
)
