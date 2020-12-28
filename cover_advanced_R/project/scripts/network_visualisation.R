#"network vizusalisation
#Tobias Stalder
#Dec 2020


# libraries ---------------------------------------------------------------
library(here)
library(tidyverse)
library(ggraph)
library(scales)


# read in edge and node data from preprocessing ---------------------------
df_nodes <- read.csv("df_nodes.csv")
df_edges <- read.csv("df_edges.csv")

# dataviz -----------------------------------------------------------------

tbl_graph(nodes = df_nodes, edges = df_edges, directed = FALSE) %>% 
  ggraph(layout = "kk") + 
  geom_edge_link(aes(color = from_type), color = "grey", width = .3, alpha = .5) +
  geom_node_point(aes(color = node_type, size = factor(node_type))) +
  scale_color_manual(values = c( "#1f9e89", "#3e4a89","#6dcd59","#31688e")) +
  scale_size_manual(values = c(2.5,1.5,3,2)) +
  scale_edge_color_viridis(discrete = TRUE)+
  theme_void()+
  theme(legend.position = "none",
        plot.background = element_rect(fill = "transparent", color = "transparent")) -> kk_net

kk_net
ggsave(plot = kk_net, filename = "plots_network/kk_net.png", width = 16, height = 8, units = "cm", dpi = 600, type = "cairo-png", bg = "transparent")

show_col(viridis_pal()(10))

tbl_graph(nodes = df_nodes, edges = df_edges, directed = FALSE) %>% 
  ggraph(layout = "linear", circular = TRUE) + 
  geom_edge_arc(aes(alpha = factor(df_edges$to_part)), color = "grey", width = .2) +
  geom_node_point(aes(color = node_type), size = .5) +
  scale_color_manual(values = c( "#1f9e89", "#3e4a89","#6dcd59","#31688e")) +
  scale_size_manual(values = c(2.5,1.5,3,2)) +
  scale_edge_color_manual(values = c( "#1f9e89", "#3e4a89","#6dcd59","#31688e"))+
  theme_void()+
  theme(legend.position = "none",
        plot.background = element_rect(fill = "transparent", color = "transparent")) -> circle_net

circle_net
ggsave(plot = circle_net, filename = "plots_network/circle_net.png", width = 16, height = 16, units = "cm", dpi = 600, type = "cairo-png", bg = "transparent")




tbl_graph(nodes = df_nodes, edges = df_edges, directed = TRUE) %>% 
  ggraph(layout = "gem", circular = FALSE) + 
  geom_edge_link(aes(color = df_edges$to_type), width =.3, check_overlap = TRUE)  +
  geom_node_point(aes(color = node_type)) +
  scale_color_manual(values = c( "#1f9e89", "#3e4a89","#6dcd59","#31688e")) +
  scale_size_manual(values = c(2.5,1.5,3,2)) +
  scale_edge_color_manual(values = c( "#1f9e89", "#3e4a89","#6dcd59","#31688e"))+
  theme_void()+
  theme(legend.position = "none",
        plot.background = element_rect(fill = "transparent", color = "transparent")) -> gem_net
gem_net
ggsave(plot = gem_net, filename = "plots_network/gem_net.png", width = 16, height = 16, units = "cm", dpi = 600, type = "cairo-png", bg = "transparent")


tbl_graph(nodes = df_nodes, edges = df_edges, directed = TRUE) %>% 
  ggraph(layout = "fr", circular = FALSE, maxiter = 1000) + 
  geom_edge_link(aes(color = df_edges$to_type)) +
  geom_node_point(aes(color = node_type, size = node_type)) +
  scale_color_manual(values = c( "#1f9e89", "#3e4a89","#6dcd59","#31688e")) +
  scale_size_manual(values = c(2.5,0.7,3,2)) +
  scale_edge_color_manual(values = c( "#1f9e89", "#3e4a89","#6dcd59","#31688e"))+
  theme_void()+
  theme(legend.position = "none",
        plot.background = element_rect(fill = "transparent", color = "transparent")) -> fr_net
fr_net
ggsave(plot = fr_net, filename = "plots_network/fr_net.png", width = 20, height = 20, units = "cm", dpi = 600, type = "cairo-png", bg = "transparent")



# tbl_graph(nodes = df_nodes, edges = df_edges, directed = TRUE) %>% 
#   ggraph(layout = "lgl", circular = FALSE) + 
#   geom_edge_link(aes(alpha = factor(df_edges$to_part))) +
#   geom_node_point(aes(color = node_type)) +
#   theme(legend.position = "none")


