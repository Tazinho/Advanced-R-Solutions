# Packages
library(tidyverse)
library(tidygraph)
library(ggraph)
library(igraph)
# -------------------------------------------------------------------
# Nodes
df_nodes <- data.table::fread("progress_data.csv") %>% 
  as_tibble() %>% 
  transmute(chapter, subchapter, exercise) %>% 
  mutate(chapter = parse_integer(str_sub(chapter, 1, 2))) %>% 
  mutate(part = case_when(
    chapter <= 8  ~ 1L,
    chapter <= 11 ~ 2L,
    chapter <= 15 ~ 3L,
    chapter <= 21 ~ 4L,
    TRUE ~ 5L
  )) %>% 
  group_by(chapter) %>%
  mutate(subchapter = match(subchapter, unique(subchapter))) %>% 
  ungroup() %>% 
  select(part, chapter, subchapter, exercise) %>% 
  mutate(node_type = "exercise", node_size = 1)

df_help <- bind_rows(
  df_nodes %>% count(part, name = "node_size") %>%
    mutate(node_type = "part"),
  df_nodes %>% count(part, chapter, name = "node_size") %>% 
    mutate(node_type = "chapter"),
  df_nodes %>% count(part, chapter, subchapter, name = "node_size") %>%
    mutate(node_type = "subachapter")
)

df_nodes <- bind_rows(
  df_nodes,
  df_help
) %>%
  mutate(id = 1:n())
# -------------------------------------------------------------------
# Edges
df_edges <- tidyr::crossing(from = df_nodes$id,
                            to   = df_nodes$id) %>%
  mutate(from_type = df_nodes %>% slice(from) %>% pull(node_type),
         to_type   = df_nodes %>% slice(to)   %>% pull(node_type)) %>% 
  filter(from != to) %>% 
  filter(from_type == "part"       & to_type %in% c("part", "chapter") |
         from_type == "chapter"    & to_type %in% c("chapter", "subchapter") |
         from_type == "subchapter" & to_type %in% c("subchapter", "exercise") |
         from_type == "exercise"   & to_type %in% c("exercise"))

# distinct
df_edges <- df_edges %>% 
  rowwise() %>% 
  mutate(key = paste(sort(c(from, to)), collapse = "_")) %>% 
  ungroup() %>% 
  filter(!duplicated(key)) 

# 1. DIES ist bereits die Version "Jeder mit Jedem auf einer Ebene"
# 2. Version als Ring
# Welche ID-Kombinationen brauchen wir für den Ring?
df_ring_ex <- df_nodes %>%
  filter(node_type == "exercise") %>% 
  mutate(from = id, to = id + 1L) %>% 
  group_by(chapter, subchapter) %>% 
  mutate(to = case_when(
    to > max(id) ~ min(id),
    TRUE ~ to
  )) %>% 
  ungroup() %>% 
  filter(from != to) %>% 
  rowwise() %>% 
  mutate(key = paste(sort(c(from, to)), collapse = "_")) %>% 
  ungroup() %>% 
  select(key) %>% 
  distinct()

df_ring_ex
df_edges %>% filter(key %in% df_ring_ex$key)
