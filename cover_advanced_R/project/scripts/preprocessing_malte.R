# Packages
library(tidyverse)
library(tidygraph)
library(ggraph)
library(igraph)
library(here)
# -------------------------------------------------------------------

# Nodes
df_nodes <- data.table::fread(r"(progress_data.csv)") %>% 
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
    mutate(node_type = "subchapter")
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
  mutate(from_part       = df_nodes %>% slice(from) %>% pull(part      ),
         from_chapter    = df_nodes %>% slice(from) %>% pull(chapter   ),
         from_subchapter = df_nodes %>% slice(from) %>% pull(subchapter),
         from_exercise   = df_nodes %>% slice(from) %>% pull(exercise  )) %>% 
  mutate(to_part       = df_nodes %>% slice(to) %>% pull(part      ),
         to_chapter    = df_nodes %>% slice(to) %>% pull(chapter   ),
         to_subchapter = df_nodes %>% slice(to) %>% pull(subchapter),
         to_exercise   = df_nodes %>% slice(to) %>% pull(exercise  )) %>% 
  filter(from != to) %>% 
  filter(from_type == "part"       & to_type %in% c("part") |
           from_type == "part"       & to_type %in% c("chapter") & from_part == to_part |
           from_type == "chapter"    & to_type %in% c("chapter") & from_part == to_part |
           from_type == "chapter"    & to_type %in% c("subchapter") & from_part == to_part & from_chapter == to_chapter |
           from_type == "subchapter" & to_type %in% c("subchapter") & from_part == to_part & from_chapter == to_chapter |
           from_type == "subchapter" & to_type %in% c("exercise") & from_part == to_part & from_chapter == to_chapter & from_subchapter == to_subchapter |
           from_type == "exercise"   & to_type %in% c("exercise") & from_part == to_part & from_chapter == to_chapter & from_subchapter == to_subchapter)

# distinct
df_edges <- df_edges %>% 
  rowwise() %>% 
  mutate(key = paste(sort(c(from, to)), collapse = "_")) %>% 
  ungroup() %>% 
  filter(!duplicated(key)) 


# write df edges and df nodes out to dir ----------------------------------
write.csv(df_nodes, file = "df_nodes.csv")
write.csv(df_edges, file = "df_edges.csv")



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




