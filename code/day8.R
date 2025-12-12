rm(list = ls())
library(tidyverse)

junction_boxes = readLines("data/day8/input") %>%
  str_split(",") %>%
  map(\(x) setNames(as.numeric(x), c("x", "y", "z"))) %>%
  do.call(what = rbind)

# part 1 -------------------------------------------------------------------
# get pairwise distances
pairs = combn(seq_len(nrow(junction_boxes)), 2)

pair_distances = tibble(
  j1 = pairs[1, ],
  j2 = pairs[2, ],
  j1_j2 = map2(j1, j2, c),
  dist = sqrt(rowSums((junction_boxes[j1, ] - junction_boxes[j2, ])**2))
) %>%
  arrange(dist)

# connect nearest N pairs
circuits = tibble(
  j = seq_len(nrow(junction_boxes)),
  conections = list(c()),
  circuit_num = j
)

for (i in seq_len(1000)) {
  new_pair = circuits$circuit_num[pair_distances$j1_j2[[i]]]
  circuits$circuit_num[circuits$circuit_num == new_pair[[2]]] = new_pair[[1]]
}

prod(sort(table(circuits$circuit_num), decreasing = TRUE)[1:3])

# part 2 -------------------------------------------------------------------
while (length(unique(circuits$circuit_num)) > 1) {
  new_pair = circuits$circuit_num[pair_distances$j1_j2[[i]]]
  i = i + 1
  circuits$circuit_num[circuits$circuit_num == new_pair[[2]]] = new_pair[[1]]
}

prod(junction_boxes[pair_distances$j1_j2[[i - 1]], "x"])
