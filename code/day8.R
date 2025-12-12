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
  dist = sqrt(rowSums((junction_boxes[j1, ] - junction_boxes[j2, ])**2)),
  joined = FALSE
)

pair_distances = arrange(pair_distances, dist)

# connect nearest N pairs
circuits = tibble(
  j = seq_len(nrow(junction_boxes)),
  conections = list(c()),
  circuit_num = NA
)

num_connections = 1000
for (i in seq_len(num_connections)) {
  new_pair = c(
    pair_distances$j1[i],
    pair_distances$j2[i]
  )

  # connect j1 -> j2
  circuits$conections[[new_pair[1]]] = append(
    circuits$conections[[new_pair[1]]],
    new_pair[2]
  )

  # connect j2 -> j1
  circuits$conections[[new_pair[2]]] = append(
    circuits$conections[[new_pair[2]]],
    new_pair[1]
  )
}

# make circuits
explore_circuit = function(j, connections, circuit = c()) {
  circuit = c(circuit, j)
  new_connections = connections[[j]][!connections[[j]] %in% circuit]
  for (jc in new_connections) {
    circuit = explore_circuit(jc, connections, circuit)
  }
  circuit
}

circuit_num = 1
for (i in seq_len(nrow(circuits))) {
  if (!is.na(circuits$circuit_num[[i]])) {
    next
  }
  circuit = explore_circuit(i, circuits$conections)
  circuits$circuit_num[circuit] = circuit_num
  circuit_num = circuit_num + 1
}

# find answer
prod(sort(table(circuits$circuit_num), decreasing = TRUE)[1:3])

# part 2 -------------------------------------------------------------------

while (length(unique(circuits$circuit_num)) > 1) {
  new_pair = c(
    pair_distances$j1[i],
    pair_distances$j2[i]
  )
  i = i + 1

  c1 = circuits$circuit_num[new_pair[[1]]]
  c2 = circuits$circuit_num[new_pair[[2]]]

  circuits$circuit_num[circuits$circuit_num == c2] = c1
  circuits$circuit_num = rank(circuits$circuit_num)
}

junction_boxes[new_pair, "x"]
