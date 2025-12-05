library(tidyverse)
read_input = function(path) {
  readLines(path) %>%
    str_split("") %>%
    map(~ case_match(.x, "@" ~ 1, "." ~ 0))
}

xyToIdx = function(x, y, ncol, nrow) {
  (x - 1) + (y - 1) * ncol + 1
}

makeAdjacencyMatrix = function(ncol, nrow) {
  edge_list = matrix(ncol = 2, nrow = 0)
  for (x in seq_len(ncol)) {
    for (y in seq_len(nrow)) {
      for (x_offset in seq(-1, 1)) {
        for (y_offset in seq(-1, 1)) {
          x2 = x + x_offset
          y2 = y + y_offset

          # fmt: skip
          if (x2 >= 1 & x2 <= ncol & y2 >= 1 & y2 <= nrow & !(x == x2 & y == y2)) {
            edge_list = rbind(
              edge_list,
              c(xyToIdx(x, y, ncol, nrow), xyToIdx(x2, y2, ncol, nrow))
            )
          }
        }
      }
    }
  }
  Matrix::sparseMatrix(edge_list[, 1], edge_list[, 2])
}

input = read_input("data/day4/input")
transfer_matrix = makeAdjacencyMatrix(length(input[[1]]), length(input))

# part 1
sum(unlist(input) & transfer_matrix %*% unlist(input) < 4)

# part 2
state = unlist(input)
while (T) {
  can_remove = state & transfer_matrix %*% state < 4
  if (!any(can_remove)) {
    break
  }

  state = state & !can_remove
}

sum(unlist(input)) - sum(state)
