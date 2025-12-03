rm(list = ls())
library(tidyverse)

read_input = function(path) {
  readLines(path) %>%
    str_split("") %>%
    map(as.integer)
}

# part 1

solve_row_1 = function(row) {
  10 * max(row[-length(row)]) + max(row[-seq_len(which.max(row[-length(row)]))])
}
read_input("data/day3/input") %>%
  map_int(solve_row_1) %>%
  sum()


# part 2
options("scipen" = 100, "digits" = 4)

solve_row_2 = function(number = c(), remaining_choices, to_add) {
  if (to_add == 0) {
    return(as.numeric(paste(number, collapse = "")))
  }
  valid_choices = remaining_choices[
    seq_len(length(remaining_choices) - to_add + 1)
  ]

  choice_idx = which.max(valid_choices)

  do.call(
    solve_row_2,
    list(
      number = c(number, valid_choices[choice_idx]),
      remaining_choices = remaining_choices[-seq_len(choice_idx)],
      to_add = to_add - 1
    )
  )
}

read_input("data/day3/input") %>%
  map_dbl(\(x) solve_row_2(c(), x, 12)) %>%
  sum()
