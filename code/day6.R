rm(list = ls())
library(tidyverse)

solve_problems = function(problems) {
  map_dbl(
    problems,
    function(problem) {
      op = list("+" = sum, "*" = prod)[[problem[["op"]]]]
      op(problem[["nums"]])
    }
  )
}

# part 1

read_input_1 = function(path) {
  readLines(path) %>%
    map_chr(trimws) %>%
    str_split("[ ]+") %>%
    list_transpose(simplify = TRUE) %>%
    map(
      ~ list(
        nums = as.numeric(.x[-length(.x)]),
        op = .x[[length(.x)]]
      )
    )
}

sum(solve_problems(read_input_1("data/day6/input")))

# part 2

read_input_2 = function(path) {
  input = readLines(path)

  nums = input[-length(input)]
  nums = nums %>%
    str_split("") %>%
    list_transpose(simplify = TRUE) %>%
    map_chr(\(x) paste0(trimws(x), collapse = "")) %>%
    paste0(collapse = "|") %>%
    str_split(fixed("||"), simplify = TRUE) %>%
    str_split(fixed("|")) %>%
    map(as.integer)

  ops = input[length(input)]
  ops = ops %>%
    trimws() %>%
    str_split("[ ]+") %>%
    unlist()

  map2(nums, ops, ~ list(nums = .x, op = .y))
}

sum(solve_problems(read_input_2("data/day6/input")))
