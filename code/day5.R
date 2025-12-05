library(tidyverse)

read_input = function(path) {
  input = readLines(path)
  intervals = input[seq_len(which(input == "") - 1)] %>%
    str_split("-") %>%
    map(as.numeric)
  test = input[seq(which(input == "") + 1, length(input))] %>%
    as.numeric()
  list(
    intervals = intervals,
    test = test
  )
}

input = read_input("data/day5/input")

# part 1
is_in_interval = function(val, interval) {
  val >= interval[[1]] & val <= interval[[2]]
}

is_in_any_interval = function(val, intervals) {
  any(map_lgl(intervals, \(iv) is_in_interval(val, iv)))
}

sum(
  map_lgl(
    input$test,
    is_in_any_interval,
    intervals = input$intervals
  )
)

# part 2
ord_intervals = input$intervals[order(map_dbl(input$intervals, 1))]

i = 1
while (i < length(ord_intervals)) {
  if (ord_intervals[[i + 1]][[1]] <= ord_intervals[[i]][[2]]) {
    ord_intervals[[i]][[2]] = max(
      ord_intervals[[i]][[2]],
      ord_intervals[[i + 1]][[2]]
    )
    ord_intervals = ord_intervals[-(i + 1)]
  } else {
    i = i + 1
  }
}

options("scipen" = 100, "digits" = 4)
sum(map_dbl(ord_intervals, diff)) + length(ord_intervals)
