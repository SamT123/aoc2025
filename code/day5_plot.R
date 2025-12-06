library(tidyverse)
library(animation)

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

plot_intervals = function(curr_intervals, orig_intervals, curr) {
  par(mai = rep(0, 4))

  plot(
    NULL,
    xlim = range(map_dbl(orig_intervals, 1)),
    ylim = c(1, length(orig_intervals)),
    axes = F,
    ylab = "",
    xlab = ""
  )

  # original intervals faintly in background
  for (i in seq_along(orig_intervals)) {
    col = "grey85"
    lines(
      x = orig_intervals[[i]],
      y = c(i, i),
      col = col,
      lend = 1
    )
  }

  # up to date intervals
  for (i in seq_along(curr_intervals)) {
    col = case_when(
      i < curr ~ "black",
      i == curr ~ "red",
      i > curr ~ "grey65"
    )
    lines(
      x = curr_intervals[[i]],
      y = c(i, i),
      col = col,
      lend = 1
    )
  }

  # num intervals
  tot = length(curr_intervals)
  text = paste0(
    c(
      rep(" ", 3 - nchar(curr)),
      curr,
      "\n",
      rep(" ", 3 - nchar(tot)),
      tot
    ),
    collapse = ""
  )
  mtext(
    text,
    side = 3,
    line = -1.5,
    at = par("usr")[1] + 0.16 * diff(par("usr")[1:2]),
    cex = 1,
    adj = 1,
    padj = 1,
    family = "mono"
  )
}

# plot part 2
input = read_input("data/day5/input")

ord_intervals = input$intervals[order(map_dbl(input$intervals, 1))]

original_intervals = ord_intervals
animation::saveGIF(
  {
    i = 1
    while (i < length(ord_intervals)) {
      plot_intervals(ord_intervals, original_intervals, curr = i)

      if (ord_intervals[[i + 1]][[1]] <= ord_intervals[[i]][[2]]) {
        ord_intervals[[i]][[2]] = max(
          ord_intervals[[i]][[2]],
          ord_intervals[[i + 1]][[2]]
        )
        ord_intervals = ord_intervals[-(i + 1)]
        next
      }

      i = i + 1
    }
  },
  interval = 0.05,
  movie.name = fs::path_abs(fs::path("fig", "day5.gif")),
  ani.res = 600,
  ani.width = 600 * 3,
  ani.height = 600 * 3,
  autobrowse = FALSE,
  clean = TRUE
)
