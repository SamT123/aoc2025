library(stringr)
library(purrr)

# part 1
i1 = readLines("data/day1/input")
deltas = as.integer(str_sub(i1, 2)) * c(L = -1, R = 1)[str_sub(i1, 1, 1)]
positions = accumulate(c(50, deltas), `+`)
sum(positions %% 100 == 0)

# part 2
sum(
  map2_int(
    positions[-length(positions)],
    positions[-1],
    \(a, b) sum((a:b)[-1] %% 100 == 0)
  )
)
