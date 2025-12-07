rm(list = ls())
options("scipen" = 100, "digits" = 4)

read_input = function(path) {
  input = readLines(path) |> stringr::str_split("")
  list(state = c("." = 0, "S" = 1)[input[[1]]], layers = input[-1])
}

layerToTransformMatrix = function(layer) {
  matrix = diag(length(layer))
  splitter = which(layer == "^")
  for (s in splitter) {
    s_neigh = s + c(-1, 1)
    matrix[s, s_neigh[s_neigh >= 1 & s_neigh <= length(layer)]] = 1
  }
  diag(matrix)[splitter] = 0
  matrix
}

input = read_input("data/day7/input")
input$transforms = purrr::map(input$layers, layerToTransformMatrix)
input$full_transform = purrr::reduce(input$transforms, `%*%`)

# part 1
state = input$state

splits = 0
for (transform in input$transforms) {
  splits = splits + sum(state & !diag(transform))
  state = state %*% transform
}
splits

# part 2
sum(input$state %*% input$full_transform)
