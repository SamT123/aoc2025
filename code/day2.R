library(stringr)
library(magrittr)

# part 1
i1 = readLines("data/day2/input")
ids = i1 %>%
  str_split(",", simplify = T) %>%
  str_split(pattern = "-", simplify = F) %>%
  map(as.numeric) %>%
  map(~ seq(.x[[1]], .x[[2]])) %>%
  unlist() %>%
  as.character()

ids_halflen = floor(nchar(ids) / 2)
id_is_invalid = str_sub(ids, 1, ids_halflen) ==
  str_sub(ids, ids_halflen + 1, nchar(ids))

sum(as.numeric(ids)[id_is_invalid])

# part 2
check_id = function(id) {
  id = as.character(id)
  len = nchar(id)
  divisors = seq_len(len - 1)[len %% seq_len(len - 1) == 0]
  for (d in divisors) {
    id_split = str_extract_all(id, paste0(".{1,", d, "}"))[[1]]
    if (length(unique(id_split)) == 1) return(TRUE)
  }
  FALSE
}

id_is_invalid = map_lgl(ids, check_id)
sum(as.numeric(ids)[id_is_invalid])
