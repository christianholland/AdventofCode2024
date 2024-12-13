library(here)
library(tidyverse)

lines <- read_lines(here("code/day13/input"))
combined_line <- str_c(lines, collapse = "")

numbers <- as.numeric(str_extract_all(combined_line, "\\d+")[[1]])
splitted_numbers <- split(x, ceiling(seq_along(x) / 6))

solve <- function(a1, b1, c1, a2, b2, c2) {
  y <- -1 * ((a1 * c2 - a2 * c1) / (a2 * b1 - a1 * b2))
  x <- (c1 - b1 * y) / a1
  return(c(x, y))
}

# part 1
map_dbl(splitted_numbers, .f = function(el) {
  res <- solve(el[1], el[3], el[5], el[2], el[4], el[6])
  if (all(res %% 1 == 0) & all(res <= 100)) {
    return(sum(res * c(3, 1)))
  } else {
    return(0)
  }
}) |>
  sum()


# part 2
map_dbl(splitted_numbers, .f = function(el) {
  res <- solve(el[1], el[3], 10000000000000 + el[5], el[2], el[4], 10000000000000 + el[6])
  if (all(res %% 1 == 0)) {
    return(sum(res * c(3, 1)))
  } else {
    return(0)
  }
}) |>
  sum() |>
  as.character()
