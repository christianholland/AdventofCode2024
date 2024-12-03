library(here)
library(tidyverse)

lines <- read_lines(here("code/day03/input"))
single_line <- str_c(lines, collapse = "")

extract_multiplications <- function(line) {
  pattern <- "mul\\((\\d{1,3}),(\\d{1,3})\\)"
  str_match_all(line, pattern)[[1]] |>
    as_tibble() |>
    transmute(product = as.integer(V2) * as.integer(V3)) |>
    pull(product) |>
    sum()
}

# part 1
extract_multiplications(single_line)

# part 2
do_positions <- unname(str_locate_all(single_line, "do\\(\\)")[[1]][, 2])
dont_positions <- unname(str_locate_all(single_line, "don't\\(\\)")[[1]][, 1])

# visualize where do and don'ts are distributed
bind_rows(
  tibble(pos = c(1, do_positions), label = "do"),
  tibble(pos = dont_positions, label = "dont")
) |>
  ggplot(aes(x = pos, y = 1, color = label)) +
  geom_point(size = 1)

# identify ranges between do's and dont's
keep_ranges <- map_dfr(c(1, do_positions), .f = function(do) {
  # find the next don't position
  diff <- dont_positions - do
  diff[diff < 0] <- NA_integer_

  next_dont_position <- dont_positions[which.min(diff)]

  if (length(next_dont_position) == 0) {
    next_dont_position <- nchar(single_line)
  }
  return(tibble(start = do, stop = next_dont_position))
}) |>
  group_by(stop) |>
  slice_min(start)

str_sub_all(single_line, start = as.matrix(keep_ranges))[[1]] |>
  str_c(collapse = "") |>
  extract_multiplications()

extract_multiplications(x)
