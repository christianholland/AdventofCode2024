library(here)
library(tidyverse)

df <- tibble(line = read_lines(here("code/day01/input"))) |>
  separate(line, into = c("left", "right"), convert = TRUE)

# part 1
map2_int(sort(df$left), sort(df$right), .f = function(left, right) {
  abs(left - right)
}) |>
  sum()


# part 2
df_right <- df |> select(right)

map_int(df$left, .f = function(left) {
  count <- df_right |>
    filter(right == left) |>
    nrow()

  return(count * left)
}) |>
  sum()
