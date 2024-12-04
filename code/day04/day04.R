library(here)
library(tidyverse)

df <- tibble(line = read_lines(here("code/day04/input")))

# assign each letter a unique position defined by row and column number
df <- df |>
  mutate(row = row_number()) |>
  separate_rows(line, sep = "") |>
  rename(letter = line) |>
  filter(letter != "") |>
  group_by(row) |>
  mutate(col = row_number()) |>
  ungroup()

# part 1

# For each available "X" I am extracting all possible positions how XMAS could
# be written
pos <- tibble()
for (r in 1:max(df$row)) {
  for (c in 1:max(df$col)) {
    if (pull(filter(df, row == r & col == c), letter) != "X") {
      next
    }
    print(c(r, c))
    # to the right
    right <- tibble(
      row = r, col = c:(c + 3), order = 1:4, id = str_glue("right-{r}-{c}")
    )

    # to the left
    left <- tibble(
      row = r, col = c:(c - 3), order = 1:4, id = str_glue("left-{r}-{c}")
    )

    # to the top
    top <- tibble(
      row = r:(r - 3), col = c, order = 1:4, id = str_glue("top-{r}-{c}")
    )

    # to the bottom
    bottom <- tibble(
      row = r:(r + 3), col = c, order = 1:4, id = str_glue("bottom-{r}-{c}")
    )

    # bottom right
    bottom_right <- tibble(
      row = r:(r + 3),
      col = c:(c + 3), order = 1:4, id = str_glue("bottom_right-{r}-{c}")
    )

    # bottom left
    bottom_left <- tibble(
      row = r:(r + 3),
      col = c:(c - 3), order = 1:4, id = str_glue("bottom_left-{r}-{c}")
    )

    # top right
    top_right <- tibble(
      row = r:(r - 3),
      col = c:(c + 3), order = 1:4, id = str_glue("top_right-{r}-{c}")
    )

    # top left
    top_left <- tibble(
      row = r:(r - 3),
      col = c:(c - 3), order = 1:4, id = str_glue("top_left-{r}-{c}")
    )

    pos <- bind_rows(
      pos, right, left, top, bottom, bottom_right, bottom_left, top_right,
      top_left
    )
  }
}

# merge possible posiitons with my "letter position data frame" and check where the
# letters of the possible positions yield "XMAS"
df |>
  inner_join(pos, by = join_by(row, col)) |>
  arrange(id, order) |>
  group_by(id) |>
  filter(n() == 4) |>
  summarise(word = str_c(letter, collapse = "")) |>
  filter(word == "XMAS") |>
  nrow()

# part 2
# similar to part 1 but checking the possible positions for a crossed MAS word
pos <- tibble()
# build vectors to check
for (r in 1:max(df$row)) {
  for (c in 1:max(df$col)) {
    if (pull(filter(df, row == r & col == c), letter) != "A") {
      next
    }

    # # diagonal line from bottom left to top right
    line_1 <- tibble(
      row = (r - 1):(r + 1),
      col = (c - 1):(c + 1),
      order = 1:3, id = str_glue("{r}-{c}"), id2 = str_glue("line_1")
    )

    # diagonal line from bottom right to top left
    line_2 <- tibble(
      row = (r + 1):(r - 1),
      col = (c - 1):(c + 1),
      order = 1:3, id = str_glue("{r}-{c}"), id2 = str_glue("line_2")
    )

    pos <- bind_rows(pos, line_1, line_2)
  }
}

df |>
  inner_join(pos, by = join_by(row, col)) |>
  arrange(id, id2, order) |>
  group_by(id) |>
  filter(n() == 6) |>
  group_by(id, id2) |>
  summarise(word = str_c(letter, collapse = "")) |>
  ungroup() |>
  pivot_wider(names_from = "id2", values_from = word) |>
  filter((line_1 == "SAM" | line_1 == "MAS") & (line_2 == "SAM" | line_2 == "MAS")) |>
  nrow()
