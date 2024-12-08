library(here)
library(tidyverse)
library(gtools)

lines <- read_lines(here("code/day08/input"))

construct_matrix <- function(input) {
  m <- matrix(
    unlist(map(input, str_split_1, "")),
    nrow = length(input),
    ncol = nchar(input[1]),
    byrow = TRUE
  )
  return(m)
}

is_inside <- function(coords, nrow = n_row, ncol = n_col) {
  return(all(coords <= nrow) & all(coords <= ncol) & all(coords > 0))
}

mat <- construct_matrix(lines)
unique_antennas <- lines |>
  str_c(collapse = "") |>
  str_extract_all(pattern = "[[:alnum:]]") |>
  pluck(1) |>
  str_unique()

n_row <- nrow(mat)
n_col <- ncol(mat)

# part 1
antinodes <- tibble(row = NULL, col = NULL)
for (antenna in unique_antennas) {
  antenna_coords <- which(mat == antenna, arr.ind = TRUE)

  pairwise_comparison_indices <- permutations(nrow(antenna_coords), 2, 1:nrow(antenna_coords))

  pairwise_comparison_indices <- subset(pairwise_comparison_indices, pairwise_comparison_indices[, 1] < pairwise_comparison_indices[, 2])

  for (pw in 1:nrow(pairwise_comparison_indices)) {
    pos1 <- antenna_coords[pairwise_comparison_indices[pw, 1], ]
    pos2 <- antenna_coords[pairwise_comparison_indices[pw, 2], ]

    # determine whether the diagonal has a positive or negative slope
    slope <- unname((pos2[1] - pos1[1]) / (pos2[2] - pos1[2]))

    delta <- abs(pos2 - pos1)
    if (slope > 0) {
      # expand to top left
      antinode_1 <- unname(pos1 - delta)
      # expand to bottom right
      antinode_2 <- unname(pos2 + delta)
    } else if (slope < 0) {
      # expand to upper right
      antinode_1 <- unname(c(pos2[1] - delta[1], pos2[2] + delta[2]))
      # expand to bottom left
      antinode_2 <- unname(c(pos1[1] + delta[1], pos1[2] - delta[2]))
    }


    if (is_inside(antinode_1)) {
      antinodes <- bind_rows(antinodes, tibble(row = antinode_1[1], col = antinode_1[2]))
    }
    if (is_inside(antinode_2)) {
      antinodes <- bind_rows(antinodes, tibble(row = antinode_2[1], col = antinode_2[2]))
    }
  }
}

antinodes |>
  distinct() |>
  nrow()


# part 2
antinodes <- tibble(row = NULL, col = NULL)
for (antenna in unique_antennas) {
  antenna_coords <- which(mat == antenna, arr.ind = TRUE)

  pairwise_comparison_indices <- permutations(nrow(antenna_coords), 2, 1:nrow(antenna_coords))
  pairwise_comparison_indices <- subset(pairwise_comparison_indices, pairwise_comparison_indices[, 1] < pairwise_comparison_indices[, 2])

  for (pw in 1:nrow(pairwise_comparison_indices)) {
    pos1 <- unname(antenna_coords[pairwise_comparison_indices[pw, 1], ])
    pos2 <- unname(antenna_coords[pairwise_comparison_indices[pw, 2], ])

    # determine whether the diagonal has a positive or negative slope
    slope <- (pos2[1] - pos1[1]) / (pos2[2] - pos1[2])

    delta <- abs(pos2 - pos1)

    if (slope > 0) {
      # expand to top left
      while (is_inside(pos1)) {
        antinodes <- bind_rows(antinodes, tibble(row = pos1[1], col = pos1[2]))
        pos1 <- pos1 - delta
      }

      # expand to bottom right
      while (is_inside(pos2)) {
        antinodes <- bind_rows(antinodes, tibble(row = pos2[1], col = pos2[2]))
        pos2 <- pos2 + delta
      }
    } else if (slope < 0) {
      # expand to upper right
      while (is_inside(pos2)) {
        antinodes <- bind_rows(antinodes, tibble(row = pos2[1], col = pos2[2]))
        pos2 <- c(pos2[1] - delta[1], pos2[2] + delta[2])
      }
      # expand to bottom left
      while (is_inside(pos1)) {
        antinodes <- bind_rows(antinodes, tibble(row = pos1[1], col = pos1[2]))
        pos1 <- c(pos1[1] + delta[1], pos1[2] - delta[2])
      }
    }
  }
}

antinodes |>
  distinct() |>
  nrow()
