library(here)
library(tidyverse)

lines <- read_lines(here("code/day10/input"))

construct_matrix <- function(input) {
  m <- matrix(
    as.numeric(unlist(map(input, str_split_1, ""))),
    nrow = length(input),
    ncol = nchar(input[1]),
    byrow = TRUE
  )
  return(m)
}

is_correct_way <- function(old_height, new_height) {
  if (length(new_height) > 0) {
    if (new_height - old_height == 1) {
      return(TRUE)
    }
    return(FALSE)
  }
}

is_inside <- function(coords, nrow, ncol) {
  return(all(coords <= nrow) & all(coords <= ncol) & all(coords > 0))
}

# part 1
m <- construct_matrix(lines)
n_row <- nrow(m)
n_col <- ncol(m)

trailheads <- which(m == 0, arr.ind = TRUE)

hike <- function(height, coords, score, reached_peaks = c()) {
  if (height == 9) {
    hashed_coords <- str_c(coords, collapse = "")
    if (!(hashed_coords %in% reached_peaks)) {
      reached_peaks <- c(reached_peaks, hashed_coords)
      score <- score + 1
    }
  }

  # check if hike continues to the right
  right_coords <- coords + c(0, 1)
  if (is_inside(right_coords, n_row, n_col)) {
    right_height <- m[right_coords[1], right_coords[2]]
    if (is_correct_way(height, right_height)) {
      result <- hike(right_height, right_coords, score, reached_peaks)
      score <- result$score
      reached_peaks <- result$reached_peaks
    }
  }


  # check if hike continues to the left
  left_coords <- coords + c(0, -1)
  if (is_inside(left_coords, n_row, n_col)) {
    left_height <- m[left_coords[1], left_coords[2]]
    if (is_correct_way(height, left_height)) {
      result <- hike(left_height, left_coords, score, reached_peaks)
      score <- result$score
      reached_peaks <- result$reached_peaks
    }
  }

  # check if hike continues to the top
  top_coords <- coords + c(-1, 0)
  if (is_inside(top_coords, n_row, n_col)) {
    top_height <- m[top_coords[1], top_coords[2]]
    if (is_correct_way(height, top_height)) {
      result <- hike(top_height, top_coords, score, reached_peaks)
      score <- result$score
      reached_peaks <- result$reached_peaks
    }
  }

  # check if hike continues to the bottom
  bottom_coords <- coords + c(1, 0)
  if (is_inside(bottom_coords, n_row, n_col)) {
    bottom_height <- m[bottom_coords[1], bottom_coords[2]]
    if (is_correct_way(height, bottom_height)) {
      result <- hike(bottom_height, bottom_coords, score, reached_peaks)
      score <- result$score
      reached_peaks <- result$reached_peaks
    }
  }
  return(list(score = score, reached_peaks = reached_peaks))
}


apply(trailheads, 1, function(coords) {
  ret <- hike(0, unname(coords), 0)
  return(ret$score)
}) |>
  sum()


# part 2
m <- construct_matrix(lines)
n_row <- nrow(m)
n_col <- ncol(m)

trailheads <- which(m == 0, arr.ind = TRUE)

hike2 <- function(height, coords, score, reached_peaks = c()) {
  if (height == 9) {
    score <- score + 1
  }

  # check if hike continues to the right
  right_coords <- coords + c(0, 1)
  if (is_inside(right_coords, n_row, n_col)) {
    right_height <- m[right_coords[1], right_coords[2]]
    if (is_correct_way(height, right_height)) {
      result <- hike2(right_height, right_coords, score, reached_peaks)
      score <- result$score
      reached_peaks <- result$reached_peaks
    }
  }


  # check if hike continues to the left
  left_coords <- coords + c(0, -1)
  if (is_inside(left_coords, n_row, n_col)) {
    left_height <- m[left_coords[1], left_coords[2]]
    if (is_correct_way(height, left_height)) {
      result <- hike2(left_height, left_coords, score, reached_peaks)
      score <- result$score
      reached_peaks <- result$reached_peaks
    }
  }

  # check if hike continues to the top
  top_coords <- coords + c(-1, 0)
  if (is_inside(top_coords, n_row, n_col)) {
    top_height <- m[top_coords[1], top_coords[2]]
    if (is_correct_way(height, top_height)) {
      result <- hike2(top_height, top_coords, score, reached_peaks)
      score <- result$score
      reached_peaks <- result$reached_peaks
    }
  }

  # check if hike continues to the bottom
  bottom_coords <- coords + c(1, 0)
  if (is_inside(bottom_coords, n_row, n_col)) {
    bottom_height <- m[bottom_coords[1], bottom_coords[2]]
    if (is_correct_way(height, bottom_height)) {
      result <- hike2(bottom_height, bottom_coords, score, reached_peaks)
      score <- result$score
      reached_peaks <- result$reached_peaks
    }
  }
  return(list(score = score, reached_peaks = reached_peaks))
}


apply(trailheads, 1, function(coords) {
  ret <- hike2(0, unname(coords), 0)
  return(ret$score)
}) |>
  sum()
