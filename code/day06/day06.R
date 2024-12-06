library(here)
library(tidyverse)
library(digest)

lines <- read_lines(here("code/day06/input"))

construct_matrix <- function(input) {
  m <- matrix(
    unlist(map(input, str_split_1, "")),
    nrow = length(input),
    ncol = nchar(input[1]),
    byrow = TRUE
  )
  return(m)
}

is_outside <- function(pos) {
  return(any(pos < 1) | pos[1] > nrow(mat) | pos[2] > ncol(mat))
}

get_next_position <- function(curr_position, direction) {
  if (direction == "up") {
    delta <- c(-1, 0)
  } else if (direction == "down") {
    delta <- c(1, 0)
  } else if (direction == "right") {
    delta <- c(0, 1)
  } else if (direction == "left") {
    delta <- c(0, -1)
  }
  return(curr_position + delta)
}

get_next_direction <- function(direction) {
  if (direction == "up") {
    next_direction <- "right"
  } else if (direction == "down") {
    next_direction <- "left"
  } else if (direction == "right") {
    next_direction <- "down"
  } else if (direction == "left") {
    next_direction <- "up"
  }
  return(next_direction)
}


# part 1
mat <- construct_matrix(lines)
curr_position <- unname(which(mat == "^", arr.ind = TRUE)[1, ])
curr_direction <- "up"
while (!is_outside(curr_position)) {
  mat[curr_position[1], curr_position[2]] <- "X"

  next_position <- get_next_position(curr_position, curr_direction)

  if (is_outside(next_position)) break
  if (mat[next_position[1], next_position[2]] == "#") {
    curr_direction <- get_next_direction(curr_direction)
    next_position <- get_next_position(curr_position, curr_direction)
    if (is_outside(next_position)) break
  }
  curr_position <- next_position
}

sum(mat == "X")

# part 2
mat <- construct_matrix(lines)
n_row <- nrow(mat)
n_col <- ncol(mat)
n_loops <- 1591
for (i in 103:n_row) {
  print(c("Row", i))
  for (j in 1:n_col) {
    curr_position <- unname(which(mat == "^", arr.ind = TRUE)[1, ])
    curr_direction <- "up"
    current_value <- mat[i, j]

    if (current_value == "#" | current_value == "^") next
    mat_copy <- mat
    mat_copy[i, j] <- "#"
    hash_table <- new.env(hash = TRUE, parent = emptyenv())

    while (!is_outside(curr_position)) {
      element_hash <- digest(
        paste(curr_position[1], curr_position[2], curr_direction, sep = "_")
        )

      if (exists(element_hash, envir = hash_table)) {
        n_loops <- n_loops + 1
        print(c("Loop detected", n_loops))
        break
      }


      assign(element_hash, TRUE, envir = hash_table)


      next_position <- get_next_position(curr_position, curr_direction)

      if (is_outside(next_position)) break
      while (mat_copy[next_position[1], next_position[2]] == "#") {
        curr_direction <- get_next_direction(curr_direction)
        next_position <- get_next_position(curr_position, curr_direction)
        if (is_outside(next_position)) break
      }
      curr_position <- next_position
    }
  }
}
n_loops
