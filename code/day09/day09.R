library(here)
library(tidyverse)

line <- read_lines(here("code/day09/input")) |>
  str_split_1("") |>
  as.numeric()


values <- imap(line, .f = function(val, idx) {
  if (val == 0) {
    return(NULL)
  }
  if (idx %% 2 != 0) {
    return(rep((idx - 1) %/% 2, val))
  } else {
    return(rep(NA_integer_, val))
  }
}) |>
  unlist()


# part 1
compacted_values <- values
na_positions <- which(is.na(compacted_values))
el <- 1
while (any(is.na(compacted_values))) {
  val <- rev(compacted_values)[1]

  if (!is.na(val)) {
    compacted_values[na_positions[el]] <- val
    el <- el + 1
  }

  # remove last element from compacted_values vector
  compacted_values <- compacted_values[-length(compacted_values)]
}

imap_int(compacted_values, .f = function(val, idx) {
  return(val * (idx - 1))
}) |>
  sum() |>
  as.character()


# part 2
get_gaps <- function(vec) {
  # Get the run lengths and values
  rle_vec <- rle(is.na(vec))

  # Initialize an empty list to store the coordinates
  na_coords <- list()

  # Initialize the starting index
  start_index <- 1

  # Loop through the run lengths and values
  for (i in seq_along(rle_vec$lengths)) {
    if (rle_vec$values[i]) {
      # If the run is NA, get the positions
      end_index <- start_index + rle_vec$lengths[i] - 1
      if (rle_vec$lengths[i] > 1) {
        na_coords <- c(na_coords, list(seq(start_index, end_index)))
      } else {
        na_coords <- c(na_coords, start_index)
      }
    }
    # Update the starting index
    start_index <- start_index + rle_vec$lengths[i]
  }
  return(na_coords)
}

files <- values |>
  enframe(name = "idx", value = "file") |>
  drop_na() |>
  filter(file != 0) |>
  count(file) |>
  arrange(-file) |>
  na.omit() |>
  deframe()

gaps <- get_gaps(values)
compacted_values <- values

for (i in seq_along(files)) {
  f <- as.numeric(names(files)[i])
  c <- as.numeric(files[i])
  file_positions <- which(compacted_values == f)


  fits_in <- c <= lengths(gaps)
  if (any(fits_in)) {
    fitting_gaps <- gaps[fits_in]

    gap_coords <- fitting_gaps[[1]]

    # make sure the gap is left of the position of the value
    if (min(gap_coords) > min(file_positions)) next

    # update gap with file value
    compacted_values[gap_coords][1:c] <- as.numeric(rep(f, c))

    # drop the n last values of vector compacted_values vector
    compacted_values[file_positions] <- NA_integer_

    # update gaps
    if (length(gap_coords) == c) {
      gaps[[which(fits_in == TRUE)[1]]] <- NA_integer_
    } else {
      gaps[[which(fits_in == TRUE)[1]]] <- gap_coords[(c + 1):length(gap_coords)]
    }
  }
  gaps <- gaps[!is.na(gaps)]
}

head(compacted_values, 200)

imap_int(compacted_values, .f = function(val, idx) {
  if (is.na(val)) {
    return(0)
  }
  return(val * (idx - 1))
}) |>
  sum() |>
  as.character()
