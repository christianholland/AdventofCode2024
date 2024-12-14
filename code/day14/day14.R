library(here)
library(tidyverse)

lines <- read_lines(here("code/day14/input"))
positions <- str_match_all(lines, "p=(?<x>\\d+),(?<y>\\d+)") |>
  map(.f = function(pos) {
    return(as.numeric(pos[2:3]))
  })
velocities <- str_match_all(lines, "v=(-?\\d+),(-?\\d+)") |>
  map(.f = function(v) {
    return(as.numeric(v[2:3]))
  })


dims <- c(101, 103)

# part 1
middle_position <- (dims - 1) / 2
final_positions <- map2(positions, velocities, .f = function(p, v) {
  final_position <- (p + 100 * v) %% dims
  if (any(final_position == middle_position)) {
    return(NULL)
  } else {
    return(final_position)
  }
})

quadrants <- c(0, 0, 0, 0)

for (pos in final_positions) {
  if (!is.null(pos)) {
    if (pos[1] < middle_position[1] && pos[2] < middle_position[2]) {
      quadrants[1] <- quadrants[1] + 1
    } else if (pos[1] < middle_position[1] && pos[2] > middle_position[2]) {
      quadrants[2] <- quadrants[2] + 1
    } else if (pos[1] > middle_position[1] && pos[2] < middle_position[2]) {
      quadrants[3] <- quadrants[3] + 1
    } else {
      quadrants[4] <- quadrants[4] + 1
    }
  }
}

prod(quadrants)


# part 2
find_max_accumulation <- function(positions) {
  accumulations <- c()

  # mark current positions of robots with a 1 in a matrix
  m <- matrix(0, dims[2], dims[1])
  for (pos in positions) {
    m[pos[2] + 1, pos[1] + 1] <- 1
  }

  # identify positions with at least 2 robots next to each other (from top to down)
  v <- rle(as.numeric(m))
  for (i in seq_along(v$lengths)) {
    if (v$values[i] == 1) {
      if (v$lengths[i] > 1) {
        accumulations <- c(accumulations, v$lengths[i])
      }
    }
  }
  return(max(accumulations))
}

i <- 0
current_positions <- positions
while (find_max_accumulation(current_positions) < 10) {
  i <- i + 1
  current_positions <- map2(positions, velocities, .f = function(p, v) {
    return((p + i * v) %% dims)
  })
}
print(i)

# plot robot positions to see the christmas tree
map_dfr(current_positions, .f = function(p) {
  return(tibble(x = p[1], y = p[2]))
}) |>
  ggplot(aes(x = x, y = y)) +
  geom_point(size = 0.1) +
  theme_void()
