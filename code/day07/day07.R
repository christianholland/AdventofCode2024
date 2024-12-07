library(here)
library(tidyverse)
library(gtools)

lines <- read_lines(here("code/day07/input"))

df <- as_tibble(lines) |>
  mutate(id = row_number()) |>
  separate(value, into = c("result", "vector"), sep = ": ", convert = TRUE) |>
  rowwise() |>
  mutate(vector = list(as.numeric(str_split_1(vector, " ")))) |>
  ungroup()

is_equation_valid <- function(vector, result, id, operators, ...) {
  print(str_glue("Equation: {id}"))
  n_operators <- length(vector) - 1

  permutation_matrix <- permutations(
    length(operators), n_operators, operators,
    set = FALSE,
    repeats.allowed = TRUE
  )

  for (row_idx in 1:nrow(permutation_matrix)) {
    row <- permutation_matrix[row_idx, ]
    res <- reduce2(vector, row, .f = function(acc, next_value, operator) {
      if (operator == "*") {
        new_value <- acc * next_value
      } else if (operator == "+") {
        new_value <- acc + next_value
      } else if (operator == "||") {
        new_value <- as.numeric(str_c(acc, next_value))
      }
      # stop reduce if the new value already exceeds the results before all
      # operations are executed
      if (new_value > result) {
        return(done(new_value))
      }
      return(new_value)
    })
    if (res == result) {
      return(TRUE)
    }
  }
  return(FALSE)
}

# part 1
are_equations_valid_1 <- pmap_lgl(
  df,
  is_equation_valid,
  operators = c("+", "*")
)
sum(df[are_equations_valid_1, ]$result)

# part 2
are_equations_valid_2 <- pmap_lgl(
  df,
  is_equation_valid,
  operators = c("+", "*", "||")
)
sum(df[are_equations_valid_2, ]$result)
