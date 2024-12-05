library(here)
library(tidyverse)

lines <- read_lines(here("code/day05/input"))

# extract rules and updates from input
separator <- which(lines == "")
rules <- lines[1:separator - 1] |>
  as_tibble() |>
  separate(value, into = c("before", "after"), convert = TRUE)
updates <- lines[(separator + 1):length(lines)] |>
  map(\(u) as.integer(str_split_1(u, ",")))


# identify for each update if its correctly ordered
are_updates_correctly_ordered <- c()
for (u in updates) {
  subsetted_rules <- rules |>
    filter(before %in% u & after %in% u)

  # the logic is based on the assumption that an update is correctly ordered if
  # the first value occurs in n-1 rules, the second in n-2 rules, etc...
  # n = length(u)
  r <- imap_lgl(u, .f = function(value, idx) {
    occurence <- subsetted_rules |>
      filter(before == value) |>
      nrow()

    return(occurence == length(u) - idx)
  })
  are_updates_correctly_ordered <- append(are_updates_correctly_ordered, all(r))
}

# part 1
updates[are_updates_correctly_ordered] |>
  map_int(.f = function(x) {
    return(x[ceiling(length(x) / 2)])
  }) |>
  sum()


# part 2
map_int(updates[!are_updates_correctly_ordered], .f = function(u) {
  subsetted_rules <- rules |>
    filter(before %in% u & after %in% u)

  # we identify the actual position of each value based on the number of
  # occurrences in the rules set
  sequence <- imap_int(u, .f = function(value, idx) {
    subsetted_rules |>
      filter(before == value) |>
      nrow()
  })

  # the "update" is not ordered correctly, instead the value that is supposed to
  # be in the middle of the update is extracted
  return(u[which(sequence == floor(length(u) / 2))])
}) |>
  sum()
