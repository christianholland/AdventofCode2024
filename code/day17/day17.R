library(here)
library(tidyverse)

lines <- read_lines(here("code/day17/input"))

A <- parse_number(lines[1])
B <- parse_number(lines[2])
C <- parse_number(lines[3])

programm <- lines[5] |>
  str_split_1(": ") |>
  pluck(2) |>
  str_split_1(",") |>
  as.numeric()


combo <- function(operand) {
  if (operand == 0) {
    return(0)
  } else if (operand == 1) {
    return(1)
  } else if (operand == 2) {
    return(2)
  } else if (operand == 3) {
    return(3)
  } else if (operand == 4) {
    return(A)
  } else if (operand == 5) {
    return(B)
  } else if (operand == 6) {
    return(C)
  } else {
    stop("Invalid operand")
  }
}


# part 1
outs <- c()
i <- 1
while (i < length(programm)) {
  opcode <- programm[i]
  operand <- programm[i + 1]
  print(c(i, opcode, operand))

  if (opcode == 0) {
    A <- floor(A / 2**combo(operand))
  } else if (opcode == 1) {
    B <- bitwXor(B, operand)
  } else if (opcode == 2) {
    B <- combo(operand) %% 8
  } else if (opcode == 3) {
    if (A > 0) {
      i <- operand + 1
      next
    }
  } else if (opcode == 4) {
    B <- bitwXor(B, C)
  } else if (opcode == 5) {
    outs <- c(outs, combo(operand) %% 8)
  } else if (opcode == 6) {
    B <- A %/% 2**combo(operand)
  } else if (opcode == 7) {
    C <- A %/% 2**combo(operand)
  }
  i <- i + 2
}

str_c(outs, collapse = "")
