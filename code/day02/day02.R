library(here)
library(tidyverse)

lines <- read_lines(here("code/day02/input"))
reports <- map(lines, .f = function(line) {
  return(as.numeric(str_split(line, " ")[[1]]))
})

is_report_safe <- function(report) {
  d <- diff(report)

  # label vectors with a 0 and too large jumps as FALSE
  if (0 %in% d | any(abs(d) > 3)) {
    return(FALSE)
  } else if (all(sort(report) == report) | all(sort(report, decreasing = TRUE) == report)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# part 1
report_evaluation_1 <- map_lgl(reports, is_report_safe)
sum(report_evaluation_1)

# part 2
# focus only on failed reports from part 1
failed_reports <- reports[!report_evaluation_1]

# check which of the failed reports do now pass the test by tolerating a single
# bad level
report_evaluation_2 <- map_lgl(failed_reports, .f = function(report) {
  ret <- imap_lgl(report, .f = function(val, idx) {
    is_safe <- is_report_safe(report[-idx])
    if (isTRUE(is_safe)) {
      return(is_safe)
      # stop testing different versions of a report once we find a safe report
      break
    }
    return(is_safe)
  })
  return(any(ret))
})

sum(report_evaluation_1) + sum(report_evaluation_2)
