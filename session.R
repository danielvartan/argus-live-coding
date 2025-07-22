# Exercise 1 -----

#' *Not the actual problem statement*
#'
#' [Calculate the rolling 30-day correlation between `OILPRICE` and `SPX`]

library(data.table)
library(gamlss)
attach(oil)

dt0 <-
  data.table::data.table(
    Time = 1:nrow(oil) , #nolint
    P = exp(oil$OILPRICE),
    SPX  = exp(oil$SPX)
  )

# View(dt0)

# Solution presented during the interview

out <- numeric()

for (i in seq_len(nrow(dt0))) {
  if (i >= 30) {
    data_i <- dt0 |> _[seq(i - 29, i)]

    out <- c(out, cor(data_i$P, data_i$SPX))
  } else {
    out <- c(out, NA)
  }
}

dt0[, cor := out] |> head(50)

# Post-interview solution 1

library(data.table)
library(gamlss)
library(runner)

gamlss.data::oil |>
  as.data.table() |>
  _[,
    .(
      TIME = seq_len(nrow(oil)),
      P = exp(OILPRICE),
      SPX = exp(SPX_log)
    )
  ] |>
  _[,
    cor := runner(
      x = .SD,
      f = function(data) {
        if (nrow(data) < 30) {
          NA_real_
        } else {
          cor(data$P, data$SPX)
        }
      },
      k = 30
    )
  ] |>
  head(50)

# Post-interview solution 2

library(dplyr)
library(gamlss)
library(slider)

gamlss.data::oil |>
  as_tibble() |>
  transmute(
    TIME = row_number(),
    P = exp(OILPRICE),
    SPX = exp(SPX_log)
  ) |>
  mutate(
    cor = slide2_dbl(
      .x = P,
      .y = SPX,
      .f = \(x, y) cor(x, y),
      .before = 29,
      .complete = TRUE
    )
  ) |>
  print(n = 50)

# Exercise 2 -----

#' *Not the actual problem statement*
#'
#' [Find peaks in `kde$y`]

D <- c(rnorm(100, 1, 1), rnorm(100, 5, 1)) #nolint
kde <- density(D)
x <- kde$x
y <- kde$y

#  plot(x, y)

# Solution presented during the interview

peaks <- function(y) {
  ifelse(
    (y > shift(y, -1)) & (y > shift(y, 1)),
    TRUE,
    FALSE
  ) |>
    base::subset(y, subset = _)
}

peaks(y)

# Solution proposed by the interviewer

peak_indexes <- diff(y) |> sign() |> diff()

y[which(c(rep(0, 1), peak_indexes) < 0)]

# Exercise 3 -----

#' Problem statement
#'
#' Write an R function are_anagrams(s1, s2) that determines if two strings s1
#' and s2 are anagrams of each other. Anagrams are words or phrases formed by
#' rearranging the letters of another, typically using all the original
#' letters exactly once.
#'
#' Ignore case.
#' Ignore spaces inside the strings.
#'
# - print(are_anagrams("listen", "silent"))
# - print(are_anagrams("eleven plus two", "twelve plus one"))

# The solution presented during the interview
# (with tips from the interviewer)

str_explode <- function(string) {
  string |>
    tolower() |>
    gsub(" ", "", x = _) |>
    strsplit("") |>
    unlist() |>
    sort()
}

are_anagrams <- function(s1, s2) {
  all(str_explode(s1) == str_explode(s2))
}

print(are_anagrams("listen", "silent"))
print(are_anagrams("eleven plus two", "twelve plus one"))
