# styler: off
box::use(
  BSDA[tsum.test],
  metafor[conv.fivenum],
  tibble[...],
  tidyr[...],
  dplyr[...],
  purrr[...],
  rlang[...],
  stringr[...],
  forcats[...]
)
# styler: on

#' @export
generic_columns <- c(
  "trial", "author", "year", "suffix", "group", "covar"
)

#' @export
generic_methods <- c(
  "EE", "DL", "HE", "HS", "HSk", "SJ", "ML",
  "REML", "EB", "PM", "GENQ", "PMM", "GENQM"
)

#' @export
binary_measures <- c("OR", "RR", "RD", "AS")

#' @export
relative_measures <- c("OR", "PETO", "RR", "HR", "IRR")

#' @export
se_from_ci <- Vectorize(function(q, lcl, ucl) {
  abs(ucl - lcl) / (q * 2)
})

#' @export
se_from_means <- Vectorize(function(
    t_mean, t_sd, t_n, c_mean, c_sd, c_n, ...) {
  res <- tsum.test(
    mean.x = t_mean, s.x = t_sd, n.x = t_n,
    mean.y = c_mean, s.y = c_sd, n.y = c_n,
    alternative = "two.sided",
    var.equal = FALSE,
    conf.level = 0.95
  )
  se_from_ci(
    qt(0.975, df = t_n + c_n - 2),
    res$conf.int[[1]],
    res$conf.int[[2]]
  )
})

#' @export
se_prop <- Vectorize(function(pos, total) {
  pos <- if (pos == 0) pos + 1 / 2
  p <- pos / total
  sqrt((p * (1 - p)) / total)
})

#' @export
add_mean_sd <- function(
    data, prefix = "t_", skew_test = TRUE, ...) {
  data <- data |> set_names(
    str_remove(names(data), paste0("^", prefix))
  )

  columns <- c(
    "trial" = NA_integer_, "author" = NA_character_,
    "year" = NA_integer_, "suffix" = NA_character_,
    "group" = NA_character_, "covar" = NA_real_,
    "min" = NA_real_, "q1" = NA_real_,
    "median" = NA_real_,
    "q3" = NA_real_, "max" = NA_real_,
    "mean" = NA_real_, "sd" = NA_real_,
    "se" = NA_real_, "lcl" = NA_real_, "ucl" = NA_real_,
    "n" = NA_integer_
  )

  data <- add_column(data, !!!columns[
    setdiff(names(columns), names(data))
  ])

  priority <- list(
    "1st" = c("mean", "sd"),
    "2nd" = c("mean", "se", "n"),
    "3rd" = c("mean", "lcl", "ucl", "n"),
    "4th" = c("min", "q1", "median", "q3", "max", "n")
  )

  data_1 <- data |> drop_na(all_of(priority[["1st"]]))
  data_2 <- data |> drop_na(all_of(priority[["2nd"]]))
  data_3 <- data |> drop_na(all_of(priority[["3rd"]]))
  data_4 <- data

  data_list <- vector(mode = "list")

  if (nrow(data_1) > 0) {
    data_list[["1st"]] <- data_1 |> mutate(idx = 1)
  }

  if (nrow(data_2) > 0) {
    data_list[["2nd"]] <- data_2 |> mutate(
      sd = se * sqrt(n),
      idx = 2
    )
  }

  if (nrow(data_3) > 0) {
    data_list[["3rd"]] <- data_3 |> mutate(
      se = se_from_ci(qt(0.975, n - 1), lcl, ucl),
      sd = se * sqrt(n),
      idx = 3
    )
  }

  if (nrow(data_4) > 0) {
    data_list[["4th"]] <- data_4 |>
      conv.fivenum(
        min = min, q1 = q1, median = median, q3 = q3, max = max, n = n,
        method = "luo/wan/shi", dist = "norm", test = skew_test,
        var.names = c("mean", "sd"),
        data = _
      ) |>
      mutate(idx = 4)
  }

  res <- reduce(data_list, bind_rows) |> filter(
    idx == min(idx),
    .by = trial
  )

  names(res) <- if_else(
    names(res) %in% generic_columns, names(res),
    paste0(prefix, names(res))
  )

  return(res)
}
