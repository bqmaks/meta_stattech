# styler: off
box::use(
  ./meta_utils[...],
  metafor[escalc],
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
make_data_2means <- function(
    data, measure = "MD", skew_test = FALSE, ...) {
  measure_options <- c("MD", "SMD")

  columns <- c(
    "trial" = NA_integer_, "author" = NA_character_,
    "year" = NA_integer_, "suffix" = NA_character_,
    "group" = NA_character_, "covar" = NA_real_,
    "t_min" = NA_real_, "t_q1" = NA_real_,
    "t_median" = NA_real_,
    "t_q3" = NA_real_, "t_max" = NA_real_,
    "t_mean" = NA_real_, "t_sd" = NA_real_,
    "t_se" = NA_real_, "t_lcl" = NA_real_, "t_ucl" = NA_real_,
    "t_n" = NA_integer_,
    "c_min" = NA_real_, "c_q1" = NA_real_,
    "c_median" = NA_real_,
    "c_q3" = NA_real_, "c_max" = NA_real_,
    "c_mean" = NA_real_, "c_sd" = NA_real_,
    "c_se" = NA_real_, "c_lcl" = NA_real_, "c_ucl" = NA_real_,
    "c_n" = NA_integer_,
    "md" = NA_real_, "md_se" = NA_real_,
    "lcl" = NA_real_, "ucl" = NA_real_
  )

  data <- add_column(data, !!!columns[
    setdiff(names(columns), names(data))
  ])

  priority <- list(
    "1st" = c("md", "md_se"),
    "2nd" = c("md", "lcl", "ucl", "t_n", "c_n"),
    "3rd" = c("t_mean", "t_sd", "t_n", "c_mean", "c_sd", "c_n"),
    "4th" = c("md", "lcl", "ucl"),
    "5th" = c("...")
  )

  data_1 <- data |> drop_na(all_of(priority[["1st"]]))
  data_2 <- data |> drop_na(all_of(priority[["2nd"]]))
  data_3 <- data |> drop_na(all_of(priority[["3rd"]]))
  data_4 <- data |> drop_na(all_of(priority[["4th"]]))
  data_5 <- data

  data_list <- vector(mode = "list")

  if (nrow(data_1) > 0) {
    data_list[["1st"]] <- data_1 |> mutate(
      yi = md,
      vi = md_se^2,
      idx = 1
    )
  }

  if (nrow(data_2) > 0) {
    data_list[["2nd"]] <- data_2 |> mutate(
      yi = md,
      vi = se_from_ci(qt(0.975, t_n + c_n - 2), lcl, ucl)^2,
      idx = 2
    )
  }

  if (nrow(data_3) > 0) {
    data_list[["3rd"]] <- data_3 |>
      escalc(
        m1i = t_mean, sd1i = t_sd, n1i = t_n,
        m2i = c_mean, sd2i = c_sd, n2i = c_n,
        data = _, measure = measure, vtype = "LS"
      ) |>
      mutate(idx = 3)
  }

  if (nrow(data_4) > 0) {
    data_list[["4th"]] <- data_4 |> mutate(
      yi = t_mean - c_mean,
      vi = se_from_ci(1.96, lcl, ucl)^2,
      idx = 4
    )
  }

  if (nrow(data_5) > 0) {
    data_t <- data_5 |>
      select(
        all_of(generic_columns), starts_with("t_")
      ) |>
      add_mean_sd(
        data = _, prefix = "t_", skew_test = skew_test
      )

    data_c <- data_5 |>
      select(
        all_of(generic_columns), starts_with("c_")
      ) |>
      add_mean_sd(
        data = _, prefix = "c_", skew_test = skew_test
      )

    data_list[["5th"]] <- full_join(
      data_t, data_c,
      by = generic_columns
    ) |>
      escalc(
        m1i = t_mean, sd1i = t_sd, n1i = t_n,
        m2i = c_mean, sd2i = c_sd, n2i = c_n,
        data = _, measure = measure, vtype = "LS"
      ) |>
      mutate(idx = 5)
  }

  reduce(data_list, bind_rows) |>
    drop_na(yi, vi) |>
    filter(
      idx == min(idx),
      .by = trial
    )
}

#' @export
make_data_1mean <- function(data, measure = "MEAN", skew_test = FALSE, ...) {
  measure_options <- c("MEAN")

  columns <- c(
    "trial" = NA_integer_, "author" = NA_character_,
    "year" = NA_integer_, "suffix" = NA_character_,
    "group" = NA_character_, "covar" = NA_real_,
    "t_min" = NA_real_, "t_q1" = NA_real_,
    "t_median" = NA_real_,
    "t_q3" = NA_real_, "t_max" = NA_real_,
    "t_mean" = NA_real_, "t_sd" = NA_real_,
    "t_se" = NA_real_, "t_lcl" = NA_real_, "t_ucl" = NA_real_,
    "t_n" = NA_integer_
  )

  data <- add_column(data, !!!columns[
    setdiff(names(columns), names(data))
  ])

  priority <- list(
    "1st" = c("t_mean", "t_se"),
    "2nd" = c("t_mean", "t_lcl", "t_ucl", "t_n"),
    "3rd" = c("t_mean", "t_sd", "t_n"),
    "4th" = c("t_mean", "t_lcl", "t_ucl"),
    "5th" = c("t_min", "t_q1", "t_median", "t_q3", "t_max", "t_n")
  )

  data_1 <- data |> drop_na(all_of(priority[["1st"]]))
  data_2 <- data |> drop_na(all_of(priority[["2nd"]]))
  data_3 <- data |> drop_na(all_of(priority[["3rd"]]))
  data_4 <- data |> drop_na(all_of(priority[["4th"]]))
  data_5 <- data

  data_list <- vector(mode = "list")

  if (nrow(data_1) > 0) {
    data_list[["1st"]] <- data_1 |>
      mutate(
        yi = t_mean,
        vi = t_se^2
      ) |>
      mutate(idx = 1)
  }

  if (nrow(data_2) > 0) {
    data_list[["2nd"]] <- data_2 |>
      mutate(
        yi = t_mean,
        vi = se_from_ci(qt(0.975, t_n - 1), t_lcl, t_ucl)^2
      ) |>
      mutate(idx = 2)
  }

  if (nrow(data_3) > 0) {
    data_list[["3rd"]] <- data_3 |>
      mutate(
        yi = t_mean,
        vi = (t_sd / sqrt(t_n))^2
      ) |>
      mutate(idx = 3)
  }

  if (nrow(data_4) > 0) {
    data_list[["4th"]] <- data_4 |>
      mutate(
        yi = t_mean,
        vi = se_from_ci(1.96, t_lcl, t_ucl)^2
      ) |>
      mutate(idx = 4)
  }

  if (nrow(data_5) > 0) {
    data_list[["5th"]] <- add_mean_sd(
      data_5,
      prefix = "t_", skew_test = skew_test
    ) |>
      mutate(
        yi = t_mean,
        vi = (t_sd / sqrt(t_n))^2
      ) |>
      mutate(idx = 5)
  }

  reduce(data_list, bind_rows) |>
    drop_na(yi, vi) |>
    filter(idx == min(idx), .by = trial) |>
    arrange(trial)
}

#' @export
make_data_generic_absolute <- function(data, measure = "CORR", ...) {
  measure_options <- c("CORR", "BETA")

  columns <- c(
    "trial" = NA_integer_, "author" = NA_character_,
    "year" = NA_integer_, "suffix" = NA_character_,
    "group" = NA_character_, "covar" = NA_real_,
    "m" = NA_real_, "se" = NA_real_,
    "lcl" = NA_real_, "ucl" = NA_real_
  )

  data <- add_column(data, !!!columns[
    setdiff(names(columns), names(data))
  ])

  priority <- list(
    "1st" = c("m", "se"),
    "2nd" = c("m", "lcl", "ucl", "n"),
    "3rd" = c("m", "lcl", "ucl")
  )

  data_1 <- data |> drop_na(all_of(priority[["1st"]]))
  data_2 <- data |> drop_na(all_of(priority[["2nd"]]))
  data_3 <- data |> drop_na(all_of(priority[["3rd"]]))

  data_list <- vector(mode = "list")

  if (nrow(data_1) > 0) {
    data_list[["1st"]] <- data_1 |>
      mutate(
        yi = m,
        vi = se^2
      ) |>
      mutate(idx = 1)
  }

  if (nrow(data_2) > 0) {
    data_list[["2nd"]] <- data_2 |>
      mutate(
        yi = m,
        vi = se_from_ci(qt(0.975, n - 1), lcl, ucl)^2
      ) |>
      mutate(idx = 2)
  }

  if (nrow(data_3) > 0) {
    data_list[["3th"]] <- data_3 |>
      mutate(
        yi = m,
        vi = se_from_ci(1.96, lcl, ucl)^2
      ) |>
      mutate(idx = 3)
  }

  reduce(data_list, bind_rows) |>
    drop_na(yi, vi) |>
    filter(idx == min(idx), .by = trial) |>
    arrange(trial)
}

#' @export
make_data_generic_relative <- function(data, measure = "HR", ...) {
  measure_options <- c("HR")

  columns <- c(
    "trial" = NA_integer_, "author" = NA_character_,
    "year" = NA_integer_, "suffix" = NA_character_,
    "group" = NA_character_, "covar" = NA_real_,
    "log_rm" = NA_real_, "se" = NA_real_,
    "rm" = NA_real_, "lcl" = NA_real_, "ucl" = NA_real_
  )

  data <- add_column(data, !!!columns[
    setdiff(names(columns), names(data))
  ])

  priority <- list(
    "1st" = c("log_rm", "se"),
    "2nd" = c("rm", "lcl", "ucl")
  )

  data_1 <- data |> drop_na(all_of(priority[["1st"]]))
  data_2 <- data |> drop_na(all_of(priority[["2nd"]]))

  data_list <- vector(mode = "list")

  if (nrow(data_1) > 0) {
    data_list[["1st"]] <- data_1 |>
      mutate(
        yi = log_rm,
        vi = se^2
      ) |>
      mutate(idx = 1)
  }

  if (nrow(data_2) > 0) {
    data_list[["2nd"]] <- data_2 |>
      mutate(
        yi = log(rm),
        vi = se_from_ci(1.96, log(lcl), log(ucl))^2
      ) |>
      mutate(idx = 2)
  }

  reduce(data_list, bind_rows) |>
    drop_na(yi, vi) |>
    filter(idx == min(idx), .by = trial) |>
    arrange(trial)
}

#' @export
make_data_1prop <- function(data, measure = "PROP", ...) {
  measure_options <- c("PROP")

  columns <- c(
    "trial" = NA_integer_, "author" = NA_character_,
    "year" = NA_integer_, "suffix" = NA_character_,
    "group" = NA_character_, "covar" = NA_real_,
    "t_pos" = NA_integer_, "t_total" = NA_integer_,
    "prop" = NA_real_, "se" = NA_real_,
    "lcl" = NA_real_, "ucl" = NA_real_
  )

  data <- add_column(data, !!!columns[
    setdiff(names(columns), names(data))
  ])

  priority <- list(
    "1st" = c("t_pos", "t_total"),
    "2nd" = c("prop", "se"),
    "3rd" = c("prop", "lcl", "ucl")
  )

  data_1 <- data |> drop_na(all_of(priority[["1st"]]))
  data_2 <- data |> drop_na(all_of(priority[["2nd"]]))
  data_3 <- data |> drop_na(all_of(priority[["3rd"]]))

  data_list <- vector(mode = "list")

  if (nrow(data_1) > 0) {
    data_list[["1st"]] <- data_1 |>
      mutate(
        yi = if_else(t_pos == 0, (t_pos + 1 / 2) / t_total, t_pos / t_total),
        vi = se_prop(t_pos, t_total)^2
      ) |>
      mutate(idx = 1)
  }

  if (nrow(data_2) > 0) {
    data_list[["2nd"]] <- data_2 |>
      mutate(
        yi = prop,
        vi = se^2
      ) |>
      mutate(idx = 2)
  }

  if (nrow(data_3) > 0) {
    data_list[["3th"]] <- data_3 |>
      mutate(
        yi = prop,
        vi = se_from_ci(1.96, lcl, ucl)^2
      ) |>
      mutate(idx = 3)
  }

  reduce(data_list, bind_rows) |>
    drop_na(yi, vi) |>
    filter(idx == min(idx), .by = trial) |>
    arrange(trial)
}

#' @export
make_data_2prop_absolute <- function(data, measure = "RD", ...) {
  # AS for the arcsine square root transformed risk difference
  measure_options <- c("RD")

  columns <- c(
    "trial" = NA_integer_, "author" = NA_character_,
    "year" = NA_integer_, "suffix" = NA_character_,
    "group" = NA_character_, "covar" = NA_real_,
    "t_pos" = NA_integer_, "t_total" = NA_integer_,
    "c_pos" = NA_integer_, "c_total" = NA_integer_,
    "m" = NA_real_, "se" = NA_real_,
    "lcl" = NA_real_, "ucl" = NA_real_
  )

  data <- add_column(data, !!!columns[
    setdiff(names(columns), names(data))
  ])

  priority <- list(
    "1st" = c("t_pos", "t_total", "c_pos", "c_total"),
    "2nd" = c("m", "se"),
    "3rd" = c("m", "lcl", "ucl")
  )

  data_1 <- data |> drop_na(all_of(priority[["1st"]]))
  data_2 <- data |> drop_na(all_of(priority[["2nd"]]))
  data_3 <- data |> drop_na(all_of(priority[["3rd"]]))

  data_list <- vector(mode = "list")

  if (nrow(data_1) > 0) {
    data_list[["1st"]] <- data_1 |>
      escalc(
        ai = t_pos, bi = t_total - t_pos,
        ci = c_pos, di = c_total - c_pos,
        data = _, add = 1 / 2, to = "only0",
        measure = measure
      ) |>
      mutate(idx = 1)
  }

  if (nrow(data_2) > 0) {
    data_list[["2nd"]] <- data_2 |>
      mutate(
        yi = m, vi = se^2
      ) |>
      mutate(idx = 2)
  }

  if (nrow(data_3) > 0) {
    data_list[["3rd"]] <- data_3 |>
      mutate(
        yi = m,
        vi = se_from_ci(1.96, lcl, ucl)^2
      ) |>
      mutate(idx = 3)
  }

  reduce(data_list, bind_rows) |>
    drop_na(yi, vi) |>
    filter(idx == min(idx), .by = trial) |>
    arrange(trial)
}

#' @export
make_data_2prop_relative <- function(data, measure = "OR", ...) {
  # PETO  for the log odds ratio estimated with Peto's method
  measure_options <- c("OR", "PETO", "RR")

  columns <- c(
    "trial" = NA_integer_, "author" = NA_character_,
    "year" = NA_integer_, "suffix" = NA_character_,
    "group" = NA_character_, "covar" = NA_real_,
    "t_pos" = NA_integer_, "t_total" = NA_integer_,
    "c_pos" = NA_integer_, "c_total" = NA_integer_,
    "log_rm" = NA_real_, "se" = NA_real_,
    "rm" = NA_real_, "lcl" = NA_real_, "ucl" = NA_real_
  )

  data <- add_column(data, !!!columns[
    setdiff(names(columns), names(data))
  ])

  priority <- list(
    "1st" = c("t_pos", "t_total", "c_pos", "c_total"),
    "2nd" = c("log_rm", "se"),
    "3rd" = c("rm", "lcl", "ucl")
  )

  data_1 <- data |> drop_na(all_of(priority[["1st"]]))
  data_2 <- data |> drop_na(all_of(priority[["2nd"]]))
  data_3 <- data |> drop_na(all_of(priority[["3rd"]]))

  data_list <- vector(mode = "list")

  if (nrow(data_1) > 0) {
    data_list[["1st"]] <- data_1 |>
      escalc(
        ai = t_pos, bi = t_total - t_pos,
        ci = c_pos, di = c_total - c_pos,
        data = _, add = 1 / 2, to = "only0",
        measure = measure
      ) |>
      mutate(idx = 1)
  }

  if (nrow(data_2) > 0) {
    data_list[["2nd"]] <- data_2 |>
      mutate(
        yi = log_rm, vi = se^2
      ) |>
      mutate(idx = 2)
  }

  if (nrow(data_3) > 0) {
    data_list[["3rd"]] <- data_3 |>
      mutate(
        yi = log(rm),
        vi = se_from_ci(1.96, log(lcl), log(ucl))^2
      ) |>
      mutate(idx = 3)
  }

  reduce(data_list, bind_rows) |>
    drop_na(yi, vi) |>
    filter(idx == min(idx), .by = trial) |>
    arrange(trial)
}

#' @export
make_data_1ir <- function(data, measure = "IR", ...) {
  measure_options <- c("IR")

  columns <- c(
    "trial" = NA_integer_, "author" = NA_character_,
    "year" = NA_integer_, "suffix" = NA_character_,
    "group" = NA_character_, "covar" = NA_real_,
    "t_pos" = NA_integer_, "t_time" = NA_integer_,
    "prop" = NA_real_, "se" = NA_real_,
    "lcl" = NA_real_, "ucl" = NA_real_
  )

  data <- add_column(data, !!!columns[
    setdiff(names(columns), names(data))
  ])

  priority <- list(
    "1st" = c("t_pos", "t_time"),
    "2nd" = c("prop", "se"),
    "3rd" = c("prop", "lcl", "ucl")
  )

  data_1 <- data |> drop_na(all_of(priority[["1st"]]))
  data_2 <- data |> drop_na(all_of(priority[["2nd"]]))
  data_3 <- data |> drop_na(all_of(priority[["3rd"]]))

  data_list <- vector(mode = "list")

  if (nrow(data_1) > 0) {
    data_list[["1st"]] <- data_1 |>
      metafor::escalc(
        xi = t_pos, ti = t_time, measure = measure,
        data = _, add = 1 / 2, to = "only0"
      ) |>
      mutate(idx = 1)
  }

  if (nrow(data_2) > 0) {
    data_list[["2nd"]] <- data_2 |>
      mutate(
        yi = prop,
        vi = se^2
      ) |>
      mutate(idx = 2)
  }

  if (nrow(data_3) > 0) {
    data_list[["3th"]] <- data_3 |>
      mutate(
        yi = prop,
        vi = se_from_ci(1.96, lcl, ucl)^2
      ) |>
      mutate(idx = 3)
  }

  reduce(data_list, bind_rows) |>
    drop_na(yi, vi) |>
    filter(idx == min(idx), .by = trial) |>
    arrange(trial)
}

#' @export
make_data_2ir_absolute <- function(
    data, measure = "IRD", ...) {
  measure_options <- c("IRD")

  columns <- c(
    "trial" = NA_integer_, "author" = NA_character_,
    "year" = NA_integer_, "suffix" = NA_character_,
    "group" = NA_character_, "covar" = NA_real_,
    "t_pos" = NA_integer_, "t_time" = NA_integer_,
    "c_pos" = NA_integer_, "c_time" = NA_integer_,
    "m" = NA_real_, "se",
    "lcl" = NA_real_, "ucl" = NA_real_
  )

  data <- add_column(data, !!!columns[
    setdiff(names(columns), names(data))
  ])

  priority <- list(
    "1st" = c("t_pos", "t_time", "c_pos", "c_time"),
    "2nd" = c("m", "se"),
    "3rd" = c("m", "lcl", "ucl")
  )

  data_1 <- data |> drop_na(all_of(priority[["1st"]]))
  data_2 <- data |> drop_na(all_of(priority[["2nd"]]))
  data_3 <- data |> drop_na(all_of(priority[["3rd"]]))

  data_list <- vector(mode = "list")

  if (nrow(data_1) > 0) {
    data_list[["1st"]] <- data_1 |>
      escalc(
        x1i = t_pos, t1i = t_time,
        x2i = c_pos, t2i = c_time,
        data = _, add = 1 / 2, to = "only0",
        measure = measure
      ) |>
      mutate(idx = 1)
  }

  if (nrow(data_2) > 0) {
    data_list[["2nd"]] <- data_2 |>
      mutate(
        yi = m, vi = se^2
      ) |>
      mutate(idx = 2)
  }

  if (nrow(data_3) > 0) {
    data_list[["3rd"]] <- data_3 |>
      mutate(
        yi = m,
        vi = se_from_ci(1.96, lcl, ucl)^2
      ) |>
      mutate(idx = 3)
  }

  reduce(data_list, bind_rows) |>
    drop_na(yi, vi) |>
    filter(idx == min(idx), .by = trial) |>
    arrange(trial)
}

#' @export
make_data_2ir_relative <- function(data, measure = "IRR", ...) {
  measure_options <- c("IRR")

  columns <- c(
    "trial" = NA_integer_, "author" = NA_character_,
    "year" = NA_integer_, "suffix" = NA_character_,
    "group" = NA_character_, "covar" = NA_real_,
    "t_pos" = NA_integer_, "t_time" = NA_integer_,
    "c_pos" = NA_integer_, "c_time" = NA_integer_,
    "log_rm" = NA_real_, "se" = NA_real_,
    "rm" = NA_real_, "lcl" = NA_real_, "ucl" = NA_real_
  )

  data <- add_column(data, !!!columns[
    setdiff(names(columns), names(data))
  ])

  priority <- list(
    "1st" = c("t_pos", "t_time", "c_pos", "c_time"),
    "2nd" = c("log_rm", "se"),
    "3rd" = c("rm", "lcl", "ucl")
  )

  data_1 <- data |> drop_na(all_of(priority[["1st"]]))
  data_2 <- data |> drop_na(all_of(priority[["2nd"]]))
  data_3 <- data |> drop_na(all_of(priority[["3rd"]]))

  data_list <- vector(mode = "list")

  if (nrow(data_1) > 0) {
    data_list[["1st"]] <- data_1 |>
      escalc(
        x1i = t_pos, t1i = t_time,
        x2i = c_pos, t2i = c_time,
        data = _, add = 1 / 2, to = "only0",
        measure = measure
      ) |>
      mutate(idx = 1)
  }

  if (nrow(data_2) > 0) {
    data_list[["2nd"]] <- data_2 |>
      mutate(
        yi = log_rm, vi = se^2
      ) |>
      mutate(idx = 2)
  }

  if (nrow(data_3) > 0) {
    data_list[["3rd"]] <- data_3 |>
      mutate(
        yi = log(rm),
        vi = se_from_ci(1.96, log(lcl), log(ucl))^2
      ) |>
      mutate(idx = 3)
  }

  reduce(data_list, bind_rows) |>
    drop_na(yi, vi) |>
    filter(idx == min(idx), .by = trial) |>
    arrange(trial)
}
