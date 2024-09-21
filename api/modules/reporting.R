# styler: off
box::use(
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
get_ma_table <- function(
    data_ma, ma_res, label_overall = "Overall", exponentiate = FALSE, ...) {
  data_ma <- data_ma |>
    mutate(suffix = replace_na(as.character(suffix), "")) |>
    mutate(study = as_factor(
      str_glue("{author} ({year}{suffix})")
    )) |>
    mutate(z = qnorm(0.975))

  # placeholder for study level statistics
  data_study <- data_ma |>
    select(trial)

  ma_res <- ma_res |>
    mutate(study = label_overall)

  if (all(c("t_n", "c_n") %in% names(data_ma))) {
    data_ma <- data_ma |>
      mutate(z = if_else(
        !is.na(t_n) & !is.na(c_n), qt(p = 0.975, df = t_n + c_n - 2), z
      ))
  } else if ("t_n" %in% names(data_ma)) {
    data_ma <- data_ma |>
      mutate(z = if_else(
        !is.na(t_n), qt(p = 0.975, df = t_n - 1), z
      ))
  }

  res <- data_ma |>
    transmute(
      trial, study, author, year, suffix, group,
      est = yi, lcl = est - (z * sqrt(vi)), ucl = est + (z * sqrt(vi)),
      weight = 1 / vi
    ) |>
    arrange(group, trial) |>
    left_join(data_study, by = join_by(trial)) |>
    bind_rows(ma_res) |>
    mutate(
      group = as_factor(group),
      study = as_factor(study)
    ) |>
    arrange(group, study)

  if (exponentiate) {
    res <- res |>
      mutate(across(
        c(all_of(c("est", "lcl", "ucl")), any_of(c("lpl", "upl"))),
        \(x) exp(x)
      ))
  }

  return(res)
}
