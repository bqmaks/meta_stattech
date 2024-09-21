# styler: off
box::use(
  ./meta_utils[...],
  ./proc_data[...],
  metafor[...],
  multcomp[contrMat],
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
compute_I_squared <- function(data_ma, method = "EE", ...) {
  res_df <- rbind(
    data_ma |>
      mutate(group = "_Overall"),
    data_ma |>
      arrange(group) |>
      drop_na(group)
  ) |>
    as_tibble() |>
    nest(data = -c(group)) |>
    mutate(
      I2 = map_dbl(
        data,
        \(d) rma(yi, vi, data = d, method = method)$I2
      )
    ) |>
    select(-data)

  res_vec <- res_df$I2
  names(res_vec) <- res_df$group

  return(as.list(res_vec))
}

#' @export
egger_test <- function(data_ma, ...) {
  res <- rbind(
    data_ma |>
      mutate(group = "_Overall"),
    data_ma |>
      arrange(group) |>
      drop_na(group)
  ) |>
    as_tibble() |>
    drop_na(yi, vi) |>
    nest(data = -c(group)) %>%
    mutate(res = map(
      data,
      \(d) regtest(yi, vi, data = d, model = "lm")
    )) %>%
    transmute(
      group,
      est = map_dbl(res, \(x) x[["est"]]),
      lcl = map_dbl(res, \(x) x[["ci.lb"]]),
      ucl = map_dbl(res, \(x) x[["ci.ub"]]),
      tval = map_dbl(res, \(x) x[["zval"]]),
      df = map_dbl(res, \(x) x[["dfs"]]),
      pval = map_dbl(res, \(x) x[["pval"]])
    )

  return(res)
}

#' @export
perform_generic_ma <- function(
    data_ma, measure = "OR", method = "DL", ...) {
  methods <- c(
    "EE", "DL", "HE", "HS", "HSk", "SJ", "ML",
    "REML", "EB", "PM", "GENQ", "PMM", "GENQM"
  )

  meta_res <- bind_rows(
    data_ma |> mutate(group = "_Overall"),
    data_ma |> arrange(group) |> drop_na(group)
  ) |>
    as_tibble() |>
    drop_na(yi, vi) |>
    nest(data = -c(group)) |>
    mutate(
      fit = map(
        data, \(d) rma(yi, vi, data = d, method = method)
      ),
      pred = map(fit, \(d) predict.rma(d))
    ) |>
    transmute(
      measure = measure,
      method = method,
      group = group,
      est = map_dbl(pred, \(x) x[["pred"]]),
      se = map_dbl(pred, \(x) x[["se"]]),
      lcl = map_dbl(pred, \(x) x[["ci.lb"]]),
      ucl = map_dbl(pred, \(x) x[["ci.ub"]]),
      lpl = map_dbl(pred, function(x) {
        res <- x[["pi.lb"]]
        ifelse(is.null(res), NA_real_, res)
      }),
      upl = map_dbl(pred, function(x) {
        res <- x[["pi.ub"]]
        ifelse(is.null(res), NA_real_, res)
      }),
      zval = map_dbl(fit, \(x) x[["zval"]]),
      pval = map_dbl(fit, \(x) x[["pval"]]),
      tau2 = map_dbl(fit, \(x) x[["tau2"]]),
      tau2_se = map_dbl(fit, \(x) x[["se.tau2"]]),
      I2 = map_dbl(fit, \(x) x[["I2"]]),
      H2 = map_dbl(fit, \(x) x[["H2"]]),
      Q = map_dbl(fit, \(x) x[["QE"]]),
      Qdf = map_dbl(data, \(d) nrow(d) - 1),
      Qpval = map_dbl(fit, \(x) x[["QEp"]])
    )

  return(meta_res)
}

#' @export
perform_peto_ma <- function(data, measure = "OR", ...) {
  measure_options <- c("OR")

  columns <- c(
    "trial" = NA_integer_, "author" = NA_character_,
    "year" = NA_integer_, "suffix" = NA_character_,
    "group" = NA_character_, "covar" = NA_real_,
    "t_pos" = NA_integer_, "t_total" = NA_integer_,
    "c_pos" = NA_integer_, "c_total" = NA_integer_
  )

  data <- add_column(data, !!!columns[
    setdiff(names(columns), names(data))
  ])

  priority <- c("t_pos", "t_total", "c_pos", "c_total")

  data <- data |> drop_na(all_of(priority))

  meta_res <- bind_rows(
    data_ma |> mutate(group = "_Overall"),
    data_ma |> arrange(group) |> drop_na(group)
  ) |>
    as_tibble() |>
    nest(data = -c(group)) |>
    mutate(
      fit = map(
        data,
        \(d) rma.peto(
          ai = t_pos, bi = t_total - t_pos,
          ci = c_pos, di = c_total - c_pos,
          data = d, add = 1 / 2, to = "only0", drop00 = TRUE
        )
      )
    ) |>
    transmute(
      measure = measure,
      method = "Peto",
      group = group,
      est = map_dbl(fit, \(x) x[["beta"]][1]),
      se = map_dbl(fit, \(x) x[["se"]]),
      lcl = map_dbl(fit, \(x) x[["ci.lb"]]),
      ucl = map_dbl(fit, \(x) x[["ci.ub"]]),
      zval = map_dbl(fit, \(x) x[["zval"]]),
      pval = map_dbl(fit, \(x) x[["pval"]]),
      I2 = map_dbl(fit, \(x) x[["I2"]]),
      H2 = map_dbl(fit, \(x) x[["H2"]]),
      Q = map_dbl(fit, \(x) x[["QE"]]),
      Qdf = map_dbl(data, \(d) nrow(d) - 1),
      Qpval = map_dbl(fit, \(x) x[["QEp"]])
    )

  return(meta_res)
}

#' @export
perform_mh_ma <- function(data, measure = "OR", ...) {
  measure_options <- c("OR", "RR", "RD", "IRR")

  #   fit <- rma.mh(
  #     ai = t_pos, bi = t_total - t_pos,
  #     ci = c_pos, di = c_total - c_pos,
  #     data = data, add = 1 / 2, to = "only0",
  #     drop00 = TRUE, correct = TRUE
  #   )

  if (measure %in% c("OR", "RR", "RD")) {
    columns <- c(
      "trial" = NA_integer_, "author" = NA_character_,
      "year" = NA_integer_, "suffix" = NA_character_,
      "group" = NA_character_, "covar" = NA_real_,
      "t_pos" = NA_integer_, "t_total" = NA_integer_,
      "c_pos" = NA_integer_, "c_total" = NA_integer_
    )

    data <- add_column(data, !!!columns[
      setdiff(names(columns), names(data))
    ])

    priority <- c("t_pos", "t_total", "c_pos", "c_total")
    data <- data |> drop_na(all_of(priority))
    meta_res <- bind_rows(
      data_ma |> mutate(group = "_Overall"),
      data_ma |> arrange(group) |> drop_na(group)
    ) |>
      as_tibble() |>
      nest(data = -c(group)) |>
      mutate(
        fit = map(
          data, \(d) rma.mh(
            ai = t_pos, bi = t_total - t_pos,
            ci = c_pos, di = c_total - c_pos,
            data = d, add = 1 / 2, to = "only0",
            drop00 = TRUE, correct = TRUE
          )
        )
      )
  } else {
    columns <- c(
      "trial" = NA_integer_, "author" = NA_character_,
      "year" = NA_integer_, "suffix" = NA_character_,
      "group" = NA_character_, "covar" = NA_real_,
      "t_pos" = NA_integer_, "t_time" = NA_integer_,
      "c_pos" = NA_integer_, "c_time" = NA_integer_
    )

    data <- add_column(data, !!!columns[
      setdiff(names(columns), names(data))
    ])

    priority <- c("t_pos", "t_time", "c_pos", "c_time")
    data <- data |> drop_na(all_of(priority))
    meta_res <- bind_rows(
      data_ma |> mutate(group = "_Overall"),
      data_ma |> arrange(group) |> drop_na(group)
    ) |>
      as_tibble() |>
      nest(data = -c(group)) |>
      mutate(
        fit = map(
          data, \(d) rma.mh(
            x1i = t_pos, t1i = t_time,
            x2i = c_pos, t2i = c_time,
            data = d, add = 1 / 2, to = "only0",
            drop00 = TRUE
          )
        )
      )
  }

  meta_res <- meta_res |>
    transmute(
      measure = measure,
      method = "MH",
      group = group,
      est = map_dbl(fit, \(x) x[["beta"]][1]),
      se = map_dbl(fit, \(x) x[["se"]]),
      lcl = map_dbl(fit, \(x) x[["ci.lb"]]),
      ucl = map_dbl(fit, \(x) x[["ci.ub"]]),
      zval = map_dbl(fit, \(x) x[["zval"]]),
      pval = map_dbl(fit, \(x) x[["pval"]]),
      CMH = map_dbl(fit, \(x) x[["MH"]]),
      CMHpval = map_dbl(fit, \(x) x[["MHp"]]),
      I2 = map_dbl(fit, \(x) x[["I2"]]),
      H2 = map_dbl(fit, \(x) x[["H2"]]),
      Q = map_dbl(fit, \(x) x[["QE"]]),
      Qdf = map_dbl(data, \(d) nrow(d) - 1),
      Qpval = map_dbl(fit, \(x) x[["QEp"]])
    )

  return(meta_res)
}

#' @export
perform_metareg <- function(data_ma, method = "DL", measure = "GEN", ...) {
  methods <- c(
    "DL", "HE", "HS", "HSk", "SJ", "ML",
    "REML", "EB", "PM", "GENQ", "PMM", "GENQM"
  )

  data_ma <- data_ma |> drop_na(covar, yi, vi)

  fit <- rma(yi, vi, mods = ~covar, data = data_ma, method = method)

  tibble(
    measure = measure,
    method = method,
    term = c("b0", "b1"),
    est = fit$beta[, 1],
    se = fit$se,
    lcl = fit$ci.lb,
    ucl = fit$ci.ub,
    zval = fit$zval,
    pval = fit$pval,
    tau2 = c(fit$tau2, NA),
    tau2_se = c(fit$se.tau2, NA),
    I2 = c(fit$I2, NA),
    H2 = c(fit$H2, NA),
    R2 = c(fit$R2, NA),
    Q_resid = c(fit$QE, NA),
    Q_resid_df = c(nrow(data_ma) - 2, NA),
    Q_resid_pval = c(fit$QEp, NA),
    Q_moder = c(fit$QM, NA),
    Q_moder_df = c(fit$QMdf[[1]], NA),
    Q_moder_pval = c(fit$QMp, NA)
  )
}

#' @export
perform_anova <- function(data_ma, method = "DL", measure = "GEN", ...) {
  methods <- c(
    "DL", "HE", "HS", "HSk", "SJ", "ML",
    "REML", "EB", "PM", "GENQ", "PMM", "GENQM"
  )

  data_ma <- data_ma |>
    drop_na(group, yi, vi) |>
    mutate(group = fct_drop(group))

  fit <- rma(yi, vi, mods = ~group, data = data_ma, method = method)

  anova_fit <- anova(fit)

  tibble(
    measure = measure,
    method = method,
    Q_moder = anova_fit$QM,
    Q_moder_df = anova_fit$QMdf[[1]],
    Q_moder_pval = anova_fit$QMp
  )
}

#' @export
perform_pairwise_comp <- function(data_ma, method = "DL", measure = "GEN", ...) {
  methods <- c(
    "DL", "HE", "HS", "HSk", "SJ", "ML",
    "REML", "EB", "PM", "GENQ", "PMM", "GENQM"
  )

  if (!is.factor(data_ma$group)) {
    data_ma <- data_ma |>
      mutate(group = as_factor(group))
  }

  data_ma <- data_ma |>
    drop_na(group, yi, vi) |>
    mutate(group = fct_drop(group))

  fit_comp <- rma(yi, vi, mods = ~ 0 + group, data = data_ma, method = method)

  cont_mat <- contrMat(n = table(data_ma$group), type = "Tukey")

  comp <- anova(fit_comp, X = cont_mat)

  tibble(
    measure = measure,
    method = method,
    comparison = rownames(comp$Xb),
    est = comp$Xb[, 1],
    se = comp$se,
    lcl = est - 1.96 * se,
    ucl = est + 1.96 * se,
    zval = comp$zval,
    pval = comp$pval
  )
}
