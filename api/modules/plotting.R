# styler: off
box::use(
  metafor[rma],
  tibble[...],
  tidyr[...],
  dplyr[...],
  purrr[...],
  rlang[...],
  stringr[...],
  forcats[...],
  ggplot2[...]
)
# styler: on

theme_funnel <- function(...) {
  theme_grey(base_size = 14, base_family = "Arimo") +
    theme(
      panel.background = element_rect(color = "#000000", linewidth = 0.25),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.text = element_text(color = "#000000", size = 13)
    )
}

theme_forest <- function(...) {
  theme_bw(base_size = 14, base_family = "Arimo") +
    theme(
      panel.border = element_blank(),
      panel.spacing = unit(0, "mm"),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      legend.position = "none",
      axis.text = element_text(size = 12, color = "#000000"),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.x = element_text(size = 12, color = "#000000", face = "bold")
    )
}

theme_forest_labels <- function(...) {
  theme_bw(base_size = 14, base_family = "Arimo") +
    theme(
      panel.border = element_blank(),
      panel.spacing = unit(0, "mm"),
      panel.grid = element_blank(),
      legend.position = "none",
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(size = 12, color = "#000000", face = "bold")
    )
}

generate_diamond <- function(data, label_overall, ...) {
  data |>
    select(trial, study, est, lcl, ucl) |>
    filter(str_detect(study, paste0("^", label_overall))) |>
    mutate(
      t__ = NA, r__ = NA, b__ = NA, l__ = NA
    ) |>
    pivot_longer(
      t__:l__,
      names_to = "side__", values_to = "y__",
      names_transform = as_factor
    ) |>
    mutate(
      y__adj = 0.15,
      y__ = case_match(
        side__,
        "t__" ~ y__adj,
        "r__" ~ 0,
        "b__" ~ -y__adj,
        "l__" ~ 0,
        .default = NA
      ),
      x__ = case_match(
        side__,
        "t__" ~ est,
        "r__" ~ ucl,
        "b__" ~ est,
        "l__" ~ lcl,
        .default = NA
      )
    )
}

#' @export
funnel_plot_overall <- function(
    data_ma, measure,
    label_y = "Standard error",
    label_x = NA,
    ...) {
  if (is.na(label_x)) {
    label_x <- measure
  }

  fit <- rma(yi, vi, measure = measure, data = data_ma, method = "EE")
  se <- sqrt(max(data_ma$vi))

  data_ma |>
    ggplot(aes(yi, sqrt(vi))) +
    annotate(
      "polygon",
      fill = "white",
      x = c(
        fit$beta[[1]] - 1.96 * se, fit$beta[[1]], fit$beta[[1]] + 1.96 * se
      ),
      y = c(Inf, 0, Inf)
    ) +
    annotate(
      "segment",
      linetype = "dashed", color = "grey25",
      x = fit$beta[[1]], xend = fit$beta[[1]], y = 0, yend = Inf
    ) +
    annotate(
      "segment",
      linetype = "dashed", color = "grey25",
      x = fit$beta[[1]], xend = fit$beta[[1]] + 1.96 * se, y = 0, yend = Inf
    ) +
    annotate(
      "segment",
      linetype = "dashed", color = "grey25",
      x = fit$beta[[1]], xend = fit$beta[[1]] - 1.96 * se, y = 0, yend = Inf
    ) +
    geom_point(size = 2, alpha = 0.75) +
    scale_x_continuous(n.breaks = 8) +
    scale_y_reverse(limits = c(NA, 0), n.breaks = 8) +
    labs(x = label_x, y = label_y) +
    theme_funnel()
}

#' @export
forest_plot_labels <- function(data, ...) {
  p <- data |>
    mutate(trial = row_number()) |>
    ggplot(aes(" ", -trial)) +
    geom_text(aes(label = study), hjust = 0, size = 4.5) +
    scale_x_discrete(expand = c(0, 0)) +
    coord_cartesian(xlim = c(1.1, NA)) +
    theme_forest_labels()

  return(p)
}

#' @export
forest_plot_measures <- function(
    data, label_est = "GEN [95% CI]", digits = 2, ...) {
  p <- data |>
    mutate(trial = row_number()) |>
    mutate(across(
      c(est, lcl, ucl),
      \(x) formatC(x, digits = digits, format = "f")
    )) |>
    mutate(
      label = str_glue("{est} [{lcl}; {ucl}]")
    ) |>
    ggplot(aes(label_est, -trial)) +
    geom_text(aes(label = label), hjust = 0.5, size = 4.5) +
    scale_x_discrete(expand = c(0, 0), position = "top") +
    theme_forest_labels()

  return(p)
}

#' @export
forest_plot_main <- function(
    data, label_est = "GEN [95% CI]",
    back_transform = FALSE, label_overall = "Overall") {
  diamond <- data |>
    mutate(trial = row_number()) |>
    generate_diamond(label_overall)

  x_line <- 0
  if (back_transform) {
    x_line <- 1
  }

  p <- data |>
    mutate(trial = row_number()) |>
    mutate(is_overall = str_detect(study, fixed(label_overall))) |>
    ggplot(aes(est, -trial, color = is_overall)) +
    geom_vline(xintercept = x_line, linewidth = 0.25) +
    geom_point(shape = "|", size = 2.5) +
    geom_point(
      aes(size = sqrt(weight)),
      shape = 15, alpha = 0.5, color = "grey45", show.legend = FALSE
    ) +
    geom_errorbarh(
      aes(xmin = lcl, xmax = ucl),
      height = 0, linewidth = 0.25
    ) +
    geom_polygon(aes(
      x = x__, y = -trial + y__, group = trial
    ), data = diamond, fill = "#002B52", inherit.aes = FALSE) +
    scale_x_continuous(n.breaks = 8) +
    scale_color_manual(values = c("black", "white")) +
    labs(x = label_est) +
    theme_forest()

  if (back_transform) {
    p <- p +
      scale_x_continuous(n.breaks = 8, transform = "log10") +
      annotation_logticks(sides = "b")
  }

  return(p)
}

#' @export
plot_metareg <- function(
    data_ma, method = "DL", exponentiate = FALSE, back_transform = FALSE,
    label_est = "GEN [95% CI]", label_covar = "covariate",
    ...) {
  fit <- rma(yi, vi, mods = ~covar, data = data_ma, method = method)

  data_ma <- data_ma |> mutate(z = qnorm(0.975))

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

  data_ma <- data_ma |> mutate(
    pred = yi, ci.lb = pred - (z * sqrt(vi)), ci.ub = pred + (z * sqrt(vi)),
    weight = 1 / vi
  )
  covar <- seq(
    min(data_ma$covar, na.rm = TRUE),
    max(data_ma$covar, na.rm = TRUE),
    length.out = 40
  )

  nd <- predict(fit, newmods = seq(
    min(data_ma$covar, na.rm = TRUE),
    max(data_ma$covar, na.rm = TRUE),
    length.out = 40
  )) |>
    as_tibble() |>
    mutate(covar = covar)

  if (exponentiate) {
    nd <- nd |>
      mutate(across(
        c(pred, ci.lb, ci.ub), \(x) exp(x)
      ))
  }

  p <- data_ma |>
    ggplot(aes(covar, pred)) +
    geom_ribbon(
      aes(ymin = ci.lb, ymax = ci.ub, group = 1),
      fill = "#002B52", alpha = 0.2, data = nd
    ) +
    geom_line(
      aes(group = 1),
      color = "#002B52", linewidth = 0.8,
      data = nd
    ) +
    geom_point(shape = "–", size = 2.5) +
    geom_point(
      aes(size = sqrt(weight)),
      shape = 15, alpha = 0.5, color = "grey45", show.legend = FALSE
    ) +
    geom_errorbar(
      aes(ymin = ci.lb, ymax = ci.ub),
      width = 0, linewidth = 0.25
    ) +
    scale_y_continuous(
      name = label_est, n.breaks = 8
    ) +
    scale_x_continuous(
      name = label_covar, n.breaks = 8
    ) +
    theme_bw(base_size = 14, base_family = "Arimo") +
    theme(
      axis.text = element_text(color = "black", size = 12)
    )

  if (back_transform) {
    p <- p +
      scale_y_continuous(
        name = label_est, n.breaks = 8, transform = "log10"
        # labels = as_labeller(\(x) str_remove(x, "\\.0+$"))
      ) +
      annotation_logticks(sides = "l")
  }

  return(p)
}
