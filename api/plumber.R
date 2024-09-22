suppressPackageStartupMessages({
  library(plumber)
  library(tidyverse)
  library(patchwork)
  library(kableExtra)
})

set.seed(42)

# styler: off
box::use(
  ./modules/meta_utils[relative_measures],
  ./modules/main[...],
  ./modules/plotting[...],
  ./modules/reporting[...],
  ./modules/dict[...],
)
# styler: on

#* @param data TEMP: relative path to test data
#* @param measure
#* @param method
#* @param exponentiate Should relative measures be exponentiated
#* @param ignore_covariates Should groups and covariates be ignored if they are present
#* @param lang Select language ("ru", "en")
#* @post /perform_meta_workflow
function(
    data = "./test_data/2props_messy.csv",
    measure = "OR", method = "DL",
    exponentiate = TRUE,
    ignore_covariates = FALSE,
    lang = "ru") {
  exponentiate <- exponentiate == "true"
  ignore_covariates <- ignore_covariates == "true"
  # loading data
  data <- read_csv(data)
  data <- data |>
    # select(-c(group, covar)) |>
    mutate(
      trial = row_number(),
      group = c(rep(LETTERS[1:2], 6), NA) |> fct_rev(),
      covar = rnorm(nrow(data))
    )

  if (ignore_covariates & ("covar" %in% names(data))) {
    data$covar <- NULL
  }
  if (ignore_covariates & ("group" %in% names(data))) {
    data$group <- NULL
  }

  res <- perform_ma(
    data, measure, method,
    skew_test = FALSE, exponentiate = exponentiate, use_t = TRUE
  )

  return(res)
}

#* @param data TEMP: relative path to test data
#* @param measure
#* @param method
#* @param digits How many decimal points needed
#* @param pvalue_digits How many decimal points needed for p-value
#* @param exponentiate Should relative measures be exponentiated
#* @param ignore_covariates Should groups and covariates be ignored if they are present
#* @param lang Select language ("ru", "en")
#* @post /get_ma_table
function(
    data = "./test_data/2props_messy.csv",
    measure = "OR", method = "DL",
    digits = 2, pvalue_digits = 3,
    exponentiate = TRUE,
    ignore_covariates = FALSE,
    lang = "ru") {
  digits <- as.integer(digits)
  pvalue_digits <- as.integer(pvalue_digits)
  exponentiate <- exponentiate == "true"
  ignore_covariates <- ignore_covariates == "true"
  if (lang == "ru") {
    decimal_mark <- ","
  } else {
    decimal_mark <- "."
  }

  # loading data
  data <- read_csv(data)
  data <- data |>
    # select(-c(group, covar)) |>
    mutate(
      trial = row_number(),
      group = c(rep(LETTERS[1:2], 6), NA) |> fct_rev(),
      covar = rnorm(nrow(data))
    )

  if ("covar" %in% names(data)) {
    data$covar <- NULL
  }
  if (ignore_covariates & ("group" %in% names(data))) {
    data$group <- NULL
  }

  res <- perform_ma(
    data, measure, method,
    skew_test = FALSE, exponentiate = exponentiate, use_t = TRUE
  )

  label_est <- effect_measures_labels[[lang]][[res[["measure"]]]]["abbr"]

  res <- res[["ma_table"]]

  if ("group" %in% names(data) & n_distinct(res$group, na.rm = TRUE) < 2) {
    res$group <- NULL
  }

  if ("group" %in% names(data)) {
    res$group <- if_else(
      res$group == "_Overall", NA, res$group
    )
  }

  res <- res |>
    mutate(study = str_replace(study, "^Overall$", overall_label[[lang]])) |>
    select(any_of(
      c("study", "group", "est", "lcl", "ucl", "pval", "weight")
    )) |>
    mutate(across(
      c(est, lcl, ucl),
      \(x) formatC(x, format = "f", digits = digits, decimal.mark = decimal_mark)
    )) |>
    mutate(
      weight = formatC(weight, format = "f", digits = 1, decimal.mark = decimal_mark),
      pval = scales::pvalue_format(
        accuracy = 10^(-1 * pvalue_digits),
        prefix = c("&lt;", "", "&gt;"), decimal.mark = decimal_mark
      )(pval)
    )

  if ("group" %in% names(res)) {
    col_names <- c(
      study_label[[lang]], group_label[[lang]], label_est,
      ci_label[[lang]], "p", weight_label[[lang]]
    )
  } else {
    col_names <- c(
      study_label[[lang]], label_est,
      ci_label[[lang]], "p", weight_label[[lang]]
    )
  }

  res <- res |>
    unite("ci", c(lcl, ucl), sep = "; ") |>
    set_names(col_names) |>
    kable(format = "html", align = c("l", rep("c", ncol(res) - 1))) |>
    as.character()

  return(res)
}

#* @param data TEMP: relative path to test data
#* @param measure
#* @param method
#* @param digits How many decimal points needed
#* @param exponentiate Should relative measures be exponentiated
#* @param back_transform:bool Applied only for exponentiated measures
#* @param ignore_covariates Should groups and covariates be ignored if they are present
#* @param lang Select language ("ru", "en")
#* @post /forest_plot
#* @serializer png list(width = 12, height = 6, units = "in", res = 200)
function(
    data = "./test_data/2props_messy.csv",
    measure = "OR", method = "DL", digits = 2,
    exponentiate = TRUE, back_transform = TRUE,
    ignore_covariates = FALSE,
    lang = "ru") {
  digits <- as.integer(digits)
  exponentiate <- exponentiate == "true"
  back_transform <- back_transform == "true"
  ignore_covariates <- ignore_covariates == "true"

  # loading data
  data <- read_csv(data)
  data <- data |>
    # select(-c(group, covar)) |>
    mutate(
      trial = row_number(),
      group = c(rep(LETTERS[1:2], 6), NA) |> fct_rev(),
      covar = rnorm(nrow(data))
    )

  if (ignore_covariates & ("covar" %in% names(data))) {
    data$covar <- NULL
  }
  if (ignore_covariates & ("group" %in% names(data))) {
    data$group <- NULL
  }

  res <- perform_ma(
    data, measure, method,
    skew_test = FALSE, exponentiate = exponentiate, use_t = TRUE
  )

  label_est <- effect_measures_labels[[lang]][[res[["measure"]]]]["abbr"]
  if (res[["exponentiate"]] & !back_transform) {
    label_est <- glue::glue(
      "ln({label_est}) [{ci_label[[lang]]}]"
    )
  } else {
    label_est <- glue::glue(
      "{label_est} [{ci_label[[lang]]}]"
    )
  }

  aa <- res[["ma_table"]] |>
    forest_plot_labels()

  bb <- res[["ma_table"]] |>
    forest_plot_measures(
      label_est = label_est, digits = digits
    )

  cc <- res[["ma_table"]] |>
    forest_plot_main(
      label_est = label_est,
      label_overall = overall_label[[lang]],
      back_transform = res[["exponentiate"]] & back_transform
    )

  p <- (aa + bb + cc) +
    plot_layout(widths = c(0.75, 0.5, 2))

  print(p)
}

#* @param data TEMP: relative path to test data
#* @param measure
#* @param lang Select language ("ru", "en")
#* @post /funnel_plot
#* @serializer png list(width = 6, height = 6, units = "in", res = 200)
function(
    data = "./test_data/2props_messy.csv",
    measure = "OR",
    lang = "ru") {
  # loading data
  data <- read_csv(data)
  data <- data |>
    mutate(
      trial = row_number(),
      group = c(rep(LETTERS[1:2], 6), NA) |> fct_rev(),
      covar = rnorm(nrow(data))
    )

  if ("covar" %in% names(data)) {
    data$covar <- NULL
  }
  if ("group" %in% names(data)) {
    data$group <- NULL
  }

  res <- perform_ma(
    data, measure,
    method = "EE",
    skew_test = FALSE, exponentiate = FALSE, use_t = TRUE
  )

  label_est <- effect_measures_labels[[lang]][[res[["measure"]]]]["abbr"]
  if (res[["measure"]] %in% relative_measures) {
    label_est <- glue::glue("ln({label_est})")
  } else {
    label_est <- glue::glue("{label_est}")
  }

  p <- funnel_plot_overall(
    res[["data_ma"]], measure,
    label_y = funnel_plot_labels[[lang]][["label_y"]],
    label_x = label_est
  )

  print(p)
}

#* @param data TEMP: relative path to test data
#* @param measure
#* @param method
#* @param exponentiate Should relative measures be exponentiated
#* @param back_transform:bool Applied only for exponentiated measures
#* @param lang Select language ("ru", "en")
#* @post /metareg_plot
#* @serializer png list(width = 8, height = 6, units = "in", res = 200)
function(
    data = "./test_data/2props_messy.csv",
    measure = "OR", method = "DL",
    exponentiate = TRUE, back_transform = TRUE,
    label_covar = "predictor",
    lang = "ru") {
  exponentiate <- exponentiate == "true"
  back_transform <- back_transform == "true"

  # loading data
  data <- read_csv(data)
  data <- data |>
    # select(-c(group, covar)) |>
    mutate(
      trial = row_number(),
      group = c(rep(LETTERS[1:2], 6), NA) |> fct_rev(),
      covar = rnorm(nrow(data))
    )

  if ("group" %in% names(data)) {
    data$group <- NULL
  }

  res <- perform_ma(
    data, measure, method,
    skew_test = FALSE, exponentiate = exponentiate, use_t = TRUE
  )

  label_est <- effect_measures_labels[[lang]][[res[["measure"]]]]["abbr"]
  if (res[["exponentiate"]] & !back_transform) {
    label_est <- glue::glue(
      "ln({label_est}) [{ci_label[[lang]]}]"
    )
  } else {
    label_est <- glue::glue(
      "{label_est} [{ci_label[[lang]]}]"
    )
  }

  p <- plot_metareg(
    res[["data_ma"]],
    method = method, measure = measure,
    use_t = TRUE, exponentiate = res[["exponentiate"]],
    back_transform = res[["exponentiate"]] & back_transform,
    label_est = label_est, label_covar = label_covar
  )

  print(p)
}
