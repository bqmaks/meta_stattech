suppressPackageStartupMessages({
  library(tidyverse)
  library(metafor)
  library(patchwork)
})

# styler: off
box::use(
  api/modules/meta_utils[...],
  api/modules/proc_data[...],
  api/modules/meta_analysis[...],
  api/modules/plotting[...],
  api/modules/reporting[...],
  api/modules/dict[...],
  api/modules/main[...],
)
# styler: on

data <- read_csv("api/test_data/2props_messy.csv")
data <- data |>
  # select(-c(group, covar)) |>
  mutate(
    trial = row_number(),
    group = c(rep(LETTERS[1:2], 6), NA) |> fct_rev(),
    covar = rnorm(nrow(data))
  )

res <- perform_ma(
  data,
  measure = "OR", method = "MH",
  skew_test = FALSE, exponentiate = TRUE, use_t = TRUE
)



data_ma <- make_data_2prop_relative(data)
ma_res <- perform_generic_ma(
  data_ma,
  measure = "OR", method = "DL"
)

plot_metareg(data_ma)

perform_anova(data_ma)
egger_test(data_ma)
perform_metareg(data_ma)
compute_I_squared(data_ma) |>
  unlist() |>
  max()
egger_test(data_ma)
# funnel_plot_overall(data_ma, measure = "MD")

dd <- get_ma_table(data_ma, ma_res, label_overall = "Overall", exponentiate = TRUE)

res[["ma_table"]]
res[["ma_res"]]

aa <- dd |>
  forest_plot_labels()

bb <- dd |>
  forest_plot_measures(label_est = "OR [95% CI]", digits = 2)

cc <- dd |>
  forest_plot_main(label_est = "OR [95% CI]", back_transform = TRUE, label_overall = "ov")

(aa + bb + cc) +
  plot_layout(widths = c(0.75, 0.5, 2))

names(res)
res[["exponentiate"]]

perform_metareg(data_ma)

plot_metareg(data_ma, exponentiate = TRUE, back_transform = TRUE)


res <- list(
  egger_test_result = NA,
  publication_bias_description = NA,
  funnel_plot = NA,
  ma_result = NA,
  ma_table_raw = NA,
  ma_table_html = NA,
  ma_methods_description = NA,
  ma_results_description = NA,
  forest_plot = NA,
  meta_regression = NA,
  meta_regression_plot = NA,
  meta_regression_description = NA,
  meta_anova_result = NA,
  meta_pairwise_result = NA,
  meta_group_description = NA
)
