suppressPackageStartupMessages({
  library(tidyverse)
  library(metafor)
  library(patchwork)
})

# styler: off
box::use(
  modules/proc_data[...],
  modules/meta_analysis[...],
  modules/plotting[...],
  modules/reporting[...],
)
# styler: on

data <- read_csv("test_data/1mean_messy.csv")
data <- data |>
  mutate(
    trial = row_number(),
    group = c(rep(LETTERS[1:2], 3), NA) |> fct_rev(),
    covar = rnorm(nrow(data))
  )

data_ma <- make_data_1mean(data)
ma_res <- perform_generic_ma(
  data_ma,
  measure = "MD", method = "EE"
)

compute_I_squared(data_ma)
egger_test(data_ma)
# funnel_plot_overall(data_ma, measure = "MD")

aa <- get_ma_table(data_ma, ma_res, label_overall = "Overall") |>
  forest_plot_labels()

bb <- get_ma_table(data_ma, ma_res, label_overall = "Overall") |>
  forest_plot_measures(label_est = "MD [95% CI]", digits = 1)

cc <- get_ma_table(data_ma, ma_res, label_overall = "Overall") |>
  forest_plot_main(back_transform = FALSE, label_est = "MD [95% CI]")

(aa + bb + cc) +
  plot_layout(widths = c(0.75, 0.5, 2))

perform_metareg(data_ma)

plot_metareg(data_ma, label_est = "MD", label_covar = "adsfa")
