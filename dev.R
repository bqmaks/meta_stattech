suppressPackageStartupMessages({
  library(tidyverse)
  library(metafor)
  library(patchwork)
})

# styler: off
box::use(
  api/modules/proc_data[...],
  api/modules/meta_analysis[...],
  api/modules/plotting[...],
  api/modules/reporting[...],
)
# styler: on

data <- read_csv("api/test_data/2props_messy.csv")
data <- data |>
  mutate(
    trial = row_number(),
    group = c(rep(LETTERS[1:4], 3), NA) |> fct_rev(),
    covar = rnorm(nrow(data))
  )

data_ma <- make_data_2prop_relative(data)
ma_res <- perform_generic_ma(
  data_ma,
  measure = "OR", method = "DL"
)

compute_I_squared(data_ma)
egger_test(data_ma)
# funnel_plot_overall(data_ma, measure = "MD")

dd <- get_ma_table(data_ma, ma_res, label_overall = "Overall", exponentiate = TRUE)

aa <- dd |>
  forest_plot_labels()

bb <- dd |>
  forest_plot_measures(label_est = "OR [95% CI]", digits = 2)

cc <- dd |>
  forest_plot_main(label_est = "OR [95% CI]", back_transform = TRUE, label_overall = "ov")

(aa + bb + cc) +
  plot_layout(widths = c(0.75, 0.5, 2))

perform_metareg(data_ma)

plot_metareg(data_ma, exponentiate = TRUE, back_transform = TRUE)
