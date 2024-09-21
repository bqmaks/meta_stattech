suppressPackageStartupMessages({
  library(plumber)
  library(tidyverse)
  library(patchwork)
})

# styler: off
box::use(
  ./modules/proc_data[...],
  ./modules/meta_analysis[...],
  ./modules/plotting[...],
  ./modules/reporting[...],
)
# styler: on

#* @param data TEMP: relative path to test data
#* @param measure TEMP: defined by test data type
#* @param method TEMP: EE, DL, PETO (for OR only) and MH (for OR, RR, RD", IRR)
#* @param exponentiate Should relative measures be exponentiated
#* @param digits How many decimal points needed
#* @param back_transform Applied only for exponentiated measures
#* @param ignore_covariates Should groups and covariates be ignored if they are present
#* @param lang Select language ("ru", "en")
#* @post /perform_meta_workflow
function(
    data = "./test_data/2props_messy.csv",
    measure = "OR", method = "DL", digits = 2,
    exponentiate = FALSE, back_transform = FALSE,
    ignore_covariates = FALSE,
    lang = "ru") {

}
