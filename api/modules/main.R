# styler: off
box::use(
  stats[...],
  metafor[...],
  ./meta_utils[...],
  ./proc_data[...],
  ./meta_analysis[...],
  ./plotting[...],
  ./reporting[...],
  ./dict[...],
)
# styler: on

anova <- anova.rma

measures_list <- c(
  "MD", "SMD", "MEAN", "CORR", "BETA", "HR", "PROP", "RD", "OR", "RR", "IR",
  "IRD", "IRR", "GEN"
)

methods_list <- c(
  "I2", "EE", "FE", "PETO", "MH", "DL", "REML",
  "HE", "HS", "HSk", "SJ", "ML", "EB", "PM", "GENQ", "PMM", "GENQM"
)

#' @export
perform_ma <- function(
    data, measure, method = "DL",
    skew_test = FALSE, exponentiate = TRUE, use_t = TRUE, ...) {
  res <- list(
    "measure" = NA,
    "method" = NA,
    "exponentiate" = FALSE,
    "data_ma" = NA,
    "egger_test" = NA,
    "ma_res" = NA,
    "ma_table" = NA,
    "meta_reg" = NA,
    "meta_anova" = NA,
    "meta_pairwise" = NA
  )

  stopifnot(exprs = {
    measure %in% measures_list

    method %in% methods_list
  })

  # Preparing data for MA
  if (measure %in% c("MD", "SMD")) {
    res[["data_ma"]] <- make_data_2means(
      data,
      measure = measure, skew_test = skew_test
    )
  } else if (measure %in% c("MEAN")) {
    res[["data_ma"]] <- make_data_1mean(
      data,
      measure = measure, skew_test = skew_test
    )
  } else if (measure %in% c("CORR", "BETA")) {
    res[["data_ma"]] <- make_data_generic_absolute(
      data,
      measure = measure
    )
  } else if (measure %in% c("HR")) {
    res[["data_ma"]] <- make_data_generic_relative(
      data,
      measure = measure
    )
  } else if (measure %in% c("PROP")) {
    res[["data_ma"]] <- make_data_1prop(
      data,
      measure = measure
    )
  } else if (measure %in% c("RD")) {
    res[["data_ma"]] <- make_data_2prop_absolute(
      data,
      measure = measure
    )
  } else if (measure %in% c("OR", "RR")) {
    res[["data_ma"]] <- make_data_2prop_relative(
      data,
      measure = measure
    )
  } else if (measure %in% c("IR")) {
    res[["data_ma"]] <- make_data_1ir(
      data,
      measure = measure
    )
  } else if (measure %in% c("IRD")) {
    res[["data_ma"]] <- make_data_2ir_absolute(
      data,
      measure = measure
    )
  } else if (measure %in% c("IRR")) {
    res[["data_ma"]] <- make_data_2ir_relative(
      data,
      measure = measure
    )
  }

  # Egger's test
  res[["egger_test"]] <- egger_test(res[["data_ma"]])

  # performing MA
  if (method == "I2") {
    res <- max(
      unlist(compute_I_squared(res[["data_ma"]]))
    )
    if (res > 50) {
      method <- "DL"
    } else {
      method <- "EE"
    }
  }

  if (measure == "OR" & method == "PETO") {
    res[["ma_res"]] <- perform_peto_ma(
      res[["data_ma"]],
      measure = measure
    )
  } else if (measure %in% c("OR", "RR", "RD", "IRR", "IRD") & method == "MH") {
    res[["ma_res"]] <- perform_mh_ma(
      res[["data_ma"]],
      measure = measure
    )
  } else {
    if (method %in% c("PETO", "MH")) {
      method <- "EE"
    }
    res[["ma_res"]] <- perform_generic_ma(
      res[["data_ma"]],
      measure = measure, method = method
    )
  }

  # getting MA table
  if (exponentiate & (measure %in% relative_measures)) {
    res[["exponentiate"]] <- TRUE
  }

  if (use_t & !(measure %in% c("MD", "SMD", "MEAN"))) {
    use_t <- FALSE
  }

  res[["ma_table"]] <- get_ma_table(
    res[["data_ma"]], res[["ma_res"]],
    use_t = use_t, exponentiate = res[["exponentiate"]],
    label_overall = "Overall"
  )

  # performing metaregression
  if (
    !(method %in% c("EE", "PETO", "MH")) &
      ("covar" %in% names(res[["data_ma"]])) &
      sum(!is.na(res[["data_ma"]]$covar)) >= 3
  ) {
    res[["meta_reg"]] <- perform_metareg(
      res[["data_ma"]],
      method = method, measure = measure
    )
  }

  if (
    !(method %in% c("EE", "PETO", "MH")) &
      ("group" %in% names(res[["data_ma"]])) &
      length(table(res[["data_ma"]]$group, useNA = "no")) > 1
  ) {
    res[["meta_anova"]] <- perform_anova(
      res[["data_ma"]],
      method = method, measure = measure
    )

    res[["meta_pairwise"]] <- perform_pairwise_comp(
      res[["data_ma"]],
      method = method, measure = measure
    )
  }

  res[["method"]] <- method
  res[["measure"]] <- measure

  return(res)
}
