#' @export
effect_measures_labels <- list(
  "ru" = list(
    "MD" = c("abbr" = "MD", "full" = "Разница средних"),
    "SMD" = c("abbr" = "SMD", "full" = "Стандартизованная разница средних"),
    "MEAN" = c("abbr" = "Среднее", "full" = "Разница средних"),
    "CORR" = c("abbr" = "r", "full" = "Коэффициент корреляции Пирсона"),
    "BETA" = c("abbr" = "β", "full" = "Угловой регрессионный коэффициент"),
    "HR" = c("abbr" = "HR", "full" = "Отношение рисков"),
    "PROP" = c("abbr" = "Риск", "full" = "Частота наступления событий"),
    "RD" = c("abbr" = "Разница рисков", "full" = "Разница рисков"),
    "OR" = c("abbr" = "ОШ", "full" = "Отношение шансов"),
    "RR" = c("abbr" = "ОР", "full" = "Относительный риск"),
    "IR" = c("abbr" = "IR", "full" = "Плотность инцидентности"),
    "IRD" = c("abbr" = "IRD", "full" = "Разница плотностей инцидентности"),
    "IRR" = c("abbr" = "IRR", "full" = "Отношение плотностей инцидентности"),
    "GEN" = c("abbr" = "Мера эффекта", "full" = "Мера эффекта")
  )
)

#' @export
funnel_plot_labels <- list(
  "ru" = list(
    "label_y" = "Стандартная ошибка"
  ),
  "en" = list(
    "label_y" = "Standard error"
  )
)

#' @export
overall_label <- list(
  "ru" = "Объединенная оценка",
  "en" = "Overall"
)

#' @export
ci_label <- list(
  "ru" = "95% ДИ",
  "en" = "95% CI"
)

#' @export
study_label <- list(
  "ru" = "Исследование",
  "en" = "Study"
)

#' @export
group_label <- list(
  "ru" = "Группа",
  "en" = "Group"
)

#' @export
weight_label <- list(
  "ru" = "Вес",
  "en" = "Weight"
)
