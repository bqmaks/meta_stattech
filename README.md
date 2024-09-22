# meta_stattech

### Общие столбцы в таблице

- "author" – Авторы (обязательный столбец)
- "year" – Год публикации (обязательный столбец)
- "suffix" – Необязательный суффикс для формирования названия
- "group" – Группа для внутригруппового МА
- "covar" – Количественная ковариата для мета-регрессии

Префикс "t_" – в основной группе, "с_" – в контрольной группе

### Меры эффекта / оценки

- "MD" – raw mean difference 
- "SMD" – standardized mean difference

Минимум (не обязательный), 1-ый квартиль, медиана, 3-ий квартиль, максимум (не обязательный):

для указанных столбов есть опция `skew_test` (по умолчанию `FALSE`)

"t_min", "t_q1", "t_median", "t_q3", "t_max"

"c_min", "c_q1", "c_median", "c_q3", "c_max"

Среднее, стандратное отклонение, размер выборки:

"t_mean", "t_sd", "t_n"

"c_mean", "c_sd", "c_n"

Стандартная ошибка среднего или нижняя и верхняя границы 95% ДИ для среднего в группах

"t_se", "t_lcl", "t_ucl"

"c_se", "c_lcl", "c_ucl"

Разница средних, ее стандратная ошибка или границы 95% ДИ

"md", "md_se", "lcl", "ucl"

- "MEAN" – raw mean

Минимум (не обязательный), 1-ый квартиль, медиана, 3-ий квартиль, максимум (не обязательный):

для указанных столбов есть опция `skew_test` (по умолчанию `FALSE`)

"t_min", "t_q1", "t_median", "t_q3", "t_max"

Среднее, стандратное отклонение, размер выборки:

"t_mean", "t_sd", "t_n"

Стандартная ошибка среднего или нижняя и верхняя границы 95% ДИ для среднего

"t_se", "t_lcl", "t_ucl"

- "OR" – odds ratio
- "RR" – relative risk / risk ratio

Количество событий и размер выборки в группах

t_pos", "t_total"

"c_pos", "c_total"

Логарифм ОШ или ОР и стандратная ошибка

"log_rm", "log_rm_se"

ОШ или ОР и границы 95% ДИ

"rm", "lcl", "ucl"

- "RD" – risk difference

Количество событий и размер выборки в группах

"t_pos", "t_total"

"c_pos" "c_total"

RD и стандартная ошибка или границы 95% ДИ

"m", "se", "lcl", "ucl"

- "PROP" – binomial proportion

Количество событий и размер выборки

"t_pos" , "t_total"

Биномиальная пропорция и стандартная ошибка или границы 95% ДИ

"prop", "se", "lcl", "ucl"

- "IRR" – incidence rate ratio

Количество событий и время под наблюдением в группах

"t_pos", "t_time"

"c_pos", "c_time"

Логарифм IRR и стандартная ошибка

"log_rm", "log_rm_se"

IRR и границы 95% ДИ

"rm", "lcl", "ucl"

- "IRD" – incidence rate difference

Количество событий и время под наблюдением в группах

"t_pos", "t_time"

"c_pos", "c_time"

IRD и стандратная ошибка или границы 95% ДИ

"m", "se", "lcl", "ucl"

- "IR" – incidence rate

Количество событий и время под наблюдением

"t_pos", "t_time",

IR и стандартная ошибка или границ 95% ДИ

"prop", "se", "lcl", "ucl"

- "CORR" – Pearson correlation coefficient
- "BETA" – Regression slope

Мера эффекта, размер выобрки (не обязательно) и стандарная ошибка или границы 95% ДИ

"m", "n", "se", "lcl", "ucl"

- "HR" – Hazard ration

Логарифм HR и стандартная ошибка

"log_rm" , "se"

HR и границы 95% ДИ

"rm", "lcl", "ucl"

### Возможные модели/методы мета-анализа:

#### Общие (generic) методы:
Метод "I2" – выбор метода определяется на основе статистики I²: если I² > 50% используется модель "EE", иначе используется "DL"

- "EE" – equal/fixed effect
- "FE" – fixed effectS

Указанные ниже методы также применимы для мета-регрессии и модификаторов (ANOVA, множественные попарные сравнения)

- "DL" – DerSimonian-Laird (метод по умолчанию)
- "REML" – restricted maximum likelihood
- "HE" – Hedges
- "HS" – Hunter-Schmidt
- "HSk" – Hunter-Schmidt with a small sample-size correction
- "SJ" – Sidik-Jonkman
- "ML" – maximum likelihood
- "EB" – empirical Bayes
- "PM" – Paule-Mandel
- "GENQ" – generalized Q-statistic
- "PMM" – median-unbiased Paule-Mandel
- "GENQM" – median-unbiased generalized Q-statistic

#### Методы, применимые для некоторых мер эффекта
- "PETO" – Peto's method (для "OR")
- "MH" – Mantel-Haenszel method (для "OR", "RR", "RD", "IRR" и "IRD")


