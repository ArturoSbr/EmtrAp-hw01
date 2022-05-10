# Ambiente --------------------------------------------------------------------------------------------------------

# Librerías
library(dplyr)
library(MatchIt)
library(stargazer)

# Leer datos
d1 <- read.csv('../dat/baseline.csv')
d2 <- read.csv('../dat/endline.csv')
d3 <- read.csv('../dat/completa.csv')

# Eliminar atritors
d1 <- d1 %>%
  filter(drop_indicator == 0)

d2 <- d2 %>% 
  filter(drop_indicator == 0)


# Q1 --------------------------------------------------------------------------------------------------------------

# Inicializar match
match_cem <- matchit(formula = T_nap ~ female_ + education_, data = d2, method = 'cem', estimand = 'ATE')
match_cem %>% summary()

# Matchear data
match_cem_fit <- match.data(match_cem)

# Calcular Neyman
t <- match_cem_fit %>% 
  group_by(T_nap) %>% 
  summarize(y_bar = mean(productivity),
            n = n(),
            s2 = sd(productivity))

# ATE
(tau <- t %>% filter(T_nap == 1) %>% pull(y_bar) - t %>% filter(T_nap == 0) %>% pull(y_bar))

tau / sqrt((t$s2 / t$n) %>% sum())

# Q2 --------------------------------------------------------------------------------------------------------------

# Unir baseline a endline
t <- merge(d2 %>% select(pid, productivity),
           d1 %>% select(pid, T_nap, female_, age_, sleep_report),
           by = 'pid')

# KNN = 1
match_1nn <- matchit(formula = T_nap ~ female_ + age_ + sleep_report, data = t, method = 'nearest',
                     ratio = 1, distance = 'mahalanobis', estimand = 'ATT', replace = TRUE)

# Tau K = 1
match.data(match_1nn) %>% 
  group_by(T_nap) %>% 
  summarise(y_bar = mean(productivity)) %>% 
  mutate(tau = y_bar - lag(y_bar)) %>% 
  pull(tau)

# KNN = 5
match_5nn <- matchit(formula = T_nap ~ female_ + age_ + sleep_report, data = t, method = 'nearest',
                     ratio = 5, distance = 'mahalanobis', estimand = 'ATT', replace = TRUE)

# Tau K = 5
match.data(match_5nn) %>% 
  group_by(T_nap) %>% 
  summarise(y_bar = mean(productivity)) %>% 
  mutate(tau = y_bar - lag(y_bar)) %>% 
  pull(tau)

# KNN = 10
match_10nn <- matchit(formula = T_nap ~ female_ + age_ + sleep_report, data = t, method = 'nearest',
                      ratio = 10, distance = 'mahalanobis', estimand = 'ATT', replace = TRUE)

# Tau K = 10
match.data(match_10nn) %>% 
  group_by(T_nap) %>% 
  summarise(y_bar = mean(productivity)) %>% 
  mutate(tau = y_bar - lag(y_bar)) %>% 
  pull(tau)

# Q3 --------------------------------------------------------------------------------------------------------------

# Crear variables baseline, outcomes endline
df <- merge(
  d1 %>% select(pid, T_nap, time_in_office, age_, female_, education_, sleep_report, no_of_children_,
                act_inbed, an_12_number_of_awakenings, an_13_average_awakening_length, unemployed),
  d2 %>% select(pid, productivity),
  by = 'pid'
)

### a) PSM por MPL, Probit y Logit

# Ajustar  modelos
m1 <- lm(formula = T_nap ~ age_ + female_ + education_ + sleep_report + no_of_children_, data = df)
m2 <- glm(formula = T_nap ~ age_ + female_ + education_ + sleep_report + no_of_children_,
          family = binomial(link = probit), data = df)
m3 <- glm(formula = T_nap ~ age_ + female_ + education_ + sleep_report + no_of_children_,
          family = binomial(link = logit), data = df)

# Resultados a tabla
stargazer(m1, m2, m3, type = "text", covariate.labels = c('age','female','education','sleep report','children'))
# stargazer(m1, m2, m3, covariate.labels = c('age','female','education','sleep report','children'))

### b) Tabla de PSM mean y sd

# Asignar scores por m1, m2 y m3
df <- df %>%
  mutate(
    mpl = m1$fitted.values,
    prb = m2$fitted.values,
    lgt = m3$fitted.values
  )

# Match por mpl, prb, lgt
match_mpl <- matchit(formula = T_nap ~ mpl, data = df, method = 'nearest', distance = 'mahalanobis', replace = TRUE)
match_prb <- matchit(formula = T_nap ~ age_ + female_ + education_ + sleep_report + no_of_children_,
                     data = df, method = 'nearest', distance = 'glm', link = 'probit', replace = TRUE)
match_lgt <- matchit(formula = T_nap ~ age_ + female_ + education_ + sleep_report + no_of_children_,
                     data = df, method = 'nearest', distance = 'glm', link = 'logit', replace = TRUE)

# 
t <- df %>%
  select(mpl, prb, lgt) %>% 
  mutate(
    mpl_w = match_mpl$weights,
    prb_w = match_prb$weights,
    lgt_w = match_lgt$weights
  )

rbind(t %>% colMeans(), apply(X = t, MARGIN = 2, FUN = sd)) %>% 
  data.frame() %>% 
  mutate(fun = c('Promedio', 'Desv. Est.')) %>% 
  select(fun, mpl, mpl_w, prb, prb_w, lgt, lgt_w) %>% 
  stargazer(summary = FALSE, label='t2.3_summary')

### c) Tabla de balance
t <- match.data(match_prb)
X <- c('time_in_office','age_','female_','education_','sleep_report','no_of_children_','act_inbed',
       'an_12_number_of_awakenings','an_13_average_awakening_length','unemployed')
t_stats <- c()
p_vals <- c()
var <- c()
for (x in X) {
  test <- t.test(
    t %>% filter(T_nap == 1) %>% select(x),
    t %>% filter(T_nap == 0) %>% select(x)
  )
  var <- c(var, x)
  t_stats <- c(t_stats, test$statistic)
  p_vals <- c(p_vals, test$p.value)
}

t <- data.frame(Covariable = var, t = t_stats, p = p_vals) %>% 
  arrange(Covariable)

t %>% stargazer(title = 'Balance sobre 10 covariables', label = 't2.3_balance', summary = FALSE, rownames = FALSE)

### d) Resultados Probit sobre todas las variables
