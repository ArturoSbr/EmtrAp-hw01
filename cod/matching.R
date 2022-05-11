# Ambiente --------------------------------------------------------------------------------------------------------

# Librerías
library(dplyr)
library(MatchIt)
library(stargazer)

# Leer datos
d1 <- read.csv('../dat/baseline.csv')
d2 <- read.csv('../dat/endline.csv')

# Eliminar atritors
d1 <- d1 %>%
  filter(drop_indicator == 0)

d2 <- d2 %>% 
  filter(drop_indicator == 0)

# Declarar cog
d2$cog <- d2 %>% 
  mutate(pvt_measure = (pvt_measure - mean(pvt_measure)) / sd(pvt_measure),
         hf_measure = (hf_measure - mean(hf_measure)) / sd(hf_measure),
         corsi_measure = (corsi_measure - mean(corsi_measure)) / sd(corsi_measure)) %>% 
  select(pvt_measure, hf_measure, corsi_measure) %>% 
  rowMeans()

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
  d2 %>% select(pid, productivity, nap_time_mins, sleep_report, happy, cog, typing_time_hr),
  by = 'pid'
)

### a) PSM por MPL, Probit y Logit

# Ajustar  modelos
m1 <- lm(formula = T_nap ~ age_ + female_ + education_ + sleep_report.x + no_of_children_, data = df)
m2 <- glm(formula = T_nap ~ age_ + female_ + education_ + sleep_report.x + no_of_children_,
          family = binomial(link = probit), data = df)
m3 <- glm(formula = T_nap ~ age_ + female_ + education_ + sleep_report.x + no_of_children_,
          family = binomial(link = logit), data = df)

# Resultados a tabla
stargazer(m1, m2, m3, type = "text", covariate.labels = c('age','female','education','sleep report','children'))
# stargazer(m1, m2, m3, covariate.labels = c('age','female','education','sleep report','children'))

### b) Tabla de PSM mean y sd

# Match por mpl, prb, lgt
match_mpl <- matchit(formula = T_nap ~ age_ + female_ + education_ + sleep_report.x + no_of_children_,
                     data = df, method = 'nearest', distance = m1$fitted.values, replace = TRUE)
match_prb <- matchit(formula = T_nap ~ age_ + female_ + education_ + sleep_report.x + no_of_children_,
                     data = df, method = 'nearest', distance = 'glm', link = 'probit', replace = TRUE)
match_lgt <- matchit(formula = T_nap ~ age_ + female_ + education_ + sleep_report.x + no_of_children_,
                     data = df, method = 'nearest', distance = 'glm', link = 'logit', replace = TRUE)

# ATT con MPL
match.data(match_mpl) %>% 
  group_by(T_nap) %>% 
  summarise(y_bar = mean(productivity)) %>% 
  mutate(tau = y_bar - lag(y_bar)) %>% 
  pull(tau)

# ATT con Probit
match.data(match_prb) %>% 
  group_by(T_nap) %>% 
  summarise(y_bar = mean(productivity)) %>% 
  mutate(tau = y_bar - lag(y_bar)) %>% 
  pull(tau)

# ATT con Logit
match.data(match_lgt) %>% 
  group_by(T_nap) %>% 
  summarise(y_bar = mean(productivity)) %>% 
  mutate(tau = y_bar - lag(y_bar)) %>% 
  pull(tau)

# Tabla de scores promedio y sdev
data.frame(T_nap = d2$T_nap, mpl = m1$fitted.values, mpl_w = match_mpl$weights,
           prb = m2$fitted.values, prb_w = match_prb$weights,
           lgt = m3$fitted.values, lgt_w = match_lgt$weights) %>% 
  group_by(T_nap) %>% 
  summarise(mpl_bar = mpl %>% mean(),
            mpl_std = mpl %>% sd(),
            prb_bar = prb %>% mean(),
            prb_std = prb %>% sd(),
            lgt_bar = lgt %>% mean(),
            lgt_std = lgt %>% sd()) %>% 
  round(3) %>% 
  stargazer(title = 'PSM promedio y desviación estándar', label = 't2.3_summary',
            summary = FALSE, rownames = FALSE)

### c) Tabla de balance
t <- match.data(match_prb)
X <- c('time_in_office','age_','female_','education_','sleep_report.y','no_of_children_','act_inbed',
       'an_12_number_of_awakenings','an_13_average_awakening_length','unemployed')
t_stats <- c()
p_vals <- c()
var <- c()
for (x in X) {
  test <- t.test(
    t %>% filter(T_nap == 1) %>% select(!!sym(x)),
    t %>% filter(T_nap == 0) %>% select(!!sym(x))
  )
  var <- c(var, x)
  t_stats <- c(t_stats, test$statistic)
  p_vals <- c(p_vals, test$p.value)
}

t <- data.frame(Covariable = var, t = t_stats, p = p_vals) %>% 
  arrange(Covariable)

t %>% stargazer(title = 'Balance sobre 10 covariables', label = 't2.3_balance', summary = FALSE, rownames = FALSE)

### d) Resultados Probit sobre todas las variables
Y <- c('productivity','nap_time_mins','sleep_report.y','happy','cog','typing_time_hr')
taus <- c()
sgms <- c()
for (y in Y) {
  t <- match.data(match_prb) %>% 
    filter(!is.na(!!sym(y))) %>% 
    group_by(T_nap) %>% 
    summarise(y_bar = !!sym(y) %>% mean(),
              s2 = !!sym(y) %>% sd(),
              n = n())
  taus <- c(taus, (t$y_bar - lag(t$y_bar))[2])
  sgms <- c(sgms, sqrt(sum(t$s2 / t$n)))
}

# Resultados a tabla
t <- data.frame(Y = Y, tau = taus,
                sigma = sgms,
                t = taus / sgms,
                p = pnorm((taus / sgms) %>% abs(), lower.tail = FALSE) * 2
           )

# Imprimir latex
t %>% stargazer(summary = FALSE, row.names = FALSE, title = 'ATTs con Probit PSM', label = 't2.3_atts')


match.data(match_prb) %>% 
  filter(!is.na(cog)) %>% 
  group_by(T_nap) %>% 
  summarise(ybar = mean(cog))

d2$cog

