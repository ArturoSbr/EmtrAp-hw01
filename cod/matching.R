# Ambiente --------------------------------------------------------------------------------------------------------

# Librerías
library(dplyr)
library(MatchIt)

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

# t$
tau <- t %>% filter(T_nap == 1) %>% pull(y_bar) - t %>% filter(T_nap == 0) %>% pull(y_bar)
tau / sqrt(sum(t$s2 / t$y_bar))


# Q2 --------------------------------------------------------------------------------------------------------------

# KNN = 1
match_1nn <- matchit(formula = T_nap ~ female_ + age_ + sleep_report, data = d1, method = 'nearest',
                     ratio = 1, distance = 'mahalanobis', estimand = 'ATT')

# Tau K = 1
match.data(match_1nn) %>% 
  group_by(T_nap) %>% 
  summarise(y_bar = mean(productivity)) %>% 
  mutate(tau = y_bar - lag(y_bar))

# KNN = 5
match_5nn <- matchit(formula = T_nap ~ female_ + age_ + sleep_report, data = d1,
                     method = 'nearest', ratio = 5, distance = 'mahalanobis', estimand = 'ATT')

# Tau K = 5
match.data(match_5nn) %>% 
  group_by(T_nap) %>% 
  summarise(y_bar = mean(productivity)) %>% 
  mutate(tau = y_bar - lag(y_bar))

# KNN = 10
match_10nn <- matchit(formula = T_nap ~ female_ + age_ + sleep_report, data = d1,
                      method = 'nearest', ratio = 10, distance = 'mahalanobis', estimand = 'ATT')

# Tau K = 10
match.data(match_10nn) %>% 
  group_by(T_nap) %>% 
  summarise(y_bar = mean(productivity)) %>% 
  mutate(tau = y_bar - lag(y_bar))

# Q3 --------------------------------------------------------------------------------------------------------------

# 
