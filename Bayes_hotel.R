library(bayesrules)
library(rstanarm)
library(bayesplot)
library(tidyverse)
library(tidybayes)
library(Rcpp)
library(broom.mixed)
library(gt)
library(janitor)


data("hotel_bookings")

hotele <- hotel_bookings %>% 
  select(is_canceled, average_daily_rate, is_repeated_guest, lead_time, previous_cancellations)

# model zmienna lead_time
canceled_model_prior_1 <- stan_glm(is_canceled ~ lead_time,
                                   data = hotele, family = binomial,
                                   prior_intercept = normal(-0.41, 0.4, autoscale = TRUE),
                                   prior = normal(0, 0.2,autoscale = TRUE),
                                   chains = 4, iter = 5000*2, seed = 84735,
                                   prior_PD = TRUE)

prior_summary(canceled_model_prior_1)
prior_summary(canceled_model_prior_1)[[1]]
set.seed(84735)

#prior haos
hotele %>% 
  add_epred_draws(canceled_model_prior_1, ndraws = 100) %>% 
  ggplot(aes(x = lead_time, y = is_canceled)) +
  geom_line(aes(y =.epred , group =.draw ), size = 0.1)

hotele %>% 
  add_predicted_draws(canceled_model_prior_1, ndraws = 100) %>% 
  group_by(.draw) %>% 
  summarize(proportion_canceled = mean(.prediction == 1)) %>% 
  ggplot(aes(x = proportion_canceled)) +
  geom_histogram(color = "white")

canceled_model_1 <- update(canceled_model_prior_1, prior_PD = FALSE)
tidy(canceled_model_1, effects = "fixed", conf.int = TRUE, conf.level = 0.70)

mcmc_trace(canceled_model_1)
mcmc_dens_overlay(canceled_model_1)
mcmc_acf(canceled_model_1)

#poster ładne
hotele %>%
  add_epred_draws(canceled_model_1, ndraws = 100) %>%
  ggplot(aes(x = lead_time, y = is_canceled)) +
  geom_line(aes(y = .epred, group = .draw), alpha = 0.15) + 
  labs(y = "probability of canceled")

hotele %>% 
  add_predicted_draws(canceled_model_1, ndraws = 100) %>% 
  group_by(.draw) %>% 
  summarize(proportion_canceled = mean(.prediction == 1)) %>% 
  ggplot(aes(x = proportion_canceled)) +
  geom_histogram(color = "white")

posterior_interval(canceled_model_1, prob = 0.70)
exp(posterior_interval(canceled_model_1, prob = 0.70))

set.seed(84735)
binary_prediction <- posterior_predict(
  canceled_model_1, newdata = data.frame(lead_time = 200))

head(binary_prediction)

set.seed(84735)
canceled_model_1_df <- as.data.frame(canceled_model_1) %>% 
  mutate(log_odds = `(Intercept)` + lead_time*70,
         odds = exp(log_odds),
         prob = odds / (1 + odds),
         Y = rbinom(20000, size = 1, prob = prob))

head(canceled_model_1_df, 2)

mcmc_hist(binary_prediction) + 
  labs(x = "Y")

ggplot(canceled_model_1_df, aes(x = Y)) + 
  stat_count()

table(binary_prediction)

colMeans(binary_prediction)

proportion_canceled <- function(x){mean(x == 1)}

pp_check(canceled_model_1, nreps = 100,
         plotfun = "stat", stat = "proportion_canceled") + 
  xlab("probability of canceled")

set.seed(84735)
canceled_pred_1 <- posterior_predict(canceled_model_1, newdata = hotele)
dim(canceled_pred_1)

canceled_classifications <- hotele %>% 
  mutate(canceled_prob = colMeans(canceled_pred_1),
         canceled_class_1 = as.numeric(canceled_prob > 0.5)) %>% 
  select(lead_time, canceled_prob, canceled_class_1, is_canceled)

head(canceled_classifications, 6)

canceled_classifications %>% 
  tabyl(is_canceled, canceled_class_1) %>% 
  adorn_totals(c("row", "col"))

czułość<-83/366
czułość
swoistość<-575/634
swoistość

set.seed(84735)
classification_summary(model = canceled_model_1, data = hotele, cutoff = 0.7)

set.seed(84735)
cv_accuracy_1 <- classification_summary_cv(
  model = canceled_model_1, data = hotele, cutoff = 0.7, k = 10)
cv_accuracy_1$cv

#______________Model 3 zmiennych_________________

canceled_model_prior_2 <- stan_glm(
  is_canceled ~ lead_time + average_daily_rate +is_repeated_guest, 
  data = hotele, family = binomial,
  prior_intercept = normal(-0.41, 0.4),
  prior = normal(0, 0.2, autoscale = TRUE), 
  chains = 4, iter = 5000*2, seed = 84735)

prior_summary(canceled_model_prior_2)

canceled_model_2 <- update(canceled_model_prior_2, prior_PD = FALSE)
tidy(canceled_model_2, effects = "fixed", conf.int = TRUE, conf.level = 0.70)

mcmc_trace(canceled_model_2)
mcmc_dens_overlay(canceled_model_2)
mcmc_acf(canceled_model_2)

pp_check(canceled_model_2, nreps = 100,
         plotfun = "stat", stat = "proportion_canceled") + 
  xlab("probability of canceled")

set.seed(84735)
cv_accuracy_2 <- classification_summary_cv(
  model = canceled_model_2, data = hotele, cutoff = 0.7, k = 10)
cv_accuracy_2$cv

#_______________Model 4 rmiennych________________

canceled_model_prior_3 <- stan_glm(
  is_canceled ~ lead_time + average_daily_rate + is_repeated_guest + previous_cancellations, 
  data = hotele, family = binomial,
  prior_intercept = normal(-0.41, 0.4),
  prior = normal(0, 0.2, autoscale = TRUE), 
  chains = 4, iter = 5000*2, seed = 84735)

prior_summary(canceled_model_prior_3)

canceled_model_3 <- update(canceled_model_prior_3, prior_PD = FALSE)
tidy(canceled_model_3, effects = "fixed", conf.int = TRUE, conf.level = 0.70)

mcmc_trace(canceled_model_3)
mcmc_dens_overlay(canceled_model_3)
mcmc_acf(canceled_model_3)

pp_check(canceled_model_3, nreps = 100,
         plotfun = "stat", stat = "proportion_canceled") + 
  xlab("probability of canceled")

set.seed(84735)
cv_accuracy_3 <- classification_summary_cv(
  model = canceled_model_3, data = hotele, cutoff = 0.7, k = 10)
cv_accuracy_3$cv

#_______________________________

loo_1 <- loo(canceled_model_1)
loo_2 <- loo(canceled_model_2)
loo_3 <- loo(canceled_model_3)

loo_compare(loo_1, loo_2, loo_3)


prediction_summary_cv(canceled_model_1,hotele,prob_inner = 0.5,prob_outer = 0.95)

#_______________________________

g1<-pp_check(canceled_model_1)+ggtitle("Model 1")
g2<-pp_check(canceled_model_2)+ggtitle("Model 2")
g3<-pp_check(canceled_model_3)+ggtitle("Model 3")


# Sprawdzenie modelu
if (!is.null(canceled_model_1)) {
  # Jeśli model został wytrenowany, sprawdź, czy zwraca sensowne wyniki
  summary(canceled_model_1)  # Wyświetlenie podsumowania modelu
} else {
  print("Model 'canceled_model_1' nie został wytrenowany lub nie został wczytany poprawnie.")
}

# Sprawdzenie modelu
if (!is.null(canceled_model_2)) {
  # Jeśli model został wytrenowany, sprawdź, czy zwraca sensowne wyniki
  summary(canceled_model_2)  # Wyświetlenie podsumowania modelu
} else {
  print("Model 'canceled_model_1' nie został wytrenowany lub nie został wczytany poprawnie.")
}

# Sprawdzenie modelu
if (!is.null(canceled_model_3)) {
  # Jeśli model został wytrenowany, sprawdź, czy zwraca sensowne wyniki
  summary(canceled_model_3)  # Wyświetlenie podsumowania modelu
} else {
  print("Model 'canceled_model_1' nie został wytrenowany lub nie został wczytany poprawnie.")
}


# Wydrukowanie LOOIC dla każdego modelu
print(loo_1)
print(loo_2)
print(loo_3)
