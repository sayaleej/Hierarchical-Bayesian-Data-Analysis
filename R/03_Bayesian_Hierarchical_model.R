library(brms)
library(posterior)
library(bayesplot)
library(tidyverse)
library(corrplot)

cleaned_data <- "data/cleaned_power_data.csv"
df <- read.csv(cleaned_data,header=TRUE)
glimpse(df)

df <- df %>%
  rename(
    Zone1 = Zone.1.Power.Consumption,
    Zone2 = Zone.2..Power.Consumption,
    Zone3 = Zone.3..Power.Consumption
  )

# Reshape data
df_long <- df %>%
  pivot_longer(cols = c(Zone1, Zone2, Zone3),
               names_to = "Zone",
               values_to = "Power")
glimpse(df_long)


# Scale continuous predictors
df_long <- df_long %>%
  mutate(
    Temperature_scaled = scale(Temperature),
    Humidity_scaled = scale(Humidity),
    Wind.Speed_scaled = scale(Wind.Speed),
    general.diffuse.flows_scaled = scale(general.diffuse.flows),
    diffuse.flows_scaled = scale(diffuse.flows)
  )

df_long$Power_scaled <- scale(df_long$Power)


df_long$Weekend <- ifelse(df_long$Weekday %in% c("Sat", "Sun"), 1, 0)
df_long$Month_num <- match(df_long$Month, month.abb)


# Bayesian hierarchical model with random intercepts per Zone

prior <- c(
  prior(normal(0, 1), class = "b"),               # Priors on fixed effects
  prior(normal(0, 1), class = "Intercept"),
  prior(cauchy(0, 1), class = "sd"),              # Prior on random effects
  prior(cauchy(0, 1), class = "sigma")            # Prior on residual std dev
)


# Fit the hierarchical Bayesian model
model <- brm(
  Power_scaled ~ Temperature_scaled + Humidity_scaled + Month_num + Weekend + (1 | Zone),
  data = df_long,
  family = gaussian(),
  prior = prior,  
  chains = 4,
  cores = 2,
  iter = 4000,
  warmup = 1000,
  seed = 123,
)


# Save summary output to a text file
sink("model_summary.txt")
summary(model)
sink()


# Plot diagnostics and posterior checks
# Save model diagnostics plot
png("model_diagnostics.png", width = 1000, height = 800)
plot(model)
dev.off()

# Save posterior predictive checks
png("posterior_predictive_check.png", width = 1000, height = 800)
pp_check(model)
dev.off()

# Extract random effects
zone_effects <- ranef(model)$Zone
fixef(model)["Intercept", "Estimate"] + ranef(model)$Zone[, , "Intercept"]

# Get the intercepts per zone
zone_intercepts <- as.data.frame(zone_effects[, , "Intercept"])
zone_intercepts$Zone <- rownames(zone_intercepts)
colnames(zone_intercepts) <- c("Estimate", "Est.Error", "Q2.5", "Q97.5", "Zone")

ggplot(zone_intercepts, aes(x = Zone, y = Estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Q2.5, ymax = Q97.5), width = 0.2) +
  labs(title = "Zone-Level Intercepts",
       y = "Intercept Estimate (with 95% CI)",
       x = "Zone") +
  theme_minimal()

ggsave("zone_level_intercepts.png", width = 6, height = 4)

