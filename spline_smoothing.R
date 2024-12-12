library(tidyverse)
library(gridExtra)
library(boot)
library(mgcv)


### Data preprocessing
# Read data file
heart = read.csv("heart_transplant.csv")

# Convert to categorical variables and create corresponding binary values
heart <- heart %>%
  mutate(
    survived = factor(survived),
    transplant = factor(transplant),
    survived_binary = ifelse(survived == 'alive', 1, 0),
    transplant_binary = ifelse(transplant == 'treatment', 1, 0)
  )


# Create sequence of ages to be estimated
ages <- seq(1, 100, by=1)

# Initialize the list to store results
results_list <- list()

# Estimate the varying-coefficient model
model <- gam(survived_binary ~ s(age, bs="cs") + s(age, by=transplant_binary, bs="cs"),
             family=binomial, data=heart, method="REML")

for (r in ages) {
  for (transplant_status in c(0, 1)) {
    new_data <- data.frame(age=r, transplant_binary=transplant_status)
    prob <- predict(model, new_data, type="response")
    
    se_fit <- predict(model, new_data, type="link", se.fit=TRUE)$se.fit
    logit <- predict(model, new_data, type="link")
    lower <- logit - 1.96 * se_fit
    upper <- logit + 1.96 * se_fit
    
    results_list[[length(results_list) + 1]] <- data.frame(
      age=r,
      prob=prob,
      lower=plogis(lower),
      upper=plogis(upper),
      group=ifelse(transplant_status == 0, "Control", "Treatment")
    )
  }
}

# Combine results into a single data frame
final_results <- bind_rows(results_list)

# Make plots
p <- ggplot(final_results, aes(x = age, y = prob, color = group)) +
  geom_line(na.rm = TRUE, size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = group), 
              alpha = 0.2, color = NA, na.rm = TRUE) +
  geom_point(data = heart, aes(x = age, y = survived_binary, color = transplant), 
             size = 1, shape = 16, na.rm = TRUE) +
  labs(
    title = "Spline Smoothing with interactions",
    x = "Age",
    y = "Survival Probability"
  ) +
  scale_color_manual(values = c("Control" = "blue", "Treatment" = "red", 
                                "control" = "blue", "treatment" = "red"),
                     labels = c("prediction (control)", "prediction (treatment)", 
                                "observation (control)", "observation (treatment)")) +
  scale_fill_manual(values = c("Control" = "blue", "Treatment" = "red"),
                    labels = c("confidence band (control)", "confidence band (treatment)")) +
  theme_minimal() +
  theme(legend.title = element_blank())+
  guides(
    color = guide_legend(override.aes = list(
      shape = c(NA, NA, 16, 16), 
      linetype = c(1, 1, NA, NA),
      size = c(1, 1, 1, 1)
    )),
    shape = "none"
  ) +
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18))

ggsave("results_ss.png", plot = p, width = 8, height = 6, dpi = 300)  


### Run spline smoothing without interactions
# Create sequence of ages to be estimated
ages <- seq(1, 100, by=1)

# Initialize the list to store results
results_list <- list()

# Estimate the varying-coefficient model
# model <- gam(survived_binary ~ s(age, bs="cs") + s(age, by=transplant_binary, bs="cs"),
#              family=binomial, data=heart, method="GCV.Cp")
model <- gam(survived_binary ~ s(age, bs="cs"),
             family=binomial, data=heart, method="REML")
# model <- gam(survived_binary ~ s(transplant_binary, bs="cs"), data=heart,
#              family=binomial, method="REML", weights=heart$age)

for (r in ages) {
  for (transplant_status in c(0, 1)) {
    new_data <- data.frame(age=r, transplant_binary=transplant_status)
    prob <- predict(model, new_data, type="response")
    
    se_fit <- predict(model, new_data, type="link", se.fit=TRUE)$se.fit
    logit <- predict(model, new_data, type="link")
    lower <- logit - 1.96 * se_fit
    upper <- logit + 1.96 * se_fit
    
    results_list[[length(results_list) + 1]] <- data.frame(
      age=r,
      prob=prob,
      lower=plogis(lower),
      upper=plogis(upper),
      group=ifelse(transplant_status == 0, "Control", "Treatment")
    )
  }
}

# Combine results into a single data frame
final_results <- bind_rows(results_list)

p <- ggplot(final_results, aes(x = age, y = prob, color = group)) +
  geom_line(na.rm = TRUE, size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = group), 
              alpha = 0.2, color = NA, na.rm = TRUE) +
  geom_point(data = heart, aes(x = age, y = survived_binary, color = transplant), 
             size = 1, shape = 16, na.rm = TRUE) +
  labs(
    title = "Spline Smoothing without interactions",
    x = "Age",
    y = "Survival Probability"
  ) +
  scale_color_manual(values = c("Control" = "blue", "Treatment" = "red", 
                                "control" = "blue", "treatment" = "red"),
                     labels = c("prediction (control)", "prediction (treatment)", 
                                "observation (control)", "observation (treatment)")) +
  scale_fill_manual(values = c("Control" = "blue", "Treatment" = "red"),
                    labels = c("confidence band (control)", "confidence band (treatment)")) +
  theme_minimal() +
  theme(legend.title = element_blank())+
  guides(
    color = guide_legend(override.aes = list(
      shape = c(NA, NA, 16, 16), 
      linetype = c(1, 1, NA, NA),
      size = c(1, 1, 1, 1)
    )),
    shape = "none"
  ) +
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18)) +
  theme(legend.position = "none")

ggsave("results_ss0.png", plot = p, width = 8, height = 6, dpi = 300)  