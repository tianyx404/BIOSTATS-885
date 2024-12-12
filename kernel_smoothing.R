library(tidyverse)
library(gridExtra)
library(boot)

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

# Split into two datasets: control and treatment group
heart_control = heart %>% filter(transplant == "control")
heart_treatment = heart %>% filter(transplant == "treatment")

### Kernel smoothing
#### choose optimal bandwidth
options(warn=-1)
# List of ages
ages <- unique(heart$age)

# Define the possible values of h to test
h_values <- seq(5, 60, by = 1)

# Number of folds for cross-validation
K <- 5

# Function to perform K-fold cross validation
cv_error <- function(data, h) {
  n <- nrow(data)
  folds <- sample(rep(1:K, length.out = n))
  cv_errors <- numeric(K)
  
  for (k in 1:K) {
    train_data <- data[folds != k, ]
    test_data <- data[folds == k, ]
    
    est_prob_0 <- numeric(length(ages)) # estimated survival probability for control group
    est_prob_1 <- numeric(length(ages)) # estimated survival probability for treatment
    for (i in 1:length(ages)) {
      # select a grid point for estimation
      r <- ages[i]
      
      # select the local subset of the data
      sub_data <- train_data %>% filter((age >= (r - h)) & (age <= r + h))
      
      # skip this case if sub_data is empty
      if (nrow(sub_data) == 0) next
      
      # perform local linear logistic regression
      model <- glm(survived_binary ~ age * transplant_binary, family = "binomial", data = sub_data)
      
      # make point estimation
      est_b00 <- coef(model)[1]
      est_b10 <- coef(model)[3]
      
      est_logit_0 <- est_b00 
      est_logit_1 <- est_b00 + est_b10 
      
      est_prob_0[i] <- plogis(est_logit_0)
      est_prob_1[i] <- plogis(est_logit_1)
    }
    # Make prediction for the test data
    test_data <- test_data %>% 
      mutate(predicted = ifelse(transplant == "control",
                                est_prob_0[match(age, ages)],
                                est_prob_1[match(age, ages)]))
    cv_errors[k] <- mean((test_data$survived_binary - test_data$predicted)^2)
  }
  return(mean(cv_errors))
}

# Set seed for reproducibility
set.seed(1)

# Select the optimal h by cross-validation
validation_errors <- sapply(h_values, function(h) cv_error(heart, h))

optimal_h <- h_values[which.min(validation_errors)]
print(paste("Optimal h:", optimal_h))


### Plots K-fild cross-validation errors
# png("optimal_bandwidth.png")
# Sample data
x <- h_values
y <- validation_errors

# Create scatter plot
plot(x, y, xlab="bandwidth", ylab="cross-validation error", pch=19, col="blue")

# Connect points with lines
lines(x, y, col="red")

# Find the point with the smallest value in y
min_y_index <- which.min(y)

# Circle the point with the smallest value
# Adding a larger point over the existing point to simulate a circle
points(x[min_y_index], y[min_y_index], col="red", pch=1, cex=2)



### Run kernel smoothing with optimal bandwidth
ages = seq(1, 100, by=1)

# Store results
## estimation and inference for control group
est_prob_0 <- NULL
lower_band_0 <- NULL
upper_band_0 <- NULL

## estimation and inference for treatment group
est_prob_1 <- NULL
lower_band_1 <- NULL
upper_band_1 <- NULL

## coefficient and standard errors
b00_hat <- NULL
b10_hat <- NULL
b00_se <- NULL
b10_se <- NULL

# Using the optimal h to estimate survival probabilities
for (i in 1:length(ages)) {
  # select a grid point for estimation
  r <- ages[i]
  
  # select the local subset of the data
  sub_data <- heart %>% filter((age >= (r - optimal_h)) & (age <= r + optimal_h))
  
  # skip this case if sub_data is empty
  if (nrow(sub_data) == 0) next
  
  # perform local linear logistic regression
  model <- glm(survived_binary ~ age * transplant_binary, family = "binomial", data = sub_data)
  model_summary <- summary(model)
  
  # extract coefficients and standard errors
  est_b00 <- coef(model)[1]
  est_b10 <- coef(model)[3]
  
  se_b00 <- model_summary$coefficients[1, "Std. Error"]
  se_b10 <- model_summary$coefficients[3, "Std. Error"]
  
  # make point estimation
  est_logit_0 <- est_b00 
  est_logit_1 <- est_b00 + est_b10 
  
  # calculate confidence bands for control group
  lower_0 <- est_logit_0 - 1.96 * se_b00
  upper_0 <- est_logit_0 + 1.96 * se_b00
  
  # calculate confidence bands for treatment group
  lower_1 <- est_logit_1 - 1.96 * sqrt(se_b00^2 + se_b10^2)
  upper_1 <- est_logit_1 + 1.96 * sqrt(se_b00^2 + se_b10^2)
  
  # convert to probability and save results
  est_prob_0[i] <- plogis(est_logit_0)
  est_prob_1[i] <- plogis(est_logit_1)
  
  lower_band_0[i] <- plogis(lower_0)
  upper_band_0[i] <- plogis(upper_0)
  
  lower_band_1[i] <- plogis(lower_1)
  upper_band_1[i] <- plogis(upper_1)
  
  # save coefficients results
  b00_hat[i] <- est_b00
  b10_hat[i] <- est_b10
  b00_se[i] <- se_b00
  b10_se[i] <- se_b10
}

results_control <- data.frame(age = ages, 
                              prob = est_prob_0,
                              lower = lower_band_0, 
                              upper = upper_band_0) %>% mutate(group = "Control")

results_treatment <- data.frame(age = ages, 
                                prob = est_prob_1,
                                lower = lower_band_1, 
                                upper = upper_band_1) %>% mutate(group = "Treatment")

results_combined <- rbind(results_control, results_treatment)
results_beta <- data.frame(age = ages, b00_hat, b10_hat, b00_se, b10_se)


### Print estimated coefficients
options(digits=3)
results_beta %>% filter(age %in% seq(0, 100, by=10))

### Plot smoothed curves with confidence bands
#### Seperate plot
ggplot(heart_control, aes(x = age, y = survived_binary)) +
  geom_point(aes(y = survived_binary), size = 2, alpha = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"),
              se = TRUE, fullrange=TRUE) +
  scale_x_continuous(limits = c(1, 100), breaks = seq(1, 100, by = 10)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1),
                     minor_breaks = NULL) +
  labs(title = "Logistic Regression with Observed Points",
       x = "Age",
       y = "Probability") +
  theme_minimal()

ggplot(heart_treatment, aes(x = age, y = survived_binary)) +
  geom_point(aes(y = survived_binary), size = 2, alpha = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"),
              se = TRUE, fullrange=TRUE) +
  scale_x_continuous(limits = c(1, 100), breaks = seq(1, 100, by = 10)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1),
                     minor_breaks = NULL) +
  labs(title = "Logistic Regression with Observed Points",
       x = "Age",
       y = "Probability") +
  theme_minimal()

#### Plot the combined data without interactions
p <- ggplot(heart, aes(x = age, y = survived_binary)) +
  geom_point(aes(y = survived_binary), size = 1, alpha = 1) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), 
              se = TRUE, fullrange = TRUE) + 
  scale_x_continuous(limits = c(1, 100), breaks = seq(1, 100, by = 10)) + 
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1), 
                     minor_breaks = NULL) + 
  labs(title = "Kernel smoothing without interactions",
       x = "Age",
       y = "Survival Probability") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18))

ggsave("results_ks0.png", plot = p, width = 8, height = 6, dpi = 300)  

# Plot the combined data with interactions
p <- ggplot(heart, aes(x = age, y = survived_binary, color = transplant, fill = transplant)) +
  geom_point(size = 1, alpha = 1) + # Color of the points determined by transplant
  geom_smooth(method = "glm", method.args = list(family = "binomial"), 
              se = TRUE, fullrange = TRUE, alpha = 0.2) + 
  scale_x_continuous(limits = c(1, 100), breaks = seq(1, 100, by = 10)) + 
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1), 
                     minor_breaks = NULL) + 
  labs(title = "Kernel smoothing with interactions",
       x = "Age",
       y = "Survival Probability") +
  theme_minimal() +
  scale_color_manual(values = c("control" = "blue", "treatment" = "red")) +
  scale_fill_manual(values = c("control" = "blue", "treatment" = "red")) +
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 14))

ggsave("results_ks.png", plot = p, width = 8, height = 6, dpi = 300)