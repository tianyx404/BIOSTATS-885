
library(tidyverse)
library(gridExtra)

### Data preprocessing

# read data file
heart = read.csv("heart_transplant.csv")

# Convert to categorical variables and create corresponding binary values
heart <- heart %>%
  mutate(
    survived = factor(survived),
    transplant = factor(transplant),
    survived_binary = ifelse(survived == 'alive', 1, 0),
    transplant_binary = ifelse(transplant == 'treatment', 1, 0)
  )

# Split dataset into different groups
heart_control = heart %>% filter(transplant == "control")
heart_treatment = heart %>% filter(transplant == "treatment")
heart_alive = heart %>% filter(survived == "alive")
heart_dead = heart %>% filter(survived == "dead")

### Prelimiary analysis
summary(heart)

# Calculate the proportion of survived patients with and without a heart transplant
heart_control %>% 
  group_by(survived) %>%
  summarise(count = n()) %>%
  mutate(percentage = round((count / sum(count)) * 100, 2))

heart_treatment %>% 
  group_by(survived) %>%
  summarise(count = n()) %>%
  mutate(percentage = round((count / sum(count)) * 100, 2))


# Make boxplots of age~survived_status, by=transplant_status
p1 <- ggplot(heart_control, aes(survived, age, fill=survived)) + 
  geom_boxplot() + 
  ggtitle("") + 
  xlab("survival outcome") + 
  ylab("age") + 
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 14))

p2 <- ggplot(heart_treatment, aes(survived, age, fill=survived)) + 
  geom_boxplot() + 
  ggtitle("") + 
  xlab("survival outcome") + 
  ylab("age") +
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 14))

ggsave("boxplot_control.png", plot = p1, width = 8, height = 6, dpi = 300)
ggsave("boxplot_treatment.png", plot = p2, width = 8, height = 6, dpi = 300)





# Make boxplots of age~transplant_status, by=survived_status
p3 <- ggplot(heart_alive, aes(transplant, age, fill=transplant)) + 
  geom_boxplot() + 
  ggtitle("") + 
  xlab("Transplant status") + 
  ylab("age") + 
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 14))

p4 <- ggplot(heart_dead, aes(transplant, age, fill=transplant)) + 
  geom_boxplot() + 
  ggtitle("") + 
  xlab("Transplant status") + 
  ylab("age") + 
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 14))

# Combine the plots side by side
grid.arrange(p3, p4, ncol=2)

ggsave("boxplot_alive.png", plot = p3, width = 8, height = 6, dpi = 300)
ggsave("boxplot_dead.png", plot = p4, width = 8, height = 6, dpi = 300)


# Make scatterplots
par(mfrow=c(2,1))
plot(heart_control$age, heart_control$survived_binary, pch=19)
plot(heart_treatment$age, heart_treatment$survived_binary, pch=19)


