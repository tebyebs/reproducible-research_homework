library(ggplot2)
library(dplyr)

viral_data <- read.csv("question-5-data/Cui_etal2014.csv")

# Q5a 
str(viral_data)

# Q5b - log transformation
log_viral_data <- viral_data %>%
  mutate(log_V = log(Virion.volume..nm.nm.nm.)) %>%
  mutate(log_L = log(Genome.length..kb.))

# Q5c - Linear Model
linmodel <- lm(log_V ~ log_L, log_viral_data)

summary(linmodel)

# Q5d - Plotting the graph 
ggplot(log_viral_data, aes(x = log_L, y = log_V)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(x = "log[Genome length (kb)]", y = "log[Virion volume (nm³)]") 

# Q5e - Calculating from model
Xlog_V <- 7.0748 + 1.5152*log(300)

exp(Xlog_V)

