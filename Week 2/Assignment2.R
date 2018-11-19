######
# Use the rnorm function in R to generate 3 random samples of size 30 from 
# a normal distribution with mean 15, and standard deviation 3. 
# Plot these as histograms, each on the same X scale. 
# Write down the mean and standard deviation for each.
######
# Packages
library(tidyverse)
library(ggplot2)

# Random sampling
set.seed(8) #set seed for consistent coding
random_sample_1 <- rnorm(30, 15, 3)
random_sample_2 <- rnorm(30, 15, 3)
random_sample_3 <- rnorm(30, 15, 3)

# Histogram of samplings
sample_histogram_1 <- ggplot() + aes(random_sample_1) + geom_histogram(binwidth = 1, fill = "#800000", colour = "black") +
      theme(panel.background = element_blank(), plot.title = element_text(size=12,face="bold"),
        axis.text=element_text(size=7), axis.title=element_text(size=10,face="bold"),
        axis.line.x.bottom = element_line(color="black", size = 0.25),
        axis.line.y.left = element_line(color="black", size = 0.25)) + 
        scale_x_continuous(breaks = seq(5, 25, 5), limits = c(5,25)) +
        scale_y_continuous(breaks = seq(0, 8, 2), limits = c(0,8)) + 
        labs(x = "", y = "Frequency", title = "Histogram of random sample 1")
ggsave("Histogram of random sample 1.pdf", dpi = 300, height = 8.4, width =12.7, units = "cm")  

sample_histogram_2 <- ggplot() + aes(random_sample_2) + geom_histogram(binwidth = 1, fill = "#00CED1", colour = "black") +
  theme(panel.background = element_blank(), plot.title = element_text(size=12,face="bold"),
        axis.text=element_text(size=7), axis.title=element_text(size=10,face="bold"),
        axis.line.x.bottom = element_line(color="black", size = 0.25),
        axis.line.y.left = element_line(color="black", size = 0.25)) + 
  scale_x_continuous(breaks = seq(5, 25, 5), limits = c(5,25)) +
  scale_y_continuous(breaks = seq(0, 8, 2), limits = c(0,8)) + 
  labs(x = "", y = "Frequency", title = "Histogram of random sample 2")
ggsave("Histogram of random sample 2.pdf", dpi = 300, height = 8.4, width =12.7, units = "cm")  

sample_histogram_3 <- ggplot() + aes(random_sample_3) + geom_histogram(binwidth = 1, fill = "#F05E23", colour = "black") +
  theme(panel.background = element_blank(), plot.title = element_text(size=12,face="bold"),
        axis.text=element_text(size=7), axis.title=element_text(size=10,face="bold"),
        axis.line.x.bottom = element_line(color="black", size = 0.25),
        axis.line.y.left = element_line(color="black", size = 0.25)) + 
  scale_x_continuous(breaks = seq(5, 25, 5), limits = c(5,25)) +
  scale_y_continuous(breaks = seq(0, 8, 2), limits = c(0,8)) + 
  labs(x = "", y = "Frequency", title = "Histogram of random sample 3")
ggsave("Histogram of random sample 3.pdf", dpi = 300, height = 8.4, width =12.7, units = "cm")  

# Mean; Standard deviation
mean(random_sample_1); sd(random_sample_1)
mean(random_sample_2); sd(random_sample_2)
mean(random_sample_3); sd(random_sample_3)

###
#226 q.4
###
# Retrieve data
World_Cup <- read.csv("chap08q05WorldCup.csv", header = T)
# Create frequency table of goals
Goal_frequency <- World_Cup %>%
  select(score) %>%
  table() %>%
  as.data.frame()
# Calculate 
sample_mean <- (0*37 + 1*44 + 2*21 +3*10 + 4*4 + 5*1)/112
expected_prob <- (dpois(0:5, sample_mean))
expected_freq <- expected_prob*112
Goal_frequency$expected_freq <- expected_freq
Goal_frequency <- Goal_frequency%>%
  mutate(Chi_Square = ((Freq - expected_freq)^2)/expected_freq)
expected_greater_than_five <- dpois(6, sample_mean)*112
Chi_Suare_greater = ((0 - expected_greater_than_five)^2)/expected_greater_than_five

Chi_Square_sum <- sum(Goal_frequency$Chi_Square) + Chi_Suare_greater
qchisq(0.95, 5)

##Grouping the category with expectation less than one
Goal_frequency_trim <- Goal_frequency %>%
  select(Freq) %>%
  top_n(5)
Goal_frequency_trim[5,1] <- Goal_frequency[5,2] + Goal_frequency[6,2]
expected_prob_trim <- c((dpois(0:3, sample_mean)), 1-ppois(3, sample_mean))
expected_freq_trim <- expected_prob_trim*112
Goal_frequency_trim$expected_freq <- expected_freq_trim
Goal_frequency_trim <- Goal_frequency_trim%>%
  mutate(Chi_Square = ((Freq - expected_freq)^2)/expected_freq)
Chi_Square_trim_sum <- sum(Goal_frequency_trim$Chi_Square)
qchisq(0.95, 3)
