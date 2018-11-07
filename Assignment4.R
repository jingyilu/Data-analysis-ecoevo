# Packages
library(tidyverse)
library(ggplot2)

######
# Suppose I toss a coin 10 times and it comes up heads 9x. 
# Set up the test that this is a fair coin, and test it using 
# (1) the binomial distribution 
n = 10
p = 0.5
p_binom <- 2*(dbinom(9, n, p)+dbinom(10, n, p))

# (2) the normal distribution.
mean_norm <- n*p
sd_norm <- sqrt(n*p*(1-p))
p_norm <- 2*(1-pnorm(9, mean_norm, sd_norm))
######

######
# Suppose femur lengths of dinosaurs in the field museum have all been measured, 
# and in this population the mean is 60cm and variance 9cm. 
# Compute the probability that a bone I randomly draw lies between 60cm and 62cm. 
# Compute the probability that the average value of 4 bones I randomly draw lie between 60cm and 61cm. 
# In each case, first sketch the probability distribution and shade the area you need to calculate. 
######
Density_plot_60_3_1 <- ggplot(data.frame(x = c(50, 70)), aes(x = x)) +
  stat_function(fun = dnorm, args = list(mean = 60, sd = 3)) + 
  scale_x_continuous(breaks = seq(50, 70, 5)) +
  theme(panel.background = element_blank(), plot.title = element_text(size=12,face="bold"),
        axis.text=element_text(size=7), axis.title=element_text(size=10,face="bold"),
        axis.line.x.bottom = element_line(color="black", size = 0.25),
        axis.line.y.left = element_line(color="black", size = 0.25))+
  stat_function(fun = dnorm, args = list(mean = 60, sd = 3), 
                xlim = c(60,62), geom = "area", fill = "#800000")
ggsave("Density plot of normal(60,3), 60-62.pdf", dpi = 300, height = 8.4, width =12.7, units = "cm")

Density_60_1.5_60_61 <- ggplot(data.frame(x = c(50, 70)), aes(x = x)) +
  stat_function(fun = dnorm, args = list(mean = 60, sd = 3/2)) + 
  scale_x_continuous(breaks = seq(50, 70, 5)) +
  theme(panel.background = element_blank(), plot.title = element_text(size=12,face="bold"),
        axis.text=element_text(size=7), axis.title=element_text(size=10,face="bold"),
        axis.line.x.bottom = element_line(color="black", size = 0.25),
        axis.line.y.left = element_line(color="black", size = 0.25)) +
  stat_function(fun = dnorm, args = list(mean = 60, sd = 3/2), 
                xlim = c(60,61), geom = "area", fill = "#800000")
ggsave("Density plot of normal(60,1.5), 60-61.pdf", dpi = 300, height = 8.4, width =12.7, units = "cm")

# Probabilities
prob_norm_60_62_1 <- pnorm(62, 60, 3) - pnorm(60, 60, 3)
prob_norm_60_61_4 <- pnorm(61, 60, 3/2) - pnorm(60, 60, 3/2)


######
# p.296 q.13
1 - pnorm(15, 14, 5/sqrt(10)); 1 - pnorm(15, 14, 5/sqrt(30))
1 - pnorm(15.5, 15, 3/sqrt(10)); 1 - pnorm(15.5, 15, 3/sqrt(30))
1 - pnorm(-22, -23, 4/sqrt(10)); 1 - pnorm(-22, -23, 4/sqrt(30))
1 - pnorm(45, 72, 50/sqrt(10)); 1 - pnorm(45, 72, 50/sqrt(30))


# p.324 q.24 
Circle <- read_csv("chap11q24WalkingInCircles.csv")
Circle_histogram <- Circle %>%
  ggplot(aes(x = angle)) + 
  geom_histogram(fill = "#800000", colour = "black")
######
# p.356 q.7
m1 = 58; n1 = 49; df1 = n1-1; s1 = 28
m2 = 25; n2 = 32; df2 = n2-1; s2 = 24

spsquare <- (df1*(s1^2)+df2*(s2^2))/(df1+df2)
SE_comparison <- sqrt(spsquare*(1/n1+1/n2))
mean_comparison <- m1-m2
df_comparison <- df1+df2

ct_comparison <- qt (0.025, df_comparison)
mean_comparison + ct_comparison*SE_comparison; mean_comparison - ct_comparison*SE_comparison
t_comparison <- mean_comparison/SE_comparison
2*(1-pt(t_comparison, df_comparison))

######
# Compute the 90% confidence intervals for the mean, 
# for a sample height of 25 people taken from a population whose sample mean X̅ = 60
# and sample standard deviation, s = 5.
qt(0.05, 24)

# Compute the 95% confidence intervals for the same data. 
qt(0.025, 24)

