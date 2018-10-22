# Packages
library(tidyverse)
library(ggplot2)

######
# Use R to calculate the probability of the chi-square statistic with 
# 3 degrees of freedom lying between 1 and 3, if the null hypothesis is true. 
# Sketch the distribution and shade the area.
######

prob_1and3_df3 <- pchisq(3,3) - pchisq(1,3)
Density_plot_prob_1and3_df3 <- ggplot(data.frame(x = c(0, 15)), aes(x = x)) +
  stat_function(fun = dchisq, args = list(df = 3)) + 
  stat_function(fun = dchisq, args = list(df = 3), 
                xlim = c(1,3), geom = "area", fill = "#800000")+ 
  scale_x_continuous(breaks = seq(1, 15, 2)) +
  theme(panel.background = element_blank(), plot.title = element_text(size=12,face="bold"),
        axis.text=element_text(size=7), axis.title=element_text(size=10,face="bold"),
        axis.line.x.bottom = element_line(color="black", size = 0.25),
        axis.line.y.left = element_line(color="black", size = 0.25))
ggsave("Density plot of Chi-Square df3.pdf", dpi = 300, height = 8.4, width =12.7, units = "cm")  

######
#p.263, q.19
######
Yawn <- read.csv("chap09q19Yawning.csv", header = T)
# Summarize the count table
Yawn_table <- table(Yawn)
# Chi-square test
chisq.test(Yawn_table)

######
# p.263, q.21: do this question in R, calculating the P value from a chi sq test, 
# and P value from Fisher’s exact test
######
Termites <- read.csv("chap09q21BlueTermites.csv", header = T)
# Summarize the count table
Termites_table <- table(Termites)
# Chi-square test
chisq.test(Termites_table)
# Fisher's Exact Test
fisher.test(Termites_table)
