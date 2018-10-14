#####
# Use R to read in the Darwin’s finch data: dfdata.xls
# Preview the document and reproduce figure 1.4-1. 
# Compute the following summary statistics: mean, variance, coefficient of variation, and standard error.
# 87, q.8
# 88, q.10: are more countries showing a decline or an increase in growth rate? Explain how you worked that out.
# 91, qs. 19,20
# 110, q.15
###
# Packages
library(tidyverse)
library(ggplot2)

# Retrieve data
Daphne_Island <- read.csv("dfdata.csv", header = T)

# Calculate the summary statistics for beak size
Summary_beak_size <- Daphne_Island %>%
  select(Beak_Size) %>%
  summarise(Beak_mean = mean(Beak_Size), Beak_variance = var(Beak_Size), CV = sd(Beak_Size)/mean(Beak_Size), Beak_se = sd(Beak_Size)/sqrt(length(Beak_Size)))

# Reproduce the frequency histogram
Beak_size_histogram <- Daphne_Island %>%
  ggplot(aes(x = Beak_Size)) + 
  geom_histogram(breaks = seq(4,14,0.5), fill = "#800000", colour = "black") + 
  theme(panel.background = element_blank(),
        axis.text=element_text(size=7), axis.title=element_text(size=10,face="bold"),
        axis.line.x.bottom = element_line(color="black", size = 0.25),
        axis.line.y.left = element_line(color="black", size = 0.25)) + 
  scale_x_continuous(breaks = seq(4,14,2)) +
  scale_y_continuous(breaks = seq(0, 187.5, 37.5), labels = paste(seq(0, 25, 5), "%"), limits = c(0,187.5)) + 
  xlab("Beak Size (mm)") + ylab("Frequency")
ggsave("Beak Size Frequency.pdf", dpi = 300, height = 8.4, width =12.7, units = "cm")  

## 91 q19
#Retrieve Data
Sparrow <- read.csv("chap03q19SparrowReproductiveSuccess.csv", header = T)

Summary_sparrow <- Sparrow %>%
  group_by(sex) %>%
  summarise(LRS_mean = mean(lifetimeRS), LRS_variance = var(lifetimeRS))

##110 15
#a
6.7/sqrt(4620)
4.6/sqrt(6228)

