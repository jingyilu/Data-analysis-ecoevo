# Packages
library(tidyverse)
library(ggplot2)
library(ape)
######
#
Hemiboea <- read.nexus("Hemiboea1.0.con.tre", tree.names = "Hemiboea")
plot(Hemiboea)

Hemiboea_morph <- read.csv("Hemiboea_morph.csv", header = T)

######
# P.409 q.21
Finch <- read.csv("chap13q21StressAndIncompatibleMates.csv", header = T)

# Plot the difference
Finch <- Finch %>%
  mutate(difference = corticosterone_concentration_incompatible - corticosterone_concentration_compatible)
Finch_dif_histogram <- Finch %>%
  ggplot(aes(x = difference)) + 
  geom_histogram(breaks = seq(-5,125,5), fill = "#800000", colour = "black") + 
  theme(panel.background = element_blank(),
        axis.text=element_text(size=7), axis.title=element_text(size=10,face="bold"),
        axis.line.x.bottom = element_line(color="black", size = 0.25),
        axis.line.y.left = element_line(color="black", size = 0.25)) +
  xlab("Difference (ng/ml)") + ylab("Frequency")
ggsave("Difference Frequency.pdf", dpi = 300, height = 8.4, width =12.7, units = "cm")  

# log transform
Finch <- Finch %>%
  mutate(log_difference = log(difference+2))

Finch_log_dif_histogram <- Finch %>%
  ggplot(aes(x = log_difference)) + 
  geom_histogram(breaks = seq(0,5,.25), fill = "#800000", colour = "black") + 
  theme(panel.background = element_blank(),
        axis.text=element_text(size=7), axis.title=element_text(size=10,face="bold"),
        axis.line.x.bottom = element_line(color="black", size = 0.25),
        axis.line.y.left = element_line(color="black", size = 0.25)) +
  xlab("Log difference") + ylab("Frequency")
ggsave("Log Difference Frequency.pdf", dpi = 300, height = 8.4, width =12.7, units = "cm")

# Sign Test
2*(dbinom(42, 43, 0.5) + dbinom(42, 43, 0.5))

######
# P.411, q.26
2*(dbinom(9, 10, 0.5) + dbinom(10, 10, 0.5))


