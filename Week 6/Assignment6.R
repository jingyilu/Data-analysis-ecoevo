# Packages
library(tidyverse)
library(nlme)
library(ape)
library(ggplot2)
######
# 3.	P.494, q.22
######
n = 2
W.S.Head <- read.csv("chap15q22WalkingStickHeads.csv", header = T)

# Random Effects
Anova.W.S.Head <- lme(fixed = headwidth ~ 1, 
                    random = ~ 1|specimen, data = W.S.Head)

# Extract variation
Var.comp.W.S.Head <- VarCorr(Anova.W.S.Head)
Among <- as.numeric(Var.comp.W.S.Head[1,1])
Within <- as.numeric(Var.comp.W.S.Head[2,1])

# Calculate repeatability
Repeatability.W.S.Head = Among/(Among+Within)

######
#	P.533, q.26
######
Choco.Nobel <- read.csv("chap16q26ChocolateAndNobel.csv", header = T)

# Plot
Choco.v.Nobel <- Choco.Nobel %>%
  ggplot(aes(x = chocolateConsumption, y = nobelPrizes.per.100.million.)) +
  geom_point(color = "#800000") + 
  theme(panel.background = element_blank(),
        axis.text=element_text(size=7), axis.title=element_text(size=10,face="bold"),
        axis.line.x.bottom = element_line(color="black", size = 0.25),
        axis.line.y.left = element_line(color="black", size = 0.25)) +
  xlab("Chocolate consumption (kg/person/year)") + ylab("Nobel Prizes (per 100 million)")
ggsave("Chocolate consumption and Nobel Prizes.pdf", dpi = 300, height = 8.4, width =12.7, units = "cm")

# Histograms
Choco.hist <- Choco.Nobel %>%
  ggplot(aes(x = chocolateConsumption)) + 
  geom_histogram(breaks = seq(0,12,1), fill = "#800000", colour = "black") + 
  theme(panel.background = element_blank(),
        axis.text=element_text(size=7), axis.title=element_text(size=10,face="bold"),
        axis.line.x.bottom = element_line(color="black", size = 0.25),
        axis.line.y.left = element_line(color="black", size = 0.25)) +
  xlab("Chocolate consumption (kg/person/year)") + ylab("Frequency")
ggsave("Chocolate consumption histogram.pdf", dpi = 300, height = 8.4, width =12.7, units = "cm")

Nobel.hist <- Choco.Nobel %>%
  ggplot(aes(x = nobelPrizes.per.100.million.)) + 
  geom_histogram(breaks = seq(0,33,3), fill = "#F05E23", colour = "black") + 
  theme(panel.background = element_blank(),
        axis.text=element_text(size=7), axis.title=element_text(size=10,face="bold"),
        axis.line.x.bottom = element_line(color="black", size = 0.25),
        axis.line.y.left = element_line(color="black", size = 0.25)) +
  xlab("Chocolate consumption (kg/person/year)") + ylab("Frequency")
ggsave("Nobel Prizes histogram.pdf", dpi = 300, height = 8.4, width =12.7, units = "cm")

# Test for correlation
cor.test(x = Choco.Nobel$chocolateConsumption, y = Choco.Nobel$nobelPrizes.per.100.million.)

######
# Phylogeny
######
Hemiboea <- read.nexus("Hemiboea4.0.root.tre", tree.names = "Hemiboea")
plot(Hemiboea)

# Force biforcation
Hemiboea.bi <- multi2di(Hemiboea)

# Morpho data
Hemiboea_morph <- read.csv("Hemiboea_morph.csv", header = T)

Corolla.length <- Hemiboea_morph$Corolla_length
Involucre.length <- Hemiboea_morph$Involucre_length
Petal.ratio <- Hemiboea_morph$Abaxial_Lip/Hemiboea_morph$Adaxial_Lip
names(Corolla.length) <- names(Involucre.length) <- names(Petal.ratio) <- Hemiboea_morph$Species

# Phylogenetic Independent Contrasts
pic.Corolla.length <- pic(Corolla.length, Hemiboea.bi)
pic.Involucre.length <- pic (Involucre.length, Hemiboea.bi)
pic.Petal.ratio <- pic (Petal.ratio, Hemiboea.bi)

# Positivise
Contrast <- as.data.frame(cbind(pic.Corolla.length, pic.Involucre.length))
Contrast.positivise <- Contrast %>%
  mutate(pic.Involucre.length = case_when(pic.Corolla.length<0 ~ -pic.Involucre.length,
                                          pic.Corolla.length>=0 ~ pic.Involucre.length),
         pic.Corolla.length = case_when(pic.Corolla.length<0 ~ -pic.Corolla.length,
                                        pic.Corolla.length>=0 ~ pic.Corolla.length))

# Plot Contrast
Hemiboea.contrast.cor.inv <- Contrast.p %>%
  ggplot(aes(x = pic.Corolla.length, y = pic.Involucre.length)) +
  geom_point(color = "#800000") + 
  theme(panel.background = element_blank(),
        axis.text=element_text(size=7), axis.title=element_text(size=10,face="bold"),
        axis.line.x.bottom = element_line(color="black", size = 0.25),
        axis.line.y.left = element_line(color="black", size = 0.25)) +
  xlab("PIC of corolla length") + ylab("PIC of involucre length")
ggsave("PIC Hemiboea Corolla Involucre.pdf", dpi = 300, height = 8.4, width =12.7, units = "cm")
