# Packages
library(tidyverse)
library(ggplot2)
library(ape)
library(raster)
######
# Phylogeny statistics 
# Read phylogenetic tree file into R
Hemiboea <- read.nexus("Hemiboea4.0.root.tre", tree.names = "Hemiboea")
# Read morphological data into R
Hemiboea_morph <- read.csv("Hemiboea_morph.csv", header = T)
# Force biforcation
Hemiboea.bi <- multi2di(Hemiboea)
# Create vectors for calculating phylogenetic independent contrasts
Corolla.length <- Hemiboea_morph$Corolla_length; Involucre.length <- Hemiboea_morph$Involucre_length
names(Corolla.length) <- names(Involucre.length) <- Hemiboea_morph$Species #Give names for matching tips
# Calculate Phylogenetic Independent Contrasts (PIC)
pic.Corolla.length <- pic(Corolla.length, Hemiboea.bi); pic.Involucre.length <- pic (Involucre.length, Hemiboea.bi)
# Positivise PIC values (using function “case_when” as logical operators)
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

######
# p.677 q.15
p <- seq(0.7, 0.99, 0.01)
voles.n <- 56
f0 <- 48/56; f1 <- 8/56
lnL.g <- log(1-p)*(f1*1) + voles.n*log(p)
lnL.g.plot <- ggplot()+
  geom_point(aes(x = p, y = lnL.g)) +
  theme(panel.background = element_blank(),
        axis.text=element_text(size=7), axis.title=element_text(size=10,face="bold"),
        axis.line.x.bottom = element_line(color="black", size = 0.25),
        axis.line.y.left = element_line(color="black", size = 0.25)) +
  xlab(expression(p)) + ylab("ln(Likelihood)")
ggsave("lnLikelihood of p.pdf", dpi = 300, height = 8.4, width =12.7, units = "cm")

lnL.g.table <- as.data.frame(cbind(p, lnL.g))
lnL.g.table %>%
  filter(lnL.g == max(lnL.g))
lnL.g.table <- lnL.g.table %>%
  mutate(unit = lnL.g - max(lnL.g))
lnL.support <- lnL.table %>%
  filter(abs(unit) <= 2)

######
# Normal distribution likelihood
mu <- seq(from = -3, to = 3, length = 10000)
x.mean <- 0; x.var <- 0; x.n <- 1
lnL <- -x.n*log(2*pi)/2 - x.n*x.var/2 - x.n*((x.mean-mu)^2)/2

max.lnL <- max(lnL)
lnL.table <- as.data.frame(cbind(mu, lnL))
lnL.table <- lnL.table %>%
  mutate(unit = lnL - max.lnL)
lnL.support <- lnL.table %>%
  filter(abs(unit) <= 2)

lnL.plot <- ggplot() +
  geom_point(aes(x = lnL.table$mu, y = lnL.table$lnL), size = 0.3) +
  geom_point(aes(x = lnL.support$mu, y = lnL.support$lnL), color = "#FFCC99", size = 0.3) +
  geom_segment(aes(x = min(lnL.support$mu), y = min(lnL.support$lnL),
                    xend = max(lnL.support$mu), yend = min(lnL.support$lnL)), color = "#FFCC99", size = 1, linetype = "dotdash") +
  theme(panel.background = element_blank(),
        axis.text=element_text(size=7), axis.title=element_text(size=10,face="bold"),
        axis.line.x.bottom = element_line(color="black", size = 0.25),
        axis.line.y.left = element_line(color="black", size = 0.25)) +
  xlab(expression(mu)) + ylab("ln(Likelihood)")
ggsave("lnLikelihood of normal distribution.pdf", dpi = 300, height = 8.4, width =12.7, units = "cm")


# Compare the effect of sample mean, variance, and size
x.n.l <- 10; x.var.s <- 5

lnL.l <- -x.n.l*log(2*pi)/2 - x.n.l*x.var/2 - x.n.l*((x.mean-mu)^2)/2
lnL.s <- -x.n*log(2*pi)/2 - x.n*x.var.s/2 - x.n*((x.mean-mu)^2)/2
lnL.plot.compare <- ggplot() +
  geom_point(aes(x = mu, y = lnL), size = 0.5) +  
  geom_point(aes(x = mu, y = lnL.l), color = "#800000", size = 0.5) +
  geom_point(aes(x = mu, y = lnL.s), color = "#FF9933", size = 0.5) +
  theme(panel.background = element_blank(),
        axis.text=element_text(size=7), axis.title=element_text(size=10,face="bold"),
        axis.line.x.bottom = element_line(color="black", size = 0.25),
        axis.line.y.left = element_line(color="black", size = 0.25)) +
  xlab(expression(mu)) + ylab("lnLikelihood")
ggsave("lnLikelihood of normal distribution comparison.pdf", dpi = 300, height = 8.4, width =12.7, units = "cm")


######
# worldclim
getData("worldclim",var="alt", res=2.5)
getData("worldclim",var="bio", res=2.5)

extent <- c(86,124,14,28)
alt <- raster("wc2-5/alt.bil")
res <- crop(alt,extent)
plot(res)

bio12 <- raster("wc2-5/bio12.bil")
res.b12 <- crop(bio12,extent)
Elevation <- as.data.frame(rasterToPoints(res))
Annual.precipitation <- as.data.frame(rasterToPoints(res.b12))
AP.Ele.table <- inner_join(Elevation, Annual.precipitation, by = c("x", "y"))
AP.Ele <- AP.Ele.table %>% ggplot() +
  geom_point(aes(x = alt, y = bio12)) +  
  theme(panel.background = element_blank(),
        axis.text=element_text(size=7), axis.title=element_text(size=10,face="bold"),
        axis.line.x.bottom = element_line(color="black", size = 0.25),
        axis.line.y.left = element_line(color="black", size = 0.25)) +
  xlab("Elevation") + ylab("Annual Percipitation")
ggsave("Elevation against annual percipitation.pdf", dpi = 300, height = 8.4, width =12.7, units = "cm")

