# Packages
library(tidyverse)
library(car)
library(ggplot2)
library(nlme)
######
#1
Galapagos <- read.csv("Galapagos.csv", header = T, na.strings="unknown")
#lm.obs.num <- Anova(lm(Observed.number.of.species~Area..km2.+Elevation..m.+Distance.from.nearest.island..km.+
#       Distance.from.Santa.Cruz..km.+Area.of.adjacent.island..km2., data = Galapagos), type = "III")
#lm.obs.nat.num <- Anova(lm(Observed.number.of.native.species~Area..km2.+Elevation..m.+Distance.from.nearest.island..km.+
#           Distance.from.Santa.Cruz..km.+Area.of.adjacent.island..km2., data = Galapagos), type = "III")
summary(lm(Observed.number.of.species~Area..km2.+Elevation..m.+Distance.from.nearest.island..km.+
             Distance.from.Santa.Cruz..km.+Area.of.adjacent.island..km2., data = Galapagos))
summary(lm(Observed.number.of.native.species~Area..km2.+Elevation..m.+Distance.from.nearest.island..km.+
             Distance.from.Santa.Cruz..km.+Area.of.adjacent.island..km2., data = Galapagos))

######
# 2
D.Rh <- read.csv("Rhphenotypes-Dutch.csv", header = T, row.names = 1)
P.Rh <- read.csv("Rhphenotypes-Poles.csv", header = T, row.names = 1)
G.Rh <- read.csv("Rhphenotypes-Greeks.csv", header = T, row.names = 1)

f.t.D.Rh <- fisher.test(D.Rh)
f.t.P.Rh <- fisher.test(P.Rh)
f.t.G.Rh <- fisher.test(G.Rh)
p.Rh <- c(f.t.D.Rh$p.value, f.t.P.Rh$p.value, f.t.G.Rh$p.value)
p.adjust(p.Rh, method = "bonferroni", n = length(p.Rh))

D.Total <- sum(D.Rh); P.Total <- sum(P.Rh); G.Total <- sum(G.Rh)

D.Rh.u <- unlist(D.Rh)
P.Rh.u <- unlist(P.Rh)
G.Rh.u <- unlist(G.Rh)
Pop.Rh <- bind_rows(D.Rh.u, P.Rh.u, G.Rh.u)

chisq.pop.rh <- chisq.test(Pop.Rh)

######
#3
Fruitflies <- read.csv("fruitflies.csv", header = T)
#Draw histograms for each treatments
G8p.hist <- Fruitflies %>%
  filter(Treatment == "8 pregnant females")%>%
  ggplot(aes(x = Lifespan)) + 
  geom_histogram(breaks = seq(15,100,5), fill = "#800000", colour = "black") + 
  theme(panel.background = element_blank(),
        axis.text=element_text(size=7), axis.title=element_text(size=10,face="bold"),
        axis.line.x.bottom = element_line(color="black", size = 0.25),
        axis.line.y.left = element_line(color="black", size = 0.25)) +
  xlab("Life Span (days)") + ylab("Frequency")
ggsave("Treatment of 8 pregnant females.pdf", dpi = 300, height = 8.4, width =12.7, units = "cm")

G80.hist <- Fruitflies %>%
  filter(Treatment == "no females added")%>%
  ggplot(aes(x = Lifespan)) + 
  geom_histogram(breaks = seq(15,100,5), fill = "#3CB371", colour = "black") + 
  theme(panel.background = element_blank(),
        axis.text=element_text(size=7), axis.title=element_text(size=10,face="bold"),
        axis.line.x.bottom = element_line(color="black", size = 0.25),
        axis.line.y.left = element_line(color="black", size = 0.25)) +
  xlab("Life Span (days)") + ylab("Frequency")
ggsave("Treatment of no females added.pdf", dpi = 300, height = 8.4, width =12.7, units = "cm")

G1p.hist <- Fruitflies %>%
  filter(Treatment == "1 pregnant female")%>%
  ggplot(aes(x = Lifespan)) + 
  geom_histogram(breaks = seq(15,100,5), fill = "#FF9933", colour = "black") + 
  theme(panel.background = element_blank(),
        axis.text=element_text(size=7), axis.title=element_text(size=10,face="bold"),
        axis.line.x.bottom = element_line(color="black", size = 0.25),
        axis.line.y.left = element_line(color="black", size = 0.25)) +
  xlab("Life Span (days)") + ylab("Frequency")
ggsave("Treatment of 1 pregnant female.pdf", dpi = 300, height = 8.4, width =12.7, units = "cm")

G1v.hist <- Fruitflies %>%
  filter(Treatment == "1 virgin female")%>%
  ggplot(aes(x = Lifespan)) + 
  geom_histogram(breaks = seq(15,100,5), fill = "#6495ED", colour = "black") + 
  theme(panel.background = element_blank(),
        axis.text=element_text(size=7), axis.title=element_text(size=10,face="bold"),
        axis.line.x.bottom = element_line(color="black", size = 0.25),
        axis.line.y.left = element_line(color="black", size = 0.25)) +
  xlab("Life Span (days)") + ylab("Frequency")
ggsave("Treatment of 1 virgin female.pdf", dpi = 300, height = 8.4, width =12.7, units = "cm")

G8v.hist <- Fruitflies %>%
  filter(Treatment == "8 virgin females")%>%
  ggplot(aes(x = Lifespan)) + 
  geom_histogram(breaks = seq(15,100,5), fill = "#4169E1", colour = "black") + 
  theme(panel.background = element_blank(),
        axis.text=element_text(size=7), axis.title=element_text(size=10,face="bold"),
        axis.line.x.bottom = element_line(color="black", size = 0.25),
        axis.line.y.left = element_line(color="black", size = 0.25)) +
  xlab("Life Span (days)") + ylab("Frequency")
ggsave("Treatment of 8 virgin female.pdf", dpi = 300, height = 8.4, width =12.7, units = "cm")

#Anova (separate treatments with female number)
#Fruitflies <- Fruitflies %>%
#  mutate(Treatment.true = 
#           case_when(Treatment == "no females added" ~ "Control",
#                     Treatment == "8 pregnant females"|Treatment == "1 pregnant female" ~ "Compete",
#                     Treatment == "8 virgin females"|Treatment == "1 virgin female" ~ "Mating"))
#Anova.fruitflies <- lme(fixed = Lifespan ~ factor(Treatment.true), 
#                      random = ~ 1|factor(No..female.partners), data = Fruitflies)
#Anova.fruitflies.f <- lme(fixed = Lifespan ~ Treatment.true,
#                          random = ~1, data = Fruitflies)


summary(lm(Lifespan ~ Treatment, data = Fruitflies))
anova.fruitflies <- Anova(lm(Lifespan ~ Treatment, data = Fruitflies), type = "III")
summary(aov(Lifespan ~ Treatment, data = Fruitflies))

#Levene's Test for variance homogeneity
leveneTest(Fruitflies$Lifespan, group = Fruitflies$Treatment)


######
#4
#random number of x min and y min
set.seed(1)
x.mean <- runif(1,1,10)
y.mean <- runif(1,1,10)

#Calculate slopes and intercept
var.y <- var.x <- runif(1,1,10); cor.x.y <- 0.5
cov.x.y <- cor.x.y*(var.x*var.y)^(1/2)
beta.slope.y.x <- cov.x.y/var.x
beta.slope.x.y <- cov.x.y/var.y
alpha.x <- y.mean-beta.slope.y.x*x.mean
alpha.y <- x.mean-beta.slope.x.y*y.mean

#For correlation equals to 1
cor.x.y.1 <- 1
cov.x.y.1 <- cor.x.y.1*(var.x*var.y)^(1/2)
beta.slope.y.x.1 <- cov.x.y.1/var.x
beta.slope.x.y.1 <- cov.x.y.1/var.y
alpha.x.1 <- y.mean-beta.slope.y.x.1*x.mean
alpha.y.1 <- x.mean-beta.slope.x.y.1*y.mean

#Plot the regression lines
df <- data.frame()
regression.effect <- ggplot(df) + 
  theme(panel.background = element_blank(),
        axis.text=element_text(size=7), axis.title=element_text(size=10,face="bold"),
        axis.line.x.bottom = element_line(color="black", size = 0.25),
        axis.line.y.left = element_line(color="black", size = 0.25)) +
  geom_abline(slope = beta.slope.y.x, intercept = alpha.x, color =  "#800000") + 
  geom_abline(slope = 1/beta.slope.x.y, intercept = -alpha.y/beta.slope.x.y) + 
  geom_abline(slope = beta.slope.x.y.1, intercept = -alpha.y.1/beta.slope.x.y.1, color = "#6495ED", linetype = "dashed") +
  geom_abline(slope = 1/beta.slope.x.y.1, intercept = -alpha.y.1/beta.slope.x.y.1, color = "#3CB371", linetype = "dotdash") +
  scale_x_continuous(limits = c(0,10), expand = c(0, 0)) + 
  scale_y_continuous(limits = c(0,10),expand = c(0, 0))
ggsave("Regression effect.pdf", dpi = 300, height = 8.4, width =12.7, units = "cm")

