# DATASET
setwd("C:/Users/aj419/OneDrive - University of Exeter/2018/PAPERS/WORKING ON THEM/Tablets/analyses/")
d<-read.csv("tablets_exp.csv")
# Gender as factor
d$GENDER <- ifelse((d$GENDER==1),"Male",
                   ifelse((d$GENDER==2),"female","other"))
d$GENDER <- as.factor(d$GENDER)

# PACKAGES
library(tidyverse)
library(lme4)
library(sjstats)
library(arm)
library(coefplot)
library(brms)
library(janitor)


# MANIPULATION CHECKS (PRESTIGE - LINEAR)
# Descriptive Statistics 
d %>%
  group_by(SOURCE) %>%
  summarise(mnptg = mean(PRESTIGE), sdptg = sd(PRESTIGE))
# Results: Cleaner (M=2.10, SD=0.831), Educator (M=3.52, SD=0.763), Pilot (M=3.30, SD=0.68)


# Null model for prestige with participant as random effect
null.ptg<-lmer(PRESTIGE~(1|PARTICIPANT), data=d)
# ICC for null model
icc(null.ptg) # 13% of variance is explained by participant

# Source model for prestige with participant as random effect
ptg<-lmer(PRESTIGE~SOURCE+(1|PARTICIPANT), data=d)
# ICC for source model
icc(ptg) # 43% of variation explain by participant when source is incorporated into the model
# this might be consequence of the fact that each participant rates two out of three sources

# Model comparisons
AIC(null.ptg, ptg)# Source model has a better fit (AIC=858) than the null model (AIC=1080)

# Summary of source model
display(ptg) # Educator (B=1.40, SE=0.08) and Pilot (B=1.09, SE=0.08) are rated as more
# prestigious than the cleaner

# Coefficient Plot for the source model
detach("package:arm", unload=TRUE)
coefplot(ptg)

# Rerun the model with Pilot as reference category
d<-within(d, SOURCE <- relevel(SOURCE, ref = "PILOT"))
ptg<-lmer(PRESTIGE~SOURCE+(1|PARTICIPANT), data=d)

library(arm)
display(ptg) # Educator is higher in prestige than the pilot (B=0.31, SE=0.08)

detach("package:arm", unload=TRUE)
library(coefplot)
coefplot(ptg)

# MANIPULATION CHECK (PRESTIGE - LINEAR - BAYESIAN)
library(brms)
d<-within(d, SOURCE <- relevel(SOURCE, ref = "CLEANER"))
ptg.linear<-brm(PRESTIGE~SOURCE+(1|PARTICIPANT), control = list(max_treedepth = 15), data=d)
summary(ptg.linear)
marginal_effects(ptg.linear)
plot(ptg.linear)
pairs(ptg.linear)
# Bayesian linear model with flat priors provides exactly the same coefficients as the
# frequentist model

# MANIPULATION CHECKS (PRESTIGE - ORDINAL)
# Descriptive statistics
# Median, minimum and  maximum in prestige for the three sources
d %>%
  group_by(SOURCE) %>%
  summarise(medianptg = median(PRESTIGE),  minpgt=min(PRESTIGE), maxpgt=max(PRESTIGE))
# Medians: Educator = 4, Pilot = 3, Cleaner = 2
# Min: Educator = 1, Pilot = 1, Cleaner = 1
# Max: Educator = 5, Pilot = 5, Cleaner = 4

# Frequences of each rating for the Educator (prestige)
library(janitor)
d %>%
  filter(SOURCE=="EDUCATOR") %>%
  tabyl(PRESTIGE, sort = TRUE)

# Frequencies of each rating for the Pilot
library(janitor)
d %>%
  filter(SOURCE=="PILOT") %>%
  tabyl(PRESTIGE, sort = TRUE)

# Frequencies of each rating for the Cleaner (prestige)
library(janitor)
d %>%
  filter(SOURCE=="CLEANER") %>%
  tabyl(PRESTIGE, sort = TRUE)


# Source model for prestige (Bayesian ordinal model with participant as random effect)
d<-within(d, SOURCE <- relevel(SOURCE, ref = "CLEANER"))
ptg.ordinal<-brm(PRESTIGE~SOURCE+(1|PARTICIPANT), control = list(max_treedepth = 15), family=cumulative("logit"),data=d)
summary(ptg.ordinal, prob=0.89)
marginal_effects(ptg.ordinal, categorical=TRUE)
plot(ptg.ordinal)
pairs(ptg.ordinal)

d<-within(d, SOURCE <- relevel(SOURCE, ref = "PILOT"))
ptg.ordinal<-brm(PRESTIGE~SOURCE+(1|PARTICIPANT), control = list(max_treedepth = 15), family=cumulative("logit"),data=d)
summary(ptg.ordinal, prob=0.89)

# Model comparison (Linear vs ordinal)
ptg.linear<-add_criterion(ptg.linear, "loo", reloo=TRUE)
ptg.ordinal<-add_criterion(ptg.ordinal, "loo", reloo=TRUE)
loo_compare(ptg.linear, ptg.ordinal)



# MANIPULATION CHECKS FOR RELEVANCE
# Descriptive statistics

d %>%
  group_by(SOURCE) %>%
  summarise(medianrel = median(RELEVANCE),  minrel=min(RELEVANCE), maxrel=max(RELEVANCE))
# Median: Educator = 2, Pilot=1, Cleaner = 0
# Minimum: Educator = -2, Pilot = -3, Cleaner = -3
# Maximum: Eudcator = 3, Pilot = 3, Cleaner = 3

# Frequencies of the different ratings of relevance for the Educator
library(janitor)
d %>%
  filter(SOURCE=="EDUCATOR") %>%
  tabyl(RELEVANCE, sort = TRUE)

# Frequencies of the different ratings of relevance for the Pilot
library(janitor)
d %>%
  filter(SOURCE=="PILOT") %>%
  tabyl(RELEVANCE, sort = TRUE)

# Frequencies of the different ratings of relevance for the Cleaner
library(janitor)
d %>%
  filter(SOURCE=="CLEANER") %>%
  tabyl(RELEVANCE, sort = TRUE)



# Source model for relevance (Bayesian linear model with participant as random effect)
d<-within(d, SOURCE <- relevel(SOURCE, ref = "EDUCATOR"))
rel.linear<-brm(RELEVANCE~SOURCE+(1|PARTICIPANT), control = list(max_treedepth = 15), data=d)
summary(rel.linear, prob=0.89)
marginal_effects(rel.linear)
plot(rel.linear)
pairs(rel.linear)

# Source model for relevance (Bayesian ordinal model with participant as random effect)
d<-within(d, SOURCE <- relevel(SOURCE, ref = "EDUCATOR"))
# Transform Relevance into positive ratings from 1 to 7
d<-d%>%
  mutate(RELEVANCEPOS = RELEVANCE+4)
head(d)
rel.ordinal<-brm(RELEVANCEPOS~SOURCE+(1|PARTICIPANT), control = list(max_treedepth = 15), family=cumulative("logit"),data=d)
summary(rel.ordinal, prob=0.89)
marginal_effects(rel.ordinal, categorical=TRUE)
plot(rel.ordinal)
pairs(rel.ordinal)

d<-within(d, SOURCE <- relevel(SOURCE, ref = "CLEANER"))
rel.ordinal<-brm(RELEVANCEPOS~SOURCE+(1|PARTICIPANT), control = list(max_treedepth = 15), family=cumulative("logit"),data=d)
summary(rel.ordinal, prob=0.89)

# Model comparison (Linear vs ordinal)
rel.linear<-add_criterion(ptg.linear, "loo", reloo=TRUE)
rel.ordinal<-add_criterion(ptg.ordinal, "loo", reloo=TRUE)
loo_compare(rel.linear, rel.ordinal)
