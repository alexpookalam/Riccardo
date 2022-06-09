###lmer analysis riccardo####

library("effects")
library("car")
library("dplyr")
library("ggplot2")
library("tidyverse")
library("fitdistrplus")
library("lme4")
library("glmmTMB")
library("DHARMa")
install.packages("boxcoxmix")
library("boxcoxmix")
library("MASS")
install.packages("permutes")
library("permutes")
install.packages("buildmer")
library("buildmer")
install.packages("permuco")
library("permuco")


raw =  read.csv(file.choose())
View(raw)
gat = raw %>% gather("location", "bird_n",6:11)
View(gat)
gat = gat%>%filter(!is.na(bird_n))%>%group_by(Age,Pen.ID,Time,location)%>%summarise(b_n = sum(bird_n))
View(gat)
summary(gat)

####1.1####
gat1.13 = gat%>%filter(Pen.ID %in% c("1.13"))  
View(gat1.13)
unique(gat1.13$location)
gat1.13 = gat1.13%>%mutate(area =
                     case_when(location == "X1ST.part.Long.ramp..Down." ~ "0.2645", 
                               location == "X2ND.part.Long.ramp..Up." ~ "0.2995",
                               location == "X1ST.Short.ramp" ~ "0.4544",
                               location == "X2ND.Short.ramp" ~ "0.599",
                               location == "Middle.tier" ~ "1.4944", 
                               location =="Upper.tier" ~ "2.6375"))
gat1.14 = gat%>%filter(Pen.ID %in% c("1.14"))
gat1.14 = gat1.14%>%mutate(area =
                             case_when(location == "X1ST.part.Long.ramp..Down." ~ "0.2645", 
                                       location == "X2ND.part.Long.ramp..Up." ~ "0.2995",
                                       location == "X1ST.Short.ramp" ~ "0.4544",
                                       location == "X2ND.Short.ramp" ~ "0.599",
                                       location == "Middle.tier" ~ "1.7408", 
                                       location =="Upper.tier" ~ "2.6375"))                   
gat1.1 = rbind(gat1.14, gat1.13)
View(gat1.1)
str(gat1.1)
gat1.1$area = as.numeric(gat1.1$area)
gat1.1 = gat1.1%>%mutate(across(where(is.character), as.factor))  
gat1.1$Pen.ID = as.factor(gat1.1$Pen.ID)
gat1.1= gat1.1%>%mutate(b_den = b_n/area)
summary(gat1.1)

####shortramp 1.1#### we have it
gatsr = subset(gat1.1,location%in% c("X1ST.Short.ramp", 'X2ND.Short.ramp'))
hist(gatsr$bird_n, breaks = 15)
hist(gatsr$b_den, breaks =30)

###boxplot for short ramps##

sr = ggplot(subset(gat1.1,location%in% c("X1ST.Short.ramp", 'X2ND.Short.ramp')), aes(x = as.factor(Age), 
                                                                                     y = b_den, fill = as.factor(location))) +
  geom_boxplot() #+ facet_wrap(~Pen.ID)
sr

###this
sr1_1 = lmer(b_den ~ Age + location + Age:location + I(Age^2) + I(Age^2):location + 
               (1|Pen.ID/Age),data = subset(gat1.1,location%in% c("X1ST.Short.ramp", 'X2ND.Short.ramp')))
s_sr1_1 <- simulateResiduals(fittedModel = sr1_1, plot = T)


plot(sr1_1)
testOutliers(s_sr1_1)
summary(sr1_1)
Anova(sr1_1)
plot(allEffects(sr1_1))

sr1_1area = lmer(b_den ~ Age + location + Age*location + I(area^2) +
               (1|Pen.ID/Age),data = subset(gat1.1,location%in% c("X1ST.Short.ramp", 'X2ND.Short.ramp')))
s_sr1_1area <- simulateResiduals(fittedModel = sr1_1area, plot = T)


sr1_1ss = lmer((b_den - 0.1) ~ Age + location + Age*location + 
               (1|Pen.ID/Age),data = subset(gat1.1,location%in% c("X1ST.Short.ramp", 'X2ND.Short.ramp')))
s_sr1_1ss <- simulateResiduals(fittedModel = sr1_1ss, plot = T)

sr1_1sqr = lmer((b_den)^(2) ~ Age + location + Age*location + 
                 (1|Pen.ID/Age),data = subset(gat1.1,location%in% c("X1ST.Short.ramp", 'X2ND.Short.ramp')))
s_sr1_1sqr <- simulateResiduals(fittedModel = sr1_1sqr, plot = T)


sr1_1as = lmer(sqrt(b_den - 0.001) ~ Age + location + Age*location + 
               (1|Pen.ID/Age),data = subset(gat1.1,location%in% c("X1ST.Short.ramp", 'X2ND.Short.ramp')))
s_sr1_1as <- simulateResiduals(fittedModel = sr1_1as, plot = T)
Anova(sr1_1as)

sr1_1log = lmer(log(b_den + 0.001) ~ Age + location + Age*location + 
               (1|Pen.ID/Age),data = subset(gat1.1,location%in% c("X1ST.Short.ramp", 'X2ND.Short.ramp')))
s_sr1_1log <- simulateResiduals(fittedModel = sr1_1log, plot = T)

sr1_1log10 = lmer(log10(b_den + 0.001) ~ Age + location + Age*location + 
                  (1|Pen.ID/Age),data = subset(gat1.1,location%in% c("X1ST.Short.ramp", 'X2ND.Short.ramp')))
s_sr1_1log10 <- simulateResiduals(fittedModel = sr1_1log10, plot = T)

sr1_1sqrt = lmer(sqrt(b_den) ~ Age + location + Age*location + 
                    (1|Pen.ID/Age),data = subset(gat1.1,location%in% c("X1ST.Short.ramp", 'X2ND.Short.ramp')))
s_sr1_1sqrt <- simulateResiduals(fittedModel = sr1_1log10, plot = T)

###this one works##
sr1_1log = lmer(log10(b_den - 0.001) ~ Age + location + Age*location + 
               (1|Pen.ID/Age),data = subset(gat1.1,location%in% c("X1ST.Short.ramp", 'X2ND.Short.ramp')))
simulationOutput <- simulateResiduals(fittedModel = sr1_1log, plot = T)
qqnorm(resid(sr1_1log))
qqline(resid(sr1_1log))
plot(sr1_1log)
plot(allEffects(sr1_1log))
summary(sr1_1log)
Anova(sr1_1log)

sr1_1lmeoff = lmer(b_n ~ Age + location + Age*location + offset(log(area)) + 
                  (1|Pen.ID/Age),data = subset(gat1.1,location%in% c("X1ST.Short.ramp", 'X2ND.Short.ramp')))
si_sr1_1lmeoff <- simulateResiduals(fittedModel = sr1_1lmeoff, plot = T)

sr1.1co = glmmTMB(b_n ~ Age + location + Age:location + I(Age^2) + I(Age^2):location + offset(log(area)) + 
                    (1|Pen.ID/Age), data = subset(gat1.1,location%in% c("X1ST.Short.ramp", 'X2ND.Short.ramp')), family="poisson",
                  control=glmmTMBControl(optimizer=optim,optArgs=list(method="BFGS",iter.max=1e100,eval.max=1e100)))
si_sr1.1co =  simulateResiduals(fittedModel = sr1.1co, plot = T)
plot(allEffects(sr1.1co))
summary(sr1.1co)
testZeroInflation(sr1.1co)
testDispersion(sr1.1co)
Anova(sr1.1co)

##converg prob
sr1.1coge = glmmTMB(bird_n ~ Age + location + Age*location + offset(log(area)) + 
                    (1|Pen.ID/Age), data = subset(gat1.1,location%in% c("X1ST.Short.ramp", 'X2ND.Short.ramp')), family="genpois",
                  control=glmmTMBControl(optimizer=nlminb,optArgs=list(method="Nelder-Mead",iter.max=1e100,eval.max=1e100)))
si_sr1.1coge =  simulateResiduals(fittedModel = sr1.1coge, plot = T)

##converg prob
sr1.1conb1 = glmmTMB(bird_n ~ Age + location + Age*location + offset(log(area))  +  
                      (1|Pen.ID/Age), data = subset(gat1.1,location%in% c("X1ST.Short.ramp", 'X2ND.Short.ramp')), family="nbinom1",
                    control=glmmTMBControl(optimizer=optim,optArgs=list(method="BFGS",iter.max=1e100,eval.max=1e100)))
si_sr1.1conb1 =  simulateResiduals(fittedModel = sr1.1conb1, plot = T)

##converg prob
sr1.1conb2 = glmmTMB(bird_n ~ Age + location + Age*location + offset(log(area)) + 
                       (1|Pen.ID/Age), data = subset(gat1.1,location%in% c("X1ST.Short.ramp", 'X2ND.Short.ramp')), family="nbinom1",
                     control=glmmTMBControl(optimizer=optim,optArgs=list(method="BFGS",iter.max=1e100,eval.max=1e100)))
si_sr1.1conb2 =  simulateResiduals(fittedModel = sr1.1conb2, plot = T)

####longramp 1.1#### we have it


lr = ggplot(subset(gat1.1,location%in% c("X1ST.part.Long.ramp..Down.", 'X2ND.part.Long.ramp..Up.')), aes(x = as.factor(Age), 
                                                                                  y = b_den, fill = as.factor(location))) +
  geom_boxplot() + facet_wrap(~Pen.ID)
lr


lr1_2 = lmer(b_den ~ Age + location + + Age*location + 
               (1|Pen.ID/Age),data = subset(gat1.1,location%in% c("X1ST.part.Long.ramp..Down.", 'X2ND.part.Long.ramp..Up.')))
simulationOutput <- simulateResiduals(fittedModel = lr1_2, plot = T)
qqnorm(resid(lr1_2))
qqline(resid(lr1_2))
plot(lr1_2)
Anova(lr1_2)
plot(allEffects(lr1_2))

lr1.1co = glmmTMB(bird_n ~ Age + location + Age*location + offset(log(area)) + 
                    (1|Pen.ID/Age), data = subset(gat1.1,location%in% c("X1ST.part.Long.ramp..Down.", 'X2ND.part.Long.ramp..Up.')), family="nbinom1",
                  control=glmmTMBControl(optimizer=optim,optArgs=list(method="BFGS",iter.max=1e100,eval.max=1e100)))
si_slr1.1co =  simulateResiduals(fittedModel = lr1.1co, plot = T)
plot(allEffects(lr1.1co))

lr1_2qq = lmer(b_den ~ Age + location + + Age*location + I(Age^2) + I(Age^2):location +
               (1|Pen.ID/Age),data = subset(gat1.1,location%in% c("X1ST.part.Long.ramp..Down.", 'X2ND.part.Long.ramp..Up.')))
simulationOutputqq <- simulateResiduals(fittedModel = lr1_2qq, plot = T)




####1.1 middel and upper tier###

##nope, AIC 3005.1
tr1.1co = glmmTMB(b_n ~ Age + location + Age*location + offset(log(area)) + 
                    (1|Pen.ID/Age),data = subset(gat1.1,location%in% c("Middle.tier", 'Upper.tier')), family="poisson",
                  control=glmmTMBControl(optimizer=optim,optArgs=list(method="BFGS",iter.max=1e100,eval.max=1e100)))
si_tr1.1co =  simulateResiduals(fittedModel = tr1.1co, plot = T)
testDispersion(si_tr1.1co)
testZeroInflation(si_tr1.1co)
testOutliers(si_tr1.1co,type = 'bootstrap')
plot(allEffects(tr1.1co))
summary(tr1.1co)

##nope, cant compute Aic
tr1.1conb1 = glmmTMB(b_n ~ Age + location + Age*location + offset(log(area)) + 
                    (1|Pen.ID/Age),ziformula = ~Age*location,data = subset(gat1.1,location%in% c("Middle.tier", 'Upper.tier')), family="nbinom1",
                  control=glmmTMBControl(optimizer=optim,optArgs=list(method="BFGS",iter.max=1e100,eval.max=1e100)))
si_tr1.1nb1 =  simulateResiduals(fittedModel = tr1.1conb1, plot = T)
testDispersion(si_tr1.1nb1)
testZeroInflation(si_tr1.1nb1)
summary(tr1.1conb1)

str(gat1.1)
##nope, AIC - 2640.5
tr1.1genp = glmmTMB(b_n ~ Age + location + Age*location + offset(log(area)) + 
                       (1|Pen.ID/Age),data = subset(gat1.1,location%in% c("Middle.tier", 'Upper.tier')), family="genpois",
                     control=glmmTMBControl(optimizer=nlminb,optArgs=list(method="BFGS",iter.max=1e100,eval.max=1e100)))
si_tr1.1genp =  simulateResiduals(fittedModel = tr1.1genp, plot = T)
summary(tr1.1genp)

plotResiduals(si_tr1.1genp, form = subset(gat1.1,location%in% c("Middle.tier", 'Upper.tier'))$location )


##nope, Aic 2666.6
tr1.1nb2 = glmmTMB(b_n ~ Age + location + Age*location + offset(log(area)) + 
                      (1|Pen.ID/Age),ziformula = ~Age*location,data = subset(gat1.1,location%in% c("Middle.tier", 'Upper.tier')), family="poisson",
                    control=glmmTMBControl(optimizer=optim,optArgs=list(method="BFGS",iter.max=1e100,eval.max=1e100)))
si_tr1.1nb2 =  simulateResiduals(fittedModel = tr1.1nb2, plot = T)
summary(tr1.1nb2)

##ziformula = ~Age
tr1.1poi = glmmTMB(b_n ~ Age + location + Age*location + offset(log(area)) + 
                     (1|Pen.ID), ziformula = ~Age,data = subset(gat1.1,location%in% c("Middle.tier", 'Upper.tier')), family="genpois",
                   control=glmmTMBControl(optimizer=optim,optArgs=list(method="BFGS",iter.max=1e100,eval.max=1e100)))
si_tr1.1poi =  simulateResiduals(fittedModel = tr1.1poi, plot = T)
summary(tr1.1poi)

#ziformula = ~Age*location
tr1.1genpo2 = glmmTMB(b_n ~ Age + location + Age*location + offset(log(area)) + 
                     (1|Pen.ID/Age), ziformula = ~Age*location,data = subset(gat1.1,location%in% c("Middle.tier", 'Upper.tier')), family="genpois",
                   control=glmmTMBControl(optimizer=optim,optArgs=list(method="BFGS",iter.max=1e100,eval.max=1e100)))
si_tr1.1genpo2 =  simulateResiduals(fittedModel = tr1.1genpo2, plot = T)
summary(tr1.1genpo2)


##ziformula = ~location
tr1.1genpo3 = glmmTMB(b_n ~ Age + location + Age*location + offset(log(area)) + 
                        (1|Pen.ID/Age), ziformula = ~location,data = subset(gat1.1,location%in% c("Middle.tier", 'Upper.tier')), family="genpois",
                      control=glmmTMBControl(optimizer=optim,optArgs=list(method="BFGS",iter.max=1e100,eval.max=1e100)))
si_tr1.1genpo3 =  simulateResiduals(fittedModel = tr1.1genpo3, plot = T)
summary(tr1.1genpo3)


##ziformula = ~1
tr1.1genpo4 = glmmTMB(b_n ~ Age + location + Age*location + offset(log(area)) + 
                        (1|Pen.ID/Age), ziformula = ~1,data = subset(gat1.1,location%in% c("Middle.tier", 'Upper.tier')), family="genpois",
                      control=glmmTMBControl(optimizer=optim,optArgs=list(method="BFGS",iter.max=1e100,eval.max=1e100)))
si_tr1.1genpo4 =  simulateResiduals(fittedModel = tr1.1genpo4, plot = T)
summary(tr1.1genpo4)



tier_1.1 = lmer(b_den  ~  Age*location  +
                  (1|Pen.ID/Age),data = subset(gat1.1,location%in% c("Middle.tier", 'Upper.tier')))
si_tier_1.1 =  simulateResiduals(fittedModel = tier_1.1, plot = T)

tier_1.1tr = lmer(sqrt(b_den)  ~  Age*location  +
               (1|Pen.ID/Age),data = subset(gat1.1,location%in% c("Middle.tier", 'Upper.tier')))
si_tier_1.1tr =  simulateResiduals(fittedModel = tier_1.1tr, plot = T)
summary(tier_1.1tr)

tier_1.1trs = lmer(log(b_den  -0.01)  ~  Age*location  +
                    (1|Pen.ID/Age),data = subset(gat1.1,location%in% c("Middle.tier", 'Upper.tier')))
si_tier_1.1s =  simulateResiduals(fittedModel = tier_1.1trs, plot = T)


tier_1.1trp = lmer((b_den)^2  ~  Age*location  +
                     (1|Pen.ID/Age),data = subset(gat1.1,location%in% c("Middle.tier", 'Upper.tier')))
si_tier_1.1p =  simulateResiduals(fittedModel = tier_1.1trp, plot = T)

tier_1.1in= lmer((1/(b_den + 0.01)) ~  Age*location  +
                     (1|Pen.ID/Age),data = subset(gat1.1,location%in% c("Middle.tier", 'Upper.tier')))
si_tier_1.1in =  simulateResiduals(fittedModel = tier_1.1in, plot = T)

tier_1.1ad= lmer((b_den + 0.01) ~  Age*location  +
                   (1|Pen.ID/Age),data = subset(gat1.1,location%in% c("Middle.tier", 'Upper.tier')))
si_tier_1.1ad =  simulateResiduals(fittedModel = tier_1.1ad, plot = T)

tier_1.1sub= lmer((b_den - 0.01) ~  Age*location  +
                   (1|Pen.ID/Age),data = subset(gat1.1,location%in% c("Middle.tier", 'Upper.tier')))
si_tier_1.1sub =  simulateResiduals(fittedModel = tier_1.1sub, plot = T)


tier_1.1cur = lmer((b_den)^(1/3)  ~  Age*location  +
                     (1|Pen.ID/Age),data = subset(gat1.1,location%in% c("Middle.tier", 'Upper.tier')))
si_tier_1.1cur =  simulateResiduals(fittedModel = tier_1.1cur, plot = T)
summary(tier_1.1cur)

plot(tier_1.1)
summary(tier_1.1)
Anova(tier_1.1)
plot(allEffects(tier_1.1))

mu_3 <- glmer(b_n ~ Age + location + Age*location + 
                (1|Pen.ID/Age) + offset(log(area)), family = "poisson", data = subset(gat1.1,location%in% c("Middle.tier", 'Upper.tier')))
summary(mu_3)
qqnorm(resid(mu_1))
qqline(resid(mu_1))
shapiro.test(resid(mu_1))  # p-value = 2.2e-16
summary(mu_1)
plot(mu_1)
si_mu_3 <- simulateResiduals(fittedModel = mu_3, plot = T)

perms <- perm.lmer(b_den ~  Age*location + (1|Pen.ID/Age),
                   data = subset(gat1.1,location%in% c("Middle.tier", 'Upper.tier')))
permlme <- permmodels(model=fm, data=Oats, block="Block", group="Variety", nsim=999)

View(subset(gat1.1,location%in% c("Middle.tier", 'Upper.tier')))

####1.2####

gat1.2 = gat%>%filter(Pen.ID %in% c("1.21", "1.22"))  
View(gat1.2)
unique(gat1.2$location)
gat1.2 = gat1.2%>%mutate(area =
                             case_when(location == "X1ST.part.Long.ramp..Down." ~ "0.25275", 
                                       location == "X2ND.part.Long.ramp..Up." ~ "0.31125",
                                       location == "X1ST.Short.ramp" ~ "0.459",
                                       location == "X2ND.Short.ramp" ~ "0.6225",
                                       location == "Middle.tier" ~ "2.327975", 
                                       location =="Upper.tier" ~ "2.66625"))
View(gat1.2)
str(gat1.2)
summary(gat1.2)
gat1.2$area = as.numeric(gat1.2$area)
gat1.2 = gat1.2%>%mutate(across(where(is.character), as.factor))  
gat1.2$Pen.ID = as.factor(gat1.2$Pen.ID)
gat1.2= gat1.2%>%mutate(b_den = b_n/area)

####1.2 short ramps#### we have it 
gat12sr = subset(gat1.2,location%in% c("X1ST.Short.ramp", 'X2ND.Short.ramp'))
hist(gat12sr$bird_n, breaks = 15)
hist(gat12sr$b_den, breaks =30)
sr1_2 = lmer(b_den ~ Age + location + Age*location + 
               (1|Pen.ID/Age),data = subset(gat1.2,location%in% c("X1ST.Short.ramp", 'X2ND.Short.ramp')))
si_sr1_2 =  simulateResiduals(fittedModel = sr1_2, plot = T)

sr1_2co = glmmTMB(bird_n ~ Age + location + Age*location + offset(log(area)) + 
                    (1|Pen.ID/Age),
                  data = subset(gat1.2,location%in% c("X1ST.Short.ramp", 'X2ND.Short.ramp')), family="genpois",
                  control=glmmTMBControl(optimizer=optim,optArgs=list(method="BFGS",iter.max=1e100,eval.max=1e100)))
si_sr1_2co =  simulateResiduals(fittedModel = sr1_2co, plot = T)
testDispersion(si_sr1_2co)
plot(allEffects(sr1_2co))
Anova(sr1_2co)

####1.2 long ramps####

lr1_2co = glmmTMB(bird_n ~ Age + location + Age*location + offset(log(area)) + 
                    (1|Pen.ID/Age),
                  data = subset(gat1.1,location%in% c("X1ST.part.Long.ramp..Down.", 'X2ND.part.Long.ramp..Up.')), family="nbinom1",
                  control=glmmTMBControl(optimizer=optim,optArgs=list(method="BFGS",iter.max=1e100,eval.max=1e100)))
si_lr1_2co =  simulateResiduals(fittedModel = lr1_2co, plot = T)
Anova(lr1_2co)
plot(allEffects(lr1_2co))

####1.2 tiers####
gat1.2tr = subset(gat1.2,location%in% c("Middle.tier", 'Upper.tier'))
View(gat1.2tr)
hist(gat1.2tr$b_n, breaks = 50)

#nope
tier1_2co = glmmTMB(b_n ~ Age + location + Age*location + offset(log(area)) + 
                    (1|Pen.ID/Age),
                  data = subset(gat1.2,location%in% c("Middle.tier", 'Upper.tier')), family="nbinom1",
                  control=glmmTMBControl(optimizer=optim,optArgs=list(method="BFGS",iter.max=1e100,eval.max=1e100)))
si_tier1_2co =  simulateResiduals(fittedModel = tier1_2co, plot = T)
testDispersion(si_tier1_2co)
testZeroInflation(si_tier1_2co)

#nope
tier1_2copo = glmmTMB(b_n ~ Age + location + Age*location + offset(log(area)) + 
                      (1|Pen.ID/Age),
                    data = subset(gat1.2,location%in% c("Middle.tier", 'Upper.tier')), family="poisson",
                    control=glmmTMBControl(optimizer=optim,optArgs=list(method="BFGS",iter.max=1e100,eval.max=1e100)))
si_tier1_2copo =  simulateResiduals(fittedModel = tier1_2copo, plot = T)

#nope
tier1_2cogp = glmmTMB(b_n ~ Age + location + Age*location + offset(log(area)) + 
                        (1|Pen.ID/Age),
                      data = subset(gat1.2,location%in% c("Middle.tier", 'Upper.tier')), family="genpois",
                      control=glmmTMBControl(optimizer=optim,optArgs=list(method="BFGS",iter.max=1e100,eval.max=1e100)))
si_tier1_2cogp =  simulateResiduals(fittedModel = tier1_2cogp, plot = T)


tier1.2lm = lmer(log(b_den + 0.18) ~ Age + location + Age*location +
                   (1|Pen.ID/Age),
                 data = subset(gat1.2,location%in% c("Middle.tier", 'Upper.tier')))
si_tier1.2lm =  simulateResiduals(fittedModel = tier1.2lm, plot = T)

gat1.2trm = gat1.2tr%>%mutate(b_dennew = (b_den +0.001))

##boxcox####
tier1.2dummy = lm(b_dennew ~ 1,
                 data = subset(gat1.2trm,location%in% c("Middle.tier", 'Upper.tier')))
bc <- boxcox(tier1.2dummy,  plotit = TRUE)

# Exact lambda
lambda <- bc$x[which.max(bc$y)]
lambda
gat1.2trm <- gat1.2trm%>%mutate(new_bden = (b_dennew ^ lambda - 1)/lambda)
View(gat1.2trm)

tier1.2bocox = lmer(new_bden~ Age + location + Age*location +
                   (1|Pen.ID/Age),
                 data = gat1.2trm)
si_tier1.2bocox =  simulateResiduals(fittedModel = tier1.2bocox, plot = T)
