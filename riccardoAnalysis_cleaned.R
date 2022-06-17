###analysis riccardo's thesis####

library("effects")
library("car")
library("dplyr")
library("ggplot2")
library("tidyverse")
library("fitdistrplus")
library("lme4")
library("glmmTMB")
library("DHARMa")
library("emmeans")
install.packages("AICcmodavg")
library("AICcmodavg")
install.packages("boxcoxmix")
library("boxcoxmix")
library("MASS")
install.packages("permutes")
library("permutes")
install.packages("buildmer")
library("buildmer")
install.packages("permuco")
library("permuco")
install.packages("robustlmm")
library("robustlmm")
install.packages("sjPlot")
library("sjPlot")


raw =  read.csv(file.choose())
View(raw)
gat = raw %>% gather("location", "bird_n",6:11)
View(gat)
gat = gat%>%filter(!is.na(bird_n))%>%group_by(Age,Pen.ID,Time,location)%>%summarise(b_n = sum(bird_n))
View(gat)
table(gat$Age, gat$location)
summary(gat)

####1.1####
gat1.13 = gat%>%filter(Pen.ID %in% c("1.13"))  
View(gat1.13)
unique(gat1.13$location)

##adding area column
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
table(gat1.1$Age, gat1.1$location)


####shortramp 1.1#### 

###boxplot for short ramps##
sr = ggplot(subset(gat1.1,location%in% c("X1ST.Short.ramp", 'X2ND.Short.ramp')), aes(x = as.factor(Age), 
                                                                                     y = b_den, fill = as.factor(location))) +
  geom_boxplot() #+ facet_wrap(~Pen.ID)
sr

##this one is ok## a bit a model assumption violations
sr1_1 = lmer(b_den ~ Age + location + Age*location + 
               (1|Pen.ID/Age),data = subset(gat1.1,location%in% c("X1ST.Short.ramp", 'X2ND.Short.ramp')))
s_sr1_1 <- simulateResiduals(fittedModel = sr1_1, plot = T)
plot(sr1_1)
testOutliers(s_sr1_1)
summary(sr1_1)
Anova(sr1_1)
plot(allEffects(sr1_1))

#linear model##this one is works##

sr1_1log = lmer(log10(b_den - 0.001) ~ Age + location + Age*location + 
                  (1|Pen.ID/Age),data = subset(gat1.1,location%in% c("X1ST.Short.ramp", 'X2ND.Short.ramp')))
simulationOutput <- simulateResiduals(fittedModel = sr1_1log, plot = T)
qqnorm(resid(sr1_1log))
qqline(resid(sr1_1log))
plot(sr1_1log)
plot(allEffects(sr1_1log))
summary(sr1_1log)
Anova(sr1_1log)

##offset function model, glmmtmb## model assumption problem
sr1.1co = glmmTMB(b_n ~ Age + location + Age*location + offset(log(area)) +
                    (1|Pen.ID/Age), data = subset(gat1.1,location%in% c("X1ST.Short.ramp", 'X2ND.Short.ramp')), family="poisson",
                  control=glmmTMBControl(optimizer=optim,optArgs=list(method="BFGS",iter.max=1e100,eval.max=1e100)))
si_sr1.1co =  simulateResiduals(fittedModel = sr1.1co, plot = T)
plot(allEffects(sr1.1co))
summary(sr1.1co)
testZeroInflation(sr1.1co)
testDispersion(sr1.1co)
Anova(sr1.1co)

##quadratic model #Aic 2040.2 ##FINAL MODEL

sr1_1 = lmer(b_den ~ Age + location + I(Age^2) + Age:location + I(Age^2):location + 
               (1|Pen.ID/Age),data = subset(gat1.1,location%in% c("X1ST.Short.ramp", 'X2ND.Short.ramp')))
summary(sr1_1)
s_sr1_1 <- simulateResiduals(fittedModel = sr1_1, plot = T)
plot(allEffects(sr1_1))
Anova(sr1_1)
anova(sr1_1,sr1_1log)
AICc(sr1_1,sr1_1log)

##posthoc comparison
bv3 = emtrends(sr1_1, specs= pairwise~location, var = "I(Age^2)", type = "response")
bv3

##make new ref grid
sr1_1.rg = ref_grid(sr1_1, at = list(Age = c(8,9,10,11,12,13,14, 15, 16)))

##plot model prediction
emmip(sr1_1.rg,location~poly(Age,degree = 2) , CIs = TRUE,
      linearg = list(linetype = "dashed"), CIarg = list(lwd = 1, alpha = 1), type = "response")


####longramp 1.1#### 

###boxplot raw data##

lr = ggplot(subset(gat1.1,location%in% c("X1ST.part.Long.ramp..Down.", 'X2ND.part.Long.ramp..Up.')), aes(x = as.factor(Age), 
                                                                                                         y = b_den, fill = as.factor(location))) +
  geom_boxplot() #+ facet_wrap(~Pen.ID)
lr

##linearmodel##model assumption problem
lr1_2 = lmer((b_den) ~ Age + location + + Age*location + 
               (1|Pen.ID/Age),data = subset(gat1.1,location%in% c("X1ST.part.Long.ramp..Down.", 'X2ND.part.Long.ramp..Up.')))
simulationOutput <- simulateResiduals(fittedModel = lr1_2, plot = T)
qqnorm(resid(lr1_2))
qqline(resid(lr1_2))
plot(lr1_2)
Anova(lr1_2)
plot(allEffects(lr1_2))

##offset function model, glmmtmb##
lr1.1co = glmmTMB(b_n ~ Age + location + Age*location + offset(log(area)) + 
                    (1|Pen.ID/Age), data = subset(gat1.1,location%in% c("X1ST.part.Long.ramp..Down.", 'X2ND.part.Long.ramp..Up.')), family="nbinom1",
                  control=glmmTMBControl(optimizer=optim,optArgs=list(method="BFGS",iter.max=1e100,eval.max=1e100)))
si_slr1.1co =  simulateResiduals(fittedModel = lr1.1co, plot = T)
plot(allEffects(lr1.1co))


###quadratic effects#####model assumption problem

lr1_2q = lmer((b_den) ~ Age + location + + Age*location + I(Age^2) + I(Age^2):location  +
                 (1|Pen.ID/Age),data = subset(gat1.1,location%in% c("X1ST.part.Long.ramp..Down.", 'X2ND.part.Long.ramp..Up.')))
simulationOutputq <- simulateResiduals(fittedModel = lr1_2q, plot = T)
summary(lr1_2q)
Anova(lr1_2q)

###quadratic effect model with transformation to solve assumptions problems####FINAL MODEL

lr1_2qq = lmer(b_den^(2/3) ~ Age + location + Age:location + I(Age^2) + I(Age^2):location  +
                 (1|Pen.ID/Age),data = subset(gat1.1,location%in% c("X1ST.part.Long.ramp..Down.", 'X2ND.part.Long.ramp..Up.')))
simulationOutputqq <- simulateResiduals(fittedModel = lr1_2qq, plot = T)
summary(lr1_2qq)
Anova(lr1_2qq)
plot(allEffects(lr1_2qq))
wg = plot(effect("I(Age^2):location", lr1_2qq))
wg

ptran = make.tran("power", 2/3) ### specify the transformation

lr1_2qq.rg = ref_grid(lr1_2qq, at = list(Age = c(8,9,10,11,12,13,14, 15, 16))) ###update the reference grid

##plot the model predictions
emmip(lr1_2qq.rg,location~poly(Age,degree = 2) , CIs = TRUE,
      linearg = list(linetype = "dashed"), CIarg = list(lwd = 1, alpha = 1), tran = ptran,type = "response")  

##posthoc comparison
bv4 = emtrends(lr1_2qq, specs= pairwise~location, var = "I(Age^2)", type = "response")
bv4

####1.1 middel and upper tier####

tie = ggplot(subset(gat1.1,location%in% c('Middle.tier', 'Upper.tier')), aes(x = as.factor(Age), 
                                                                             y = b_den, fill = as.factor(location))) +
  geom_boxplot() #+  facet_wrap(~Pen.ID)
tie

##linear model##bad assump
tier_1.1 = lmer(b_den  ~  Age*location  +
                  (1|Pen.ID/Age),data = subset(gat1.1,location%in% c("Middle.tier", 'Upper.tier')))
si_tier_1.1 =  simulateResiduals(fittedModel = tier_1.1, plot = T)
###tried many transformtions for the same modelbut model assumptions remained bad###

##quadratic model. bad assummption
tier_1.1q = lmer((b_den)  ~  Age + location + Age:location + I(Age^2) + I(Age^2):location   +
                    (1|Pen.ID/Age),data = subset(gat1.1,location%in% c("Middle.tier", 'Upper.tier')))
si_tier_1.1q =  simulateResiduals(fittedModel = tier_1.1q, plot = T)
Anova(tier_1.1q)
plot(allEffects(tier_1.1q))

####quadratic model with transformation ##FINAL MODEL##

View(subset(gat1.1,location%in% c("Middle.tier", 'Upper.tier')))
hist(subset(gat1.1,location%in% c("Middle.tier", 'Upper.tier'))$b_den, breaks = 50, group = "Age")
tier_1.1qq = lmer((b_den)^(2/3)  ~  Age + location + Age:location + I(Age^2) + I(Age^2):location   +
                    (1|Pen.ID/Age),data = subset(gat1.1,location%in% c("Middle.tier", 'Upper.tier')))
si_tier_1.1qq =  simulateResiduals(fittedModel = tier_1.1qq, plot = T)
###eventhough model assumptions are violated, the model predictions look true to the raw data, so I think this model is ok!
plotResiduals(si_tier_1.1qq , subset(gat1.1,location%in% c("Middle.tier", 'Upper.tier'))$Age)
plotResiduals(si_tier_1.1qq , subset(gat1.1,location%in% c("Middle.tier", 'Upper.tier'))$location)
plot(tier_1.1qq)
Anova(tier_1.1qq)
plot(allEffects(tier_1.1qq))
qtran = make.tran("power", 2/3)

##make new ref grid
tier_1.1qq.rg = ref_grid(tier_1.1qq, at = list(Age = c(8,9,10,11,12,13,14, 15, 16)))


##plotmodel estimates
emmip(tier_1.1qq.rg,location~I(Age^2) , CIs = TRUE,
      linearg = list(linetype = "dashed"), tran = qtran, type = "response")

##posthoc comparison
bv5 = emtrends(tier_1.1qq, specs= pairwise~location, var = "I(Age^2)", type = "response")
bv5

###count model###
### bas assump for poisson, genpois, nbinom1, nbinom2#

tr1.1genpo2 = glmmTMB(b_n ~ Age + location + Age:location + I(Age^2) + I(Age^2):location  + offset(log(area)) + 
                        (1|Pen.ID/Age), data = subset(gat1.1,location%in% c("Middle.tier", 'Upper.tier')), family="nbinom2",
                      control=glmmTMBControl(optimizer=optim,optArgs=list(method="BFGS",iter.max=1e100,eval.max=1e100)))
si_tr1.1genpo2 =  simulateResiduals(fittedModel = tr1.1genpo2, plot = T)
summary(tr1.1genpo2)
testZeroInflation(si_tr1.1genpo2)

##count model with ziformula parameter = ~Age*location
### bas assump for poisson (Aic = 2906.2), genpois (2646.1), nbinom1 (conveg prob), nbinom2 (2670.8)#
tr1.1couzi = glmmTMB(b_n ~ Age + location + Age:location + I(Age^2) +  I(Age^2):location  + offset(log(area)) +
                       (1|Pen.ID/Age),ziformula = ~Age*location, data = subset(gat1.1,location%in% c("Middle.tier", 'Upper.tier')), family="nbinom2",
                     control=glmmTMBControl(optimizer=optim,optArgs=list(method="BFGS",iter.max=1e100,eval.max=1e100)))
si_tr1.1couzi =  simulateResiduals(fittedModel = tr1.1couzi, plot = T)
summary(tr1.1couzi)
testZeroInflation(tr1.1couzi)

##bird density as response variable and gaussian as family
##gaussain no convergence, no convergence with multiple transformtaions as well
tr1.1couga = glmmTMB((b_den)^(2/3) ~ Age + location + Age:location + I(Age^2) + I(Age^2):location  +  
                        (1|Pen.ID/Age),ziformula = ~location,data = subset(gat1.1,location%in% c("Middle.tier", 'Upper.tier')), family="gaussian",
                      control=glmmTMBControl(optimizer=nlminb,optArgs=list(method="CG",iter.max=1e100,eval.max=1e100)))
si_tr1.1couga =  simulateResiduals(fittedModel = tr1.1couga, plot = T)
summary(tr1.1couga)
testZeroInflation(tr1.1couga)

##bird density as response variable and gaussian as family and ziformula = ~Age
##no convergence, no convergence with multiple transformtaions as well
tr1.1cougazi = glmmTMB((b_den) ~ Age + location + Age:location + I(Age^2) + I(Age^2):location  +  
                       (1|Pen.ID/Age),ziformula = ~location,data = subset(gat1.1,location%in% c("Middle.tier", 'Upper.tier')), family="gaussian",
                     control=glmmTMBControl(optimizer=optim,optArgs=list(method="Nelder-Mead",iter.max=1e100,eval.max=1e100)))
si_tr1.1cougazi =  simulateResiduals(fittedModel = tr1.1cougazi, plot = T)
summary(tr1.1cougazi)
testZeroInflation(tr1.1cougazi)


####
tr1.1couzi = glmmTMB(b_n ~ Age + location + Age:location + I(Age^2) +  I(Age^2):location  +
                        (1|Pen.ID/Age),ziformula = ~Age*location,data = subset(gat1.1,location%in% c("Middle.tier", 'Upper.tier')), family="nbinom2",
                      control=glmmTMBControl(optimizer=optim,optArgs=list(method="BFGS",iter.max=1e100,eval.max=1e100)))
si_tr1.1couzi =  simulateResiduals(fittedModel = tr1.1couzi, plot = T)
summary(tr1.1couzi)
testZeroInflation(tr1.1couzi)

##count model with ziformula parameter = ~Age
### bas assump for poisson, genpois, nbinom1, nbinom2#



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

####1.2 short ramps#### 
gat12sr = subset(gat1.2,location%in% c("X1ST.Short.ramp", 'X2ND.Short.ramp'))
hist(gat12sr$bird_n, breaks = 15)
hist(gat12sr$b_den, breaks =30)

rslr = ggplot(subset(gat1.2,location%in% c("X1ST.Short.ramp", 'X2ND.Short.ramp')), aes(x = as.factor(Age), 
                                                                                    y = b_den, fill = as.factor(location))) +
  geom_boxplot() #+ facet_wrap(~Pen.ID)
rslr


##model assumption problem
sr1_2 = lmer((b_den)^(1/3) ~ Age + location + Age*location + 
               (1|Pen.ID/Age),data = subset(gat1.2,location%in% c("X1ST.Short.ramp", 'X2ND.Short.ramp')))
si_sr1_2 =  simulateResiduals(fittedModel = sr1_2, plot = T)

##offset function model, glmmtmb##
sr1_2co = glmmTMB(b_n ~ Age + location + Age*location + offset(log(area)) + 
                    (1|Pen.ID/Age),
                  data = subset(gat1.2,location%in% c("X1ST.Short.ramp", 'X2ND.Short.ramp')), family="genpois",
                  control=glmmTMBControl(optimizer=optim,optArgs=list(method="BFGS",iter.max=1e100,eval.max=1e100)))
si_sr1_2co =  simulateResiduals(fittedModel = sr1_2co, plot = T)
summary(sr1_2co)
testDispersion(si_sr1_2co)
plot(allEffects(sr1_2co))
Anova(sr1_2co)

##quadratic model

sr1_2coqq = lmer(sqrt(b_den)  ~  Age + location + Age:location + I(Age^2) + I(Age^2):location   +
                    (1|Pen.ID/Age),data = subset(gat1.2,location%in% c("X1ST.Short.ramp", 'X2ND.Short.ramp')))
si_sr1_2coqq =  simulateResiduals(fittedModel = sr1_2coqq, plot = T)

summary(sr1_2coqq)
plot(sr1_2coqq)
Anova(sr1_2coqq)
plot(allEffects(sr1_2coqq))


##make new ref grid
sr1_2coqq.rg = ref_grid(sr1_2coqq, at = list(Age = c(8,9,10,11,12,13,14, 15, 16)))


##plotmodel estimates
emmip(sr1_2coqq.rg,location~I(Age^2) , CIs = TRUE,
      linearg = list(linetype = "dashed"), type = "response")

##posthoc comparison
bv6 = emtrends(sr1_2coqq, specs= pairwise~location, var = "I(Age^2)", type = "response")
bv6

####1.2 long ramps####


lrlr = ggplot(subset(gat1.2,location%in% c("X1ST.part.Long.ramp..Down.", 'X2ND.part.Long.ramp..Up.')), aes(x = as.factor(Age), 
                                                                                       y = b_den, fill = as.factor(location))) +
  geom_boxplot() #+ facet_wrap(~Pen.ID)
lrlr

##count model 
lr1_2co = glmmTMB(b_n ~ Age + location + Age*location + offset(log(area)) + 
                    (1|Pen.ID/Age),
                  data = subset(gat1.2,location%in% c("X1ST.part.Long.ramp..Down.", 'X2ND.part.Long.ramp..Up.')), family="nbinom1",
                  control=glmmTMBControl(optimizer=optim,optArgs=list(method="BFGS",iter.max=1e100,eval.max=1e100)))
si_lr1_2co =  simulateResiduals(fittedModel = lr1_2co, plot = T)
summary(lr1_2co)
Anova(lr1_2co)
plot(allEffects(lr1_2co))


##quadratic model

lr1_2qq = lmer((b_den)  ~  Age + location + Age:location + I(Age^2) + I(Age^2):location   +
                   (1|Pen.ID/Age),subset(gat1.2,location%in% c("X1ST.part.Long.ramp..Down.", 'X2ND.part.Long.ramp..Up.')))
si_lr1_2qq =  simulateResiduals(fittedModel = lr1_2qq, plot = T)

summary(lr1_2qq)
plot(lr1_2qq)
Anova(lr1_2qq)
plot(allEffects(lr1_2qq))

##make new ref grid
lr1_2qq.rg = ref_grid(lr1_2qq, at = list(Age = c(8,9,10,11,12,13,14, 15, 16)))

##plotmodel estimates
emmip(lr1_2qq.rg ,location~I(Age^2) , CIs = TRUE,
      linearg = list(linetype = "dashed"), type = "response")

##posthoc comparison
bv7 = emtrends(lr1_2qq, specs= pairwise~location, var = "I(Age^2)", type = "response")
bv7

####1.2 tiers####
hist(subset(gat1.2,location%in% c("Middle.tier", 'Upper.tier'))$b_den, breaks = 50, group = "Age")

gat12tr = subset(gat1.2,location%in% c("Middle.tier", 'Upper.tier'))
View(gat12tr)
trlr = ggplot(subset(gat1.2,location%in% c("Middle.tier", 'Upper.tier')), aes(x = as.factor(Age), 
                                                                 y = b_den, fill = as.factor(location))) +
  geom_boxplot() #+ facet_wrap(~Pen.ID)
trlr

###quadratic interaction

###add a new column of scaled data of b_den###
gat12tr["b_densc"] = scale(gat12tr$b_den, center =  TRUE, scale = TRUE)
hist(gat12tr$b_densc)
View(gat12tr)


##well it was an idea, but model assumptions are still a prob###
tier1_2qs = lmer((b_densc)^(1/2) ~  Age + location + Age:location + I(Age^2) + I(Age^2):location +
                 (1|Pen.ID/Age),gat12tr)
si_tier1_2qs =  simulateResiduals(fittedModel = tier1_2qs, plot = T)

####
tier1_2qq = lmer((b_den) ~  Age + location + Age:location + I(Age^2) + I(Age^2):location +
                  (1|Pen.ID/Age),gat12tr)
si_tier1_2qq =  simulateResiduals(fittedModel = tier1_2qq, plot = T)

plot(tier1_2qq)
testZeroInflation(tier1_2qq) ###four zeros is not zero inflation
plot(allEffects(tier1_2qq))
plotResiduals(si_tier1_2qq ,gat12tr$Age)
plotResiduals(si_tier1_2qq , gat12tr$location)
table(subset(gat1.2,location%in% c("Middle.tier", 'Upper.tier'))$location,subset(gat1.2,location%in% c("Middle.tier", 'Upper.tier'))$Age)

####robust lmm####

robust.model <- rlmer((b_den) ~  Age + location + Age:location + I(Age^2) + I(Age^2):location +
                        (1|Pen.ID/Age),gat12tr)
si_robust.model =  simulateResiduals(fittedModel = robust.model, plot = T)
summary(robust.model)
tab_model(robust.model)
Anova(robust.model)

# get coefficients from non-robust model to extract Satterthwaite approximated DFs
coefs <- data.frame(coef(summary(robust.model)))
coefs
# get coefficients from robust model to extract t-values
coefs.robust <- data.frame(coef(summary(robust.model)))
coefs.robust
str(coefs.robust)
# calculate p-values based on robust t-values and non-robust approx. DFs
p.values <- 2*pt(abs(as.numeric(coefs.robust[,3])), coefs$df, lower=FALSE)
p.values

coefs$df

##make new ref grid
robust.model.rg = ref_grid(robust.model, at = list(Age = c(8,9,10,11,13,14, 15, 16)))


##plotmodel estimates
emmip(robust.model.rg,location~I(Age^2) , CIs = TRUE,
      linearg = list(linetype = "dashed"), type = "response")

##posthoc comparison
bv8 = emtrends(robust.model, specs= pairwise~location, var = "I(Age^2)", type = "response")
bv8


####
tier1_2co = glmmTMB(b_n ~ Age + location + Age*location + Age:location + I(Age^2) + I(Age^2):location +
                      (1|Pen.ID/Age),
                    data = gat12tr, family="nbinom1",
                    control=glmmTMBControl(optimizer=optim,optArgs=list(method="BFGS",iter.max=1e100,eval.max=1e100)))
si_tier1_2co =  simulateResiduals(fittedModel = tier1_2co, plot = T)
testDispersion(si_tier1_2co)
testZeroInflation(si_tier1_2co)


###covergence prob
tier1_2cgu = glmmTMB(sqrt(b_densc) ~  Age + location + Age:location + I(Age^2) + I(Age^2):location +
                      (1|Pen.ID),
                    data = gat12tr, family="gaussian",
                    control=glmmTMBControl(optimizer=optim,optArgs=list(method="BFGS",iter.max=1e100,eval.max=1e100)))
si_tier1_2cgu =  simulateResiduals(fittedModel = tier1_2cgu, plot = T)
testDispersion(si_tier1_2co)
testZeroInflation(si_tier1_2cgu)
plotResiduals(si_tier1_2cgu , subset(gat1.2,location%in% c("Middle.tier", 'Upper.tier'))$Age)
plotResiduals(si_tier1_2cgu , subset(gat1.2,location%in% c("Middle.tier", 'Upper.tier'))$location)





plot(tier_1.1qq)
Anova(tier_1.1qq)
plot(allEffects(tier_1.1qq))
qtran = make.tran("power", 2/3)

##make new ref grid
tier_1.1qq.rg = ref_grid(tier_1.1qq, at = list(Age = c(8,9,10,11,12,13,14, 15, 16)))


##plotmodel estimates
emmip(tier_1.1qq.rg,location~I(Age^2) , CIs = TRUE,
      linearg = list(linetype = "dashed"), tran = qtran, type = "response")

