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

####shortramp 1.1#### 

##this one is ok##
sr1_1 = lmer(b_den ~ Age + location + Age*location + 
               (1|Pen.ID/Age),data = subset(gat1.1,location%in% c("X1ST.Short.ramp", 'X2ND.Short.ramp')))
s_sr1_1 <- simulateResiduals(fittedModel = sr1_1, plot = T)
plot(sr1_1)
testOutliers(s_sr1_1)
summary(sr1_1)
Anova(sr1_1)

##this one is works##
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

####longramp 1.1#### we have it

##model assumption problem
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

##model assumption problem
sr1_2 = lmer(b_den ~ Age + location + Age*location + 
               (1|Pen.ID/Age),data = subset(gat1.2,location%in% c("X1ST.Short.ramp", 'X2ND.Short.ramp')))
si_sr1_2 =  simulateResiduals(fittedModel = sr1_2, plot = T)

##offset function model, glmmtmb##
sr1_2co = glmmTMB(b_n ~ Age + location + Age*location + offset(log(area)) + 
                    (1|Pen.ID/Age),
                  data = subset(gat1.2,location%in% c("X1ST.Short.ramp", 'X2ND.Short.ramp')), family="genpois",
                  control=glmmTMBControl(optimizer=optim,optArgs=list(method="BFGS",iter.max=1e100,eval.max=1e100)))
si_sr1_2co =  simulateResiduals(fittedModel = sr1_2co, plot = T)
testDispersion(si_sr1_2co)
plot(allEffects(sr1_2co))
Anova(sr1_2co)

####1.2 long ramps####

lr1_2co = glmmTMB(b_n ~ Age + location + Age*location + offset(log(area)) + 
                    (1|Pen.ID/Age),
                  data = subset(gat1.1,location%in% c("X1ST.part.Long.ramp..Down.", 'X2ND.part.Long.ramp..Up.')), family="nbinom1",
                  control=glmmTMBControl(optimizer=optim,optArgs=list(method="BFGS",iter.max=1e100,eval.max=1e100)))
si_lr1_2co =  simulateResiduals(fittedModel = lr1_2co, plot = T)
Anova(lr1_2co)
plot(allEffects(lr1_2co))

