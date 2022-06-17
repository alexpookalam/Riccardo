####riccardo final cleaned#####

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

##quadratic model #Aic 2040.2 ##FINAL MODEL

sr1_1 = lmer(b_den ~ Age + location + I(Age^2) + Age:location + I(Age^2):location + 
               (1|Pen.ID/Age),data = subset(gat1.1,location%in% c("X1ST.Short.ramp", 'X2ND.Short.ramp')))
summary(sr1_1)
s_sr1_1 <- simulateResiduals(fittedModel = sr1_1, plot = T)
plot(allEffects(sr1_1)) ###rough to look at model predicitions
Anova(sr1_1) ###obtain p values by wald chisquare tests


##posthoc comparison, here the comparison is between estimated mariginal means from the  ##
##https://aosmith.rbind.io/2019/03/25/getting-started-with-emmeans/
##https://cran.r-project.org/web/packages/emmeans/vignettes/interactions.html
##https://rdrr.io/cran/emmeans/man/emtrends.html
bv3 = emtrends(sr1_1, specs= pairwise~location, var = "I(Age^2)", type = "response")
bv3

##make new ref grid
sr1_1.rg = ref_grid(sr1_1, at = list(Age = c(8,9,10,11,13,14, 15, 16)))

##plot model predictions (Estimated marginal means (EMM)) ###Read about EMM
emmip(sr1_1.rg,location~poly(Age,degree = 2) , CIs = TRUE,
      linearg = list(linetype = "dashed"), CIarg = list(lwd = 1, alpha = 1), type = "response")


###get Estimated Marginal means (EMM) from the model for ages 8,9,10,11,13,14, 15, 16
sr1grid = emmeans(sr1_1.rg, specs= pairwise~I(Age^2):location, type = "response")
sr1grid

###convert EMMs obtained into a dataframe for plotting
sr1grid.df = as.data.frame(sr1grid)%>%filter(location != ".")
sr1grid.df
sr1grid.df$Age = as.integer(sr1grid.df$Age)


###create a boxplot (raw data) with point and line (EMM) and ribbon (confidence intervals)
gatsr = subset(gat1.1,location%in% c("X1ST.Short.ramp", 'X2ND.Short.ramp'))  
sr1grid_plot = ggplot()+
  geom_boxplot(data = gatsr, 
               aes(x = as.integer(Age), y = b_den, fill = location, group =interaction(location, Age), color = location),
               width = .8, position = position_dodge2(width = .75),alpha= 0.5) +
  geom_point(data = sr1grid.df, aes(x= as.integer(Age), y= emmean, color = location),
             position = position_dodge2(width = .75))  + 
  geom_line(data=sr1grid.df, aes(x=  as.integer(Age), y= emmean, color =  location),
            position = position_dodge2(width = .75)) +
  geom_ribbon(data=sr1grid.df, aes(x= as.integer(Age), ymin=lower.CL, ymax=upper.CL, fill = location),
              position = position_dodge2(width = .75), alpha= 0.3) +
  scale_color_manual(values = c("darksalmon", "darkslategray4"), labels=c("Bottom short ramp","Top short ramp")) +
  scale_fill_manual(values = c("darksalmon", "darkslategray4"), labels=c("Bottom short ramp","Top short ramp")) +
  scale_x_continuous(breaks = c(8,9,10,11,13,14,15, 16)) + scale_y_continuous( "Density of birds") + xlab("Days of Age") +
  theme(axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5, size = 24),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 14),
        axis.title.x = element_text(size = 24),
        axis.text.x = element_text(size = 20),
        axis.title.y = element_text(size = 24),
        axis.text.y = element_text(size = 20),
        legend.position = c(.8,.9),legend.title = element_blank(),legend.text = element_text(size = 16),
        panel.background = element_blank())
sr1grid_plot


####longramp 1.1#### 
###boxplot raw data##

lr = ggplot(subset(gat1.1,location%in% c("X1ST.part.Long.ramp..Down.", 'X2ND.part.Long.ramp..Up.')), aes(x = as.factor(Age), 
                                                                                                         y = b_den, fill = as.factor(location))) +
  geom_boxplot() #+ facet_wrap(~Pen.ID)
lr


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

lr1_2qq.rg = ref_grid(lr1_2qq, at = list(Age = c(8,9,10,11,13,14, 15, 16))) ###update the reference grid

##plot the model predictions,###tran function plus type function is used to get values backtransformed to the orginal scale of the data
emmip(lr1_2qq.rg,location~poly(Age,degree = 2) , CIs = TRUE,
      linearg = list(linetype = "dashed"), CIarg = list(lwd = 1, alpha = 1), tran = ptran,type = "response")  

##posthoc comparison
bv4 = emtrends(lr1_2qq, specs= pairwise~location, var = "I(Age^2)", type = "response")
bv4

####1.1 middel and upper tier####

###raw data plot
tie = ggplot(subset(gat1.1,location%in% c('Middle.tier', 'Upper.tier')), aes(x = as.factor(Age), 
                                                                             y = b_den, fill = as.factor(location))) +
  geom_boxplot() #+  facet_wrap(~Pen.ID)
tie



###histogram to visualize b_den
hist(subset(gat1.1,location%in% c("Middle.tier", 'Upper.tier'))$b_den, breaks = 50, group = "Age")

####quadratic model with transformation ##FINAL MODEL##
tier_1.1qq = lmer((b_den)^(2/3)  ~  Age + location + Age:location + I(Age^2) + I(Age^2):location   +
                    (1|Pen.ID/Age),data = subset(gat1.1,location%in% c("Middle.tier", 'Upper.tier')))
si_tier_1.1qq =  simulateResiduals(fittedModel = tier_1.1qq, plot = T)
###eventhough model assumptions are violated slightly, the model predictions look true to the raw data, so I think this model is ok!

Anova(tier_1.1qq) ##obtain p values by wald chisquare tests
plot(allEffects(tier_1.1qq)) ##rough model predictions plot

qtran = make.tran("power", 2/3) ##specify transfromation

##make new ref grid
tier_1.1qq.rg = ref_grid(tier_1.1qq, at = list(Age = c(8,9,10,11,13,14, 15, 16)))


##plotmodel estimates
emmip(tier_1.1qq.rg,location~I(Age^2) , CIs = TRUE,
      linearg = list(linetype = "dashed"), tran = qtran, type = "response")

##posthoc comparison
bv5 = emtrends(tier_1.1qq, specs= pairwise~location, var = "I(Age^2)", type = "response")
bv5

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

###boxplot of raw data
rslr = ggplot(subset(gat1.2,location%in% c("X1ST.Short.ramp", 'X2ND.Short.ramp')), aes(x = as.factor(Age), 
                                                                                       y = b_den, fill = as.factor(location))) +
  geom_boxplot() #+ facet_wrap(~Pen.ID)
rslr

##quadratic model

sr1_2coqq = lmer(sqrt(b_den)  ~  Age + location + Age:location + I(Age^2) + I(Age^2):location   +
                   (1|Pen.ID/Age),data = subset(gat1.2,location%in% c("X1ST.Short.ramp", 'X2ND.Short.ramp')))
si_sr1_2coqq =  simulateResiduals(fittedModel = sr1_2coqq, plot = T)

summary(sr1_2coqq)
plot(sr1_2coqq)
Anova(sr1_2coqq)
plot(allEffects(sr1_2coqq))


##make new ref grid
sr1_2coqq.rg = ref_grid(sr1_2coqq, at = list(Age = c(8,9,10,11,13,14, 15, 16)))


##plotmodel estimates
emmip(sr1_2coqq.rg,location~I(Age^2) , CIs = TRUE,
      linearg = list(linetype = "dashed"), type = "response")

##posthoc comparison
bv6 = emtrends(sr1_2coqq, specs= pairwise~location, var = "I(Age^2)", type = "response")
bv6

####1.2 long ramps####

###boxplots/ raw data
lrlr = ggplot(subset(gat1.2,location%in% c("X1ST.part.Long.ramp..Down.", 'X2ND.part.Long.ramp..Up.')), aes(x = as.factor(Age), 
                                                                                                           y = b_den, fill = as.factor(location))) +
  geom_boxplot() #+ facet_wrap(~Pen.ID)
lrlr

##quadratic model

lr1_2qq = lmer((b_den)  ~  Age + location + Age:location + I(Age^2) + I(Age^2):location   +
                 (1|Pen.ID/Age),subset(gat1.2,location%in% c("X1ST.part.Long.ramp..Down.", 'X2ND.part.Long.ramp..Up.')))
si_lr1_2qq =  simulateResiduals(fittedModel = lr1_2qq, plot = T)

summary(lr1_2qq)

Anova(lr1_2qq) ##p blauesm wald chisq
plot(allEffects(lr1_2qq)) ##rough model prediction plot

##make new ref grid
lr1_2qq.rg = ref_grid(lr1_2qq, at = list(Age = c(8,9,10,11,13,14, 15, 16)))

##plotmodel estimates
emmip(lr1_2qq.rg ,location~I(Age^2) , CIs = TRUE,
      linearg = list(linetype = "dashed"), type = "response")

##posthoc comparison
bv7 = emtrends(lr1_2qq, specs= pairwise~location, var = "I(Age^2)", type = "response")
bv7

####1.2 tiers####


gat12tr = subset(gat1.2,location%in% c("Middle.tier", 'Upper.tier'))
View(gat12tr)

##raw data, boxplot
trlr = ggplot(subset(gat1.2,location%in% c("Middle.tier", 'Upper.tier')), aes(x = as.factor(Age), 
                                                                              y = b_den, fill = as.factor(location))) +
  geom_boxplot() #+ facet_wrap(~Pen.ID)
trlr

####robust lmm####

##https://stats.stackexchange.com/questions/430490/obtaining-p-values-in-a-robustlmm-mixed-model-via-satterthwaite-approximated-dfs
###https://cran.r-project.org/web/packages/robustlmm/vignettes/rlmer.pdf

###robust klmm model
robust.model <- rlmer((b_den) ~  Age + location + Age:location + I(Age^2) + I(Age^2):location +
                        (1|Pen.ID/Age),gat12tr)
si_robust.model =  simulateResiduals(fittedModel = robust.model, plot = T)
summary(robust.model)
tab_model(robust.model) ##simialr to summary function bit looks more nicer
Anova(robust.model) ##p values from wald chsq

#make new ref grid
robust.model.rg = ref_grid(robust.model, at = list(Age = c(8,9,10,11,13,14, 15, 16)))


##plotmodel estimates
emmip(robust.model.rg,location~I(Age^2) , CIs = TRUE,
      linearg = list(linetype = "dashed"), type = "response")

##posthoc comparison
bv8 = emtrends(robust.model, specs= pairwise~location, var = "I(Age^2)", type = "response")
bv8
