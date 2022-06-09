###riccardo data first look###

###for  geom_smooth explanation, check https://ggplot2.tidyverse.org/reference/geom_smooth.html

setwd("E:/riccardo")
getwd()


library("dplyr")
library("ggplot2")
library("tidyverse")
library("fitdistrplus")
library("lme4")
library("glmmTMB")
library("DHARMa")

raw =  read.csv(file.choose())
head(raw)
gat = raw %>% gather("location", "bird_n",6:11) ##takes columns from 6 to 11 make two new columns named location and bird_n
View(gat)
unique(gat$location) ##show unique levels in factor "location"
gat$location <- recode_factor(gat$location, 'X1ST.part.Long.ramp..Down.'  = "Long_ramp_bottom",
                              "X2ND.part.Long.ramp..Up." = "Long_ramp_up",
                              "X1ST.Short.ramp" = "Short_ramp_bottom",
                              "X2ND.Short.ramp" = 'Short_ramp_top',
                              'Middle.tier' = 'Middle_tier',
                              'Upper.tier' = 'Upper_tier')  ###rename levels in factor "location"

gat = gat%>%filter(!is.na(bird_n)) ##remove rows where bird_n is na
summary(gat)
table(gat$Age,gat$Pen.side, gat$Time) ##check no of observations


###barn side 1.1####
gatls = gat%>%filter(Pen.ID %in% c("1.13", "1.14")) ###make dataframe with only pen 1.13, 1.14
unique(gatls$Pen.ID)
str(gatls)
summary(gatls)

##boxplot for long ramp bootom and top##
lr = ggplot(subset(gatls,location%in% c("Long_ramp_bottom", "Long_ramp_up")), aes(x = as.factor(Age), 
                                                                           y = bird_n, fill = as.factor(location))) +
  geom_boxplot() + facet_wrap(~Pen.ID)
lr

##boxplot for middle and upper##

tie = ggplot(subset(gatls,location%in% c('Middle_tier', 'Upper_tier')), aes(x = as.factor(Age), 
                                                                                  y = bird_n, fill = as.factor(location))) +
  geom_boxplot() #+  facet_wrap(~Pen.ID)
tie

###boxplot for short ramps##

sr = ggplot(subset(gatls,location%in% c("Short_ramp_bottom", 'Short_ramp_top')), aes(x = as.factor(Age), 
                                                                            y = bird_n, fill = as.factor(location))) +
  geom_boxplot() #+ facet_wrap(~Pen.ID)
sr



###barn side 1.2####
gatrs = gat%>%filter(Pen.ID %in% c("1.21", "1.22")) ###make dataframe with only pen 1.13, 1.14
unique(gatrs$Pen.ID)

#####boxplot for long ramp bootom and top##
rslr = ggplot(subset(gatrs,location%in% c("Long_ramp_bottom", "Long_ramp_up")), aes(x = as.factor(Age), 
                                                                                  y = bird_n, fill = as.factor(location))) +
  geom_boxplot() #+ facet_wrap(~Pen.ID)
rslr

##boxplot for middle and upper##
rstie = ggplot(subset(gatrs,location%in% c('Middle_tier', 'Upper_tier')), aes(x = as.factor(Age), 
                 y = bird_n, fill = as.factor(location))) + geom_boxplot()
rstie


###geom_smooth(lm)for middle and upper##

rstielm = ggplot(subset(gatrs,location%in% c('Middle_tier', 'Upper_tier')), aes(x = Age, 
              y = bird_n, color = as.factor(location), group = as.factor(location))) +
  scale_y_continuous('bird_n')+
  scale_x_continuous('Age')+ geom_point() + 
  geom_smooth(method = "lm") ###warning geom_smooth()` using formula 'y ~ x', just says that the line is plotted with assuming linear relationship b/w x and y
rstielm

##geom smooth quadratic
rstieqd = ggplot(subset(gatrs,location%in% c('Middle_tier', 'Upper_tier')), aes(x = Age, 
                                                                                y = bird_n, color = as.factor(location), group = as.factor(location))) +
  scale_y_continuous('bird_n')+
  scale_x_continuous('Age')+ geom_point() + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 2)) ###now the formula is specified for a quadrtic effect
rstieqd

##geom smooth 
rstiess = ggplot(subset(gatrs,location%in% c('Middle_tier', 'Upper_tier')), aes(x = Age, 
                                                                                y = bird_n, color = as.factor(location), group = as.factor(location))) +
  scale_y_continuous('bird_n')+
  scale_x_continuous('Age')+ geom_point() + 
  geom_smooth() ###now the formula is specified for a quadrtic effect
rstiess

View(gatrs)
str(gatrs)
sum(is.na(gatrs$bird_n))
length(gatrs$bird_n)
###boxplot for short ramps##

rssr = ggplot(subset(gatrs,location%in% c("Short_ramp_bottom", 'Short_ramp_top')), aes(x = as.factor(Age), 
                                                                     y = bird_n, fill = as.factor(location))) +
  geom_boxplot() #+ facet_wrap(~Pen.ID)
rssr

####1.1, longramp, doa effect, lmm & glmm analysis##
str(gatls)
gatls = gatls%>%mutate(across(where(is.character), as.factor))
gatls$Pen.ID =  as.factor(gatls$Pen.ID)

hist(gatls$bird_n, breaks = 50)
plot(gatls$bird_n~gatls$Age)
gatfit.nbm = fitdist(gatls$bird_n, "nbinom")
gatfit.p = fitdist(gatls$bird_n, "pois")
kgfs = gofstat(list(gatfit.nbm, gatfit.p), fitnames = c("poisson", "nbinom" ))
kgfs

par(mfrow=c(2,2))
plot.legend <- c("poisson", "nbinom")
denscomp(list(gatfit.nbm, gatfit.p), legendtext = plot.legend)
cdfcomp (list(gatfit.nbm, gatfit.p), legendtext = plot.legend)
qqcomp  (list(gatfit.nbm, gatfit.p), legendtext = plot.legend)
ppcomp  (list(gatfit.nbm, gatfit.p), legendtext = plot.legend)
par(mfrow=c(1,1))


str(gatls)
gatls$Pen.ID = as.factor(gatls$Pen.ID)
lr_gl = lmer(bird_n ~ Age + location +  Age*location + 
                (1|Pen.ID/Age),data = subset(gatls,location%in% c("Short_ramp_bottom", 'Short_ramp_top')))
lrn <- simulateResiduals(fittedModel = lr_gl, plot = T)
qqnorm(resid(lr_gl))
qqline(resid(lr_gl))
ks.test(resid(lr_gl), "pnorm", mean=mean(resid(lr_gl)), sd=sd(resid(lr_gl)))
shapiro.test(resid(lr_gl))
plot(lr_gl)



####ignore for now#######
lr_11= glmmTMB(bird_n ~ Age + location + + Age*location + 
                 (1|Pen.ID/Age),data = subset(gatls,location%in% c("Short_ramp_bottom", 'Short_ramp_top')),
               family = "poisson",
               control = glmmTMBControl(optimizer = optim, 
                                        optArgs = list(method="BFGS",iter.max=1e50,eval.max=1e50)))
lrsim <- simulateResiduals(fittedModel = lr_11, plot = T)
testDispersion(lrsim)
summary(lr_11)

lr_112= glmmTMB(bird_n ~ Age + location + + Age*location + 
                 (1|Pen.ID/Age),data = subset(gatls,location%in% c("Short_ramp_bottom", 'Short_ramp_top')),
               family = "genpois",
               control = glmmTMBControl(optimizer = optim, 
                                        optArgs = list(method="BFGS",iter.max=1e50,eval.max=1e50)))
lrsim2 <- simulateResiduals(fittedModel = lr_112, plot = T)
summary(lr_112)

str(gatls)

lr_113= glmmTMB(bird_n ~ Age + location + + Age*location + 
                  (1|Pen.ID/Age),data = subset(gatls,location%in% c("Short_ramp_bottom", 'Short_ramp_top')),
                family = "nbinom1",
                control = glmmTMBControl(optimizer = optim, 
                                         optArgs = list(method="BFGS",iter.max=1e50,eval.max=1e50)))
lrsim3 <- simulateResiduals(fittedModel = lr_113, plot = T)

lr_114= glmmTMB(bird_n ~ Age + location + + Age*location + 
                  (1|Pen.ID/Age),data = subset(gatls,location%in% c("Short_ramp_bottom", 'Short_ramp_top')),
                family = "nbinom2",
                control = glmmTMBControl(optimizer = optim, 
                                         optArgs = list(method="BFGS",iter.max=1e50,eval.max=1e50)))
lrsim4 <- simulateResiduals(fittedModel = lr_114, plot = T)
