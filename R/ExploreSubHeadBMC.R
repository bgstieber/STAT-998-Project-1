library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(lme4)
library(gamm4)
library(splines)
theme_set(theme_bw())
library(MASS)


#using one of the response variables, Sub.Head.BMC
#let's try to fit a model
gym <- read.csv('Gymnastics002.csv', stringsAsFactors = FALSE)
gym$Group_Label <- c('PERI','POST','NON')[gym$Group.Division + 1]

#density plots

ggplot(gym, aes(x = Sub.head.BMC))+
  geom_density()

ggplot(gym, aes(x = Group_Label, y = Sub.head.BMC))+
  geom_violin(draw_quantiles = c(.25, .5, .75), scale = 'count')+
  stat_summary(fun.data = 'mean_cl_boot', col = 'blue')

#suggests a common slope might be okay
#probably need to transform Sub.Head.LM, though
ggplot(gym, aes(x = Sub.Head.LM, y = Sub.head.BMC))+
  geom_line(aes(group = ID))

#random slope by ID?
ggplot(gym, aes(x = Standing.Height, y = Sub.head.BMC))+
  geom_line(aes(group = ID))

#maybe convert to categorical variable?
ggplot(gym, aes(x = FA.length, y = Sub.head.BMC))+
  geom_line(aes(group = ID))

#makes strong case for random intercept
#not sure if random slope makes sense.
ggplot(gym, aes(x = Menarcheal.Age.at.DXA, y = Sub.head.BMC))+
  geom_line(aes(group = ID))

coplot(Sub.head.BMC ~ Menarcheal.Age.at.DXA | Group_Label * Sub.Head.LM, data = gym,
       panel = panel.smooth)

#non gym physical activity
#no real relationship
ggplot(gym, aes(x = NonGym.Physical.Activity, y = Sub.head.BMC))+
  geom_line(aes(group = ID))

#will be interesting to explore this relationship
ggplot(gym, aes(x = Gymnastics, y = Sub.head.BMC))+
  geom_line(aes(group = ID))

ggplot(gym, aes(x = Gymnastics + NonGym.Physical.Activity, y = Sub.head.BMC))+
  geom_point()

ggplot(gym, aes(x = Chronologic.Age.at.Menarche, y = Sub.head.BMC))+
  geom_jitter()+
  stat_smooth(se = F)

#may need to actually convert that to categorical
gym$ChronAgeAtMenarche_Group <- cut(gym$Chronologic.Age.at.Menarche,
                                    breaks = quantile(gym$Chronologic.Age.at.Menarche, seq(0,1,.25)),
                                    include.lowest = TRUE, 
                                    labels = c('low','medlow','medhigh','high'))

ggplot(gym, aes(x = Chronologic.Age.at.Menarche, y = Sub.head.BMC))+
  geom_jitter(aes(colour = ChronAgeAtMenarche_Group), pch = 1)+
  geom_smooth(aes(group = ChronAgeAtMenarche_Group,
                  colour = ChronAgeAtMenarche_Group),
              method = 'lm', se = F, size = 2)

ggplot(gym, aes(x = Group_Label, y = Chronologic.Age.at.Menarche))+
  geom_violin(draw_quantiles = c(.25, .5, .75))+
  stat_summary(fun.y = 'mean', size = 2, col = 'blue', geom = 'point')

#fit our first model

fit1 <- lmer(Sub.head.BMC ~ Sub.Head.LM + Standing.Height + Group_Label + ChronAgeAtMenarche_Group
             + Menarcheal.Age.at.DXA  + (Standing.Height | ID),
             data = gym)

summary(fit1)
anova(fit1)


#maybe we need to redefine the group labels...
#something like:
# 0 - Never in gymnastics
# 1 - Currently in gymnastics
# 2 - Was in gymnastics, now has quit
# could use it to interact with age @ DXA

# Another idea:
# Aim 1: investigate coefficient from each model we fit and interpret etc.
# Aim 2: Design an early / late gymnastics variable and investigate?
# Still kinda stuck on Aim 2  
# Aim 3: Repeat analysis for Aim 1, yet this time use 
# centered and scaled measurements for dependent variables
# (center and scale indepdent variables, too?)

gym$YearsAfterGymQuit <- gym$Menarcheal.Age.at.DXA - gym$Menarcheal.Age.at.Quit.Date


ggplot(gym, aes(x = YearsAfterGymQuit, y = Sub.head.BMC))+
  geom_jitter()

gym$Group_Label2 <- ifelse(is.na(gym$YearsAfterGymQuit), 'Never',
                           ifelse(gym$YearsAfterGymQuit >= (45 / 365), 'Quit',
                                  'In Gymnastics'))

#enter and interact with Age?
ggplot(gym, aes(x = Menarcheal.Age.at.DXA, y = Sub.head.BMC))+
  geom_jitter(aes(colour = Group_Label2), pch = 1, size = 2)+
  geom_smooth(aes(colour = Group_Label2),
              se = F, size = 1.2)+
  scale_colour_brewer(palette = 'Dark2')


ggplot(gym, aes(x = Menarcheal.Age.at.DXA, y = Sub.head.BMC))+
  geom_jitter(aes(colour = Group_Label), pch = 1, size = 2)+
  geom_smooth(aes(colour = Group_Label),
              se = F, size = 1.2)+
  scale_colour_brewer(palette = 'Dark2')

fit2 <- lmer(Sub.head.BMC ~ Sub.Head.LM + Standing.Height + 
        Group_Label2 + Menarcheal.Age.at.DXA + 
         + ChronAgeAtMenarche_Group
         + (Standing.Height | ID) + (1 | ID : Group_Label2),
       data = gym)

summary(fit2)
anova(fit2)

#remove the Group within ID random intercept
#it is probably not needed
fit3 <- lmer(Sub.head.BMC ~ Sub.Head.LM + Standing.Height + 
               Group_Label2 + Menarcheal.Age.at.DXA + 
               + ChronAgeAtMenarche_Group
             + (Standing.Height | ID),
             data = gym)

summary(fit3)
anova(fit3)

response_names <- c("Sub.head.BMC", "Distal.Radius.Third.Area", "Distal.bone.mineral.content.third", 
                    "Distal.Modulus.Third.Radius", "UD.Area.Radius", "UD.BMC", "UD.IBS.Radius", 
                    "Femoral.Neck.BMC.Hip", "NN.Section.Modulus.Hip", "NN.BR.Hip", 
                    "NN.width.Hip", "NN.ED.Hip", "NN.ACT.Hip", "PA.L3.BMC.Spine")



gym_sub_byID <- gym[c('ID','Group_Label','Group_Label2', 'Menarcheal.Age.at.DXA',
                 'Sub.Head.LM',response_names)] %>%
  group_by(ID) %>%
  mutate_each_(funs((. - mean(., na.rm = T)) / sd(., na.rm = T)), vars = response_names) %>%
  melt(., id.vars = c('ID','Group_Label','Group_Label2','Menarcheal.Age.at.DXA',
                      'Sub.Head.LM'))


gym_sub_nogroup <- gym[c('ID','Group_Label','Group_Label2', 'Menarcheal.Age.at.DXA',
                         'Standing.Height',
                      'Sub.Head.LM',response_names)] %>%
  mutate_each_(funs((. - mean(., na.rm = T)) / sd(., na.rm = T)), vars = response_names) %>%
  melt(., id.vars = c('ID','Group_Label','Group_Label2','Menarcheal.Age.at.DXA',
                      'Sub.Head.LM', 'Standing.Height'))


gym_sub_base <- gym[c('ID','Group_Label','Group_Label2', 'Menarcheal.Age.at.DXA',
                      'Standing.Height',
                      'Sub.Head.LM',response_names)] %>%
  melt(., id.vars = c('ID','Group_Label','Group_Label2','Menarcheal.Age.at.DXA',
                      'Sub.Head.LM', 'Standing.Height'))


#definitely need to fit more than just a linear effect for age

ggplot(gym_sub_nogroup, aes(x = Sub.Head.LM, y = value))+
  geom_point(aes(colour = Group_Label2), pch = 1)+
  geom_smooth(aes(colour = Group_Label2), se = F, span = .9)+
  facet_wrap(~ variable)+
  scale_colour_brewer(palette = 'Set1',
                      breaks = c('Never','Quit','In Gymnastics'),
                      name = 'Gym Group')+
  ylab('Standardized Value (zero mean, unit variance)')

ggplot(gym_sub_nogroup, aes(x = Menarcheal.Age.at.DXA, y = value))+
  geom_point(aes(colour = Group_Label2), pch = 1)+
  geom_smooth(aes(colour = Group_Label2), se = F, span = .9)+
  facet_wrap(~ variable)+
  scale_colour_brewer(palette = 'Set1',
                      breaks = c('Never','Quit','In Gymnastics'),
                      name = 'Gym Group')+
  ylab('Standardized Value (zero mean, unit variance)')

ggplot(gym_sub_nogroup, aes(x = Standing.Height, y = value))+
  geom_point(aes(colour = Group_Label2), pch = 1)+
  geom_smooth(aes(colour = Group_Label2), se = F, span = .9)+
  facet_wrap(~ variable)+
  scale_colour_brewer(palette = 'Set1',
                      breaks = c('Never','Quit','In Gymnastics'),
                      name = 'Gym Group')+
  ylab('Standardized Value (zero mean, unit variance)')


#visualize the densities

ggplot(gym_sub_nogroup, aes(x = value))+
  stat_function(fun = 'dnorm', colour = 'red', size = 2)+
  geom_density(aes(group = variable), alpha = .2)+
  ggtitle('Centered and Scaled Variable Density Plots',
          subtitle = 'Red Curve is the Standard Normal Density')

ggplot(gym_sub_base, aes(x = value))+
  geom_density(fill = 'grey90')+
  facet_wrap(~variable, scales = 'free')+
  xlab('')+
  ggtitle('Density Plots of Response Variables')+
  theme(panel.grid = element_blank(),
        strip.text = element_text(face = 'bold'),
        strip.background = element_rect(fill = 'white'))

#random slope for standing height?

ggplot(gym_sub_nogroup, aes(x = Standing.Height, y = value))+
  geom_line(aes(group = ID), alpha = .3)+
  facet_wrap(~variable)+
  xlab('Standing Height')+
  ylab('Standardized Value (zero mean, unit variance)')+
  ggtitle('Individual Trajectories for Each Response Variable')

# #create a new age group variable, just three levels
# gym$ChronAgeAtMenarche_Group2 <- cut(gym$Chronologic.Age.at.Menarche,
#     breaks = quantile(gym$Chronologic.Age.at.Menarche, seq(0,1,1/3)),
#     include.lowest = TRUE, 
#     labels = c('low','med','high'))
# 
# 
# fit4 <- update(fit3, .~. - ChronAgeAtMenarche_Group + ChronAgeAtMenarche_Group2)
# summary(fit4)
# anova(fit4)

#FA.length doesn't need to be in the model
#super correlated with StandingHeight
fit5 <- update(fit3, .~. + FA.length)

#fit3 still best model 
#what if we fit using a smoother on Age?
summary(fit3.gs <- gamm4(Sub.head.BMC ~ Sub.Head.LM + Standing.Height + Group_Label2 + 
                           s(Menarcheal.Age.at.DXA) + ChronAgeAtMenarche_Group, data = gym,
                         random = ~(1 + Standing.Height | ID)))
#fit a cubic to it
fit3.cubicage <- update(fit3, .~. + I(Menarcheal.Age.at.DXA ^ 2) + I(Menarcheal.Age.at.DXA^3))
