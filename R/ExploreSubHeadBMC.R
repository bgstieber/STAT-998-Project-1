library(plyr)
library(ggplot2)
library(lme4)
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

