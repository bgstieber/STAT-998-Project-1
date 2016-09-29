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