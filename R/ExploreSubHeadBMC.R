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

ggplot(gym, aes(x = Chronologic.Age.at.Menarche, y = Sub.head.BMC))+
  geom_jitter(aes(colour = ChronAgeAtMenarche_Group), pch = 1)+
  geom_smooth(aes(group = ChronAgeAtMenarche_Group,
                  colour = ChronAgeAtMenarche_Group),
              method = 'lm', se = F, size = 2)+
  xlab('Chronologic Age at Menarche')+
  ylab('Sub head BMC')+
  scale_colour_brewer(palette = 'Dark2',
                      name = 'Age at Menarche Group')+
  ggtitle('Age at Menarche by Sub head BMC')+
  theme(legend.position = 'top',
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.margin = unit(0, 'cm'))

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
                           ifelse(gym$YearsAfterGymQuit >= (90 / 365), 'Quit',
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
  geom_density(aes(group = variable), alpha = .2)+
  stat_function(fun = 'dnorm', colour = 'red', size = 2)+
  xlab('Standardized Values (zero mean, unit variance)')+
  ylab('Density')+
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

#plot individual trajectories for each response


ggplot(gym_sub_nogroup, aes(x = Sub.Head.LM, y = value))+
  geom_line(aes(group = ID), alpha = .3)+
  facet_wrap(~variable)+
  xlab('Sub Head LM')+
  ylab('Standardized Value (zero mean, unit variance)')+
  ggtitle('Individual Trajectories for Each Response Variable')

ggplot(gym_sub_nogroup, aes(x = Menarcheal.Age.at.DXA, y = value))+
  geom_line(aes(group = ID), alpha = .3)+
  facet_wrap(~variable)+
  xlab('Menarcheal Age at DXA')+
  ylab('Standardized Value (zero mean, unit variance)')+
  ggtitle('Individual Trajectories for Each Response Variable')

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

do.call('data.frame', 
    aggregate(value ~ Group_Label + variable, function(x)
      paste0(round(mean(x, na.rm = T), 2), 
             " (", 
             round(sd(x, na.rm = T), 2), ")"), 
      data = gym_sub_base)) -> df1_base

reshape(df1, idvar = 'variable', timevar = 'Group_Label', direction = 'wide') -> df2

##presentation plots
df1_cs <- structure(list(variable = structure(c(1L, 2L, 3L, 4L, 5L, 6L, 
                                                7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 1L, 2L, 3L, 4L, 5L, 6L, 
                                                7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 1L, 2L, 3L, 4L, 5L, 6L, 
                                                7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L), .Label = c("Sub.head.BMC", 
                                                                                                 "Distal.Radius.Third.Area", "Distal.bone.mineral.content.third", 
                                                                                                 "Distal.Modulus.Third.Radius", "UD.Area.Radius", "UD.BMC", "UD.IBS.Radius", 
                                                                                                 "Femoral.Neck.BMC.Hip", "NN.Section.Modulus.Hip", "NN.BR.Hip", 
                                                                                                 "NN.width.Hip", "NN.ED.Hip", "NN.ACT.Hip", "PA.L3.BMC.Spine"), class = "factor"), 
                         Group_Label = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
                                                   1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
                                                   2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 
                                                   3L, 3L, 3L, 3L), .Label = c("NON", "PERI", "POST"), class = "factor"), 
                         value = c(-0.061472977396549, -0.432321421767705, -0.384652606357142, 
                                   -0.433231766287853, -0.356155532928317, -0.430769287128757, 
                                   -0.374326524362265, -0.13616416536045, -0.0647393890113162, 
                                   0.309450539340368, 0.203860003705822, 0.27996831602529, -0.261525656205867, 
                                   -0.0614109381209648, -0.16096273244324, 0.29747685374581, 
                                   0.123460668416626, 0.203153823446502, 0.587537528475897, 
                                   0.318209902034928, 0.0615222913491079, -0.151322191741991, 
                                   -0.0817516550842374, -0.120499870897454, -0.189210976836825, 
                                   -0.152502978970673, -0.0557624217988491, -0.175098754834998, 
                                   0.224473739108582, 0.760485004519492, 0.748652058521092, 
                                   0.810510576909655, 0.451844034956042, 0.746636145803863, 
                                   0.758452597816335, 0.389141836727661, 0.189469273699493, 
                                   -0.639792791435959, -0.364372500457071, -0.556226528312661, 
                                   0.622659698922718, 0.231682400811208)), .Names = c("variable", 
                                                                                      "Group_Label", "value"), row.names = c(NA, -42L), class = "data.frame")


merge(df1_base, df1_cs, by = c('variable','Group_Label')) -> df1_merge

factor_levels <- c("Sub.head.BMC", "Distal.Radius.Third.Area", "Distal.bone.mineral.content.third", 
                   "Distal.Modulus.Third.Radius", "UD.Area.Radius", "UD.BMC", "UD.IBS.Radius",
                   "PA.L3.BMC.Spine","NN.ACT.Hip", "Femoral.Neck.BMC.Hip", "NN.Section.Modulus.Hip", "NN.BR.Hip", 
                   "NN.width.Hip", "NN.ED.Hip")

df1_merge$variable_f <- factor(df1_merge$variable, levels = rev(factor_levels))

ggplot(df1_merge, aes(x = Group_Label, y = variable_f))+
  geom_tile(aes(fill = value.y), colour = 'black')+
  geom_text(aes(label = value.x), size = 6)+
  scale_fill_gradient(low = 'white', high = 'grey60')+
  theme(legend.position = 'none',
        axis.title = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_text(size = 14))

ggplot(df1_merge, aes(x = 1, y = variable_f))+
  geom_tile(aes(fill = value.y), colour = 'black')+
  facet_wrap(~Group_Label) +
  geom_text(aes(label = value.x), size = 6)+
  scale_fill_gradient(low = 'white', high = 'grey60')+
  theme(legend.position = 'none',
        axis.title = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_text(size = 14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(face = 'bold', size = 14),
        strip.background = element_rect(fill = 'white'),
        panel.margin = unit(0, 'cm'))


ggplot(gym_sub_nogroup, aes(x = value))+
  stat_function(fun = 'dnorm', colour = 'red', size = 2)+
  ylab('Standardized Values (zero meaun, unit variance)')+
  geom_density(aes(group = variable), alpha = .2)+
  ggtitle('Centered and Scaled Variable Density Plots',
          subtitle = 'Red Curve is the Standard Normal Density')

ggplot(gym, aes(x = Chronologic.Age.at.Menarche, y = Sub.head.BMC))+
  geom_jitter(aes(colour = ChronAgeAtMenarche_Group), pch = 1)+
  geom_smooth(aes(group = ChronAgeAtMenarche_Group,
                  colour = ChronAgeAtMenarche_Group),
              method = 'lm', se = F, size = 2)+
  xlab('Chronologic Age at Menarche')+
  ylab('Sub head BMC')+
  scale_colour_brewer(palette = 'Dark2',
                      name = 'Age at Menarche Group')+
  ggtitle('Age at Menarche by Sub head BMC')+
  theme(legend.position = 'top',
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.margin = unit(0, 'cm'),
        plot.title = element_text(size = 18),
        legend.title = element_text(size = 16)) 


ggplot(gym, aes(x = Chronologic.Age.at.Menarche, y = Sub.head.BMC))+
  geom_jitter(aes(colour = ChronAgeAtMenarche_Group), pch = 1, size = 2.5,
              show.legend = F)+
  geom_smooth(aes(group = ChronAgeAtMenarche_Group,
                  colour = ChronAgeAtMenarche_Group),
              method = 'lm', se = F, size = 2.5)+
  xlab('Chronologic Age at Menarche')+
  ylab('Sub head BMC')+
  scale_colour_brewer(palette = 'Dark2',
                      name = 'Age at Menarche Group')+
  ggtitle('Age at Menarche by Sub head BMC')+
  theme(legend.position = 'top',
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.margin = unit(0, 'cm'),
        plot.title = element_text(size = 18),
        legend.title = element_text(size = 16),
        panel.grid.minor = element_blank())+
  # geom_vline(aes(xintercept = 12.34 + .001),
  #            linetype = 'dashed', alpha = .6)+
  # geom_vline(aes(xintercept = 13.07+ .001),
  #            linetype = 'dashed', alpha = .6)+
  # geom_vline(aes(xintercept = 13.92),
  #            linetype = 'dashed', alpha = .6)+
   guides(colour = guide_legend(override.aes = list(size = 5))) #+
  # geom_text(data = NULL, aes(x = 11.68, y = 2000, label = 'low', colour = 'low'),
  #           check_overlap = TRUE, size = 9, show.legend = FALSE)+
  # geom_text(data = NULL, aes(x = 12.73, y = 2250, label = 'medlow', colour = 'medlow'),
  #           check_overlap = TRUE, size = 9, show.legend = FALSE)+
  # geom_text(data = NULL, aes(x = 13.8, y = 750, label = 'medhigh', colour = 'medhigh'),
  #           check_overlap = TRUE, size = 9, show.legend = FALSE)+
  # geom_text(data = NULL, aes(x = 15, y = 1450, label = 'high', colour = 'high'),
  #           check_overlap = TRUE, size = 9, show.legend = FALSE)