library(lme4)
library(lmerTest)
library(nlme)
library(ggplot2)
theme_set(theme_bw())

gym <- read.csv('Gymnastics002.csv', stringsAsFactors = FALSE)

gym$YearsAfterGymQuit <- gym$Menarcheal.Age.at.DXA - gym$Menarcheal.Age.at.Quit.Date

gym$Group_Label <- c('PERI','POST','NON')[gym$Group.Division + 1]

gym$Group_Label2 <- ifelse(is.na(gym$YearsAfterGymQuit), 'Never',
                           ifelse(gym$YearsAfterGymQuit >= (90 / 365), 'Quit',
                                  'In Gymnastics'))

#dummy for gymnastics participation
#use for aim 1
gym$GymnasticsDummy <- ifelse(gym$Group_Label2 == 'Never', 0, 1) 

gym$ChronAgeAtMenarche_Group <- cut(gym$Chronologic.Age.at.Menarche,
                                    breaks = quantile(gym$Chronologic.Age.at.Menarche, seq(0,1,.25)),
                                    include.lowest = TRUE, 
                                    labels = c('low','medlow','medhigh','high'))

response_names <- c("Sub.head.BMC", "Distal.Radius.Third.Area", "Distal.bone.mineral.content.third", 
                    "Distal.Modulus.Third.Radius", "UD.Area.Radius", "UD.BMC", "UD.IBS.Radius", 
                    "Femoral.Neck.BMC.Hip", "NN.Section.Modulus.Hip", "NN.BR.Hip", 
                    "NN.width.Hip", "NN.ED.Hip", "NN.ACT.Hip", "PA.L3.BMC.Spine")

response_names_short <- c('SHBMC','DRA','DRBMC',
                          'DRM','UDRA','UDBMC','UDIBS',
                          'FNBMCHIP','HIPM','HIPBR','HIPW',
                          'HIPED','HIPACT','SBMC')

rhs_form_linear <- "~ Sub.Head.LM + Standing.Height + GymnasticsDummy + Menarcheal.Age.at.DXA +
I(Menarcheal.Age.at.DXA ^ 2) + I(Menarcheal.Age.at.DXA^3) + ChronAgeAtMenarche_Group + 
                   (1 + Menarcheal.Age.at.DXA | ID) "


rhs_form_cubic <- "~ Sub.Head.LM + Standing.Height + GymnasticsDummy + Menarcheal.Age.at.DXA +
I(Menarcheal.Age.at.DXA ^ 2) + I(Menarcheal.Age.at.DXA^3) + ChronAgeAtMenarche_Group +
(1 + Menarcheal.Age.at.DXA + I(Menarcheal.Age.at.DXA ^ 2) + I(Menarcheal.Age.at.DXA^3)  | ID)
"

rhs_form_linear_nlme <- "~ Sub.Head.LM + Standing.Height + GymnasticsDummy + Menarcheal.Age.at.DXA +
I(Menarcheal.Age.at.DXA ^ 2) + I(Menarcheal.Age.at.DXA^3) + ChronAgeAtMenarche_Group"


rhs_form_cubic_nlme <- "~ Sub.Head.LM + Standing.Height + GymnasticsDummy + Menarcheal.Age.at.DXA +
I(Menarcheal.Age.at.DXA ^ 2) + I(Menarcheal.Age.at.DXA^3) + ChronAgeAtMenarche_Group"


fitSHBMC <- lmer(Sub.head.BMC ~ GymnasticsDummy +
                   Sub.Head.LM + 
                   Standing.Height + 
                   Menarcheal.Age.at.DXA +
                   I(Menarcheal.Age.at.DXA ^ 2) + 
                   I(Menarcheal.Age.at.DXA^3) +
                   ChronAgeAtMenarche_Group + 
                   (1 + Menarcheal.Age.at.DXA | ID),
             data = gym)


# fitSHBMC.sub <- lmer(Sub.head.BMC ~ GymnasticsDummy +
#                    Sub.Head.LM + 
#                    Standing.Height + 
#                    Menarcheal.Age.at.DXA +
#                    I(Menarcheal.Age.at.DXA ^ 2) + 
#                    I(Menarcheal.Age.at.DXA^3) +
#                    ChronAgeAtMenarche_Group + 
#                    (1 + Menarcheal.Age.at.DXA | ID),
#                  data = gym[gym$Menarcheal.Age.at.DXA <= 2.5, ])

iter <- 1
##for lme4
for(i in response_names){
  
  assign(paste0('fit.', response_names_short[iter]),
         
         lmer(as.formula(paste0(i, rhs_form_linear)),
             data = gym, na.action = na.exclude)
         
  )
  
  assign(paste0('fit.cub.', response_names_short[iter]),
         
         lmer(as.formula(paste0(i, rhs_form_cubic)),
             data = gym,
             na.action = na.exclude)
         
  )
  iter = iter + 1
}

##for nlme
for(i in response_names){
  
  assign(paste0('fit.', response_names_short[iter]),
         
         lme(as.formula(paste0(i, rhs_form_linear)),
             random = 
               ~ 1 + Menarcheal.Age.at.DXA| ID,
              data = gym, na.action = na.exclude)
         
  )
  
  assign(paste0('fit.cub.', response_names_short[iter]),
         
         lme(as.formula(paste0(i, rhs_form_cubic)),
             random = 
               ~ 1 + Menarcheal.Age.at.DXA + I(Menarcheal.Age.at.DXA ^ 2) + I(Menarcheal.Age.at.DXA^3)| ID,
              data = gym,
              na.action = na.exclude)
         
  )
  iter = iter + 1
}

# for(i in response_names){
# 
#   assign(paste0('fit', response_names_short[iter]),
# 
#          lmer(as.formula(paste0(i, rhs_form)),
#               data = gym, na.action = na.exclude)
# 
#          )
# 
#   assign(paste0('sub.fit', response_names_short[iter]),
# 
#          lmer(as.formula(paste0(i, rhs_form)),
#               data = gym[gym$Menarcheal.Age.at.DXA <= 2.5, ],
#               na.action = na.exclude)
# 
#   )
#   iter = iter + 1
# }

model_list_linear <- list(
  'fit.SHBMC' = fit.SHBMC,
  'fit.DRA' = fit.DRA,
  'fit.DRBMC' = fit.DRBMC,
  'fit.DRM' = fit.DRM,
  'fit.UDRA' = fit.UDRA,
  'fit.UDBMC' = fit.UDBMC,
  'fit.UDIBS' = fit.UDIBS,
  'fit.FNBMCHIP' = fit.FNBMCHIP,
  'fit.HIPM' = fit.HIPM,
  'fit.HIPBR' = fit.HIPBR,
  'fit.HIPW' = fit.HIPW,
  'fit.HIPED' = fit.HIPED,
  'fit.HIPACT' = fit.HIPACT,
  'fit.SBMC' = fit.SBMC
)

model_list_cubic <- list(
  'fitSHBMC' = fit.cub.SHBMC,
  'fitDRA' = fit.cub.DRA,
  'fitDRBMC' = fit.cub.DRBMC,
  'fitDRM' = fit.cub.DRM,
  'fitUDRA' = fit.cub.UDRA,
  'fitUDBMC' = fit.cub.UDBMC,
  'fitUDIBS' = fit.cub.UDIBS,
  ##'fitFNBMCHIP' = fit.cub.FNBMCHIP,
  'fitHIPM' = fit.cub.HIPM,
  'fitHIPBR' = fit.cub.HIPBR,
  'fitHIPW' = fit.cub.HIPW,
  'fitHIPED' = fit.cub.HIPED,
  'fitHIPACT' = fit.cub.HIPACT,
  'fitSBMC' = fit.cub.SBMC
)

# sub_model_list <- list(
#   'sub.fitSHBMC' = sub.fitSHBMC,
#   'sub.fitDRA' = sub.fitDRA,
#   'sub.fitDRBMC' = sub.fitDRBMC,
#   'sub.fitDRM' = sub.fitDRM,
#   'sub.fitUDRA' = sub.fitUDRA,
#   'sub.fitUDBMC' = sub.fitUDBMC,
#   'sub.fitUDIBS' = sub.fitUDIBS,
#   'sub.fitFNBMCHIP' = sub.fitFNBMCHIP,
#   'sub.fitHIPM' = sub.fitHIPM,
#   'sub.fitHIPBR' = sub.fitHIPBR,
#   'sub.fitHIPW' = sub.fitHIPW,
#   'sub.fitHIPED' = sub.fitHIPED,
#   'sub.fitHIPACT' = sub.fitHIPACT,
#   'sub.fitSBMC' = sub.fitSBMC
# )


lapply(model_list_linear, anova)
lapply(model_list_cubic, anova)


pdf('Obj1_Linear_Resid001.pdf', width = 11, height = 8.5)
par(mfrow = c(2,2))
lapply(seq_along(model_list_linear), function(i){
  plot(fitted(model_list_linear[[i]]), resid(model_list_linear[[i]]),
       main = paste('Model:', names(model_list_linear)[i]),
       xlab = 'Fitted Values',
       ylab = 'Residuals')
  # lines(
  #   lowess(
  #   na.omit(fitted(model_list[[i]])), 
  #   na.omit(resid(model_list[[i]])),
  #   f = 7/8
  #   ), col = 'red'
  #   )
  abline(h = 0, lty = 2)
  }
)

dev.off()

pdf('Obj1_Cubic_Resid001.pdf', width = 11, height = 8.5)
par(mfrow = c(2,2))
lapply(seq_along(model_list_cubic), function(i){
  plot(fitted(model_list_cubic[[i]]), resid(model_list_cubic[[i]]),
       main = paste('Model:', names(model_list_cubic)[i]),
       xlab = 'Fitted Values',
       ylab = 'Residuals')
  # lines(
  #   lowess(
  #     na.omit(fitted(sub_model_list[[i]])), 
  #     na.omit(resid(sub_model_list[[i]])),
  #     f = 7/8
  #   ), col = 'red'
  # )
  abline(h = 0, lty = 2)
}
)

dev.off()


#in each model we fit, the gymnastics coefficient was the fourth coefficient

gym_summary_lin <- lapply(model_list_linear, function(x) summary(x)$coefficients[4,c(1,4,5)])

gym_summary_lin <- t(do.call('data.frame', gym_summary_lin))

gym_summary_cub <- lapply(model_list_cubic, function(x) 
  summary(x)$coefficients[4,c(1,4,5)]
)

gym_summary_cub <- t(do.call('data.frame', gym_summary_cub))

fit.FNBMC_nlme <- lme(as.formula(paste0('Femoral.Neck.BMC.Hip', rhs_form_cubic_nlme)),
                 random = 
                   ~ 1 + Menarcheal.Age.at.DXA + I(Menarcheal.Age.at.DXA ^ 2) + I(Menarcheal.Age.at.DXA^3)| ID,
                 data = gym,
                 na.action = na.exclude)

summary_vec <- summary(fit.FNBMC_nlme)$tTable[4,c(1,4,5)]

gym_summary_cub <- rbind.data.frame(gym_summary_cub, summary_vec)

gym_summary_cub[,3] = round(gym_summary_cub[,3], 3)
# gym_models_1 <- setNames(cbind.data.frame(gym_summary_lin, gym_summary_cub),
#                          c('Estimate Linear', 't value Linear',
#                            'Estimate Cubic','t value Cubic'))



rownames(gym_summary_cub) <- c(response_names[-8], response_names[8])

summary_objective1_PERIPOST_IN_NON_NEVER <- gym_summary_cub

# gym_models_1

  # y = b0 + 

  #     b1 * SH LM + 
  #     b2 * Height + 
  #     b3 * GymGroup + 
  #     b4 * age + 
  #     b5 * age^2 + 
  #     b6 * age^3 + 
  #     b7 * AgeAtMenarcheGroup #might leave this term out
  #     eij +
  #     b0i


