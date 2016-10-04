library(lme4)
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

rhs_form <- "~ Sub.Head.LM + Standing.Height + GymnasticsDummy + Menarcheal.Age.at.DXA +
I(Menarcheal.Age.at.DXA ^ 2) + I(Menarcheal.Age.at.DXA^3) + ChronAgeAtMenarche_Group + 
(1 + Menarcheal.Age.at.DXA | ID)"

fitSHBMC <- lmer(Sub.head.BMC ~ GymnasticsDummy +
                   Sub.Head.LM + 
                   Standing.Height + 
                   Menarcheal.Age.at.DXA +
                   I(Menarcheal.Age.at.DXA ^ 2) + 
                   I(Menarcheal.Age.at.DXA^3) +
                   ChronAgeAtMenarche_Group + 
                   (1 + Menarcheal.Age.at.DXA | ID),
             data = gym)


fitSHBMC.sub <- lmer(Sub.head.BMC ~ GymnasticsDummy +
                   Sub.Head.LM + 
                   Standing.Height + 
                   Menarcheal.Age.at.DXA +
                   I(Menarcheal.Age.at.DXA ^ 2) + 
                   I(Menarcheal.Age.at.DXA^3) +
                   ChronAgeAtMenarche_Group + 
                   (1 + Menarcheal.Age.at.DXA | ID),
                 data = gym[gym$Menarcheal.Age.at.DXA <= 2.5, ])

iter <- 1

for(i in response_names){
  
  assign(paste0('fit', response_names_short[iter]),
         
         lmer(as.formula(paste0(i, rhs_form)),
              data = gym, na.action = na.exclude)
         
         )
  
  assign(paste0('sub.fit', response_names_short[iter]),
         
         lmer(as.formula(paste0(i, rhs_form)),
              data = gym[gym$Menarcheal.Age.at.DXA <= 2.5, ],
              na.action = na.exclude)
         
  )
  iter = iter + 1
}

model_list <- list(
  'fitSHBMC' = fitSHBMC,
  'fitDRA' = fitDRA,
  'fitDRBMC' = fitDRBMC,
  'fitDRM' = fitDRM,
  'fitUDRA' = fitUDRA,
  'fitUDBMC' = fitUDBMC,
  'fitUDIBS' = fitUDIBS,
  'fitFNBMCHIP' = fitFNBMCHIP,
  'fitHIPM' = fitHIPM,
  'fitHIPBR' = fitHIPBR,
  'fitHIPW' = fitHIPW,
  'fitHIPED' = fitHIPED,
  'fitHIPACT' = fitHIPACT,
  'fitSBMC' = fitSBMC
)


sub_model_list <- list(
  'sub.fitSHBMC' = sub.fitSHBMC,
  'sub.fitDRA' = sub.fitDRA,
  'sub.fitDRBMC' = sub.fitDRBMC,
  'sub.fitDRM' = sub.fitDRM,
  'sub.fitUDRA' = sub.fitUDRA,
  'sub.fitUDBMC' = sub.fitUDBMC,
  'sub.fitUDIBS' = sub.fitUDIBS,
  'sub.fitFNBMCHIP' = sub.fitFNBMCHIP,
  'sub.fitHIPM' = sub.fitHIPM,
  'sub.fitHIPBR' = sub.fitHIPBR,
  'sub.fitHIPW' = sub.fitHIPW,
  'sub.fitHIPED' = sub.fitHIPED,
  'sub.fitHIPACT' = sub.fitHIPACT,
  'sub.fitSBMC' = sub.fitSBMC
)


lapply(model_list, anova)
lapply(sub_model_list, anova)


pdf('ResidPlots003.pdf', width = 11, height = 8.5)
par(mfrow = c(2,2))
lapply(seq_along(model_list), function(i){
  plot(fitted(model_list[[i]]), resid(model_list[[i]]),
       main = paste('Model:', names(model_list)[i]),
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

pdf('SubResidPlots003.pdf', width = 11, height = 8.5)
par(mfrow = c(2,2))
lapply(seq_along(sub_model_list), function(i){
  plot(fitted(sub_model_list[[i]]), resid(sub_model_list[[i]]),
       main = paste('Model:', names(sub_model_list)[i]),
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

gym_summary <- lapply(model_list, function(x) summary(x)$coefficients[4,c(1,3)])

gym_summary <- t(do.call('data.frame', gym_summary))

gym_summary_sub <- lapply(sub_model_list, function(x) summary(x)$coefficients[4,c(1,3)])

gym_summary_sub <- t(do.call('data.frame', gym_summary_sub))


gym_models_1 <- setNames(cbind.data.frame(gym_summary, gym_summary_sub),
                         c('Estimate Full', 't value Full',
                           'Estimate Subset','t value Subset'))


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


