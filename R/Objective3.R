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

age_to_sub <- 2.5

gym_sub <- gym[gym$Menarcheal.Age.at.DXA <= age_to_sub, ]

gym_sub_response <- gym_sub[response_names]

gym_sub_response_center <- scale(gym_sub_response, center = T, scale = T)

colnames(gym_sub_response_center) <- paste0(response_names,
                                            '_norm')

gym_sub <- cbind.data.frame(gym_sub, gym_sub_response_center)


gym_response <- gym[response_names]

gym_response_center <- scale(gym_response, center = T, scale = T)

colnames(gym_response_center) <- paste0(response_names,
                                        '_norm')

gym <- cbind.data.frame(gym, gym_response_center)

#fit the damn models

response_names <- paste0(response_names, '_norm')

response_names_short <- paste0(c('SHBMC','DRA','DRBMC',
                          'DRM','UDRA','UDBMC','UDIBS',
                          'FNBMCHIP','HIPM','HIPBR','HIPW',
                          'HIPED','HIPACT','SBMC'),
                          'norm')

rhs_form <- "~ Sub.Head.LM + Standing.Height + GymnasticsDummy + Menarcheal.Age.at.DXA +
I(Menarcheal.Age.at.DXA ^ 2) + I(Menarcheal.Age.at.DXA^3) + ChronAgeAtMenarche_Group + 
(1 + Menarcheal.Age.at.DXA | ID)"

fitSHBMC <- lmer(Sub.head.BMC_norm ~ GymnasticsDummy +
                   Sub.Head.LM + 
                   Standing.Height + 
                   Menarcheal.Age.at.DXA +
                   I(Menarcheal.Age.at.DXA ^ 2) + 
                   I(Menarcheal.Age.at.DXA^3) +
                   ChronAgeAtMenarche_Group + 
                   (1 + Menarcheal.Age.at.DXA | ID),
                 data = gym)


fitSHBMC.sub <- lmer(Sub.head.BMC_norm ~ GymnasticsDummy +
                       Sub.Head.LM + 
                       Standing.Height + 
                       Menarcheal.Age.at.DXA +
                       I(Menarcheal.Age.at.DXA ^ 2) + 
                       I(Menarcheal.Age.at.DXA^3) +
                       ChronAgeAtMenarche_Group + 
                       (1 + Menarcheal.Age.at.DXA | ID),
                     data = gym_sub)

iter <- 1

for(i in response_names){
  
  assign(paste0('fit', response_names_short[iter]),
         
         lmer(as.formula(paste0(i, rhs_form)),
              data = gym, na.action = na.exclude)
         
  )
  
  assign(paste0('sub.fit', response_names_short[iter]),
         
         lmer(as.formula(paste0(i, rhs_form)),
              data = gym_sub, 
              na.action = na.exclude)
         
  )
  iter = iter + 1
}

model_list <- list(
  'fitSHBMC' = fitSHBMCnorm,
  'fitDRA' = fitDRAnorm,
  'fitDRBMC' = fitDRBMCnorm,
  'fitDRM' = fitDRMnorm,
  'fitUDRA' = fitUDRAnorm,
  'fitUDBMC' = fitUDBMCnorm,
  'fitUDIBS' = fitUDIBSnorm,
  'fitFNBMCHIP' = fitFNBMCHIPnorm,
  'fitHIPM' = fitHIPMnorm,
  'fitHIPBR' = fitHIPBRnorm,
  'fitHIPW' = fitHIPWnorm,
  'fitHIPED' = fitHIPEDnorm,
  'fitHIPACT' = fitHIPACTnorm,
  'fitSBMC' = fitSBMCnorm
)


sub_model_list <- list(
  'sub.fitSHBMC' = sub.fitSHBMCnorm,
  'sub.fitDRA' = sub.fitDRAnorm,
  'sub.fitDRBMC' = sub.fitDRBMCnorm,
  'sub.fitDRM' = sub.fitDRMnorm,
  'sub.fitUDRA' = sub.fitUDRAnorm,
  'sub.fitUDBMC' = sub.fitUDBMCnorm,
  'sub.fitUDIBS' = sub.fitUDIBSnorm,
  'sub.fitFNBMCHIP' = sub.fitFNBMCHIPnorm,
  'sub.fitHIPM' = sub.fitHIPMnorm,
  'sub.fitHIPBR' = sub.fitHIPBRnorm,
  'sub.fitHIPW' = sub.fitHIPWnorm,
  'sub.fitHIPED' = sub.fitHIPEDnorm,
  'sub.fitHIPACT' = sub.fitHIPACTnorm,
  'sub.fitSBMC' = sub.fitSBMCnorm
)

lapply(model_list, anova)
lapply(sub_model_list, anova)

#grab predicted values

preds_full_models <- do.call('cbind', lapply(model_list, predict))
preds_full_models <- cbind.data.frame('Gymnastics' = gym$GymnasticsDummy, 
                                      preds_full_models)

preds_sub_models <- do.call('cbind', lapply(sub_model_list, predict))
preds_sub_models <- cbind('Gymnastics' = gym_sub$GymnasticsDummy, 
                          preds_sub_models)