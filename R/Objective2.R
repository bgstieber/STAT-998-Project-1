library(lme4)
library(lmerTest)
library(ggplot2)
library(multcomp)
library(dplyr)
theme_set(theme_bw())

gym <- read.csv('Gymnastics002.csv', stringsAsFactors = FALSE)

gym$YearsAfterGymQuit <- gym$Menarcheal.Age.at.DXA - gym$Menarcheal.Age.at.Quit.Date

gym$Group_Label <- c('PERI','POST','NON')[gym$Group.Division + 1]

gym$Group_Label2 <- ifelse(is.na(gym$YearsAfterGymQuit), 'Never',
                           ifelse(gym$YearsAfterGymQuit >= (90 / 365), 'Quit',
                                  'In Gymnastics'))
gym$Group_Label2 <- factor(gym$Group_Label2,
                           levels = c('Never','Quit','In Gymnastics'))
#dummy for gymnastics participation
#use for aim 1
gym$GymnasticsDummy <- ifelse(gym$Group_Label2 == 'Never', 0, 1) 

gym$ChronAgeAtMenarche_Group <- cut(gym$Chronologic.Age.at.Menarche,
                                    breaks = quantile(gym$Chronologic.Age.at.Menarche, seq(0,1,.25)),
                                    include.lowest = TRUE, 
                                    labels = c('low','medlow','medhigh','high'))

gym_sub <- gym[gym$Group_Label != 'POST',]

response_names <- c("Sub.head.BMC", "Distal.Radius.Third.Area", "Distal.bone.mineral.content.third", 
                    "Distal.Modulus.Third.Radius", "UD.Area.Radius", "UD.BMC", "UD.IBS.Radius", 
                    "Femoral.Neck.BMC.Hip", "NN.Section.Modulus.Hip", "NN.BR.Hip", 
                    "NN.width.Hip", "NN.ED.Hip", "NN.ACT.Hip", "PA.L3.BMC.Spine")

response_names_short <- c('SHBMC','DRA','DRBMC',
                          'DRM','UDRA','UDBMC','UDIBS',
                          'FNBMCHIP','HIPM','HIPBR','HIPW',
                          'HIPED','HIPACT','SBMC')

gym_sub[response_names] <- scale(gym_sub[response_names], center = T, scale = T)

rhs_form_PERINON <- "~ Sub.Head.LM + Standing.Height + Group_Label2 + Menarcheal.Age.at.DXA +
I(Menarcheal.Age.at.DXA ^ 2) + I(Menarcheal.Age.at.DXA^3) + ChronAgeAtMenarche_Group +
(1 + Menarcheal.Age.at.DXA + I(Menarcheal.Age.at.DXA ^ 2) + I(Menarcheal.Age.at.DXA^3)  | ID)"


iter <- 1

for(i in response_names){
  
  assign(paste0('fit', response_names_short[iter]),
         
         lmer(as.formula(paste0(i, rhs_form_PERINON)),
              data = gym_sub, na.action = na.exclude)
         
  )
  
  # assign(paste0('sub.fit', response_names_short[iter]),
  #        
  #        lmer(as.formula(paste0(i, rhs_form)),
  #             data = gym_sub[gym_sub$Menarcheal.Age.at.DXA <= 2.5, ],
  #             na.action = na.exclude)
  #        
  # )
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

# 
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


# lapply(model_list, anova)
# lapply(sub_model_list, anova)
# 
# 
# pdf('ResidPlots001_ob2.pdf', width = 11, height = 8.5)
# par(mfrow = c(2,2))
# lapply(seq_along(model_list), function(i){
#   plot(fitted(model_list[[i]]), resid(model_list[[i]]),
#        main = paste('Model:', names(model_list)[i]),
#        xlab = 'Fitted Values',
#        ylab = 'Residuals')
#   # lines(
#   #   lowess(
#   #   na.omit(fitted(model_list[[i]])), 
#   #   na.omit(resid(model_list[[i]])),
#   #   f = 7/8
#   #   ), col = 'red'
#   #   )
#   abline(h = 0, lty = 2)
# }
# )
# 
# dev.off()
# 
# pdf('SubResidPlots001_ob2.pdf', width = 11, height = 8.5)
# par(mfrow = c(2,2))
# lapply(seq_along(sub_model_list), function(i){
#   plot(fitted(sub_model_list[[i]]), resid(sub_model_list[[i]]),
#        main = paste('Model:', names(sub_model_list)[i]),
#        xlab = 'Fitted Values',
#        ylab = 'Residuals')
#   # lines(
#   #   lowess(
#   #     na.omit(fitted(sub_model_list[[i]])), 
#   #     na.omit(resid(sub_model_list[[i]])),
#   #     f = 7/8
#   #   ), col = 'red'
#   # )
#   abline(h = 0, lty = 2)
# }
# )
# 
# dev.off()


#in each model we fit, the gymnastics coefficient was the fourth coefficient

gym_summaryQUIT <- lapply(model_list, function(x) summary(x)$coefficients[4,c(1,4,5)])

gym_summaryQUIT <- t(do.call('data.frame', gym_summaryQUIT))

rownames(gym_summaryQUIT) <- response_names

gym_summaryQUIT[,3] <- round(gym_summaryQUIT[,3], 3)

summary_objective2_PERI_QUIT_NON_NEVER <- gym_summaryQUIT

# gym_summary_subQUIT <- lapply(sub_model_list, function(x) summary(x)$coefficients[4,c(1,3)])
# 
# gym_summary_subQUIT <- t(do.call('data.frame', gym_summary_subQUIT))

# 
# gym_models_QUIT <- setNames(cbind.data.frame(gym_summaryQUIT, gym_summary_subQUIT),
#                          c('Estimate Full', 't value Full',
#                            'Estimate Subset','t value Subset'))


gym_summaryIN <- lapply(model_list, function(x) summary(x)$coefficients[5,c(1,4,5)])

gym_summaryIN <- t(do.call('data.frame', gym_summaryIN))

rownames(gym_summaryIN) <- response_names

gym_summaryIN[,3] <- round(gym_summaryIN[,3], 3)

summary_objective2_PERI_IN_NON_NEVER <- gym_summaryIN

# gym_summary_subIN <- lapply(sub_model_list, function(x) summary(x)$coefficients[5,c(1,3)])
# 
# gym_summary_subIN <- t(do.call('data.frame', gym_summary_subIN))
# 
# 
# gym_models_IN <- setNames(cbind.data.frame(gym_summaryIN, gym_summary_subIN),
#                             c('Estimate Full', 't value Full',
#                               'Estimate Subset','t value Subset'))


gym_sub2 <- gym[gym$Group_Label != 'NON',]

gym_sub2$InGymnastics <- ifelse(gym_sub2$Group_Label2 != 'Quit', 1, 0)

gym_sub2$my_interaction <- interaction(gym_sub2$Group_Label, gym_sub2$InGymnastics)

gym_sub2[response_names] <- scale(gym_sub2[response_names], scale = T, center = T)

rhs_form_PERIPOST <- "~ Sub.Head.LM + Standing.Height + my_interaction + Menarcheal.Age.at.DXA +
I(Menarcheal.Age.at.DXA ^ 2) + I(Menarcheal.Age.at.DXA^3) + ChronAgeAtMenarche_Group +
(1 + Menarcheal.Age.at.DXA + I(Menarcheal.Age.at.DXA ^ 2) + I(Menarcheal.Age.at.DXA^3)  | ID)"


iter <- 1

for(i in response_names){
  
  assign(paste0('fit', response_names_short[iter]),
         
         lmer(as.formula(paste0(i, rhs_form_PERIPOST)),
              data = gym_sub2, na.action = na.exclude)
         
  )
  
  # assign(paste0('sub.fit', response_names_short[iter]),
  #        
  #        lmer(as.formula(paste0(i, rhs_form)),
  #             data = gym_sub[gym_sub$Menarcheal.Age.at.DXA <= 2.5, ],
  #             na.action = na.exclude)
  #        
  # )
  iter = iter + 1
}

model_listPERIPOST <- list(
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

##multiple comparisons tests

#compare PERI QUIT to POST QUIT
glht(model_listPERIPOST$fitSHBMC, linfct = mcp(my_interaction = "POST.0 - PERI.0 = 0")) %>% summary


lapply(model_listPERIPOST, function(x) 
    broom::tidy(summary(glht(x, linfct = mcp(my_interaction = "POST.0 - PERI.0 = 0"))))) %>% 
  data.table::rbindlist() %>% 
  cbind(response_names, .) %>% 
  dplyr::select(response_names, estimate, statistic, p.value) %>% 
  dplyr::mutate(p.value = round(p.value, 3)) -> summary_objective2_PERI_QUIT_POST_QUIT

#compare QUIT to IN

lapply(model_listPERIPOST, function(x) 
    broom::tidy(summary(glht(x, 
    linfct = mcp(my_interaction = "POST.1 + PERI.1 - POST.0 - PERI.0 = 0"))))) %>% 
  data.table::rbindlist() %>% 
  cbind(response_names, .) %>% 
  dplyr::select(response_names, estimate, statistic, p.value) %>% 
  dplyr::mutate(p.value = round(p.value, 3)) -> summary_objective2_PERIPOST_IN_PERIPOST_QUIT


#compare PERI in to PERI quit


lapply(model_listPERIPOST, 
       function(x) broom::tidy(summary(
           glht(x, linfct = mcp(my_interaction = "PERI.1 - PERI.0 = 0"))))) %>% 
    data.table::rbindlist() %>% 
    cbind(response_names, .) %>% 
    dplyr::select(response_names, estimate, statistic, p.value) %>% 
    dplyr::mutate(p.value = round(p.value, 3)) -> summary_objective2_PERI_IN_PERI_QUIT

