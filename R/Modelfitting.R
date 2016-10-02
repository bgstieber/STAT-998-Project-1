library(lme4)
library(ggplot2)
theme_set(theme_bw())

gym <- read.csv('Gymnastics002.csv', stringsAsFactors = FALSE)

gym$YearsAfterGymQuit <- gym$Menarcheal.Age.at.DXA - gym$Menarcheal.Age.at.Quit.Date

gym$Group_Label <- c('PERI','POST','NON')[gym$Group.Division + 1]

gym$Group_Label2 <- ifelse(is.na(gym$YearsAfterGymQuit), 'Never',
                           ifelse(gym$YearsAfterGymQuit >= (45 / 365), 'Quit',
                                  'In Gymnastics'))

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

rhs_form <- "~ Sub.Head.LM + Standing.Height + Group_Label2 + Menarcheal.Age.at.DXA +
I(Menarcheal.Age.at.DXA ^ 2) + I(Menarcheal.Age.at.DXA^3) + ChronAgeAtMenarche_Group + 
(Standing.Height | ID)"

# fitSHBMC <- lmer(Sub.head.BMC ~
#                Sub.Head.LM + 
#                Standing.Height + 
#                Group_Label2 + 
#                Menarcheal.Age.at.DXA +
#                I(Menarcheal.Age.at.DXA ^ 2) + 
#                I(Menarcheal.Age.at.DXA^3) + 
#                ChronAgeAtMenarche_Group + 
#               (Standing.Height | ID),
#              data = gym)

iter <- 1

for(i in response_names){
  
  assign(paste0('fit', response_names_short[iter]),
         
         lmer(as.formula(paste0(i, rhs_form)),
              data = gym, na.action = na.exclude)
         
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

lapply(model_list, anova)

# par(mfrow = c(2,2))
# 
# pdf('ResidPlots.pdf', width = 11, height = 8.5)

lapply(seq_along(model_list), function(i){
  plot(fitted(model_list[[i]]), resid(model_list[[i]]),
       main = paste('Model:', names(model_list)[i]),
       xlab = 'Fitted Values',
       ylab = 'Residuals')
  lines(
    lowess(
    na.omit(fitted(model_list[[i]])), 
    na.omit(resid(model_list[[i]])),
    f = 7/8
    ), col = 'red'
    )
  }
)

# dev.off()

  # y = b0 + 
  #     b1 * SH LM + 
  #     b2 * Height + 
  #     b3 * GymGroup + 
  #     b4 * age + 
  #     b5 * age^2 + 
  #     b6 * age^3 + 
  #     b7 * AgeAtMenarcheGroup
  #     eij +
  #     b0i + b2i * Height