## examine correlations between t_i and t_{i-1}
## gym data set should be loaded 
source('PrepWork.R')
head(gym)

#examine correlations for BMC variables

bmc_names <- grep(names(gym), pattern = 'BMC', value = TRUE)

bmc_ID <- gym[c('ID', 'TimeBetweenMeasurements', bmc_names)]

bmc_ID <- bmc_ID %>%
  group_by(ID) %>%
  mutate_each_(funs(lagged = lag(.)), vars = bmc_names)

#create groups based on quantiles of TimeBetweenMeasurements
#summarise correlations based on these groups

bmc_ID$TimeGroup <- 
  cut(bmc_ID$TimeBetweenMeasurements,
    breaks = quantile(bmc_ID$TimeBetweenMeasurements,
                      probs = seq(0,1,.1),
                      na.rm = T), include.lowest = TRUE,
    labels = 1:10)

do.call('rbind',
by(bmc_ID, bmc_ID$TimeGroup, function(x){
  c(
   'MinTime' = min(x$TimeBetweenMeasurements, na.rm = T),
   'MaxTime' = max(x$TimeBetweenMeasurements, na.rm = T),
   'Count' = nrow(x),
   'SubheadCor' = cor(x$Sub.head.BMC, x$Sub.head.BMC_lagged, use = 'pair'),
   'UDCor' = cor(x$UD.BMC, x$UD.BMC_lagged, use = 'pair'),
   'FemoralNeckCor' = cor(x$Femoral.Neck.BMC.Hip, x$Femoral.Neck.BMC.Hip_lagged, use = 'pair'),
   'SpineCor' = cor(x$PA.L3.BMC.Spine, x$PA.L3.BMC.Spine_lagged, use = 'pair')
  )
}
)
) -> bmc_corrs

par(mfrow = c(2,2))
for(i in 4:7){
  plot(1:10, bmc_corrs[,i],
      xlab = 'Time Between Measures Rank',
      ylab = dimnames(bmc_corrs)[[2]][i],
      type = 'b')
  lines(lowess(1:10, bmc_corrs[,i]), col = 'blue')
}


#for bmc it looks fairly safe to assume constant correlation between measurement times

##now we repeat the process for other variables

radius_names <- c("Distal.Radius.Third.Area", "Distal.bone.mineral.content.third", 
                  "Distal.Modulus.Third.Radius", "UD.Area.Radius", "UD.BMC", "UD.IBS.Radius")

radius_ID <- gym[c('ID','TimeBetweenMeasurements',radius_names)]

radius_ID <- radius_ID %>%
  group_by(ID) %>%
  mutate_each_(funs(lagged = lag(.)), vars = radius_names)

radius_ID$TimeGroup <- 
  cut(radius_ID$TimeBetweenMeasurements,
      breaks = quantile(radius_ID$TimeBetweenMeasurements, na.rm = T,
                        probs = seq(0,1,.1)),
      include.lowest = TRUE, labels = 1:10)

do.call('rbind',
        by(radius_ID, radius_ID$TimeGroup, function(x){
          c(
            'MinTime' = min(x$TimeBetweenMeasurements, na.rm = T),
            'MaxTime' = max(x$TimeBetweenMeasurements, na.rm = T),
            'Count' = nrow(x),
            'DistalBMC' = cor(x$Distal.bone.mineral.content.third,
                              x$Distal.bone.mineral.content.third_lagged, use = 'pair'),
            'DistalModulus' = cor(x$Distal.Modulus.Third.Radius,
                                  x$Distal.Modulus.Third.Radius_lagged, use = 'pair'),
            'DistalRadius' = cor(x$Distal.Radius.Third.Area, 
                                   x$Distal.Radius.Third.Area_lagged, use = 'pair'),
            'UDAreaRadius' = cor(x$UD.Area.Radius,
                             x$UD.Area.Radius_lagged, use = 'pair'),
            'UDBMC' = cor(x$UD.BMC,
                                 x$UD.BMC_lagged, use = 'pair'),
            'UDIBS' = cor(x$UD.IBS.Radius,
                                 x$UD.IBS.Radius_lagged, use = 'pair')
          )
        }
        )
) -> radius_corrs


par(mfrow = c(2,3))

for(i in 4:ncol(radius_corrs)) {
  plot(1:10, radius_corrs[,i],
       xlab = 'Time Between Measures Rank',
       ylab = dimnames(radius_corrs)[[2]][i],
       type = 'b')
  lines(lowess(1:10, radius_corrs[,i]), col = 'blue')
}
#obviously decreases toward the end, 
#but it seems __okay__ to consider constant correlation

#repeat for HIP

hip_names <- c("Femoral.Neck.BMC.Hip", "NN.Section.Modulus.Hip", "NN.BR.Hip", 
               "NN.width.Hip", "NN.ED.Hip", "NN.ACT.Hip")


hip_ID <- gym[c('ID','TimeBetweenMeasurements',hip_names)]

hip_ID <- hip_ID %>%
  group_by(ID) %>%
  mutate_each_(funs(lagged = lag(.)), vars = hip_names)

hip_ID$TimeGroup <- 
  cut(hip_ID$TimeBetweenMeasurements,
      breaks = quantile(hip_ID$TimeBetweenMeasurements, na.rm = T,
                        probs = seq(0,1,.1)),
      include.lowest = TRUE, labels = 1:10)

do.call('rbind',
        by(hip_ID, hip_ID$TimeGroup, function(x){
          c(
            'MinTime' = min(x$TimeBetweenMeasurements, na.rm = T),
            'MaxTime' = max(x$TimeBetweenMeasurements, na.rm = T),
            'Count' = nrow(x),
            'FemoralNeckBMC' = cor(x$Femoral.Neck.BMC.Hip,
                              x$Femoral.Neck.BMC.Hip_lagged, use = 'pair'),
            'NNACTHIP' = cor(x$NN.ACT.Hip,
                                  x$NN.ACT.Hip_lagged, use = 'pair'),
            'NNBRHIP' = cor(x$NN.BR.Hip, 
                                 x$NN.BR.Hip_lagged, use = 'pair'),
            'NNEDHIP' = cor(x$NN.ED.Hip,
                                 x$NN.ED.Hip_lagged, use = 'pair'),
            'NNModulusHip' = cor(x$NN.Section.Modulus.Hip,
                          x$NN.Section.Modulus.Hip_lagged, use = 'pair'),
            'UDIBS' = cor(x$NN.width.Hip,
                          x$NN.width.Hip_lagged, use = 'pair')
          )
        }
        )
) -> hip_corrs


par(mfrow = c(2,3))

for(i in 4:ncol(hip_corrs)){
  plot(1:10, hip_corrs[,i],
       xlab = 'Time Between Measures Rank',
       ylab = dimnames(hip_corrs)[[2]][i],
       type = 'b')
  lines(lowess(1:10, hip_corrs[,i]), col = 'blue')
  
}