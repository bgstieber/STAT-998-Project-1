## examine correlations between t_i and t_{i-1}
## gym data set should be loaded 
library(dplyr)

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