library(plyr)
library(dplyr)
library(knitr)
#manipulate and clean up tables so they're report ready
#load each csv
objective1_results <- read.csv('summary_objective1.csv',
                               stringsAsFactors = FALSE)
#reorder
objective1_results <- objective1_results[c(1:7,14,8:13),]

#compares peri group while in gymnastics to non group
objective2_PERIIn_NONNever <- read.csv('summary_objective2_PERIIn_NONNever.csv',
                                       stringsAsFactors = FALSE)
#compares peri group after quitting to non group
objective2_PERIQuit_NONNever <- read.csv('summary_objective2_PERIQuit_NONNever.csv',
                                         stringsAsFactors = FALSE)

#compares PERI and POST while in gymnastics to PERI and POST after they've quit
objective2_PERIPOSTin_PERIPOSTquit <- read.csv('summary_objective2_PERIPOSTin_PERIPOSTquit.csv',
                                       stringsAsFactors = FALSE)[,-1]
#compares POST quit to PERI quit
objective2_PERIQuit_POSTQuit <- read.csv('summary_objective2_PERIQuit_POSTQuit.csv',
                                       stringsAsFactors = FALSE)[,-1]


change_col_names <- c('Response Variable',
                  'Estimate',
                  't value',
                  'p value')
    
names(objective1_results) <- change_col_names
names(objective2_PERIIn_NONNever) <- change_col_names
names(objective2_PERIQuit_NONNever) <- change_col_names
names(objective2_PERIPOSTin_PERIPOSTquit) <- change_col_names
names(objective2_PERIQuit_POSTQuit) <- change_col_names

#BMC - Bone Mineral Content
#DR - Distal Radius
#Mod - Section Modulus
#UDR - ultra-distal radius
#IBS - index of structural strength
#NN - narrow neck
#BR - Buckling Ratio
#ED - Endosteal Diameter
#CT - Cortical Thickness
#PA L3 - lumbar spine vertebra 3
#femoral neck

change_response_names <- c(
    'Sub-head BMC',
    'DR 1/3 Area',
    'DR 1/3 BMC',
    'DR 1/3 Mod',
    'UDR Area',
    'UDR BMC',
    'UDR IBS',
    'FN BMC',
    'NN Mod',
    'NN BR',
    'NN Width',
    'NN ED',
    'NN CT',
    'PA L3 BMC'
)

objective1_results[,1] <- change_response_names
objective2_PERIIn_NONNever[,1] <- change_response_names
objective2_PERIQuit_NONNever[,1] <- change_response_names
objective2_PERIPOSTin_PERIPOSTquit[,1] <- change_response_names
objective2_PERIQuit_POSTQuit[,1] <- change_response_names

rownames(objective1_results) <- NULL
rownames(objective2_PERIIn_NONNever) <- NULL
rownames(objective2_PERIQuit_NONNever) <- NULL
rownames(objective2_PERIPOSTin_PERIPOSTquit) <- NULL
rownames(objective2_PERIQuit_POSTQuit) <- NULL

round_all <- function(df, digits = 3){
    mutate_each(df, funs(round(.,digits = digits)), c(-`Response Variable`))
}

(objective1_results <- round_all(objective1_results))
(objective2_PERIIn_NONNever <- round_all(objective2_PERIIn_NONNever))
(objective2_PERIQuit_NONNever <- round_all(objective2_PERIQuit_NONNever))
(objective2_PERIPOSTin_PERIPOSTquit <- round_all(objective2_PERIPOSTin_PERIPOSTquit))
(objective2_PERIQuit_POSTQuit <- round_all(objective2_PERIQuit_POSTQuit))


#clean up pairwise comparisons table,
#put it into a correlation plot - not implemented yet
#save it as a pdf - not implemented yet


library(corrgram)

objective3_results <-read.csv('summary_objective3_pairwisecomps.csv', 
                              stringsAsFactors = FALSE)[,-1]

model_names <- c("allME.fitSHBMC", "allME.fitDRA", "allME.fitDRBMC", "allME.fitDRM", 
                 "allME.fitUDRA", "allME.fitUDBMC", "allME.fitUDIBS", "allME.fitFNBMCHIP", 
                 "allME.fitHIPM", "allME.fitHIPBR", "allME.fitHIPW", "allME.fitHIPED", 
                 "allME.fitHIPACT", "allME.fitSBMC")

response_names <- c('Sub-head BMC', 'DR 1/3 Area', 'DR 1/3 BMC','DR 1/3 Mod',
                    'UDR Area', 'UDR BMC', 'UDR IBS', 'FN BMC', 'NN Mod',
                    'NN BR', 'NN Width', 'NN ED', 'NN CT', 'PA L3 BMC')

lookup_table <- data.frame('match' = model_names,
                           'replace' = response_names,
                           stringsAsFactors = FALSE)

objective3_results[,1] <- lookup_table[match(objective3_results[,1], lookup_table[,1]), 2]
objective3_results[,2] <- lookup_table[match(objective3_results[,2], lookup_table[,1]), 2]

names(objective3_results) <- c('LHS', 'RHS', 'LHS GTE RHS','p-value',
                               'p-value adjusted')
#put percentages into a matrix
comps <- t(combn(14,2))
comp_matrix_perc <- diag(14)
comp_matrix_num <- diag(14)


for(i in 1:nrow(comps)){
    
    comp_matrix_perc[comps[i,1], comps[i,2]] <- 
        
        paste(scales::percent(objective3_results[i, 3]),
              ifelse(objective3_results[i,]$`p-value adjusted` < .05, '*',''))
    
    comp_matrix_perc[comps[i,2], comps[i,1]] <-         
        paste(scales::percent(objective3_results[i, 3]),
              ifelse(objective3_results[i,]$`p-value adjusted` < .05, '*',''))
    
    
    comp_matrix_num[comps[i,1], comps[i,2]] <- objective3_results[i, 3]
    
    comp_matrix_num[comps[i,2], comps[i,1]] <- objective3_results[i, 3]
    
}


