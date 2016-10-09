library(plyr)
library(dplyr)
library(knitr)
#manipulate and clean up tables so they're report ready
#load each csv
objective1_results <- read.csv('summary_objective1.csv',
                               stringsAsFactors = FALSE)
#reorder
objective1_results <- objective1_results[c(1:7,14,8:13),]

objective1_standardized_results <- read.csv('Objective1_Standardized_coef.csv')

#compares peri group while in gymnastics to non group
objective2_PERIIn_NONNever <- read.csv('summary_objective2_PERIIn_NONNever.csv',
                                       stringsAsFactors = FALSE)

objective2_PERIIn_NONNever_stdzd <- read.csv('summary_objective2_PERIIn_NONNever_stdzd.csv',
                                       stringsAsFactors = FALSE)

#compares peri group after quitting to non group
objective2_PERIQuit_NONNever <- read.csv('summary_objective2_PERIQuit_NONNever.csv',
                                         stringsAsFactors = FALSE)

objective2_PERIQuit_NONNever_stdzd <- read.csv('summary_objective2_PERIQuit_NONNever_stdzd.csv',
                                         stringsAsFactors = FALSE)

#compares PERI and POST while in gymnastics to PERI and POST after they've quit
objective2_PERIPOSTin_PERIPOSTquit <- read.csv('summary_objective2_PERIPOSTin_PERIPOSTquit.csv',
                                       stringsAsFactors = FALSE)[,-1]

objective2_PERIPOSTIn_PERIPOSTQuit_stdzd <- 
    read.csv('summary_objective2_PERIPOSTIn_PERIPOSTquit_stdzd.csv',
                                               stringsAsFactors = FALSE)[,-1]

#compares POST quit to PERI quit
objective2_PERIQuit_POSTQuit <- read.csv('summary_objective2_PERIQuit_POSTQuit.csv',
                                       stringsAsFactors = FALSE)[,-1]

objective2_PERIQuit_POSTQuit_stdzd <- read.csv('summary_objective2_PERIQuit_POSTQuit_stdzd.csv',
                                         stringsAsFactors = FALSE)[,-1]




change_col_names <- c('Response Variable',
                  'Estimate',
                  't value',
                  'p value')
    
names(objective1_results) <- change_col_names
names(objective1_standardized_results) <- change_col_names
names(objective2_PERIIn_NONNever) <- change_col_names
names(objective2_PERIQuit_NONNever) <- change_col_names
names(objective2_PERIPOSTin_PERIPOSTquit) <- change_col_names
names(objective2_PERIQuit_POSTQuit) <- change_col_names

names(objective2_PERIIn_NONNever_stdzd) <- change_col_names
names(objective2_PERIQuit_NONNever_stdzd) <- change_col_names
names(objective2_PERIPOSTIn_PERIPOSTQuit_stdzd) <- change_col_names
names(objective2_PERIQuit_POSTQuit_stdzd) <- change_col_names

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
objective1_standardized_results[,1] <- change_response_names
objective2_PERIIn_NONNever[,1] <- change_response_names
objective2_PERIQuit_NONNever[,1] <- change_response_names
objective2_PERIPOSTin_PERIPOSTquit[,1] <- change_response_names
objective2_PERIQuit_POSTQuit[,1] <- change_response_names

objective2_PERIIn_NONNever_stdzd[,1] <- change_response_names
objective2_PERIQuit_NONNever_stdzd[,1] <- change_response_names
objective2_PERIPOSTIn_PERIPOSTQuit_stdzd[,1] <- change_response_names
objective2_PERIQuit_POSTQuit_stdzd[,1] <- change_response_names

rownames(objective1_results) <- NULL
rownames(objective2_PERIIn_NONNever) <- NULL
rownames(objective2_PERIQuit_NONNever) <- NULL
rownames(objective2_PERIPOSTin_PERIPOSTquit) <- NULL
rownames(objective2_PERIQuit_POSTQuit) <- NULL

round_all <- function(df, digits = 3){
    mutate_each(df, funs(round(.,digits = digits)), c(-`Response Variable`))
}

(objective1_results <- round_all(objective1_results))
(objective1_standardized_results <- round_all(objective1_standardized_results))
(objective2_PERIIn_NONNever <- round_all(objective2_PERIIn_NONNever))
(objective2_PERIQuit_NONNever <- round_all(objective2_PERIQuit_NONNever))
(objective2_PERIPOSTin_PERIPOSTquit <- round_all(objective2_PERIPOSTin_PERIPOSTquit))
(objective2_PERIQuit_POSTQuit <- round_all(objective2_PERIQuit_POSTQuit))

(objective2_PERIIn_NONNever_stdzd <- round_all(objective2_PERIIn_NONNever_stdzd))
(objective2_PERIQuit_NONNever_stdzd <- round_all(objective2_PERIQuit_NONNever_stdzd))
(objective2_PERIPOSTIn_PERIPOSTQuit_stdzd <- round_all(objective2_PERIPOSTIn_PERIPOSTQuit_stdzd))
(objective2_PERIQuit_POSTQuit_stdzd <- round_all(objective2_PERIQuit_POSTQuit_stdzd))


objective1_results$`Standardized Estimate` = objective1_standardized_results$Estimate
objective1_results <- objective1_results[,c(1,2, 5,3,4)]

objective2_PERIIn_NONNever$`Standardized Estimate` <- objective2_PERIIn_NONNever$Estimate
objective2_PERIIn_NONNever <- objective2_PERIIn_NONNever[,c(1,2,5,3,4)]

objective2_PERIQuit_NONNever$`Standardized Estimate` <- objective2_PERIQuit_NONNever_stdzd$Estimate
objective2_PERIQuit_NONNever <- objective2_PERIQuit_NONNever[,c(1,2,5,3,4)]

objective2_PERIPOSTin_PERIPOSTquit$`Standardized Estimate` <- objective2_PERIPOSTIn_PERIPOSTQuit_stdzd$Estimate
objective2_PERIPOSTin_PERIPOSTquit <- objective2_PERIPOSTin_PERIPOSTquit[,c(1,2,5,3,4)] 

objective2_PERIQuit_POSTQuit$`Standardized Estimate` <- objective2_PERIQuit_POSTQuit$Estimate
objective2_PERIQuit_POSTQuit <- objective2_PERIQuit_POSTQuit[,c(1,2,5,3,4)]

#clean up pairwise comparisons table,
#put it into a correlation plot - not implemented yet
#save it as a pdf - not implemented yet


library(corrgram)

# objective3_results <-read.csv('summary_objective3_pairwisecomps.csv', 
#                               stringsAsFactors = FALSE)[,-1]

objective3_results <-read.csv('summary_objective3_pairwisecomps002.csv', 
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
comp_matrix_pvalue <- diag(14)



for(i in 1:nrow(comps)){
    
    comp_matrix_perc[comps[i,1], comps[i,2]] <- 
        
        paste(scales::percent(objective3_results[i, 3]),
              ifelse(objective3_results[i,]$`p-value adjusted` < .05, '*',''))
    
    comp_matrix_perc[comps[i,2], comps[i,1]] <-         
        paste(scales::percent(objective3_results[i, 3]),
              ifelse(objective3_results[i,]$`p-value adjusted` < .05, '*',''))
    
    
    comp_matrix_num[comps[i,1], comps[i,2]] <- objective3_results[i, 3]
    
    comp_matrix_num[comps[i,2], comps[i,1]] <- objective3_results[i, 3]
    
    
    comp_matrix_pvalue[comps[i,1], comps[i,2]] <- objective3_results[i, 5]
    
    comp_matrix_pvalue[comps[i,2], comps[i,1]] <- objective3_results[i, 5]
    
}


# panel.cor2 <- function (x, y, corr = NULL, cor.method = 'pearson', digits = 2, 
#                         cex.cor, ...) 
# {
#     if (is.null(corr)) {
#         if (sum(complete.cases(x, y)) < 2) {
#             warning("Need at least 2 complete cases for cor()")
#             return()
#         }
#         else {
#             corr <- cor(x, y, use = "pair", method = cor.method)
#         }
#     }
#     auto <- missing(cex.cor)
#     usr <- par("usr")
#     on.exit(par(usr))
#     par(usr = c(0, 1, 0, 1))
#     ncol <- 14
#     pal <- col.regions(ncol)
#     col.ind <- as.numeric(cut(corr, breaks = seq(from = -1, to = 1, 
#                                                  length = ncol + 1), include.lowest = TRUE))
#     corr <- formatC(corr, digits = digits, format = "f")
#     if (auto) 
#         cex.cor <- 0.7/strwidth(corr)
#     text(0.5, 0.5, corr, cex = cex.cor, col = pal[col.ind])
# }

response_names2 <- c('Sub-head\nBMC', 'DR 1/3\nArea', 'DR 1/3\nBMC','DR 1/3\nMod',
                    'UDR\nArea', 'UDR\nBMC', 'UDR\nIBS', 'FN\nBMC', 'NN\nMod',
                    'NN\nBR', 'NN\nWidth', 'NN\nED', 'NN\nCT', 'PA L3\nBMC')

corrgram(comp_matrix_pvalue, upper.panel = NULL, lower.panel = panel.cor,
         col.regions = function(x) gray.colors(x, start = 0, end = .6),
         labels = response_names2, cex.labels = 1.2)
