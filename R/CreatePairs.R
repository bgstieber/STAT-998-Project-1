gym <- read.csv('Gymnastics002.csv', stringsAsFactors = FALSE)

#make a whole bunch of pairs plots
panel.cor2 <- function (x, y, corr = NULL, cor.method = 'pearson', digits = 2, 
                        cex.cor, ...) 
{
  require(colorspace)
  if (is.null(corr)) {
    if (sum(complete.cases(x, y)) < 2) {
      warning("Need at least 2 complete cases for cor()")
      return()
    }
    else {
      corr <- cor(x, y, use = "pair", method = cor.method)
    }
  }
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  ncol <- 14
  pal <- diverge_hcl(ncol)
  col.ind <- as.numeric(cut(corr, breaks = seq(from = -1, to = 1, 
                                               length = ncol + 1), include.lowest = TRUE))
  col.text <- ifelse(col.ind %in% c(1,2, ncol, ncol - 1), 'grey', 'black')
  corr <- formatC(corr, digits = digits, format = "f")
  
  usr <- par("usr")
  rect(usr[1], usr[3], usr[2], usr[4], col = pal[col.ind], 
       border = NA)
  box(col = "lightgray")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  
  r <- formatC(corr, digits = 2, format = "f")
  cex.cor <- .8/strwidth("-X.xx")
  text(0.5, 0.5, corr, cex = cex.cor, col = col.text)
}


response_names <- c("Sub.head.BMC", "Distal.Radius.Third.Area", "Distal.bone.mineral.content.third", 
                    "Distal.Modulus.Third.Radius", "UD.Area.Radius", "UD.BMC", "UD.IBS.Radius", 
                    "Femoral.Neck.BMC.Hip", "NN.Section.Modulus.Hip", "NN.BR.Hip", 
                    "NN.width.Hip", "NN.ED.Hip", "NN.ACT.Hip", "PA.L3.BMC.Spine")

non_response <- c('Sub.Head.LM','Standing.Height','Menarcheal.Age.at.DXA',
                  'NonGym.Physical.Activity','Gymnastics')

gym_df <- data.frame()

name <- ''
col_to_use <- c("#CC6666", "#9999CC", "#66CC99")

for(i in response_names){
  
  gym_df <- gym[c('Group.Division', i, non_response)]
  
  name <- gsub(pattern = ".", replacement = "_",
               x = i, fixed = TRUE)
  
  pdf(paste0(name, 'Pairs', '.pdf'), width = 11, height = 8.5)
  
  pairs(gym_df, 
        upper.panel = panel.cor2,
        lower.panel = panel.smooth,
        col.smooth = 'black',
        horInd = 2:ncol(gym_df),
        verInd = 2:ncol(gym_df),
        col = col_to_use[gym_df$Group.Division + 1],
        main = name)
  par(xpd = T)
  legend('topright', legend = c('PERI','POST','NON'), col = col_to_use, pch = 1, ncol = 3, bty = 'n')
  
  dev.off()
  
  par(xpd = F)
  
}