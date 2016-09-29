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

replace_with_space <- function(x) gsub(x, pattern = ".", replacement = '\n', fixed = T)



after_second_slash <- function(x){
  
  substr(x = substr(x, regexec(pattern = "/", x)[[1]][1] + 1, nchar(x)),
         start = regexec(text = substr(x, regexec(pattern = "/", x)[[1]][1] + 1, nchar(x)), 
                         pattern = "/")[[1]][1]+1,
         stop = nchar(x))
}

after_second_slashV <- Vectorize(after_second_slash, 'x')

library(dplyr)
library(ggplot2)
theme_set(theme_bw())

read.csv('Gymnastics001.csv', header = T, stringsAsFactors = FALSE) %>%
  group_by(ID) %>%
  mutate(MeasurementTime = row_number(),
         NonGym.Physical.Activity = ifelse(is.na(NonGym.Physical.Activity), 0,
                                           NonGym.Physical.Activity),
         Gymnastics = ifelse(is.na(Gymnastics), 0, Gymnastics)) -> gym

summary(gym)


gym %>%
  mutate(DXADate.fix =
           ifelse(nchar(after_second_slashV(DXADate)) == 2,
                  as.Date(DXADate, '%m/%d/%y'),
                  as.Date(DXADate, '%m/%d/%Y'))) %>%
  mutate(DXADate.fix = as.Date(DXADate.fix, origin = '1970-01-01'))-> gym

table(gym$ID)


# Days between measurements fairly constant?
gym %>%
  group_by(ID) %>%
  mutate(PreviousAge = lag(Menarcheal.Age.at.DXA),
         TimeBetweenMeasurements = Menarcheal.Age.at.DXA - PreviousAge) -> gym

quantile(gym$TimeBetweenMeasurements, probs = seq(0,1,.1), na.rm = T)

#Number of IDs per number of observations

table(stack(table(gym$ID))[,1])


setNames(aggregate(ID ~ Group.Division, function(x) length(unique(x)), data = gym),
         c('Group','Count')) -> sum1

sum1$Group <- ifelse(sum1$Group == 0, 'PERI', 
                     ifelse(sum1$Group == 1, 'POST','NON'))

sum1


gym %>%
  group_by(Group.Division, ID) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  group_by(Group.Division, Count) %>%
  summarise(Count_Count = n()) %>%
  ggplot(., aes(x = Count, y = Count_Count))+
  geom_bar(stat = 'identity', aes(group = Group.Division,
                                  fill = factor(Group.Division)),
           position = 'dodge', colour = 'black')+
  xlab('Number of Measurements')+
  ylab('Number of Individuals')+
  scale_fill_brewer(palette = 'Set1', name = 'Group',
                    labels = c('PERI','POST','NON'))
#There are 19 NAs for FA.length, and all of the radius measures


table(gym[which(is.na(gym$FA.length)), ]$ID)
table(gym[which(is.na(gym$Distal.Radius.Third.Area)), ]$ID)
table(gym[which(is.na(gym$Distal.bone.mineral.content.third)), ]$ID)
table(gym[which(is.na(gym$Distal.Modulus.Third.Radius)), ]$ID)
table(gym[which(is.na(gym$UD.Area.Radius)), ]$ID)
table(gym[which(is.na(gym$UD.BMC)), ]$ID)
table(gym[which(is.na(gym$UD.IBS.Radius)), ]$ID)

# Which are NA for the same IDs

# We would want to remove IDs 23 and 47

# For ID 13 their first measurement is missing...no big deal

# Perhaps mean replace for ID 156?

table(gym[which(is.na(gym$NN.Section.Modulus.Hip)), ]$ID)
table(gym[which(is.na(gym$NN.BR.Hip)), ]$ID)
table(gym[which(is.na(gym$NN.width.Hip)), ]$ID)
table(gym[which(is.na(gym$NN.ED.Hip)), ]$ID)


#create a few different subsets of the response variables
#we also include age in each plot!
col_to_use <- c("#CC6666", "#9999CC", "#66CC99")

bmc <- gym[,c(7,11,14,19,21,27)] #bone mineral content
radius <- gym[,c(7,11,15:20)]
hip <- gym[,c(7,11,21:26)]

pairs(bmc, 
      upper.panel = panel.cor2,
      lower.panel = panel.smooth,
      col.smooth = 'black',
      horInd = 2:ncol(bmc),
      verInd = 2:ncol(bmc),
      col = col_to_use[bmc$Group.Division + 1],
      main = 'BMC')
par(xpd = T)
legend('topright', legend = c('PERI','POST','NON'), col = col_to_use, pch = 1, ncol = 3, bty = 'n')

par(xpd = FALSE)
pairs(radius, 
      upper.panel = panel.cor2,
      lower.panel = panel.smooth,
      col.smooth = 'black',
      horInd = 2:ncol(radius),
      verInd = 2:ncol(radius),
      col = col_to_use[radius$Group.Division + 1],
      main = 'RADIUS')
par(xpd = T)
legend('topright', legend = c('PERI','POST','NON'), col = col_to_use, pch = 1, ncol = 3, bty = 'n')


# Distal Radius Third Area = Distal Modulus Third Radius ^ (1/3)
plot(gym$Distal.Radius.Third.Area, gym$Distal.Modulus.Third.Radius ^ (1/3))

par(xpd = FALSE)
pairs(hip, 
      upper.panel = panel.cor2,
      lower.panel = panel.smooth,
      col.smooth = 'black',
      horInd = 2:ncol(hip),
      verInd = 2:ncol(hip),
      col = col_to_use[hip$Group.Division + 1],
      main = 'HIP')
par(xpd = T)
legend('topright', legend = c('PERI','POST','NON'), col = col_to_use, pch = 1, ncol = 3, bty = 'n')


ggplot(gym, aes(x = Menarcheal.Age.at.DXA, y = Sub.head.BMC))+
  geom_line(aes(group = ID, colour = factor(Group.Division)))+
  geom_smooth(aes(group = factor(Group.Division), colour = factor(Group.Division)),
              se = F, size = 3)

table(gym$Group.Division)



