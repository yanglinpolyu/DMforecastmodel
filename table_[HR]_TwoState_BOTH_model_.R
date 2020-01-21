




library( readxl )
library( Epi )
#library( popEpi )
library( foreign )

# for table plot
library(gridExtra)
library(grid)
#
library(vioplot)

#
library(survival)
#





#
##
load('dm_baseline_BOTH_stage_Lexis_[all.var]__.RData')
##

original.split.dm.lexis = curr.split.dm.lexis
##
non.na.index = which(!is.na(original.split.dm.lexis$is.Drug.ACEI))
curr.split.dm.lexis$is.Drug.ACEI[non.na.index] = 'yes'
na.index = which(is.na(original.split.dm.lexis$is.Drug.ACEI))
curr.split.dm.lexis$is.Drug.ACEI[na.index] = 'no'
#
non.na.index = which(!is.na(original.split.dm.lexis$is.Drug.ARB))
curr.split.dm.lexis$is.Drug.ARB[non.na.index] = 'yes'
na.index = which(is.na(original.split.dm.lexis$is.Drug.ARB))
curr.split.dm.lexis$is.Drug.ARB[na.index] = 'no'
##
#



















# lvl
# sd
# cv

file.type = 'lvl'
#


#
load(file = paste0('dm_BOTH_[glm]_models_[HbA1c.',file.type,']__.RData'))
load(file = paste0('dm_BOTH_[cox]_models_[HbA1c.',file.type,']__.RData'))
#
#


kd.HbA1c.0.1
























#
#-#
mod.ad.0.1
curr.start.stage = '0'; curr.end.stage = '1'
#
mod.ad.1.2
curr.start.stage = '1'; curr.end.stage = '2'
#
mod.ad.2.3
curr.start.stage = '2'; curr.end.stage = '3'
#-#
#


#
#-#
mod.ad.1.0
curr.start.stage = '1'; curr.end.stage = '0'
#
mod.ad.2.1
curr.start.stage = '2'; curr.end.stage = '1'
#
mod.ad.3.2
curr.start.stage = '3'; curr.end.stage = '2'
#-#
#





#
temp.glm.mod = mod.ad.3.2
temp.glm.summary = summary(temp.glm.mod)

#
temp.glm.coeff = as.data.frame(temp.glm.summary$coefficients[,c(1,2,4)])
delete.index = NA
if(file.type == 'lvl'){
  delete.index = c(1:5)
} else {
  delete.index = c(1:3)
}
temp.glm.coeff = temp.glm.coeff[-c(delete.index),]
colnames(temp.glm.coeff) = c('pnt.est','pnt.sd', 'p.value')
#
temp.glm.coeff$est.low = exp(temp.glm.coeff$pnt.est -1.96*temp.glm.coeff$pnt.sd)
temp.glm.coeff$est.up = exp(temp.glm.coeff$pnt.est +1.96*temp.glm.coeff$pnt.sd)
temp.glm.coeff$pnt.est = exp(temp.glm.coeff$pnt.est)
#
temp.glm.coeff = round(temp.glm.coeff, 2)


gg = paste0(temp.glm.coeff$est.low, '-', temp.glm.coeff$est.up)


#
write.csv(gg, file = 'temp_[HR]_[glm]_est_records__.csv')
#















#
#-#
cx.ad.0.1
curr.start.stage = '0'; curr.end.stage = '1'
#
cx.ad.1.2
curr.start.stage = '1'; curr.end.stage = '2'
#
cx.ad.2.3
curr.start.stage = '2'; curr.end.stage = '3'
#-#
#





##
temp.cx.mod = cx.ad.2.3
temp.cx.summary = summary(temp.cx.mod)


temp.cx.coeff = as.data.frame(temp.cx.summary$coefficients[,c(2,5)])
delete.index = NA
if(file.type == 'lvl'){
  delete.index = c(1:2)
} else {
  delete.index = c(1)
}
temp.cx.coeff = temp.cx.coeff[-c(delete.index),]
colnames(temp.cx.coeff) = c('pnt.est', 'p.value')
#


#
write.csv(temp.cx.coeff, file = 'temp_[HR]_[cox]_est_records__.csv')
#









