




library( readxl )
library( Epi )
library( popEpi )
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
temp.glm.mod = mod.ad.2.3
summary(temp.glm.mod)
#
#1- temp.glm.mod$deviance /temp.glm.mod$null.deviance
temp.glm.mod$aic
#
n.para = temp.glm.mod$df.null - temp.glm.mod$df.residual +1
#
mod.max.ll = -(temp.glm.mod$aic - n.para*2) /2
mod.null.ll = -temp.glm.mod$null.deviance
LR.D = - 2 * (mod.null.ll - mod.max.ll)
#
LR.p = pchisq(q = LR.D, df = n.para, lower.tail = F)
#
R.squared = 1- exp(- LR.D /(temp.glm.mod$df.null +1))
R.squared.max = 1- exp(mod.null.ll *(2 /temp.glm.mod$df.null))
#
scaled.R.squared = R.squared /R.squared.max
#R.squared
scaled.R.squared
#n.para
LR.p
#

# MAE
mean(abs(temp.glm.mod$fitted.values - temp.glm.mod$model$`(lex.Xst %in% curr.end.stage)`))
# MAE


















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
scaled.R.squared = temp.cx.summary$rsq[1] /temp.cx.summary$rsq[2]
#
scaled.R.squared
# LR p
temp.cx.summary$logtest[3]
#



#anova(temp.cx.mod)




#
# MAE
#temp.cx.mod
temp.new.data = subset(curr.split.dm.lexis, lex.Cst %in% curr.start.stage)
#
if(nrow(temp.new.data) > 1000){
  sample.index = sample(x = c(1:nrow(temp.new.data)), size = 1000, replace = F)
  temp.new.data = temp.new.data[sample.index,]
}
#dim(temp.new.data)
temp.cx.fit = survfit(temp.cx.mod, newdata = temp.new.data)
temp.cx.time = temp.cx.fit$time
temp.cx.surv = temp.cx.fit$surv
dim(temp.cx.surv)
#
temp.abs.err = NULL
for(kk in 1:ncol(temp.cx.surv)){#       kk = 2
  frk.cx.surv = temp.cx.surv[,kk]
  if(sum(is.na(frk.cx.surv)) > 2){ next }
  #
  frk.obs.time = curr.split.dm.lexis$tfd[kk] + curr.split.dm.lexis$lex.dur[kk]
  frk.obs.outcome = 1- (curr.split.dm.lexis$lex.Xst %in% curr.end.stage)[kk]
  #
  frk.cx.index = which.min(abs(temp.cx.time - frk.obs.time))[1]
  frk.cx.outcome = frk.cx.surv[frk.cx.index]
  #
  frk.abs.err = abs((frk.obs.outcome - frk.cx.outcome))
  temp.abs.err = c(temp.abs.err, frk.abs.err)
  print(paste0(kk /ncol(temp.cx.surv) *100, '%'))
}
#
temp.cx.fit$n
length(temp.abs.err)
mean(temp.abs.err)
# MAE












