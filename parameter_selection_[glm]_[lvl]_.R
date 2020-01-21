




#library( readxl )
library( Epi )
#library( popEpi )
library( foreign )

# for table plot
library(gridExtra)
library(grid)

library(vioplot)








#
##
load('dm_baseline_BOTH_stage_Lexis_[all.var]__.RData')
##
#

original.split.dm.lexis = curr.split.dm.lexis
##
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













head(curr.split.dm.lexis, n = 11)
summary(as.data.frame(curr.split.dm.lexis))


dim(curr.split.dm.lexis)
summary(curr.split.dm.lexis$HbA1c.lvl)
summary(curr.split.dm.lexis)


split.dm.data = (as.data.frame(curr.split.dm.lexis))
dim(split.dm.data)
length(unique(split.dm.data$lex.id[!is.na(split.dm.data$HbA1c.lvl)]))








#
kd.ad = list(
  list(),
  list(),
  list(),
  list()
)
## setup the node points
#knots.pt = c(0.025,0.1,0.33,0.67,0.9,0.975)
#knots.pt = c(0.001, 0.33,0.67, 0.999)
#knots.pt = c(0.01, 0.25, 0.5, 0.75, 0.99)
knots.pt = c(0.05, 0.25,0.5,0.75,0.95)
#knots.pt = c(0.01, 1:9/10, 0.99)
#knots.pt = c(0.01, 0.99)
#
kd.ad[[1]][[2]] <- with( subset( curr.split.dm.lexis, lex.Cst== '0' & lex.Xst== '1'), quantile(tfd + lex.dur, probs=knots.pt) )
kd.ad[[2]][[3]] <- with( subset( curr.split.dm.lexis, lex.Cst== '1' & lex.Xst== '2'), quantile(tfd + lex.dur, probs=knots.pt) )
kd.ad[[3]][[4]] <- with( subset( curr.split.dm.lexis, lex.Cst== '2' & lex.Xst== '3'), quantile(tfd + lex.dur, probs=knots.pt) )
#
knots.pt = c(0.05,0.33,0.95)#, 0.99
kd.ad[[2]][[1]] <- with( subset( curr.split.dm.lexis, lex.Cst== '1' & lex.Xst== '0'), quantile(tfd + lex.dur, probs=knots.pt) )
kd.ad[[3]][[2]] <- with( subset( curr.split.dm.lexis, lex.Cst== '2' & lex.Xst== '1'), quantile(tfd + lex.dur, probs=knots.pt) )
kd.ad[[4]][[3]] <- with( subset( curr.split.dm.lexis, lex.Cst== '3' & lex.Xst== '2'), quantile(tfd + lex.dur, probs=knots.pt) )






#
kd.HbA1c = list(
  list(),
  list(),
  list(),
  list()
)
#knots.pt = c(0.01, 0.25, 0.5, 0.75, 0.99)
knots.pt = c(0.1, 0.5, 0.9)
#
kd.HbA1c[[1]][[2]] <- with( subset( curr.split.dm.lexis, lex.Cst== '0' & lex.Xst== '1'), quantile(HbA1c.lvl, probs=knots.pt, na.rm = T) )
kd.HbA1c[[2]][[1]] <- with( subset( curr.split.dm.lexis, lex.Cst== '1' & lex.Xst== '0'), quantile(HbA1c.lvl, probs=knots.pt, na.rm = T) )
#
kd.HbA1c[[2]][[3]] <- with( subset( curr.split.dm.lexis, lex.Cst== '1' & lex.Xst== '2'), quantile(HbA1c.lvl, probs=knots.pt, na.rm = T) )
kd.HbA1c[[3]][[2]] <- with( subset( curr.split.dm.lexis, lex.Cst== '2' & lex.Xst== '1'), quantile(HbA1c.lvl, probs=knots.pt, na.rm = T) )
#
kd.HbA1c[[3]][[4]] <- with( subset( curr.split.dm.lexis, lex.Cst== '2' & lex.Xst== '3'), quantile(HbA1c.lvl, probs=knots.pt, na.rm = T) )
kd.HbA1c[[4]][[3]] <- with( subset( curr.split.dm.lexis, lex.Cst== '3' & lex.Xst== '2'), quantile(HbA1c.lvl, probs=knots.pt, na.rm = T) )
#












#
head(curr.split.dm.lexis, n = 11)
here.split.dm.lexis = na.omit(subset(curr.split.dm.lexis, select = c(
  sex, age, BMI, SBP, DBP, Tot.Cholesterol, Creatinine, HDLC, LDLC, Triglycerides, is.Drug.ACEI, is.Drug.ARB,
  tfd, lex.dur, lex.Cst, lex.Xst,
  HbA1c.lvl
)))
#






















##
# train the model
##


# summary(as.data.frame(here.split.dm.lexis))
# names(as.data.frame(here.split.dm.lexis))



#Ns(curr.split.dm.lexis$tfd, knots = kd.ad.0.1)


#
curr.start.stage = '3'; curr.end.stage = '2'
idx.in = as.integer(curr.start.stage) +1
idx.out = as.integer(curr.end.stage) +1
#


#
curr.mod.3 <- glm(
  (lex.Xst %in% curr.end.stage) ~ Ns(tfd, knots = kd.ad[[idx.in]][[idx.out]]) + Ns(HbA1c.lvl, knots = kd.HbA1c[[idx.in]][[idx.out]]) #-1#
  + sex + age + BMI 
  + SBP + DBP + Tot.Cholesterol 
  + Creatinine + HDLC + LDLC + Triglycerides# + smoke.status + Alcohol.status
  + is.Drug.ACEI + is.Drug.ARB,
  offset = c(lex.dur), family = binomial, #poisson
  data = subset(here.split.dm.lexis, lex.Cst %in% curr.start.stage)
)

#
curr.mod.2 = update(curr.mod.3, . ~ . - is.Drug.ACEI - is.Drug.ARB)#
#
curr.mod.1 = update(curr.mod.2, . ~ . - Creatinine - HDLC - LDLC - Triglycerides)
#
curr.mod.0 = update(curr.mod.1, . ~ . - SBP - DBP - Tot.Cholesterol)
#
#


#curr.mod.3$data
#summary(as.data.frame(curr.split.dm.lexis))




curr.mod = curr.mod.3

#
round(c(MAE = mean(abs(curr.mod$y -curr.mod$fitted.values))),2)
#
num.data = curr.mod$df.null
null.ll = - curr.mod$null.deviance /2
full.ll = - curr.mod$deviance /2
#
pseudo.R2 = (1 - exp(2*(null.ll - full.ll) /num.data)) / (1 - exp(2*null.ll /num.data))
round(c(R2 = pseudo.R2),2)














#anova(curr.mod.0, curr.mod.1, curr.mod.2, curr.mod.3)


prev.mod = curr.mod.2
post.mod = curr.mod.3

temp.anova = anova(prev.mod, post.mod)
#print(temp.anova)
p.value = 1 - pchisq(q = temp.anova$Deviance[2], df = temp.anova$Df[2])
print(c(p.value = p.value))
















###
# AUC
###



##
temp.mod = curr.mod.0
##



fit.value = temp.mod$fitted.values
obs.value = temp.mod$y
#
obs.pos.index = which(obs.value == 1)
obs.neg.index = which(obs.value == 0)

# vioplot(fit.value[true.index], fit.value[false.index])
# t.test(fit.value[true.index], fit.value[false.index])


#
prob.threshold.array = c(
  c(1:9) *0.1,
  c(2:19) *0.5, 
  c(1:9) *10
) / 100
#
#plot(prob.threshold.array)

# 
TPR.array = NULL
FPR.array = NULL

plot(c(0,1),c(0,1), type = 'l', lty = 2, xlim = c(0,1), ylim = c(0,1), xaxs = 'i', yaxs = 'i')
for(i in 1:length(prob.threshold.array)){#       i = 2
  prob.threshold = prob.threshold.array[i]
  #
  fit.pos.index = which(fit.value >= prob.threshold)
  fit.neg.index = which(fit.value < prob.threshold)
  
  #
  true.pos.count = length(intersect(x = obs.pos.index, y = fit.pos.index))
  false.neg.count = length(intersect(x = obs.pos.index, y = fit.neg.index))
  false.pos.count = length(intersect(x = obs.neg.index, y = fit.pos.index))
  true.neg.count = length(intersect(x = obs.neg.index, y = fit.neg.index))
  #
  TPR = true.pos.count / (true.pos.count + false.neg.count)
  FPR = false.pos.count / (false.pos.count + true.neg.count)
  TPR.array = c(c(TPR.array), TPR)
  FPR.array = c(c(FPR.array), FPR)
  #
  points(FPR, TPR, pch = 20)
  #
}

# plot(TPR.array)
# plot(FPR.array)


AUC = sum(diff(rev(FPR.array)) *rev(TPR.array)[-1])
print(c(AUC = AUC))
#























