




# library( readxl )
library( Epi )
# library( popEpi )
# library( foreign )
# 
# # for table plot
# library(gridExtra)
# library(grid)
# 
# library(vioplot)



#
library(survival)
#





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











head(curr.split.dm.lexis, n = 11)
#
here.split.dm.lexis = na.omit(subset(curr.split.dm.lexis, select = c(
  sex, age, BMI, SBP, DBP, Tot.Cholesterol, Creatinine, HDLC, LDLC, Triglycerides, is.Drug.ACEI, is.Drug.ARB,
  tfd, lex.dur, lex.Cst, lex.Xst,
  HbA1c.lvl,
  P, ref.key
)))
#



# unq.ref.key.array = unique(there.split.dm.lexis$ref.key)
# match.index = match(unq.ref.key.array, there.split.dm.lexis$ref.key)
# here.split.dm.lexis = there.split.dm.lexis[c(match.index),]















##
# train the model
##


#
curr.start.stage = '0'; curr.end.stage = '1'
idx.in = as.integer(curr.start.stage) +1
idx.out = as.integer(curr.end.stage) +1
#



#
curr.mod.3 <- coxph(
  Surv(tfd, tfd + lex.dur, (lex.Xst %in% curr.end.stage)) 
  ~ Ns(HbA1c.lvl, knots = kd.HbA1c[[idx.in]][[idx.out]]) #+ Ns(tfd, knots = kd.ad[[idx.in]][[idx.out]])
  + sex + age + BMI 
  + SBP + DBP + Tot.Cholesterol 
  + Creatinine + HDLC + LDLC + Triglycerides
  + is.Drug.ACEI + is.Drug.ARB, 
  data = subset(here.split.dm.lexis, lex.Cst %in% curr.start.stage), 
  method = "breslow"
)

#
curr.mod.2 = update(curr.mod.3, . ~ . - is.Drug.ACEI - is.Drug.ARB)#
#
curr.mod.1 = update(curr.mod.2, . ~ . - Creatinine - HDLC - LDLC - Triglycerides)
#
curr.mod.0 = update(curr.mod.1, . ~ . - SBP - DBP - Tot.Cholesterol)
#
# plot(survfit(curr.mod.0))


#curr.mod.3$data
#summary(as.data.frame(curr.split.dm.lexis))






library(DescTools)





PseudoR2(curr.mod.3, which = 'all')











#   anova(curr.mod.0)
null.mod.summary = summary(curr.mod.0)
null.mod.summary$logtest
null.mod.summary$waldtest
null.mod.summary$sctest
#anova(curr.mod.0, curr.mod.1, curr.mod.2, curr.mod.3)

prev.mod = curr.mod.0
post.mod = curr.mod.1
#
temp.anova = anova(prev.mod, post.mod)
#print(temp.anova)
p.value = temp.anova$`P(>|Chi|)`[2]
print(c(p.value = p.value))
















# #
# temp.cx.mod = curr.mod.0
# #
# temp.cx.curve = (survfit(temp.cx.mod))
# temp.cx.curve$time
# temp.cx.curve$surv
# #
# plot(
#   x = 1 - ((temp.cx.curve$surv - min(temp.cx.curve$surv)) /temp.cx.curve$surv), 
#   y = 1-temp.cx.curve$surv, 
#   xlim = c(0,1), ylim = c(0,1), xaxs = 'i', yaxs = 'i'
# )


























###
# AUC
###


temp.cx.mod = curr.mod.0

# curr.start.stage = '0'; curr.end.stage = '1'
# curr.start.stage = '1'; curr.end.stage = '2'
# curr.start.stage = '2'; curr.end.stage = '3'
#


temp.new.data = subset(here.split.dm.lexis, lex.Cst %in% curr.start.stage)
# summary(as.data.frame(temp.new.data))
#  temp.new.data = temp.new.data[sample(x = c(1:nrow(temp.new.data)), size = 9999), ]
temp.cx.fit = survfit(temp.cx.mod, newdata = temp.new.data)
#plot(temp.cx.fit)
temp.cx.time = temp.cx.fit$time
temp.cx.surv = temp.cx.fit$surv
dim(temp.cx.surv)
#
comb.data = NULL
for(kk in 1:ncol(temp.cx.surv)){#       kk = 20
  frk.cx.surv = temp.cx.surv[,kk]
  if(sum(is.na(frk.cx.surv)) > 2){ next }
  # plot(temp.cx.time, frk.cx.surv, type = 'S', ylim = c(0,1), yaxs = 'i')
  #
  frk.obs.time = temp.new.data$tfd[kk] + temp.new.data$lex.dur[kk]
  frk.obs.outcome = (temp.new.data$lex.Xst %in% curr.end.stage)[kk]
  #
  frk.cx.index = which.min(abs(temp.cx.time - frk.obs.time))[1]
  frk.cx.outcome.1 = 1 - frk.cx.surv[frk.cx.index]         # onset before t
  frk.cx.outcome.2 = 1 - (frk.cx.outcome.1 - min(frk.cx.surv)) /frk.cx.outcome.1       #  will not onset ever
  #
  comb.data = rbind(comb.data, c(frk.obs.outcome, frk.cx.outcome.1, frk.cx.outcome.2))
  #
  print(paste0(kk /ncol(temp.cx.surv) *100, '%'))
}
#temp.cx.fit$n
#



comb.data = as.data.frame(comb.data)
colnames(comb.data) = c('obs', 'fit.1', 'fit.2')
dim(comb.data)
#







##
##


#
obs.value = comb.data$obs
obs.pos.index = which(obs.value == T)
obs.neg.index = which(obs.value == F)
#

length(obs.pos.index) + length(obs.neg.index)


#
# prob.threshold.array = c(
#   c(1:9) *0.1,
#   c(2:19) *0.5, 
#   c(1:9) *10,
#   90 + c(1:9) *1,
#   99 + c(1:9) *0.1
# ) / 100
prob.threshold.array = c(1:9999)/10000
#
#plot(prob.threshold.array)

# 
TPR.array = NULL
FPR.array = NULL

plot(c(0,1),c(0,1), type = 'l', lty = 2, xlim = c(0,1), ylim = c(0,1), xaxs = 'i', yaxs = 'i')
for(i in 1:length(prob.threshold.array)){#       i = 10
  prob.threshold = prob.threshold.array[i]
  #
  fit.pos.index = which( comb.data$fit.1[obs.pos.index] >= prob.threshold )
  fit.neg.index = which( comb.data$fit.1[obs.neg.index] < (prob.threshold) )
  
  #
  true.pos.count = length(fit.pos.index)
  false.neg.count = length(obs.pos.index) - length(fit.pos.index)
  false.pos.count = length(obs.neg.index) - length(fit.neg.index)
  true.neg.count = length(fit.neg.index)
  #
  TPR = true.pos.count / (true.pos.count + false.neg.count)
  FPR = false.pos.count / (false.pos.count + true.neg.count)
  TPR.array = c(c(TPR.array), TPR)
  FPR.array = c(c(FPR.array), FPR)
  #
  points(FPR, TPR, pch = 20)
  #
}
#
TPR.array = c(1,TPR.array,0)
FPR.array = c(1,FPR.array,0)
# plot(TPR.array)
# plot(FPR.array)


AUC = sum(diff(rev(FPR.array)) *rev(TPR.array)[-1])
print(c(AUC = AUC))
#


















































#View(survivalROC)




# library(survivalROC)
# 
# 
# data(mayo)
# nobs <- NROW(mayo)
# cutoff <- 365
# ## MAYOSCORE 4, METHOD = NNE
# Mayo4.1= survivalROC(Stime=mayo$time,  
#                      status=mayo$censor,      
#                      marker = mayo$mayoscore4,     
#                      predict.time = cutoff,span = 0.25*nobs^(-0.20) )
# plot(Mayo4.1$FP, Mayo4.1$TP, type="l", xlim=c(0,1), ylim=c(0,1),   
#      xlab=paste( "FP", "\n", "AUC = ",round(Mayo4.1$AUC,3)), 
#      ylab="TP",main="Mayoscore 4, Method = NNE \n  Year = 1")
# abline(0,1)
# 
# ## MAYOSCORE 4, METHOD = KM
# Mayo4.2= survivalROC(Stime=mayo$time,  
#                      status=mayo$censor,      
#                      marker = mayo$mayoscore4,     
#                      predict.time =  cutoff, method="KM")
# plot(Mayo4.2$FP, Mayo4.2$TP, type="l", xlim=c(0,1), ylim=c(0,1),   
#      xlab=paste( "FP", "\n", "AUC = ",round(Mayo4.2$AUC,3)), 
#      ylab="TP",main="Mayoscore 4, Method = KM \n Year = 1")
# abline(0,1)
# 
# 
# Mayo4.2$AUC












