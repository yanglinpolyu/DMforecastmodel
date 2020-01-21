













library( readxl )
library( Epi )
library( popEpi )
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






## setup the node points
#knots.pt = c(0.025,0.1,0.33,0.67,0.9,0.975)
#knots.pt = c(0.001, 0.33,0.67, 0.999)
#knots.pt = c(0.01, 0.25, 0.5, 0.75, 0.99)
knots.pt = c(0.05, 0.25,0.5,0.75,0.95)
#knots.pt = c(0.01, 1:9/10, 0.99)
#knots.pt = c(0.01, 0.99)
#
kd.ad.0.0 <- with( subset( curr.split.dm.lexis, lex.Cst== '0' & lex.Xst== '0'), quantile(tfd + lex.dur, probs=knots.pt) )
kd.ad.0.1 <- with( subset( curr.split.dm.lexis, lex.Cst== '0' & lex.Xst== '1'), quantile(tfd + lex.dur, probs=knots.pt) )
kd.ad.0.2 <- with( subset( curr.split.dm.lexis, lex.Cst== '0' & lex.Xst== '2'), quantile(tfd + lex.dur, probs=knots.pt) )
kd.ad.0.3 <- with( subset( curr.split.dm.lexis, lex.Cst== '0' & lex.Xst== '3'), quantile(tfd + lex.dur, probs=knots.pt) )
#
kd.ad.1.1 <- with( subset( curr.split.dm.lexis, lex.Cst== '1' & lex.Xst== '1'), quantile(tfd + lex.dur, probs=knots.pt) )
kd.ad.1.2 <- with( subset( curr.split.dm.lexis, lex.Cst== '1' & lex.Xst== '2'), quantile(tfd + lex.dur, probs=knots.pt) )
kd.ad.1.3 <- with( subset( curr.split.dm.lexis, lex.Cst== '1' & lex.Xst== '3'), quantile(tfd + lex.dur, probs=knots.pt) )
#
kd.ad.2.2 <- with( subset( curr.split.dm.lexis, lex.Cst== '2' & lex.Xst== '2'), quantile(tfd + lex.dur, probs=knots.pt) )
kd.ad.2.3 <- with( subset( curr.split.dm.lexis, lex.Cst== '2' & lex.Xst== '3'), quantile(tfd + lex.dur, probs=knots.pt) )
#
kd.ad.3.3 <- with( subset( curr.split.dm.lexis, lex.Cst== '3' & lex.Xst== '3'), quantile(tfd + lex.dur, probs=knots.pt) )
#
knots.pt = c(0.05,0.33,0.95)#, 0.99
kd.ad.1.0 <- with( subset( curr.split.dm.lexis, lex.Cst== '1' & lex.Xst== '0'), quantile(tfd + lex.dur, probs=knots.pt) )
kd.ad.2.1 <- with( subset( curr.split.dm.lexis, lex.Cst== '2' & lex.Xst== '1'), quantile(tfd + lex.dur, probs=knots.pt) )
kd.ad.3.2 <- with( subset( curr.split.dm.lexis, lex.Cst== '3' & lex.Xst== '2'), quantile(tfd + lex.dur, probs=knots.pt) )



#summary(curr.split.dm.lexis$HbA1c.lvl)
#knots.pt = c(0.01, 0.25, 0.5, 0.75, 0.99)
knots.pt = c(0.1, 0.5, 0.9)
#
kd.HbA1c.0.0 <- with( subset( curr.split.dm.lexis, lex.Cst== '0' & lex.Xst== '0'), quantile(HbA1c.lvl, probs=knots.pt, na.rm = T) )
kd.HbA1c.0.1 <- with( subset( curr.split.dm.lexis, lex.Cst== '0' & lex.Xst== '1'), quantile(HbA1c.lvl, probs=knots.pt, na.rm = T) )
kd.HbA1c.1.0 <- with( subset( curr.split.dm.lexis, lex.Cst== '1' & lex.Xst== '0'), quantile(HbA1c.lvl, probs=knots.pt, na.rm = T) )
#
kd.HbA1c.1.1 <- with( subset( curr.split.dm.lexis, lex.Cst== '1' & lex.Xst== '1'), quantile(HbA1c.lvl, probs=knots.pt, na.rm = T) )
kd.HbA1c.1.2 <- with( subset( curr.split.dm.lexis, lex.Cst== '1' & lex.Xst== '2'), quantile(HbA1c.lvl, probs=knots.pt, na.rm = T) )
kd.HbA1c.2.1 <- with( subset( curr.split.dm.lexis, lex.Cst== '2' & lex.Xst== '1'), quantile(HbA1c.lvl, probs=knots.pt, na.rm = T) )
#
kd.HbA1c.2.2 <- with( subset( curr.split.dm.lexis, lex.Cst== '2' & lex.Xst== '2'), quantile(HbA1c.lvl, probs=knots.pt, na.rm = T) )
kd.HbA1c.2.3 <- with( subset( curr.split.dm.lexis, lex.Cst== '2' & lex.Xst== '3'), quantile(HbA1c.lvl, probs=knots.pt, na.rm = T) )
kd.HbA1c.3.2 <- with( subset( curr.split.dm.lexis, lex.Cst== '3' & lex.Xst== '2'), quantile(HbA1c.lvl, probs=knots.pt, na.rm = T) )
#












summary(as.data.frame(curr.split.dm.lexis))
names(as.data.frame(curr.split.dm.lexis))



#Ns(curr.split.dm.lexis$tfd, knots = kd.ad.0.1)

#   glm models
#
curr.start.stage = '0'; curr.end.stage = '0'
mod.ad.0.0 <- glm(
  (lex.Xst %in% curr.end.stage) ~ Ns(tfd, knots = kd.ad.0.0) + Ns(HbA1c.lvl, knots = kd.HbA1c.0.0) #-1#
  + sex + age + BMI + SBP + DBP# + smoke.status + Alcohol.status
  + Tot.Cholesterol + Creatinine + HDLC + LDLC + Triglycerides
  #+ is.Drug.ACEI + is.Drug.ARB
  ,
  offset = c(lex.dur), family = binomial, #poisson
  data = subset(curr.split.dm.lexis, lex.Cst %in% curr.start.stage)
)
#
#    summary(mod.ad.0.0)

#
curr.start.stage = '0'; curr.end.stage = '1'
mod.ad.0.1 <- glm(
  (lex.Xst %in% curr.end.stage) ~ Ns(tfd, knots = kd.ad.0.1) + Ns(HbA1c.lvl, knots = kd.HbA1c.0.1) #-1#
  + sex + age + BMI + SBP + DBP# + smoke.status + Alcohol.status
  + Tot.Cholesterol + Creatinine + HDLC + LDLC + Triglycerides
  #+ is.Drug.ACEI + is.Drug.ARB
  ,
  offset = c(lex.dur), family = binomial, #poisson
  data = subset(curr.split.dm.lexis, lex.Cst %in% curr.start.stage)
)
#
#    summary(mod.ad.0.1)

#
curr.start.stage = '1'; curr.end.stage = '0'
mod.ad.1.0 <- glm(
  (lex.Xst %in% curr.end.stage) ~ Ns(tfd, knots = kd.ad.1.0) + Ns(HbA1c.lvl, knots = kd.HbA1c.1.0) #-1#
  + sex + age + BMI + SBP + DBP# + smoke.status + Alcohol.status
  + Tot.Cholesterol + Creatinine + HDLC + LDLC + Triglycerides
  #+ is.Drug.ACEI + is.Drug.ARB
  ,
  offset = c(lex.dur), family = binomial, #poisson
  data = subset(curr.split.dm.lexis, lex.Cst %in% curr.start.stage)
)
#
#    summary(mod.ad.1.0)

#
curr.start.stage = '0'; curr.end.stage = c('0', '1')#, '2'
mod.comb.0.1 <- glm(
  (lex.Xst %in% curr.end.stage) ~ Ns(tfd, knots = kd.ad.0.1) + Ns(HbA1c.lvl, knots = kd.HbA1c.0.1) #-1#
  + sex + age + BMI + SBP + DBP# + smoke.status + Alcohol.status
  + Tot.Cholesterol + Creatinine + HDLC + LDLC + Triglycerides
  #+ is.Drug.ACEI + is.Drug.ARB
  ,
  offset = c(lex.dur), family = binomial, #poisson
  data = subset(curr.split.dm.lexis, lex.Cst %in% curr.start.stage)
)
#
#    summary(mod.comb.0.1)

#
curr.start.stage = '1'; curr.end.stage = '1'
mod.ad.1.1 <- glm(
  (lex.Xst %in% curr.end.stage) ~ Ns(tfd, knots = kd.ad.1.1) + Ns(HbA1c.lvl, knots = kd.HbA1c.1.1) #-1#
  + sex + age + BMI + SBP + DBP# + smoke.status + Alcohol.status
  + Tot.Cholesterol + Creatinine + HDLC + LDLC + Triglycerides
  #+ is.Drug.ACEI + is.Drug.ARB
  ,
  offset = c(lex.dur), family = binomial, #poisson
  data = subset(curr.split.dm.lexis, lex.Cst %in% curr.start.stage)
)
#
#    summary(mod.ad.1.1)

#
curr.start.stage = '1'; curr.end.stage = '2'
mod.ad.1.2 <- glm(
  (lex.Xst %in% curr.end.stage) ~ Ns(tfd, knots = kd.ad.1.2) + Ns(HbA1c.lvl, knots = kd.HbA1c.1.2) #-1#
  + sex + age + BMI + SBP + DBP# + smoke.status + Alcohol.status
  + Tot.Cholesterol + Creatinine + HDLC + LDLC + Triglycerides
  #+ is.Drug.ACEI + is.Drug.ARB
  ,
  offset = c(lex.dur), family = binomial, #poisson
  data = subset(curr.split.dm.lexis, lex.Cst %in% curr.start.stage)
)
#
#    summary(mod.ad.1.2)

#
curr.start.stage = '2'; curr.end.stage = '1'
mod.ad.2.1 <- glm(
  (lex.Xst %in% curr.end.stage) ~ Ns(tfd, knots = kd.ad.2.1) + Ns(HbA1c.lvl, knots = kd.HbA1c.2.1) #-1#
  + sex + age + BMI + SBP + DBP# + smoke.status + Alcohol.status
  + Tot.Cholesterol + Creatinine + HDLC + LDLC + Triglycerides
  #+ is.Drug.ACEI + is.Drug.ARB
  ,
  offset = c(lex.dur), family = binomial, #poisson
  data = subset(curr.split.dm.lexis, lex.Cst %in% curr.start.stage)
)
#
#    summary(mod.ad.2.1)

#
curr.start.stage = '1'; curr.end.stage = c('0', '1', '2')
mod.comb.1.2 <- glm(
  (lex.Xst %in% curr.end.stage) ~ Ns(tfd, knots = kd.ad.1.2) + Ns(HbA1c.lvl, knots = kd.HbA1c.1.2) #-1#
  + sex + age + BMI + SBP + DBP# + smoke.status + Alcohol.status
  + Tot.Cholesterol + Creatinine + HDLC + LDLC + Triglycerides
  #+ is.Drug.ACEI + is.Drug.ARB
  ,
  offset = c(lex.dur), family = binomial, #poisson
  data = subset(curr.split.dm.lexis, lex.Cst %in% curr.start.stage)
)
#
#    summary(mod.comb.1.2)


#
curr.start.stage = '2'; curr.end.stage = '2'
mod.ad.2.2 <- glm(
  (lex.Xst %in% curr.end.stage) ~ Ns(tfd, knots = kd.ad.2.2) + Ns(HbA1c.lvl, knots = kd.HbA1c.2.2) #-1#
  + sex + age + BMI + SBP + DBP# + smoke.status + Alcohol.status
  + Tot.Cholesterol + Creatinine + HDLC + LDLC + Triglycerides
  #+ is.Drug.ACEI + is.Drug.ARB
  ,
  offset = c(lex.dur), family = binomial, #poisson
  data = subset(curr.split.dm.lexis, lex.Cst %in% curr.start.stage)
)
#
#    summary(mod.ad.2.2)


#
curr.start.stage = '2'; curr.end.stage = '3'
mod.ad.2.3 <- glm(
  (lex.Xst %in% curr.end.stage) ~ Ns(tfd, knots = kd.ad.2.3) + Ns(HbA1c.lvl, knots = kd.HbA1c.2.3) #-1#
  + sex + age + BMI + SBP + DBP# + smoke.status + Alcohol.status
  + Tot.Cholesterol + Creatinine + HDLC + LDLC + Triglycerides
  #+ is.Drug.ACEI + is.Drug.ARB
  ,
  offset = c(lex.dur), family = binomial, #poisson
  data = subset(curr.split.dm.lexis, lex.Cst %in% curr.start.stage)
)
#
#    summary(mod.ad.2.3)


#
curr.start.stage = '3'; curr.end.stage = '2'
mod.ad.3.2 <- glm(
  (lex.Xst %in% curr.end.stage) ~ Ns(tfd, knots = kd.ad.3.2) + Ns(HbA1c.lvl, knots = kd.HbA1c.3.2) #-1#
  + sex + age + BMI + SBP + DBP# + smoke.status + Alcohol.status
  + Tot.Cholesterol + Creatinine + HDLC + LDLC + Triglycerides
  #+ is.Drug.ACEI + is.Drug.ARB
  ,
  offset = c(lex.dur), family = binomial, #poisson
  data = subset(curr.split.dm.lexis, lex.Cst %in% curr.start.stage)
)
#
#    summary(mod.ad.3.2)


#
curr.start.stage = '2'; curr.end.stage = c('1', '2', '3')
mod.comb.2.3 <- glm(
  (lex.Xst %in% curr.end.stage) ~ Ns(tfd, knots = kd.ad.2.3) + Ns(HbA1c.lvl, knots = kd.HbA1c.2.3) #-1#
  + sex + age + BMI + SBP + DBP# + smoke.status + Alcohol.status
  + Tot.Cholesterol + Creatinine + HDLC + LDLC + Triglycerides
  #+ is.Drug.ACEI + is.Drug.ARB
  ,
  offset = c(lex.dur), family = binomial, #poisson
  data = subset(curr.split.dm.lexis, lex.Cst %in% curr.start.stage)
)
#
#    summary(mod.comb.2.3)




summary(as.data.frame(curr.split.dm.lexis))

















save(
  mod.ad.0.1, mod.ad.1.2, mod.ad.2.3,    mod.ad.1.0, mod.ad.2.1, mod.ad.3.2,
  kd.HbA1c.0.1, kd.HbA1c.1.2, kd.HbA1c.2.3,    kd.HbA1c.1.0, kd.HbA1c.2.1, kd.HbA1c.3.2,
  kd.ad.0.1, kd.ad.1.2, kd.ad.2.3,    kd.ad.1.0, kd.ad.2.1, kd.ad.3.2,
  file = 'dm_BOTH_[glm]_models_[HbA1c.lvl]__.RData'
)







































mod.ad.0.1
mod.ad.1.2
mod.ad.2.3



curr.start.stage = '0'; curr.end.stage = '1'
curr.start.stage = '1'; curr.end.stage = '2'
curr.start.stage = '2'; curr.end.stage = '3'




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
#
#R.squared
n.para
LR.p
scaled.R.squared
#











#dim(temp.glm.mod$model)


mean(abs(temp.glm.mod$fitted.values - temp.glm.mod$model$`(lex.Xst %in% curr.end.stage)`))


#mean(abs(temp.glm.mod$residuals))









summary(as.data.frame(curr.split.dm.lexis))
head(curr.split.dm.lexis)

















#








#        dev.off()
#        dev.off()
#        dev.off()
#        dev.off()
#        dev.off()
c(1)
#            pdf('plot_TwoState_BOTH_model_HbA1c_RR_[sex.M]_[ACEI.Y]_[ARB.N]__v03.pdf', width = 11, height = 5)
#
HbA1c.array = c(5.5,6,6.5,7,7.5,8,8.5)
#
par(las = 1, mar = c(3.75,4.25,1.5,1), mfrow = c(2,4), oma = c(0, 0, 2, 0))
#
for(ii in 1:length(HbA1c.array)){#        ii = 3
  temp.HbA1c = HbA1c.array[ii]
  #
  this.tfd.array = seq(0,3, length.out = 101)
  nd <- data.frame(
    HbA1c.lvl = temp.HbA1c,
    #lex.dur = 100,
    lex.dur = 0.25,
    tfd = this.tfd.array,
    # Alcohol.status = here.Alcohol.status,
    # smoke.status = here.smoke.status,
    sex = 'M',
    age = 62,
    BMI = 25.72,
    SBP = 130,
    DBP = 75,
    #
    Tot.Cholesterol = 4,
    Creatinine = 75,
    HDLC = 1.20,
    LDLC = 2.11,
    Triglycerides = 1.22,
    #
    is.Drug.ACEI = 'yes',
    is.Drug.ARB = 'no'
  )
  #
  #temp.rr.list = get.rr(nd)
  temp.rr.list = get.risk(nd)
  #
  temp.panel.lab = paste0('(',letters[ii],') ','HbA1c = ',temp.HbA1c, '%')
  plot.one.rr(rr.list = temp.rr.list, panel.lab = temp.panel.lab)
  #
}
#
curr.var = na.omit(curr.split.dm.lexis$HbA1c.lvl[which(
  curr.split.dm.lexis$sex == 'M' 
  & curr.split.dm.lexis$is.Drug.ACEI == 'yes'
  & curr.split.dm.lexis$is.Drug.ARB == 'no'
)])
vioplot(x = curr.var, pchMed = 18, border = '#FFFFFF00', col = '#00000033', names = 'HbA1c', ylim = c(4,13))
mtext(side = 3, adj = 0, cex = 1.1, text = '(h) HbA1c distribution')# the of beginning stage
mtext(side = 2, line = 2.75, 'HbA1c (%)', las = 0)
grid(ny = NULL, nx = NA, col = '#00000033', lty = 2)
#
mtext('gender: Male; ACEI: yes; ARB: no', side = 3, outer = T, line = 0.5, adj = 0, las = 1, cex = 1.11)
#        dev.off()
#        dev.off()
#        dev.off()
#        dev.off()
#        dev.off()
#        dev.off()
#        dev.off()
#        dev.off()




#





















plot.one.rr = function(rr.list, panel.lab){
  rr.list = rr.list
  # rr.list = get.risk(nd)
  rr.0.1 = rr.list$rr.0.1
  rr.1.2 = rr.list$rr.1.2
  rr.2.3 = rr.list$rr.2.3
  
  ##
  ###
  ##
  y.range = c(0,100)
  ###
  par(las = 1)
  plot(1,1, ylim = y.range, xlim = c(0, 3), type = 'n', yaxs = 'i', xaxs = 'i', log = "", axe = F, ann = F, frame = T)
  xlab = "time since diagnosis (yr)"; ylab = "risk rate (%)"
  mtext(side = 1, line = 2.5, xlab); mtext(side = 2, line = 2.75, ylab, las = 0)
  axis(4, labels = F); axis(1); grid(col = '#00000033', lty = 2)
  axis(2)
  #
  temp.index = c(nd$tfd > min(kd.ad.0.1) & nd$tfd < max(kd.ad.0.1));    temp.col = '#FF0000'
  lines(nd$tfd[temp.index], rr.0.1$Estimate[temp.index], lty = 1, lwd = 2, col = paste0(temp.col,99))
  points(nd$tfd[!temp.index], rr.0.1$Estimate[!temp.index], pch = 20, cex = 0.33, col = paste0(temp.col,99))
  polygon(x = c(nd$tfd, rev(nd$tfd)), y = c(rr.0.1$`5.0%`, rev(rr.0.1$`95.0%`)), col = paste0(temp.col,13), border = paste0(temp.col,66), lty = 4)
  #
  temp.index = c(nd$tfd > min(kd.ad.1.2) & nd$tfd < max(kd.ad.1.2));    temp.col = '#008800'
  lines(nd$tfd[temp.index], rr.1.2$Estimate[temp.index], lty = 1, lwd = 2, col = paste0(temp.col,99))
  points(nd$tfd[!temp.index], rr.1.2$Estimate[!temp.index], pch = 20, cex = 0.33, col = paste0(temp.col,99))
  polygon(x = c(nd$tfd, rev(nd$tfd)), y = c(rr.1.2$`5.0%`, rev(rr.1.2$`95.0%`)), col = paste0(temp.col,13), border = paste0(temp.col,66), lty = 4)
  #
  temp.index = c(nd$tfd > min(kd.ad.2.3) & nd$tfd < max(kd.ad.2.3));    temp.col = '#0000FF'
  lines(nd$tfd[temp.index], rr.2.3$Estimate[temp.index], lty = 1, lwd = 2, col = paste0(temp.col,99))
  points(nd$tfd[!temp.index], rr.2.3$Estimate[!temp.index], pch = 20, cex = 0.33, col = paste0(temp.col,99))
  polygon(x = c(nd$tfd, rev(nd$tfd)), y = c(rr.2.3$`5.0%`, rev(rr.2.3$`95.0%`)), col = paste0(temp.col,13), border = paste0(temp.col,66), lty = 4)
  #
  legend('topleft', col = c('#FF0000', '#008800', '#0000FF'), lwd = 2, bty = 'n', legend = c('progress: 0 - 1','progress: 1 - 2','progress: 2 - 3'))
  #
  mtext(side = 3, adj = 0, cex = 1.1, text = panel.lab)# the of beginning stage
  #
}














##  get the rr output

get.rr = function(sample.data){
  nd = sample.data
  #
  comb.rate.0.1 <- (ci.pred( mod.comb.0.1, nd, alpha = 0.1))
  ad.rate.0.1 <- (ci.pred( mod.ad.0.1, nd, alpha = 0.1))
  rr.0.1 = as.data.frame(ci.ratio( r1 = ad.rate.0.1, r2 = comb.rate.0.1, alpha = 0.1 ))*100
  rr.0.1 = narrow.in.range(rr.0.1, 100)
  #
  comb.rate.1.2 <- (ci.pred( mod.comb.1.2, nd, alpha = 0.1 ))
  ad.rate.1.2 <- (ci.pred( mod.ad.1.2, nd, alpha = 0.1 ))
  rr.1.2 = as.data.frame(ci.ratio( r1 = ad.rate.1.2, r2 = comb.rate.1.2, alpha = 0.1 ))*100
  rr.1.2 = narrow.in.range(rr.1.2, 100)
  #
  comb.rate.2.3 <- (ci.pred( mod.comb.2.3, nd, alpha = 0.1 ))
  ad.rate.2.3 <- (ci.pred( mod.ad.2.3, nd, alpha = 0.1 ))
  rr.2.3 = as.data.frame(ci.ratio( r1 = ad.rate.2.3, r2 = comb.rate.2.3, alpha = 0.1 ))*100
  rr.2.3 = narrow.in.range(rr.2.3, 100)
  #
  rr.list = list(
    rr.0.1 = rr.0.1,
    rr.1.2 = rr.1.2,
    rr.2.3 = rr.2.3
  )
  return(rr.list)
}
















##  get the risk output

get.risk = function(sample.data){
  nd = sample.data
  #
  ad.rate.0.1 <- as.data.frame(ci.pred( mod.ad.0.1, nd, alpha = 0.1, Exp = T))*100
  rr.0.1 = ad.rate.0.1
  rr.0.1 = narrow.in.range(rr.0.1, 100)
  #
  ad.rate.1.2 <- as.data.frame(ci.pred( mod.ad.1.2, nd, alpha = 0.1, Exp = T))*100
  rr.1.2 = ad.rate.1.2
  rr.1.2 = narrow.in.range(rr.1.2, 100)
  #
  ad.rate.2.3 <- as.data.frame(ci.pred( mod.ad.2.3, nd, alpha = 0.1, Exp = T))*100
  rr.2.3 = ad.rate.2.3
  rr.2.3 = narrow.in.range(rr.2.3, 100)
  #
  rr.list = list(
    rr.0.1 = rr.0.1,
    rr.1.2 = rr.1.2,
    rr.2.3 = rr.2.3
  )
  return(rr.list)
}














## to fix the range to be 0~1
#
narrow.in.range = function(data, up){
  this.data = data
  for (i in 1:ncol(this.data)) {
    temp.data = c(this.data[,i])
    this.data[,i] = ifelse(temp.data > up, up, temp.data)
  }
  return(this.data)
}






















































#           pdf('plot_TwoState_transition_table__v01.pdf', width = 9, height = 6)
## print summary table
#grid.table(round(curr.summary.tab$Transitions, 1))
tab.to.plot = tableGrob(round(curr.summary.tab$Transitions, 1))
grid.arrange(#tableGrob(' ', theme = ttheme_minimal()), 
             tableGrob(' ', theme = ttheme_minimal()),
             tab.to.plot, nrow = 3, newpage = F)
#grid.draw(tableGrob(round(curr.summary.tab$Transitions, 1)))
#
#             dev.off()
#             dev.off()









load('raw_baseline_DM_data.RData')

head(dm.data)
length(unique(dm.data$ReferenceKey))
length(unique(na.omit(dm.data)$ReferenceKey))



































#
library(survival)
#

#
curr.start.stage = '0'; curr.end.stage = '1'
cx.ad.0.1 <- coxph(
  Surv(tfd, tfd + lex.dur, (lex.Xst %in% curr.end.stage)) 
  ~ Ns(HbA1c.lvl, knots = kd.HbA1c.0.1) #+ Ns(tfd, knots = kd.ad.0.1)
  + sex + age + BMI + SBP + DBP# + smoke.status + Alcohol.status
  + Tot.Cholesterol + Creatinine + HDLC + LDLC + Triglycerides
  #+ is.Drug.ACEI + is.Drug.ARB
  , 
  data = subset(curr.split.dm.lexis, lex.Cst %in% curr.start.stage), 
  method = "breslow"
)
#
#    summary(cx.ad.0.1)


#
curr.start.stage = '1'; curr.end.stage = '2'
cx.ad.1.2 <- coxph(
  Surv(tfd, tfd + lex.dur, (lex.Xst %in% curr.end.stage)) 
  ~ Ns(HbA1c.lvl, knots = kd.HbA1c.1.2) #+ Ns(tfd, knots = kd.ad.1.2)
  + sex + age + BMI + SBP + DBP# + smoke.status + Alcohol.status
  + Tot.Cholesterol + Creatinine + HDLC + LDLC + Triglycerides
  #+ is.Drug.ACEI + is.Drug.ARB
  , 
  data = subset(curr.split.dm.lexis, lex.Cst %in% curr.start.stage), 
  method = "breslow"
)
#
#    summary(cx.ad.1.2)


#
curr.start.stage = '2'; curr.end.stage = '3'
cx.ad.2.3 <- coxph(
  Surv(tfd, tfd + lex.dur, (lex.Xst %in% curr.end.stage)) 
  ~ Ns(HbA1c.lvl, knots = kd.HbA1c.2.3) #+ Ns(tfd, knots = kd.ad.2.3)
  + sex + age + BMI + SBP + DBP# + smoke.status + Alcohol.status
  + Tot.Cholesterol + Creatinine + HDLC + LDLC + Triglycerides
  #+ is.Drug.ACEI + is.Drug.ARB
  , 
  data = subset(curr.split.dm.lexis, lex.Cst %in% curr.start.stage), 
  method = "breslow"
)
#
#    summary(cx.ad.2.3)


#save(cx.ad.0.1, cx.ad.1.2, cx.ad.2.3, file = 'dm_BOTH_[cox]_models_[HbA1c.lvl]__.RData')





cx.ad.0.1
cx.ad.1.2
cx.ad.2.3


##
temp.cx.mod = cx.ad.0.1
temp.cx.summary = summary(temp.cx.mod)
scaled.R.squared = temp.cx.summary$rsq[1] /temp.cx.summary$rsq[2]
scaled.R.squared
temp.cx.summary$logtest[3]






##







cx.ad.0.1
cx.ad.1.2
cx.ad.2.3

curr.start.stage = '0'; curr.end.stage = '1'
curr.start.stage = '1'; curr.end.stage = '2'
curr.start.stage = '2'; curr.end.stage = '3'
#
#
#
temp.cx.mod = cx.ad.2.3
temp.new.data = subset(curr.split.dm.lexis, lex.Cst %in% curr.start.stage)
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
  frk.obs.time = curr.split.dm.lexis$tfd[kk]
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




