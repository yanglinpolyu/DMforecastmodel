








library( readxl )
library( Epi )
library( popEpi )
library( foreign )

# for table plot
library(gridExtra)
library(grid)

library(vioplot)









source('functions_for_plot_risk_curve.R')







#
##
load('dm_baseline_BOTH_stage_Lexis_[all.var]__.RData')
##
#
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





#summary(original.split.dm.lexis$age)
summary(subset(original.split.dm.lexis, sex == 'F' & age <= 65)$age)
summary(subset(original.split.dm.lexis, sex == 'F' & age > 65)$age)
summary(subset(original.split.dm.lexis, sex == 'M' & age <= 65)$age)
summary(subset(original.split.dm.lexis, sex == 'M' & age > 65)$age)




#
#
load('dm_BOTH_[glm]_models_[HbA1c.lvl]__.RData')

#






summary(as.data.frame(curr.split.dm.lexis))


summary(mod.ad.0.1)





# 'prog'
# 'regr'
temp.code = 'regr'




##
##



temp.gender = 'F'
# '00.64'
# '65+'
temp.age.grp = '00.64'
temp.panel.index = 0
# temp.Drug.ACEI = 'yes'
# temp.Drug.ARB = 'no'
#
if(temp.gender == 'M'){
  if(temp.age.grp == '00.64'){
    temp.age = 56.12
  } else {
    temp.age = 72.44
  }
}
if(temp.gender == 'F'){
  if(temp.age.grp == '00.64'){
    temp.age = 56.43
  } else {
    temp.age = 73.72
  }
}
temp.attach.index = ifelse(temp.age.grp == '65+', 1,0) #+ ifelse(temp.Drug.ARB == 'yes', 1,0) #+1
temp.panel.index = temp.panel.index + 4*temp.attach.index
temp.y.up = ifelse(temp.code == 'prog', 1000, 4000)
#        dev.off()
#        dev.off()
#
pdf.file.name = paste0('plot_TwoState_BOTH_model_[HbA1c.lvl]_RR_[',temp.code,']_[sex.', temp.gender,']_[age.',temp.age.grp,']__v01.pdf')
pdf(file = pdf.file.name, width = 11, height = 3)
#
HbA1c.array = c(6,7,8,9)
HbA1c.sd.array = c(0.01,0.1,0.3,1)
HbA1c.cv.array = c(0.01,0.03,0.1,0.3)
#
#par(las = 1, mar = c(3.75,4.25,1.5,1), mfrow = c(2,4), oma = c(0, 0, 2, 0))
par(las = 1, mar = c(3.75,4.25,1.5,1), mfrow = c(1,4), oma = c(0, 0, 0, 0))
#
for(ii in 1:length(HbA1c.array)){#        ii = 3
  temp.HbA1c = HbA1c.array[ii]
  temp.HbA1c.sd = HbA1c.sd.array[ii]
  temp.HbA1c.cv = HbA1c.cv.array[ii]
  #
  this.tfd.array = seq(0.0, 3, length.out = 101)
  nd <- data.frame(
    HbA1c.lvl = temp.HbA1c,
    HbA1c.sd = temp.HbA1c.sd,
    HbA1c.cv = temp.HbA1c.cv,
    #lex.dur = 1000,
    lex.dur = 0.25,
    tfd = this.tfd.array,
    # Alcohol.status = here.Alcohol.status,
    # smoke.status = here.smoke.status,
    sex = temp.gender,
    age = temp.age,
    BMI = 25.70,
    SBP = 130,
    DBP = 75,
    #
    Tot.Cholesterol = 4.0,
    Creatinine = 75,
    HDLC = 1.20,
    LDLC = 2.10,
    Triglycerides = 1.210,
    #
    is.Drug.ACEI = 'no',
    is.Drug.ARB = 'no'
  )
  #
  #temp.rr.list = get.rr(nd)
  temp.rr.list = get.risk(sample.data = nd, code = temp.code)
  #
  #temp.panel.lab = paste0('(',letters[ii], temp.attach.index,') ','HbA1c = ',temp.HbA1c, '%')
  temp.panel.lab = paste0('(',letters[temp.panel.index +ii],') ','HbA1c = ',temp.HbA1c, '%')
  plot.one.rr(rr.list = temp.rr.list, panel.lab = temp.panel.lab, y.upper.lim = temp.y.up, code = temp.code)
  #
}
#-#
#-#
# plot.lab = paste0('gender: ',temp.gender,'; ACEI: ',temp.Drug.ACEI,'; ARB: ',temp.Drug.ARB)
# mtext(text = plot.lab, side = 3, outer = T, line = 0.5, adj = 0, las = 1, cex = 1.11)
#        dev.off()
#        dev.off()
#        dev.off()
#        dev.off()
        dev.off()
#        dev.off()
#        dev.off()
#        dev.off()






























