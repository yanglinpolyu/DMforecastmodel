



####
####

library( readxl )
library( Epi )
library( popEpi )
library( foreign )

# for table plot
library(gridExtra)
library(grid)

library(vioplot)

####
####






#
##
load('dm_baseline_BOTH_stage_Lexis_[all.var]__.RData')
##
#




summary(curr.split.dm.lexis)


unique.id.array = unique(curr.split.dm.lexis$lex.id[!is.na(curr.split.dm.lexis$HbA1c.sd)])
length(unique.id.array)










#load('raw_baseline_DM_data.RData')
load('dm_baseline_BOTH_stage_record.RData')
dim(dm.record)
head(dm.record)
colnames(dm.record)

#
set.seed( 1993 )
dm.lexis <- Lexis(
  entry.status = start.stage,
  entry = list(
    tfd = 0,
    A = age   + runif(nrow(dm.record),-0.5,0.5),
    P = start.time
  ),
  exit = list(tfd = (end.time+1/(365.25*24*60) - start.time)),
  exit.status = end.stage,
  id = ref.key,
  data = dm.record
)

#
#summary( dm.lexis )
names( dm.lexis )



#reduced.dm.lexis

reduced.dm.lexis = subset(dm.lexis, lex.id %in% unique.id.array)
#

length(unique(reduced.dm.lexis$lex.id))
summary( reduced.dm.lexis )





names(reduced.dm.lexis)






#reduced.split.dm.lexis







lexis.record = NULL
sex.record = NULL
for(ii in 1:length(unique.id.array)){#       ii = 1
  this.id = unique.id.array[ii]
  #
  this.lexis = subset(reduced.dm.lexis, lex.id %in% c(this.id))
  this.lexis = this.lexis[order(this.lexis$P),] 
  #
  this.record = c(
    this.id, 
    this.lexis$P[1], as.character(this.lexis$lex.Cst[1]), 
    ( this.lexis$end.time[nrow(this.lexis)] ), as.character(this.lexis$lex.Xst[nrow(this.lexis)])
  )
  this.record = as.numeric(this.record)
  this.record = c(this.record, this.lexis$age[1])
  sex.record = c(sex.record, as.character(this.lexis$sex[1]))
  lexis.record = rbind(lexis.record, c(this.record))
  print(paste0(ii/length(unique.id.array)*100,'%'))
}
lexis.record = as.data.frame(lexis.record)
colnames(lexis.record) = c('id', 'base.time', 'base.status', 'end.time', 'end.status', 'age')
lexis.record$duration = lexis.record$end.time - lexis.record$base.time
lexis.record$sex = c(sex.record)
dim(lexis.record)
summary(lexis.record)





reduced.dm.record = lexis.record



#
save(reduced.dm.lexis, reduced.dm.record, file = 'reduced_DM_data_for_summary_table.RData')
#



















































































####
####

library( readxl )
library( Epi )
library( popEpi )
library( foreign )

# for table plot
library(gridExtra)
library(grid)

library(vioplot)

####
####



source('functions_for_process_Lexis.R')


load(file = 'reduced_DM_data_for_summary_table.RData')
dim(reduced.dm.record)



curr.reduced.dm.record = reduced.dm.record








###################################################
###  include co-var
###################################################

load('RAMP-DM_MRAM_HbA1c.RData')
head(HbA1c.Test, n = 11)
summary(HbA1c.Test)

load('RAMP-DM_MRAM_TC.RData')
head(Cholesterol.Test)


load('RAMP-DM_MRAM_PersonInfo.RData')
head(PersonInfo)
summary(PersonInfo)
names(PersonInfo)
length(unique(PersonInfo$Ref.Key.ID))



load('RAMP-DM_MRAM_Creatinine.RData')
head(CreatinineTest)


load('RAMP-DM_MRAM_HDL.RData')
head(HDLC.Test)

load('RAMP-DM_MRAM_LDL.RData')
head(LDLC.Test)


load('RAMP-DM_MRAM_Tg.RData')
head(Triglycerides.Test)


load('RAMP-DM_MRAM_UACR.RData')
head(UACR.Test)


load('RAMP-DM_MRAM_UPCR.RData')
head(UPCR.Test)


load('RAMP-DM_MRAM_[drug]_ACEI.RData')
head(Drug.ACEI.record)
load('RAMP-DM_MRAM_[drug]_ARB.RData')
head(Drug.ARB.record)



load('baseline_DM_data.RData')
head(dm.data)




#
curr.var.data.01 = HbA1c.Test
curr.var.data.01$test.Date = c(cal.yr(curr.var.data.01$test.Date))
#
curr.var.data.02 = Cholesterol.Test
curr.var.data.02$test.Date = c(cal.yr(curr.var.data.02$test.Date))
#
curr.var.data.03 = PersonInfo
curr.var.data.03$test.Date = c(cal.yr(curr.var.data.03$MRAM.date))


curr.var.data.04 = CreatinineTest
curr.var.data.04$test.Date = c(cal.yr(curr.var.data.04$test.Date))

curr.var.data.05 = HDLC.Test
curr.var.data.05$test.Date = c(cal.yr(curr.var.data.05$test.Date))

curr.var.data.06 = LDLC.Test
curr.var.data.06$test.Date = c(cal.yr(curr.var.data.06$test.Date))

curr.var.data.07 = Triglycerides.Test
curr.var.data.07$test.Date = c(cal.yr(curr.var.data.07$test.Date))


curr.var.data.08 = UACR.Test
curr.var.data.08$test.Date = c(cal.yr(curr.var.data.08$test.Date))

curr.var.data.09 = UPCR.Test
curr.var.data.09$test.Date = c(cal.yr(curr.var.data.09$test.Date))


curr.var.data.10 = Drug.ACEI.record
curr.var.data.10$test.Date = c(cal.yr(curr.var.data.10$test.Date))
curr.var.data.11 = Drug.ARB.record
curr.var.data.11$test.Date = c(cal.yr(curr.var.data.11$test.Date))



curr.var.data.12 = dm.data
curr.var.data.12$test.Date = c((curr.var.data.12$Date.egfr))

#
dim(curr.reduced.dm.record)
names(curr.reduced.dm.record)
#



##  start matching
#
prev.time = Sys.time()
print(prev.time)
#--#
curr.reduced.dm.record$HbA1c.lvl = NA
curr.reduced.dm.record$HbA1c.sd = NA
curr.reduced.dm.record$HbA1c.cv = NA
#
curr.reduced.dm.record$Tot.Cholesterol = NA
#
curr.reduced.dm.record$smoke.status = NA
curr.reduced.dm.record$Alcohol.status = NA
curr.reduced.dm.record$BMI = NA
curr.reduced.dm.record$SBP = NA
curr.reduced.dm.record$DBP = NA
curr.reduced.dm.record$Edu.level = NA
curr.reduced.dm.record$DR.lvl.right = NA
curr.reduced.dm.record$DR.lvl.left = NA
curr.reduced.dm.record$Laser.right = NA
curr.reduced.dm.record$Laser.left = NA
curr.reduced.dm.record$Vitrectomy.right = NA
curr.reduced.dm.record$Vitrectomy.left = NA
curr.reduced.dm.record$anti.VEGF.Rx.right = NA
curr.reduced.dm.record$anti.VEGF.Rx.left = NA
curr.reduced.dm.record$Maculopathy.right = NA
curr.reduced.dm.record$Maculopathy.left = NA
curr.reduced.dm.record$CHD = NA
curr.reduced.dm.record$Stroke = NA
curr.reduced.dm.record$PAD = NA
#
curr.reduced.dm.record$Creatinine = NA
curr.reduced.dm.record$HDLC = NA
curr.reduced.dm.record$LDLC = NA
curr.reduced.dm.record$Triglycerides = NA
curr.reduced.dm.record$Ur.AC.ratio = NA
curr.reduced.dm.record$Ur.PC.ratio = NA
#
curr.reduced.dm.record$is.Drug.ACEI = NA
curr.reduced.dm.record$is.Drug.ARB = NA
#
curr.reduced.dm.record$eGFR = NA
#--#
for(i in 1:nrow(curr.reduced.dm.record)){#      i = 1
  temp.dm.lexis = curr.reduced.dm.record[i,]
  #
  # HbA1c lvl
  temp.match.index.01 = which(curr.var.data.01$Ref.Key.ID == temp.dm.lexis$id)
  temp.var.data.01 = curr.var.data.01[temp.match.index.01,]
  temp.suitable.index.01 = find.suitable.biomarker(his.now = temp.dm.lexis$base.time, his.record = temp.var.data.01$test.Date)
  if(is.na(temp.suitable.index.01)){temp.dm.lexis$HbA1c.lvl = c(NA)}
  temp.dm.lexis$HbA1c.lvl = mean(temp.var.data.01$HbA1c[temp.suitable.index.01])
  #
  # HbA1c sd
  temp.match.index.01 = which(curr.var.data.01$Ref.Key.ID == temp.dm.lexis$id)
  temp.var.data.01 = curr.var.data.01[temp.match.index.01,]
  temp.suitable.index.01 = find.suitable.HbA1c.sd(
    his.start = (temp.dm.lexis$base.time), #
    his.end = (temp.dm.lexis$end.time), 
    his.record = temp.var.data.01$test.Date
  )
  temp.dm.lexis$HbA1c.sd = sd(temp.var.data.01$HbA1c[temp.suitable.index.01], na.rm = T)
  #
  # HbA1c cv
  temp.dm.lexis$HbA1c.cv = sd(temp.var.data.01$HbA1c[temp.suitable.index.01], na.rm = T) /mean(temp.var.data.01$HbA1c[temp.suitable.index.01])
  #
  # 
  temp.match.index.02 = which(curr.var.data.02$Ref.Key.ID == temp.dm.lexis$id)
  temp.var.data.02 = curr.var.data.02[temp.match.index.02,]
  temp.suitable.index.02 = find.suitable.date.index(his.now = temp.dm.lexis$base.time, his.record = temp.var.data.02$test.Date)
  if(is.na(temp.suitable.index.02)){temp.dm.lexis$Tot.Cholesterol = c(NA)}
  temp.dm.lexis$Tot.Cholesterol = mean(temp.var.data.02$Tot.Cholesterol[temp.suitable.index.02])
  #
  temp.match.index.03 = which(curr.var.data.03$Ref.Key.ID == temp.dm.lexis$id)
  temp.var.data.03 = curr.var.data.03[temp.match.index.03,]
  temp.suitable.index.03 = find.suitable.date.index(his.now = temp.dm.lexis$base.time, his.record = temp.var.data.03$test.Date)
  if(is.na(temp.suitable.index.03)){
    temp.dm.lexis$smoke.status = c(NA)
    temp.dm.lexis$Alcohol.status = c(NA)
    temp.dm.lexis$BMI = c(NA)
    temp.dm.lexis$SBP = c(NA)
    temp.dm.lexis$DBP = c(NA)
    temp.dm.lexis$Edu.level = NA
    temp.dm.lexis$DR.lvl.right = NA
    temp.dm.lexis$DR.lvl.left = NA
    temp.dm.lexis$Laser.right = NA
    temp.dm.lexis$Laser.left = NA
    temp.dm.lexis$Vitrectomy.right = NA
    temp.dm.lexis$Vitrectomy.left = NA
    temp.dm.lexis$anti.VEGF.Rx.right = NA
    temp.dm.lexis$anti.VEGF.Rx.left = NA
    temp.dm.lexis$Maculopathy.right = NA
    temp.dm.lexis$Maculopathy.left = NA
    temp.dm.lexis$CHD = NA
    temp.dm.lexis$Stroke = NA
    temp.dm.lexis$PAD = NA
  }
  temp.dm.lexis$smoke.status = c(temp.var.data.03$smoke.status[temp.suitable.index.03])[1]
  temp.dm.lexis$Alcohol.status = c(temp.var.data.03$Alcohol.status[temp.suitable.index.03])[1]
  temp.dm.lexis$BMI = mean(temp.var.data.03$BMI[temp.suitable.index.03])
  temp.dm.lexis$SBP = mean(temp.var.data.03$SBP[temp.suitable.index.03])
  temp.dm.lexis$DBP = mean(temp.var.data.03$DBP[temp.suitable.index.03])
  temp.dm.lexis$Edu.level = c(temp.var.data.03$Edu.level[temp.suitable.index.03])[1]
  temp.dm.lexis$DR.lvl.right = c(temp.var.data.03$DR.lvl.right[temp.suitable.index.03])[1]
  temp.dm.lexis$DR.lvl.left = c(temp.var.data.03$DR.lvl.left[temp.suitable.index.03])[1]
  temp.dm.lexis$Laser.right = c(temp.var.data.03$Laser.right[temp.suitable.index.03])[1]
  temp.dm.lexis$Laser.left = c(temp.var.data.03$Laser.left[temp.suitable.index.03])[1]
  temp.dm.lexis$Vitrectomy.right = c(temp.var.data.03$Vitrectomy.right[temp.suitable.index.03])[1]
  temp.dm.lexis$Vitrectomy.left = c(temp.var.data.03$Vitrectomy.left[temp.suitable.index.03])[1]
  temp.dm.lexis$anti.VEGF.Rx.right = c(temp.var.data.03$anti.VEGF.Rx.right[temp.suitable.index.03])[1]
  temp.dm.lexis$anti.VEGF.Rx.left = c(temp.var.data.03$anti.VEGF.Rx.left[temp.suitable.index.03])[1]
  temp.dm.lexis$Maculopathy.right = c(temp.var.data.03$Maculopathy.right[temp.suitable.index.03])[1]
  temp.dm.lexis$Maculopathy.left = c(temp.var.data.03$Maculopathy.left[temp.suitable.index.03])[1]
  temp.dm.lexis$CHD = c(temp.var.data.03$CHD[temp.suitable.index.03])[1]
  temp.dm.lexis$Stroke = c(temp.var.data.03$Stroke[temp.suitable.index.03])[1]
  temp.dm.lexis$PAD = c(temp.var.data.03$PAD[temp.suitable.index.03])[1]
  #
  temp.match.index.04 = which(curr.var.data.04$Ref.Key.ID == temp.dm.lexis$id)
  temp.var.data.04 = curr.var.data.04[temp.match.index.04,]
  temp.suitable.index.04 = find.suitable.date.index(his.now = temp.dm.lexis$base.time, his.record = temp.var.data.04$test.Date)
  if(is.na(temp.suitable.index.04)){temp.dm.lexis$Creatinine = c(NA)}
  temp.dm.lexis$Creatinine = mean(temp.var.data.04$Creatinine[temp.suitable.index.04])
  #
  temp.match.index.05 = which(curr.var.data.05$Ref.Key.ID == temp.dm.lexis$id)
  temp.var.data.05 = curr.var.data.05[temp.match.index.05,]
  temp.suitable.index.05 = find.suitable.date.index(his.now = temp.dm.lexis$base.time, his.record = temp.var.data.05$test.Date)
  if(is.na(temp.suitable.index.05)){temp.dm.lexis$HDLC = c(NA)}
  temp.dm.lexis$HDLC = mean(temp.var.data.05$HDLC[temp.suitable.index.05])
  #
  temp.match.index.06 = which(curr.var.data.06$Ref.Key.ID == temp.dm.lexis$id)
  temp.var.data.06 = curr.var.data.06[temp.match.index.06,]
  temp.suitable.index.06 = find.suitable.date.index(his.now = temp.dm.lexis$base.time, his.record = temp.var.data.06$test.Date)
  if(is.na(temp.suitable.index.06)){temp.dm.lexis$LDLC = c(NA)}
  temp.dm.lexis$LDLC = mean(temp.var.data.06$LDLC[temp.suitable.index.06])
  #
  temp.match.index.07 = which(curr.var.data.07$Ref.Key.ID == temp.dm.lexis$id)
  temp.var.data.07 = curr.var.data.07[temp.match.index.07,]
  temp.suitable.index.07 = find.suitable.date.index(his.now = temp.dm.lexis$base.time, his.record = temp.var.data.07$test.Date)
  if(is.na(temp.suitable.index.07)){temp.dm.lexis$Triglycerides = c(NA)}
  temp.dm.lexis$Triglycerides = mean(temp.var.data.07$Triglycerides[temp.suitable.index.07])
  #
  temp.match.index.08 = which(curr.var.data.08$Ref.Key.ID == temp.dm.lexis$id)
  temp.var.data.08 = curr.var.data.08[temp.match.index.08,]
  temp.suitable.index.08 = find.suitable.biomarker(his.now = temp.dm.lexis$base.time, his.record = temp.var.data.08$test.Date)
  if(is.na(temp.suitable.index.08)){temp.dm.lexis$Ur.AC.ratio = c(NA)}
  temp.dm.lexis$Ur.AC.ratio = mean(temp.var.data.08$Ur.AC.ratio[temp.suitable.index.08])
  #
  temp.match.index.09 = which(curr.var.data.09$Ref.Key.ID == temp.dm.lexis$id)
  temp.var.data.09 = curr.var.data.09[temp.match.index.09,]
  temp.suitable.index.09 = find.suitable.biomarker(his.now = temp.dm.lexis$base.time, his.record = temp.var.data.09$test.Date)
  if(is.na(temp.suitable.index.09)){temp.dm.lexis$Ur.PC.ratio = c(NA)}
  temp.dm.lexis$Ur.PC.ratio = mean(temp.var.data.09$Ur.PC.ratio[temp.suitable.index.09])
  #
  temp.match.index.10 = which(curr.var.data.10$Ref.Key.ID == temp.dm.lexis$id)
  temp.var.data.10 = curr.var.data.10[temp.match.index.10,]
  temp.suitable.index.10 = find.suitable.biomarker(his.now = temp.dm.lexis$base.time, his.record = temp.var.data.10$test.Date)
  if(is.na(temp.suitable.index.10)){temp.dm.lexis$is.Drug.ACEI = c(NA)}
  temp.dm.lexis$is.Drug.ACEI = c(temp.var.data.10$drug.class[temp.suitable.index.10])[1]
  #
  temp.match.index.11 = which(curr.var.data.11$Ref.Key.ID == temp.dm.lexis$id)
  temp.var.data.11 = curr.var.data.11[temp.match.index.11,]
  temp.suitable.index.11 = find.suitable.biomarker(his.now = temp.dm.lexis$base.time, his.record = temp.var.data.11$test.Date)
  if(is.na(temp.suitable.index.11)){temp.dm.lexis$is.Drug.ARB = c(NA)}
  temp.dm.lexis$is.Drug.ARB = c(temp.var.data.11$drug.class[temp.suitable.index.11])[1]
  #
  temp.match.index.12 = which(curr.var.data.12$ReferenceKey == temp.dm.lexis$id)
  temp.var.data.12 = curr.var.data.12[temp.match.index.12,]
  temp.suitable.index.12 = find.suitable.biomarker(his.now = temp.dm.lexis$base.time, his.record = temp.var.data.12$test.Date)
  if(is.na(temp.suitable.index.12)){temp.dm.lexis$eGFR = c(NA)}
  temp.dm.lexis$eGFR = mean(temp.var.data.12$eGFR[temp.suitable.index.12])
  #
  #
  #class(temp.dm.lexis)
  print( paste0(round(i/nrow(curr.reduced.dm.record) *100, digits = 4), '%') )
  curr.reduced.dm.record[i,] = temp.dm.lexis
}
head(curr.reduced.dm.record)
#--#
curr.time = Sys.time()
print(curr.time)
print(curr.time -prev.time)



reduced.record = curr.reduced.dm.record



#             save(reduced.record, file = 'reduced_DM_data_and_covariable_for_summary_table__.RData')





























