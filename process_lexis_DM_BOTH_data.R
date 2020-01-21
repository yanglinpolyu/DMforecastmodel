








library( readxl )
library( Epi )
library( popEpi )
library( foreign )

# for table plot
library(gridExtra)
library(grid)









source('functions_for_process_Lexis.R')








#load('raw_baseline_DM_data.RData')
load('dm_baseline_BOTH_stage_record.RData')
dim(dm.record)
head(dm.record)
colnames(dm.record)

#summary(dm.record$end.time - dm.record$start.time)













# set lexis obj

#set.seed( 1993 )
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
summary( dm.lexis )
names( dm.lexis )
head(dm.lexis)






# pre set the progress
BOTH.stage.list = as.character(0:3)






curr.dm.lexis = dm.lexis














##     start cutting!
#
curr.cut.dm.lexis = curr.dm.lexis





##
#   to cut from: 1
curr.cut.dm.lexis <- cutLexis(
  data = curr.cut.dm.lexis,
  cut = c(curr.cut.dm.lexis$time.1 - curr.cut.dm.lexis$P),
  timescale = "tfd",
  precursor.states = NULL,
  new.state = '1',
  split.states = F,
  new.scale = F,
  count = F
)
#
summary( curr.cut.dm.lexis, timeScale = T )
curr.summary.tab = summary( curr.cut.dm.lexis, timeScale = F )
curr.summary.tab$Transitions
#   end cut
##


##
#   to cut from: 2
curr.cut.dm.lexis <- cutLexis(
  data = curr.cut.dm.lexis,
  cut = c(curr.cut.dm.lexis$time.2 - curr.cut.dm.lexis$P),
  timescale = "tfd",
  precursor.states = NULL,
  new.state = '2',
  split.states = F,
  new.scale = F,
  count = F
)
#
summary( curr.cut.dm.lexis, timeScale = T )
curr.summary.tab = summary( curr.cut.dm.lexis, timeScale = F )
curr.summary.tab$Transitions
#   end cut
##



### final end of the cutting!



































###################################################
### code chunk number 7: box-r
###################################################
# boxes(
#   curr.cut.dm.lexis,
#   boxpos = list(x = c(15, 15, 85, 85),
#                 y = c(85, 15, 85, 15)),
#   show.BE = T,
#   scale.R = 100
# )


#   boxes( curr.cut.dm.lexis )






###################################################
###  split
###################################################
system.time(
  curr.split.dm.lexis <- splitLexis(curr.cut.dm.lexis, breaks = seq(0, 11, 1 /4), "tfd")
)
# system.time(
#   curr.split.dm.lexis <- splitLexis(curr.cut.dm.lexis, breaks = seq(0, 111, 99), "tfd")
# )
summary( curr.split.dm.lexis )


# # a more elegent approach
# system.time(
# Sbc <- splitMulti( Rbc, tfd=seq(0,100,1/12) ) )
# summary( Sbc )



original.split.dm.lexis = curr.split.dm.lexis
dim(original.split.dm.lexis)


####
####
####
####

































































###################################################
###  include co-var
###################################################

## start to include co-var


load('RAMP-DM_MRAM_HbA1c.RData')
head(HbA1c.Test, n = 11)
summary(HbA1c.Test)

load('RAMP-DM_MRAM_TC.RData')
head(Cholesterol.Test)


load('RAMP-DM_MRAM_PersonInfo.RData')
head(PersonInfo)
dim(PersonInfo)
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

dim(curr.split.dm.lexis)








##  start matching
#
prev.time = Sys.time()
print(prev.time)
#--#
curr.split.dm.lexis$HbA1c.lvl = NA
curr.split.dm.lexis$HbA1c.sd = NA
curr.split.dm.lexis$HbA1c.cv = NA
#
curr.split.dm.lexis$Tot.Cholesterol = NA
curr.split.dm.lexis$smoke.status = NA
curr.split.dm.lexis$Alcohol.status = NA
curr.split.dm.lexis$BMI = NA
curr.split.dm.lexis$SBP = NA
curr.split.dm.lexis$DBP = NA
#
curr.split.dm.lexis$Creatinine = NA
curr.split.dm.lexis$HDLC = NA
curr.split.dm.lexis$LDLC = NA
curr.split.dm.lexis$Triglycerides = NA
curr.split.dm.lexis$Ur.AC.ratio = NA
curr.split.dm.lexis$Ur.PC.ratio = NA
#
curr.split.dm.lexis$is.Drug.ACEI = NA
curr.split.dm.lexis$is.Drug.ARB = NA
#--#
for(i in 1:nrow(curr.split.dm.lexis)){#      i = 1
  temp.dm.lexis = curr.split.dm.lexis[i,]
  #
  # HbA1c lvl
  temp.match.index.01 = which(curr.var.data.01$Ref.Key.ID == temp.dm.lexis$lex.id)
  temp.var.data.01 = curr.var.data.01[temp.match.index.01,]
  temp.suitable.index.01 = find.suitable.biomarker(his.now = temp.dm.lexis$P, his.record = temp.var.data.01$test.Date)
  if(is.na(temp.suitable.index.01)){temp.dm.lexis$HbA1c.lvl = c(NA)}
  temp.dm.lexis$HbA1c.lvl = mean(temp.var.data.01$HbA1c[temp.suitable.index.01])
  #
  # HbA1c sd
  temp.match.index.01 = which(curr.var.data.01$Ref.Key.ID == temp.dm.lexis$lex.id)
  temp.var.data.01 = curr.var.data.01[temp.match.index.01,]
  temp.suitable.index.01 = find.suitable.HbA1c.sd(
    his.start = (temp.dm.lexis$P - temp.dm.lexis$tfd), #
    his.end = (temp.dm.lexis$P + temp.dm.lexis$lex.dur), 
    his.record = temp.var.data.01$test.Date
  )
  temp.dm.lexis$HbA1c.sd = sd(temp.var.data.01$HbA1c[temp.suitable.index.01], na.rm = T)
  #
  # HbA1c cv
  temp.dm.lexis$HbA1c.cv = sd(temp.var.data.01$HbA1c[temp.suitable.index.01], na.rm = T) /mean(temp.var.data.01$HbA1c[temp.suitable.index.01])
  #
  # 
  temp.match.index.02 = which(curr.var.data.02$Ref.Key.ID == temp.dm.lexis$lex.id)
  temp.var.data.02 = curr.var.data.02[temp.match.index.02,]
  temp.suitable.index.02 = find.suitable.date.index(his.now = temp.dm.lexis$P, his.record = temp.var.data.02$test.Date)
  if(is.na(temp.suitable.index.02)){temp.dm.lexis$Tot.Cholesterol = c(NA)}
  temp.dm.lexis$Tot.Cholesterol = mean(temp.var.data.02$Tot.Cholesterol[temp.suitable.index.02])
  #
  temp.match.index.03 = which(curr.var.data.03$Ref.Key.ID == temp.dm.lexis$lex.id)
  temp.var.data.03 = curr.var.data.03[temp.match.index.03,]
  temp.suitable.index.03 = find.suitable.date.index(his.now = temp.dm.lexis$P, his.record = temp.var.data.03$test.Date)
  if(is.na(temp.suitable.index.03)){
    temp.dm.lexis$smoke.status = c(NA)
    temp.dm.lexis$Alcohol.status = c(NA)
    temp.dm.lexis$BMI = c(NA)
    temp.dm.lexis$SBP = c(NA)
    temp.dm.lexis$DBP = c(NA)
  }
  temp.dm.lexis$smoke.status = c(temp.var.data.03$smoke.status[temp.suitable.index.03])[1]
  temp.dm.lexis$Alcohol.status = c(temp.var.data.03$Alcohol.status[temp.suitable.index.03])[1]
  temp.dm.lexis$BMI = mean(temp.var.data.03$BMI[temp.suitable.index.03])
  temp.dm.lexis$SBP = mean(temp.var.data.03$SBP[temp.suitable.index.03])
  temp.dm.lexis$DBP = mean(temp.var.data.03$DBP[temp.suitable.index.03])
  #
  temp.match.index.04 = which(curr.var.data.04$Ref.Key.ID == temp.dm.lexis$lex.id)
  temp.var.data.04 = curr.var.data.04[temp.match.index.04,]
  temp.suitable.index.04 = find.suitable.date.index(his.now = temp.dm.lexis$P, his.record = temp.var.data.04$test.Date)
  if(is.na(temp.suitable.index.04)){temp.dm.lexis$Creatinine = c(NA)}
  temp.dm.lexis$Creatinine = mean(temp.var.data.04$Creatinine[temp.suitable.index.04])
  #
  temp.match.index.05 = which(curr.var.data.05$Ref.Key.ID == temp.dm.lexis$lex.id)
  temp.var.data.05 = curr.var.data.05[temp.match.index.05,]
  temp.suitable.index.05 = find.suitable.date.index(his.now = temp.dm.lexis$P, his.record = temp.var.data.05$test.Date)
  if(is.na(temp.suitable.index.05)){temp.dm.lexis$HDLC = c(NA)}
  temp.dm.lexis$HDLC = mean(temp.var.data.05$HDLC[temp.suitable.index.05])
  #
  temp.match.index.06 = which(curr.var.data.06$Ref.Key.ID == temp.dm.lexis$lex.id)
  temp.var.data.06 = curr.var.data.06[temp.match.index.06,]
  temp.suitable.index.06 = find.suitable.date.index(his.now = temp.dm.lexis$P, his.record = temp.var.data.06$test.Date)
  if(is.na(temp.suitable.index.06)){temp.dm.lexis$LDLC = c(NA)}
  temp.dm.lexis$LDLC = mean(temp.var.data.06$LDLC[temp.suitable.index.06])
  #
  temp.match.index.07 = which(curr.var.data.07$Ref.Key.ID == temp.dm.lexis$lex.id)
  temp.var.data.07 = curr.var.data.07[temp.match.index.07,]
  temp.suitable.index.07 = find.suitable.date.index(his.now = temp.dm.lexis$P, his.record = temp.var.data.07$test.Date)
  if(is.na(temp.suitable.index.07)){temp.dm.lexis$Triglycerides = c(NA)}
  temp.dm.lexis$Triglycerides = mean(temp.var.data.07$Triglycerides[temp.suitable.index.07])
  #
  temp.match.index.08 = which(curr.var.data.08$Ref.Key.ID == temp.dm.lexis$lex.id)
  temp.var.data.08 = curr.var.data.08[temp.match.index.08,]
  temp.suitable.index.08 = find.suitable.biomarker(his.now = temp.dm.lexis$P, his.record = temp.var.data.08$test.Date)
  if(is.na(temp.suitable.index.08)){temp.dm.lexis$Ur.AC.ratio = c(NA)}
  temp.dm.lexis$Ur.AC.ratio = mean(temp.var.data.08$Ur.AC.ratio[temp.suitable.index.08])
  #
  temp.match.index.09 = which(curr.var.data.09$Ref.Key.ID == temp.dm.lexis$lex.id)
  temp.var.data.09 = curr.var.data.09[temp.match.index.09,]
  temp.suitable.index.09 = find.suitable.biomarker(his.now = temp.dm.lexis$P, his.record = temp.var.data.09$test.Date)
  if(is.na(temp.suitable.index.09)){temp.dm.lexis$Ur.PC.ratio = c(NA)}
  temp.dm.lexis$Ur.PC.ratio = mean(temp.var.data.09$Ur.PC.ratio[temp.suitable.index.09])
  #
  temp.match.index.10 = which(curr.var.data.10$Ref.Key.ID == temp.dm.lexis$lex.id)
  temp.var.data.10 = curr.var.data.10[temp.match.index.10,]
  temp.suitable.index.10 = find.suitable.biomarker(his.now = temp.dm.lexis$P, his.record = temp.var.data.10$test.Date)
  if(is.na(temp.suitable.index.10)){temp.dm.lexis$is.Drug.ACEI = c(NA)}
  temp.dm.lexis$is.Drug.ACEI = c(temp.var.data.10$drug.class[temp.suitable.index.10])[1]
  #
  temp.match.index.11 = which(curr.var.data.11$Ref.Key.ID == temp.dm.lexis$lex.id)
  temp.var.data.11 = curr.var.data.11[temp.match.index.11,]
  temp.suitable.index.11 = find.suitable.biomarker(his.now = temp.dm.lexis$P, his.record = temp.var.data.11$test.Date)
  if(is.na(temp.suitable.index.11)){temp.dm.lexis$is.Drug.ARB = c(NA)}
  temp.dm.lexis$is.Drug.ARB = c(temp.var.data.11$drug.class[temp.suitable.index.11])[1]
  #
  #
  #class(temp.dm.lexis)
  print( paste0(round(i/nrow(curr.split.dm.lexis) *100, digits = 4), '%') )
  curr.split.dm.lexis[i,] = temp.dm.lexis
}
head(curr.split.dm.lexis)
#--#
curr.time = Sys.time()
print(curr.time)
print(curr.time -prev.time)





#             save(curr.split.dm.lexis, file = 'dm_baseline_BOTH_stage_Lexis_[all.var]__.RData')





# curr.var.data
# temp.match.index = which(curr.var.data$Ref.Key.ID == temp.dm.lexis$lex.id)
# temp.var.data = curr.var.data[temp.match.index,]
# temp.suitable.index = find.suitable.date.index(his.now = temp.dm.lexis$P, his.record = temp.var.data$test.Date)
# if(is.na(temp.suitable.index)){next}
# temp.dm.lexis$HbA1c.lvl = mean(temp.var.data$HbA1c[temp.suitable.index])























