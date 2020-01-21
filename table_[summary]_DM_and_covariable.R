

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





load(file = 'reduced_DM_data_and_covariable_for_summary_table__.RData')



##
#
original.reduced.record = reduced.record
dim(original.reduced.record)
##
##
non.na.index = which(!is.na(original.reduced.record$is.Drug.ACEI))
reduced.record$is.Drug.ACEI[non.na.index] = 'yes'
na.index = which(is.na(original.reduced.record$is.Drug.ACEI))
reduced.record$is.Drug.ACEI[na.index] = 'no'
#
non.na.index = which(!is.na(original.reduced.record$is.Drug.ARB))
reduced.record$is.Drug.ARB[non.na.index] = 'yes'
na.index = which(is.na(original.reduced.record$is.Drug.ARB))
reduced.record$is.Drug.ARB[na.index] = 'no'




summary(reduced.record)
dim(reduced.record)





head(reduced.record)



names(reduced.record)
mean(reduced.record$followup.duration[reduced.record$followup.duration>0])
quantile(reduced.record$followup.duration[reduced.record$followup.duration>0], probs = c(0, 1))





#

prog.record = subset(reduced.record, as.numeric(base.status) < as.numeric(end.status))
dim(prog.record)
mean(prog.record$followup.duration[prog.record$followup.duration>0])
quantile(prog.record$followup.duration[prog.record$followup.duration>0], probs = c(0, 1))
#
regr.record = subset(reduced.record, as.numeric(base.status) > as.numeric(end.status))
dim(regr.record)
mean(regr.record$followup.duration[regr.record$followup.duration>0])
quantile(regr.record$followup.duration[regr.record$followup.duration>0], probs = c(0, 1))
#
stay.record = subset(reduced.record, as.numeric(base.status) == as.numeric(end.status))
dim(stay.record)
mean(stay.record$followup.duration[stay.record$followup.duration>0])
quantile(stay.record$followup.duration[stay.record$followup.duration>0], probs = c(0, 1))












#  for factor
names(reduced.record)
#
#

temp.var = 'PAD'
temp.index = which(names(reduced.record) == temp.var)
sum(!is.na(reduced.record[,temp.index]))
## factor var
#
table(reduced.record[,temp.index])
round(prop.table(table(reduced.record[,temp.index])), digits = 3)
#
table(regr.record[,temp.index])
round(prop.table(table(regr.record[,temp.index])), digits = 3)
#
table(stay.record[,temp.index])
round(prop.table(table(stay.record[,temp.index])), digits = 3)
#
table(prog.record[,temp.index])
round(prop.table(table(prog.record[,temp.index])), digits = 3)
#












# dim(reduced.record)
# names(reduced.record)
# mean(subset(reduced.record, sex == 'F')$age)
# quantile(subset(reduced.record, sex == 'F')$age, probs = c(0.025,0.975))
# mean(subset(reduced.record, sex == 'M')$age)
# quantile(subset(reduced.record, sex == 'M')$age, probs = c(0.025,0.975))












#   for numeric
names(reduced.record)
#
#

temp.var = 'HDLC'
temp.index = which(names(reduced.record) == temp.var)
sum(!is.na(reduced.record[,temp.index]))
## numeric var
#
temp.dig = 1
round(mean(reduced.record[,temp.index], na.rm = T), digits = temp.dig)
round(sd(reduced.record[,temp.index], na.rm = T), digits = temp.dig)
#
round(mean(regr.record[,temp.index], na.rm = T), digits = temp.dig)
round(sd(regr.record[,temp.index], na.rm = T), digits = temp.dig)
#
round(mean(stay.record[,temp.index], na.rm = T), digits = temp.dig)
round(sd(stay.record[,temp.index], na.rm = T), digits = temp.dig)
#
round(mean(prog.record[,temp.index], na.rm = T), digits = temp.dig)
round(sd(prog.record[,temp.index], na.rm = T), digits = temp.dig)
#











##
temp.range = c(-1, 18.5)
temp.overall = sum(temp.range[1] <= reduced.record[,temp.index] & reduced.record[,temp.index] < temp.range[2], na.rm = T)
temp.overall
round(temp.overall/ sum(!is.na(reduced.record[,temp.index])), digits = 3)
#
temp.regr = sum(temp.range[1] <= regr.record[,temp.index] & regr.record[,temp.index] < temp.range[2], na.rm = T)
temp.regr
round(temp.regr/ sum(!is.na(regr.record[,temp.index])), digits = 3)
#
temp.stay = sum(temp.range[1] <= stay.record[,temp.index] & stay.record[,temp.index] < temp.range[2], na.rm = T)
temp.stay
round(temp.stay/ sum(!is.na(stay.record[,temp.index])), digits = 3)
#
temp.prog = sum(temp.range[1] <= prog.record[,temp.index] & prog.record[,temp.index] < temp.range[2], na.rm = T)
temp.prog
round(temp.prog/ sum(!is.na(prog.record[,temp.index])), digits = 3)
##




















###
#  for DR
###




names(reduced.record)
#
#
#




names(reduced.record)
#  head(reduced.record, n = 11)
table(reduced.record$Laser.left)





# reduced.record
# regr.record
# stay.record
# prog.record


#### sight
temp.record = stay.record
#
temp.num = nrow(
  subset(
    temp.record, 
    #
    (DR.lvl.right %in% c('Severe NPDR','PDR') | DR.lvl.left %in% c('Severe NPDR','PDR')) 
    | (Laser.right %in% c('Yes') | Laser.left %in% c('Yes'))
    | (Vitrectomy.right %in% c('Yes') | Vitrectomy.left %in% c('Yes'))
    | (anti.VEGF.Rx.right %in% c('Yes') | anti.VEGF.Rx.left %in% c('Yes'))
    | (Maculopathy.right %in% c('Yes') | Maculopathy.left %in% c('Yes'))
  )
)
temp.num
round(temp.num/ nrow(temp.record), digits = 3)
#



# reduced.record
# regr.record
# stay.record
# prog.record

#### non-sight
temp.record = regr.record
#
temp.num = nrow(
  subset(
    temp.record, 
    #
    (DR.lvl.right %in% c('Mild NPDR','Moderate NPDR') | DR.lvl.left %in% c('Mild NPDR','Moderate NPDR')) 
    & (Laser.right %in% c('No') & Laser.left %in% c('No'))
    & (Maculopathy.right %in% c('No') & Maculopathy.left %in% c('No'))
  )
)
temp.num
round(temp.num/ nrow(temp.record), digits = 3)
#



# reduced.record
# regr.record
# stay.record
# prog.record

#### NO
temp.record = stay.record
#
temp.num = nrow(
  subset(
    temp.record, 
    #
    (DR.lvl.right %in% c('No DR') & DR.lvl.left %in% c('No DR')) 
    # & (Laser.right %in% c('No') & Laser.left %in% c('No'))
    # & (Vitrectomy.right %in% c('No') & Vitrectomy.left %in% c('No'))
    # & (anti.VEGF.Rx.right %in% c('No') & anti.VEGF.Rx.left %in% c('No'))
    # & (Maculopathy.right %in% c('No') & Maculopathy.left %in% c('No'))
  )
)
temp.num
round(temp.num/ nrow(temp.record), digits = 3)
#





# reduced.record
# regr.record
# stay.record
# prog.record

#### Not Known
temp.record = stay.record
#
temp.num = nrow(
  subset(
    temp.record, 
    #
    (DR.lvl.right %in% c('') | DR.lvl.left %in% c('')
     & !DR.lvl.right %in% c('Severe NPDR','PDR') & !DR.lvl.left %in% c('Severe NPDR','PDR')) 
    #& (Laser.right %in% c('Not known','') | Laser.left %in% c('Not known',''))
    & (Vitrectomy.right %in% c('No') & Vitrectomy.left %in% c('No'))
    & (anti.VEGF.Rx.right %in% c('No') & anti.VEGF.Rx.left %in% c('No'))
    #& (Maculopathy.right %in% c('No') & Maculopathy.left %in% c('No'))
  )
)
temp.num
round(temp.num/ nrow(temp.record), digits = 3)
#




















##
#  for p-values
##











# for binomial var
p.list = NULL
for(i in 1:1000){
  temp.t.test = t.test(
    x = rbinom(n = 4378, size = 1, prob = 0.520),  # progression
    y = rbinom(n = 21819, size = 1, prob = 0.412)  # no progression
  )
  p.list = c(p.list, temp.t.test$p.value)
}
#round(temp.t.test$p.value, digits = 4)
round(median(p.list), digits = 4)
#









#   for numeric
names(reduced.record)
#

temp.var = 'HDLC'
temp.index = which(names(reduced.record) == temp.var)
sum(!is.na(reduced.record[,temp.index]))
## numeric var
temp.dig = 4
#
round(mean(stay.record[,temp.index], na.rm = T), digits = temp.dig)
round(sd(stay.record[,temp.index], na.rm = T), digits = temp.dig)
#
round(mean(prog.record[,temp.index], na.rm = T), digits = temp.dig)
round(sd(prog.record[,temp.index], na.rm = T), digits = temp.dig)
#
round(mean(regr.record[,temp.index], na.rm = T), digits = temp.dig)
round(sd(regr.record[,temp.index], na.rm = T), digits = temp.dig)
#

# for anova test
stay.var = c(stay.record[,temp.index])
prog.var = c(prog.record[,temp.index])
regr.var = c(regr.record[,temp.index])

curr.var.data = data.frame(
  var = c(stay.var, prog.var, regr.var),
  lab = c(rep('stay', length(stay.var)), rep('prog', length(prog.var)), rep('regr', length(regr.var)))
)
#
curr.lm = lm(var ~ lab, data = curr.var.data)
summary(curr.lm)
anova(curr.lm)







# # for normal var, t-test
# p.list = NULL
# for(i in 1:1000){
#   temp.t.test = t.test(
#     x = rnorm(n = 4378, mean = 3.5185, sd = 0.6757),  # progression
#     y = rnorm(n = 21819, mean = 3.6134, sd = 0.7101)  # no progression
#   )
#   p.list = c(p.list, temp.t.test$p.value)
# }
# #round(temp.t.test$p.value, digits = 4)
# round(median(p.list), digits = 4)
# #











###
###
###






#
all.var.array = c(1971, 19848, 4378)
#
temp.mat = rbind(
  c(25,207,54), 
  c(309,4013,858), 
  c(379,4333,853), 
  c(1254,11280,2606)
)
temp.mat = rbind(
  temp.mat,
  all.var.array - colSums(temp.mat)
)
#
temp.table <- as.table(temp.mat)
#

chisq.test(temp.table)










#
all.var.array = c(1971, 19848, 4378)
#
bi.var.array.1 = c(81, 669, 243)
bi.var.array.2 = all.var.array - bi.var.array.1
#

temp.table <- as.table(rbind(
  bi.var.array.1,
  bi.var.array.2
))
#

chisq.test(temp.table)











#
sum(reduced.record$followup.duration)
sum(regr.record$followup.duration)
sum(stay.record$followup.duration)
sum(prog.record$followup.duration)







quantile(reduced.record$followup.duration, probs = c(0.00210,0.99999))
quantile(prog.record$followup.duration, probs = c(0.00210,0.99999))
quantile(regr.record$followup.duration, probs = c(0.00210,0.99999))




