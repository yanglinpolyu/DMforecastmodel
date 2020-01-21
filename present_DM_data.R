



####
####
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








#
##
load('dm_baseline_BOTH_stage_record__with_HbA1c.sd_and_other_var.RData')
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














head(curr.split.dm.lexis)
summary(as.data.frame(curr.split.dm.lexis))




dim(curr.split.dm.lexis)
summary(curr.split.dm.lexis$HbA1c.sd)



summary(curr.split.dm.lexis)
(3293+136+ 8+849+35+258) /207352
class(curr.split.dm.lexis)









# ##
# 
# 
# id.list = unique(split.dm.non.na.data$lex.id)
# is.with.CKD = NULL
# is.with.CKD.at.end = NULL
# for(jj in 1:length(id.list)){#      jj = 1 +1
#   here.id = id.list[jj]
#   here.match.index = which(split.dm.non.na.data$lex.id == here.id)
#   here.dm.non.na.data = split.dm.non.na.data[here.match.index,]
#   here.is.zero.stage = here.dm.non.na.data$lex.Xst != '0'
#   here.is.with.CKD = sum(here.is.zero.stage) >0
#   here.is.with.CKD.at.end = here.is.zero.stage[length(here.is.zero.stage)]
#   #
#   is.with.CKD.at.end = c(is.with.CKD.at.end, here.is.with.CKD.at.end)
#   is.with.CKD = c(is.with.CKD, here.is.with.CKD)
#   #
#   print(paste0((jj /length(id.list))*100, '%'))
# }
# #
# length(is.with.CKD)
# sum(!is.with.CKD)
# sum(is.with.CKD)
# 
# 
# length(is.with.CKD.at.end)
# sum(!is.with.CKD.at.end)
# sum(is.with.CKD.at.end)
























summary(curr.split.dm.lexis)
(3293+136+ 8+849+35+258) /207352
class(curr.split.dm.lexis)



split.dm.data = (as.data.frame(curr.split.dm.lexis))
dim(split.dm.data)



split.dm.non.na.index = which(!is.na(split.dm.data$HbA1c.sd))
split.dm.non.na.data = split.dm.data[split.dm.non.na.index,]

length(unique(split.dm.non.na.data$lex.id))


class(split.dm.non.na.data) = c('Lexis', 'data.frame')
curr.summary.tab = summary(split.dm.non.na.data)


#           pdf('[table]plot_TwoState_BOTH_transition_table__v02.pdf', width = 9, height = 6)
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











summary(as.data.frame(split.dm.non.na.data))
dim(as.data.frame(split.dm.non.na.data))



names(split.dm.non.na.data)

length(unique(subset(split.dm.non.na.data, lex.Cst == '0' & lex.Xst == '2')$lex.id))
296 + 150






length(unique(subset(split.dm.non.na.data, lex.Cst == '3' & lex.Xst == '1')$lex.id))
110 + 29






dim(split.dm.non.na.data)


summary(as.data.frame(split.dm.non.na.data))




summary(split.dm.non.na.data$tfd + split.dm.non.na.data$lex.dur)


summary(split.dm.non.na.data$P + split.dm.non.na.data$lex.dur)



head(split.dm.non.na.data, n = 20)




names(split.dm.non.na.data)

here.dm.data = subset(split.dm.non.na.data, lex.dur < 0.25)
#here.dm.data = subset(split.dm.non.na.data, as.integer(lex.Cst) < as.integer(lex.Xst) & lex.dur < 0.25)
here.dm.data = subset(split.dm.non.na.data, as.integer(lex.Cst) >= as.integer(lex.Xst) & lex.dur < 0.25)


summary(here.dm.data$tfd + here.dm.data$lex.dur)










