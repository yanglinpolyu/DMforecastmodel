



load(file = 'RAMP-DM_MRAM_DM_diagnosis.RData')

load(file = 'reduced_DM_data_and_covariable_for_summary_table__.RData')



head(DM.diagnosis.history)
dim(DM.diagnosis.history)



original.reduced.record = reduced.record
#
curr.record = reduced.record








curr.record$DM.diagnosis.time = NA
for(i in 1:nrow(curr.record)){#       i = 1
  temp.id = curr.record$id[i]
  temp.index = which(DM.diagnosis.history$Ref.Key.ID %in% c(temp.id))
  temp.DM.history = DM.diagnosis.history[temp.index,]
  #
  curr.record$DM.diagnosis.time = temp.DM.history$DM.id.time[1]
  #
  print(paste0(i/nrow(curr.record)*100, '%'))
}
#



curr.record$DM.duration = curr.record$base.time - curr.record$DM.diagnosis.time
curr.record$followup.duration = curr.record$end.time - curr.record$base.time







reduced.record = curr.record
save(reduced.record, file = 'reduced_DM_data_and_covariable_for_summary_table__.RData')





