


##
#  function to find the suitable date
find.suitable.date.index = function(his.now, his.record){
  #     his.now = 2015.281; his.record = c(2011.561)
  #
  dummy.period = 1000
  threshold.period.1 = 3
  threshold.period.2 = 1
  #
  time.diff = his.record - his.now
  time.sign = sign(time.diff)
  #
  the.index = which(time.sign == 0)
  if(length(the.index) > 0){
    return(the.index)
  }
  #
  min.before.period = min(c(abs(time.diff[which(time.sign < 0)]), dummy.period))
  min.after.period = min(c(abs(time.diff[which(time.sign > 0)]), dummy.period))
  #
  if(min.before.period >= threshold.period.1 & min.after.period >= threshold.period.2){#
    return(NA)
  } else {
    if(min.before.period <= min.after.period){
      the.index = which(abs(time.diff) == min.before.period & time.sign < 0)
    } else {
      the.index = which(abs(time.diff) == min.after.period & time.sign > 0)
    }
    return(the.index)
  }
  #
}









##
#  function to find the suitable date
find.suitable.biomarker = function(his.now, his.record){
  #     his.now = 2015.281; his.record = c(2011.561)
  #
  dummy.period = 1000
  threshold.period.1 = 0.5
  #threshold.period.2 = 0.1
  threshold.period.2 = 10
  #
  time.diff = his.record - his.now
  time.sign = sign(time.diff)
  #
  the.index = which(time.sign == 0)
  if(length(the.index) > 0){
    return(the.index)
  }
  #
  min.before.period = min(c(abs(time.diff[which(time.sign < 0)]), dummy.period))
  min.after.period = min(c(abs(time.diff[which(time.sign > 0)]), dummy.period))
  #
  if(min.before.period >= threshold.period.1 & min.after.period >= threshold.period.2){#
    return(NA)
  } else {
    if(min.before.period <= min.after.period){
      the.index = which(abs(time.diff) == min.before.period & time.sign < 0)
    } else {
      the.index = which(abs(time.diff) == min.after.period & time.sign > 0)
    }
    return(the.index)
  }
  #
}









##
#  function to find the suitable date fo sd
find.suitable.HbA1c.sd = function(his.start, his.end, his.record){
  #     his.start = 2015.281; his.end = 2017.3; his.record = c(2000:2019)
  #
  dummy.period = 1000
  threshold.period.1 = 0.5
  threshold.period.2 = 0.0001
  #
  early.time = his.start - threshold.period.1
  late.time = his.end + threshold.period.2
  #
  the.index = which((his.record >= early.time) & (his.record <= late.time))
  return(the.index)
}

###################################################
###  end of include co-var
###################################################




























