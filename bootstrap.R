################function#########
# bootstrap content for apc

boot_apc <- function(mat, dose.mat, age.min, per.min, unit, model.family="poisson.response") {
  sample <- matrix(rpois(length(mat), mat), nrow=nrow(mat), byrow=FALSE)
  sample.data <- apc.data.list(
    response=sample,
    dose=dose.mat,
    data.format="AP",
    age1=age.min,
    per1=per.min,
    unit=unit,
    coh1=NULL,
    per.zero=NULL,
    per.max=NULL,
    time.adjust=0,
    label="TB"
  )
  
  sample.apc <- apc.fit.model(sample.data, model.family=model.family, model.design="APC")
  
  pred.mat <- matrix(sample.apc$fitted.values, nrow=nrow(sample), byrow=TRUE)
  fitted.data <- apc.data.list(
    response=pred.mat,
    dose=dose.mat,
    data.format="AP",
    age1=age.min,
    per1=per.min,
    unit=unit,
    coh1=NULL,
    per.zero=NULL,
    per.max=NULL,
    time.adjust=0,
    label="TB"
  )
  
  rate.pred <- apc.data.sums(fitted.data, data.type="m", average=FALSE, apc.index=NULL)
  return(rate.pred)
}
#################################
