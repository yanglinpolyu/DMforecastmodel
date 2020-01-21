



load(file = 'obs_time_pdf_.RData')
# att.time.pdf
# plot(att.time.pdf)
smooth.obs.pdf = smooth.spline(y = att.time.pdf$y, x = att.time.pdf$x, spar = 0.66)
# plot(smooth.obs.pdf$x, predict(smooth.obs.pdf, x = smooth.obs.pdf$x)$y)


# temp.rr.list$rr.0.1 /predict(smooth.obs.pdf, x = nd$tfd)$y
# apply(temp.rr.list$rr.0.1, 2, function(gg){return(gg /predict(smooth.obs.pdf, x = nd$tfd)$y)})



# par(las = 1)
# plot(att.time.pdf, xlim = c(0,3), type = 'h', ylab = 'PDF', xlab = 'time since diagnosis (yr)', xaxs = 'i', yaxs = 'i')





# rr.list = temp.rr.list
# panel.lab = 'gg'
# y.upper.lim = 500
# code = 'regr'

#
plot.one.rr = function(rr.list, panel.lab, y.upper.lim, code){
  rr.list = rr.list
  
  #(predict(smooth.obs.pdf, x = nd$tfd)$y +0.123)
  #ifelse(predict(smooth.obs.pdf, x = nd$tfd)$y < 0.123,0.123, predict(smooth.obs.pdf, x = nd$tfd)$y)
  rr.0.1 = rr.list$rr.0.1 /(predict(smooth.obs.pdf, x = nd$tfd)$y +0.123)
  rr.1.2 = rr.list$rr.1.2 /(predict(smooth.obs.pdf, x = nd$tfd)$y +0.123)
  rr.2.3 = rr.list$rr.2.3 /(predict(smooth.obs.pdf, x = nd$tfd)$y +0.123)
  
  if(code == 'prog'){
    this.kd.ad.0.1 = kd.ad.0.1
    this.kd.ad.1.2 = kd.ad.1.2
    this.kd.ad.2.3 = kd.ad.2.3
  }
  if(code == 'regr'){
    this.kd.ad.0.1 = kd.ad.1.0
    this.kd.ad.1.2 = kd.ad.2.1
    this.kd.ad.2.3 = kd.ad.3.2
  }
  
  ##
  ###
  ##
  y.range = c(0,y.upper.lim)
  ###
  par(las = 1)
  plot(1,1, ylim = y.range, xlim = c(0, 3), type = 'n', yaxs = 'i', xaxs = 'i', log = "", axe = F, ann = F, frame = T)
  xlab = "time since diagnosis (yr)"; ylab = "adjusted incidence (/1000 PY)"
  mtext(side = 1, line = 2.5, xlab); mtext(side = 2, line = 2.75, ylab, las = 0)
  axis(4, labels = F); axis(1); grid(col = '#00000033', lty = 2)
  axis(2)
  #
  temp.col = '#FF0000'
  lines(nd$tfd, rr.0.1$Estimate, lty = 1, lwd = 2, col = paste0(temp.col,99))
  # temp.index = c(nd$tfd > min(this.kd.ad.0.1) & nd$tfd < max(this.kd.ad.0.1))
  # lines(nd$tfd[temp.index], rr.0.1$Estimate[temp.index], lty = 1, lwd = 2, col = paste0(temp.col,99))
  # points(nd$tfd[!temp.index], rr.0.1$Estimate[!temp.index], pch = 20, cex = 0.33, col = paste0(temp.col,99))
  polygon(x = c(nd$tfd, rev(nd$tfd)), y = c(rr.0.1$`5.0%`, rev(rr.0.1$`95.0%`)), col = paste0(temp.col,13), border = paste0(temp.col,66), lty = 4)
  #
  
  temp.col = '#008800'
  lines(nd$tfd, rr.1.2$Estimate, lty = 1, lwd = 2, col = paste0(temp.col,99))
  # temp.index = c(nd$tfd > min(this.kd.ad.1.2) & nd$tfd < max(this.kd.ad.1.2))   
  # lines(nd$tfd[temp.index], rr.1.2$Estimate[temp.index], lty = 1, lwd = 2, col = paste0(temp.col,99))
  # points(nd$tfd[!temp.index], rr.1.2$Estimate[!temp.index], pch = 20, cex = 0.33, col = paste0(temp.col,99))
  polygon(x = c(nd$tfd, rev(nd$tfd)), y = c(rr.1.2$`5.0%`, rev(rr.1.2$`95.0%`)), col = paste0(temp.col,13), border = paste0(temp.col,66), lty = 4)
  #
  temp.col = '#0000FF'
  lines(nd$tfd, rr.2.3$Estimate, lty = 1, lwd = 2, col = paste0(temp.col,99))
  # temp.index = c(nd$tfd > min(this.kd.ad.2.3) & nd$tfd < max(this.kd.ad.2.3))   
  # lines(nd$tfd[temp.index], rr.2.3$Estimate[temp.index], lty = 1, lwd = 2, col = paste0(temp.col,99))
  # points(nd$tfd[!temp.index], rr.2.3$Estimate[!temp.index], pch = 20, cex = 0.33, col = paste0(temp.col,99))
  polygon(x = c(nd$tfd, rev(nd$tfd)), y = c(rr.2.3$`5.0%`, rev(rr.2.3$`95.0%`)), col = paste0(temp.col,13), border = paste0(temp.col,66), lty = 4)
  #
  if(code == 'prog'){
    legend('topleft', col = c('#FF0000', '#008800', '#0000FF'), lwd = 2, bty = 'n', legend = c('progression: 0 - 1','progression: 1 - 2','progression: 2 - 3'))
  }
  if(code == 'regr'){
    legend('topleft', col = c('#FF0000', '#008800', '#0000FF'), lwd = 2, bty = 'n', legend = c('regression: 1 - 0','regression: 2 - 1','regression: 3 - 2'))
  }
  #
  mtext(side = 3, adj = 0, cex = 1.1, text = panel.lab)# the of beginning stage
  #
}














# ##  get the rr output
# 
# get.rr = function(sample.data){
#   nd = sample.data
#   scale.factor = 1000
#   upper.lim = 10000000
#   #
#   comb.rate.0.1 <- (ci.pred( mod.comb.0.1, nd, alpha = 0.1))
#   ad.rate.0.1 <- (ci.pred( mod.ad.0.1, nd, alpha = 0.1))
#   rr.0.1 = as.data.frame(ci.ratio( r1 = ad.rate.0.1, r2 = comb.rate.0.1, alpha = 0.1 ))*scale.factor
#   rr.0.1 = narrow.in.range(rr.0.1, upper.lim)
#   #
#   comb.rate.1.2 <- (ci.pred( mod.comb.1.2, nd, alpha = 0.1 ))
#   ad.rate.1.2 <- (ci.pred( mod.ad.1.2, nd, alpha = 0.1 ))
#   rr.1.2 = as.data.frame(ci.ratio( r1 = ad.rate.1.2, r2 = comb.rate.1.2, alpha = 0.1 ))*scale.factor
#   rr.1.2 = narrow.in.range(rr.1.2, upper.lim)
#   #
#   comb.rate.2.3 <- (ci.pred( mod.comb.2.3, nd, alpha = 0.1 ))
#   ad.rate.2.3 <- (ci.pred( mod.ad.2.3, nd, alpha = 0.1 ))
#   rr.2.3 = as.data.frame(ci.ratio( r1 = ad.rate.2.3, r2 = comb.rate.2.3, alpha = 0.1 ))*scale.factor
#   rr.2.3 = narrow.in.range(rr.2.3, upper.lim)
#   #
#   rr.list = list(
#     rr.0.1 = rr.0.1,
#     rr.1.2 = rr.1.2,
#     rr.2.3 = rr.2.3
#   )
#   return(rr.list)
# }
















##  get the risk output

get.risk = function(sample.data, code){
  nd = sample.data
  scale.factor = 1000
  upper.lim = 10000000
  
  if(code == 'prog'){
    this.mod.ad.0.1 = mod.ad.0.1
    this.mod.ad.1.2 = mod.ad.1.2
    this.mod.ad.2.3 = mod.ad.2.3
  }
  if(code == 'regr'){
    this.mod.ad.0.1 = mod.ad.1.0
    this.mod.ad.1.2 = mod.ad.2.1
    this.mod.ad.2.3 = mod.ad.3.2
  }
  
  #
  ad.rate.0.1 <- as.data.frame(ci.pred( this.mod.ad.0.1, nd, alpha = 0.1, Exp = T))*scale.factor
  rr.0.1 = ad.rate.0.1
  rr.0.1 = narrow.in.range(rr.0.1, upper.lim)
  #
  ad.rate.1.2 <- as.data.frame(ci.pred( this.mod.ad.1.2, nd, alpha = 0.1, Exp = T))*scale.factor
  rr.1.2 = ad.rate.1.2
  rr.1.2 = narrow.in.range(rr.1.2, upper.lim)
  #
  ad.rate.2.3 <- as.data.frame(ci.pred( this.mod.ad.2.3, nd, alpha = 0.1, Exp = T))*scale.factor
  rr.2.3 = ad.rate.2.3
  rr.2.3 = narrow.in.range(rr.2.3, upper.lim)
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















