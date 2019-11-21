formula = as.formula(formula)
y = lhs(formula)
if(is.null(lowR)){
  lowR = floor(nrow(data) * .15)
}
if(is.null(highR)){
  highR = ceiling(nrow(data) * .85)
}
# enc_vec1 = rep(NA_real_,highR-lowR)
# enc_vec2 = rep(NA_real_,highR-lowR)
kores = detectCores()-2
cl = makeCluster(kores,type = "FORK")
registerDoParallel(cl)
enc_vec = foreach(Rhat = lowR:highR,.verbose = TRUE,.combine = c) %dopar% {
  pred1 = rep(NA_real_,nrow(data)-Rhat)
  pred2 = rep(NA_real_,nrow(data)-Rhat)
  bench1 = rep(NA_real_,nrow(data)-Rhat)
  bench2 = rep(NA_real_,nrow(data)-Rhat)
  for(d in Rhat:(nrow(data)-1)){
    # expanding
    tmplm1 = lm(formula , data = data[1:(d-1)])
    pred1[d-Rhat+1] = predict(tmplm1,newdata = data[d])
    if(is.null(bench)){
      f = as.formula(paste0(c(y)," ~ 1"))
    } else {
      f = as.formula(paste0(c(y)," ~ ",bench))
    }
    tmplm2 = lm(f, data = data[1:(d-1)])
    bench1[d-Rhat+1] = predict(tmplm2,newdata = data[d])
    # rolling
    tmplm3 = lm(formula , data = data[(d-Rhat+1):(d-1)])
    pred2[d-Rhat+1] = predict(tmplm3,newdata = data[d])
    tmplm4 = lm(f, data = data[(d-Rhat+1):(d-1)])
    bench2[d-Rhat+1] = predict(tmplm4,newdata = data[d])
  }
  hist = data[(Rhat):(nrow(data)-1), eval(y)]
  u1 = (hist-bench1)
  u2 = (hist-pred1)
  u3 = (hist-bench2)
  u4 = (hist-pred2)
  return(c(exp=enc.new(u1,u2),roll=enc.new(u3,u4)))
}
stopCluster(cl)
gc()
enc_vec1 = enc_vec[seq(1,length(enc_vec),by = 2)]
enc_vec2 = enc_vec[seq(2,length(enc_vec),by = 2)]
exp_RET = max(enc_vec1)
exp_AET = (1/(highR-lowR+1))*sum(enc_vec1)
roll_RET = max(enc_vec2)
roll_AET = (1/(highR-lowR+1))*sum(enc_vec2)
k2 = length(rhs(formula)) - max(length(bench),1) + 1
mu = lowR / nrow(data)
return(list(Exp_AET = exp_AET, Exp_RET = exp_RET, Roll_AET = roll_AET, Roll_RET = roll_RET, K2 = k2, MU = mu))
# add pvalues to function
