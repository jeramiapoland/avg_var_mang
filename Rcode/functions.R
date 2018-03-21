out_var = function(x,method,timep = NULL){
  if(length(x) < 33){
    if(grubbs.test(x,type = 20)$p.value <=.05){
      return((IQR(x)/1.55)^2)
    }
  }
  if(method == "var"){
    return(var(x))
  } else {
    return(alt_var(x,timep))
  }
}

alt_var = function(x,timep){
  return((var(x) + (1 + mean(x))^2)^timep - (1 + mean(x))^(2*timep))
}

adj_cor = function(r,nl,method){
  if(method==1){
    out = r * Re(hypergeo::hypergeo(A=(1/2),B=(1/2),C=((nl-1)/2),z = (1-r^2),maxiter = 1000))
  } else if(method==2){
    out = r * Re(hypergeo::f15.3.4(A=(1/2),B=(1/2),C=((nl-1)/2),z = (1-r^2),maxiter = 1000))
  } else {
    out = r * Re(hypergeo::f15.3.1(A=(1/2),B=(1/2),C=((nl-1)/2),z = (1-r^2),maxiter = 1000))  
  }
  return(out)
}

aprox_adj_cor = function(r,nl){
  return(r*(1+(1-r^2)/(2*(nl-3))))
}



c_var_run = function(x,qd_adj){
  if(length(na.omit(x)) >= qd_adj){
    na_idx = is.na(x)
    tmp = runVar(na.omit(x),n = qd_adj)
    out = rep(NA_real_,length(x))
    out[!na_idx] = tmp
    return(out)
  } else {
    rep(NA_real_,length(x))
  }
}

cor_var = function(mtrx) {
  # if(freq=="monthly"){
  #   X = "weight"
  # } else { X = "q_weight"}
  mtrx1 = subset(mtrx,select = c("date","PERMNO","RET"))
  mtrx2 = subset(mtrx,select = c("weight","PERMNO"))
  mtrx3 = subset(mtrx,select = c("date","vwretd.daily"))
  tmp_m = dcast(data = mtrx1, formula = date ~ ..., value.var = "RET")
  tmp_m = as.data.table(tmp_m)
  tmp_m[, date := NULL]
  timep = nrow(tmp_m)
  c_m = cor(tmp_m)
  if(nrow(tmp_m < 33)){
    c_m[] = vapply(c_m, aprox_adj_cor,FUN.VALUE =  numeric(1),nrow(tmp_m))
  }
  m_tr = apply(X = tmp_m,MARGIN = 2,FUN = var)
  # m_tr_alt = apply(X = tmp_m,MARGIN = 2,FUN = out_var,method = "alt_var",timep)
  # m_tr_sd = apply(X = tmp_m,MARGIN = 2,FUN = sd)
  # m_tr_down = apply(X = tmp_m,MARGIN = 2,FUN = DownsideDeviation)
  # m_tr_downp = apply(X = tmp_m,MARGIN = 2,FUN = DownsidePotential)
  weight = unique(mtrx2,by=c("PERMNO"))$weight
  avg_var = (m_tr %*% weight) * timep
  #avg_var_alt = (m_tr_alt %*% weight)
  #avg_sd =  (m_tr_sd %*% weight) * sqrt(timep)
  #avg_down = (m_tr_down %*% weight) * timep
  #avg_downp = (m_tr_downp %*% weight) * sqrt(timep)
  diag(c_m) = 0
  avg_cor = crossprod(weight,crossprod(c_m,weight))
  #avg_cor_ta = avg_cor / (1 - sum(weight^2))
  mkt_r = unique(mtrx3,by="date")$vwretd.daily
  mkt_var = var(mkt_r) * timep
  #mkt_var_alt = alt_var(mkt_r,timep)
  return(c(avg_var,avg_cor,mkt_var))
}

unbiased_lm <- function(X,y,alt_y,lag,reg,tstat,w){
  # following the Amihud and Hurvich 2004 residual inclusion style bias reduction estimator
  # check the dimensions of X and find the residual vector v
  X = as.matrix(X)
  N <- 1000 # number of simulations
  v_matrix <- matrix(data = NA_real_, nrow = nrow(X), ncol = ncol(X))
  phi_matrix_0 <- matrix(data = NA_real_, nrow = ncol(X), ncol = ncol(X))
  I <- diag(ncol(X))
  if(ncol(X) > 1){
    var_model <- ar(X,order.max = lag,var.method = "yule-walker",demean = T)
    for(i in 1:ncol(X)){
      phi_matrix_0[,i] <- as.vector(var_model$ar[1,i,])
    }
    v_matrix <- var_model$resid
    v_matrix[1,] <- rep(0,ncol(X))
    t_phi <- t(phi_matrix_0)
    e_values <- eigen(t_phi)$values
    sigma_lam <- matrix(data = 0,ncol = ncol(X),nrow = ncol(X))
    for(l in 1:length(e_values)){
      sigma_lam <- sigma_lam + e_values[l]*solve((I-e_values[l]*t_phi))
    }
    sigma_v_0 <- cov(v_matrix)
    sigma_x <- cov(X)
    in_bracket <- solve(I-t_phi) + t_phi%*%solve((I-(t_phi%*%t_phi))) +  sigma_lam
    b <- sigma_v_0 %*% in_bracket %*% solve(sigma_x)
    phi_matrix_1 <- phi_matrix_0 + b/nrow(X)
    est_X <- matrix(data = NA_real_, nrow = nrow(X), ncol = ncol(X))
    est_X[1,] <- unlist(X[1,]) 
    est_X[2:nrow(X),] <- as.matrix(X[1:(nrow(X)-1),])%*%phi_matrix_1 + colMeans(X)
    v_matrix_2 <- X - est_X
    for(c in 1:ncol(v_matrix_2)){
      v_matrix_2[,c] <- Imzap(v_matrix_2[,c])
    }
    if(sum(colSums(apply(v_matrix_2,2,Im))) <= .Machine$double.eps){
      v_matrix_2 <- apply(v_matrix_2,2,as.numeric)
    }
    sigma_v_1 <- cov(v_matrix_2)
    t_phi <- t(phi_matrix_1)
    e_values <- eigen(t_phi)$values
    sigma_lam <- matrix(data = 0,ncol = ncol(X),nrow = ncol(X))
    for(l in 1:length(e_values)){
      sigma_lam <- sigma_lam + e_values[l]*solve((I-e_values[l]*t_phi))
    }
    in_bracket <- solve(I-t_phi) + t_phi%*%solve((I-(t_phi%*%t_phi))) +  sigma_lam
    b <- sigma_v_1 %*% in_bracket %*% solve(sigma_x)
    phi_matrix_2 <- phi_matrix_0 + b/nrow(X)
    est_X <- matrix(data = NA_real_, nrow = nrow(X), ncol = ncol(X))
    est_X[1,] <- unlist(X[1,])
    est_X[2:nrow(X),] <- as.matrix(X[1:(nrow(X)-1),])%*%phi_matrix_2 + colMeans(X)
    est_X <- apply(est_X,2,Imzap)
    v_matrix_3 <- X - est_X #- colMeans(X)
    v_matrix_3 <- apply(v_matrix_3,2,Imzap)
    if(sum(colSums(apply(v_matrix_3,2,Im))) <= .Machine$double.eps){
      v_matrix_3 <- apply(v_matrix_3,2,as.numeric)
    }
    aug_X <- cbind(X[1:(nrow(X)-1),],v_matrix_3[2:nrow(X),])
    colnames(aug_X) <- c(colnames(X),paste0("V",1:ncol(X)))
    y_var = y[1:(nrow(X)-1)]
    alt_y_var = alt_y[1:(nrow(X)-1)]
    cor_model <- Z_lm(aug_X,alt_y_var,w)
    cor_betas <- cor_model$coefficients[1:ncol(X)]
    ols_mod <- lm(y_var~aug_X)
    tstat_vec <- matrix(data = NA_real_,nrow = N,ncol = ncol(X))
    ols_res <- ols_mod$residuals
    y_bar <- mean(y_var)
    alt_y_bar <- mean(alt_y_var)
    for(i in 1:N){
      draw <- rnorm(length(ols_res))
      alt_y_hat <- alt_y_bar + (ols_res * draw)
      x_hat <- est_X[1:(nrow(X)-1),] + (v_matrix_3[1:(nrow(X)-1),]*draw)
      temp_lm <- Z_lm(x_hat,alt_y_hat,w)
      tstat_vec[i,] <- temp_lm$tstats
    }
    #not general revist for improvement later #
    if(reg %in% c(1,2)){
      mc_p <- sum(tstat_vec[,1] <= tstat[1])/N
      dp_p <- sum(tstat_vec[,2] >= tstat[2])/N
    } else if (reg %in% c(4,5,6)){
      mc_p <- sum(tstat_vec[,1] <= tstat[1])/N
      dp_p <- sum(tstat_vec[,2] <= tstat[2])/N
    } else {
      mc_p <- sum(tstat_vec[,1] >= tstat[1])/N
      dp_p <- sum(tstat_vec[,2] >= tstat[2])/N
    }
    names(mc_p) <- "MC_pval"
    names(dp_p) <- "DP_pval"
    outp <- c(cor_betas,tstat,mc_p,dp_p)
  } else {
    var_model <- ar(X,order.max = lag,var.method = "ols",aic=FALSE)
    phi_matrix_0 <- as.vector(var_model$ar)
    phi_matrix_1 <- phi_matrix_0 + (1+3*phi_matrix_0)/nrow(X)
    phi_matrix_2 <- phi_matrix_0 + (1+3*phi_matrix_1)/nrow(X)
    est_X <- matrix(data = NA_real_, nrow = nrow(X), ncol = ncol(X))
    est_X[1,] = X[1,]
    est_X[2:nrow(X),] <- as.matrix(X[1:(nrow(X)-1),])%*%phi_matrix_2 + colMeans(X)
    v_matrix <- X - est_X
    aug_X <- cbind(X[1:(nrow(X)-1)],v_matrix[2:nrow(X)])
    colnames(aug_X) <- c(colnames(X),paste0("V",1:ncol(X)))
    y_var = y[1:(nrow(X)-1)]
    alt_y_var = alt_y[1:(nrow(X)-1)]
    cor_model <- Z_lm(aug_X,alt_y_var,w)
    cor_betas <- cor_model$coefficients[1:ncol(X)]
    names(cor_betas) <- colnames(X)
    ols_mod <- lm(y_var~aug_X)
    tstat_vec <- rep(NA_real_,N)
    ols_res <- ols_mod$residuals
    y_bar <- mean(y_var)
    alt_y_bar <- mean(alt_y_var)
    for(i in 1:N){
      draw <- rnorm(length(ols_res))
      alt_y_hat <- alt_y_bar + (ols_res * draw)
      x_hat <- est_X[1:(nrow(X)-1)] + v_matrix[1:(nrow(X)-1)] * draw
      temp_lm <- Z_lm(x_hat,alt_y_hat,w)
      tstat_vec[i] <- temp_lm$tstats
    }
    # not general revist for improvement later #
    if(reg ==1){
      mc_p <- sum(tstat_vec <= tstat[1])/N
    } else {
      mc_p <- sum(tstat_vec >= tstat[1])/N
    }
    names(mc_p) <- paste0(colnames(X),"_pval")
    outp <- c(cor_betas,tstat,mc_p)
  }
  return(outp)
}

Z_lm <- function(X,y,w){
  X = as.matrix(X)
  output <- list("coefficients" = NULL, "tstats" = NULL, "r.squared" = NULL)
  new_X <- apply(X,2,runSum,w)
  new_X <- na.omit(new_X)
  X = X[(w+1):nrow(X),]
  Z <- w * new_X %*% ginv(t(new_X) %*% new_X) %*% t(X) %*% X
  y_var <- tail(y,nrow(Z))
  Z <- data.table(y_var,Z)
  lm_Z_y <- lm(y_var ~ ., data = Z)
  lm_Z_coef <- coeftest(lm_Z_y,vcov=NeweyWest(lm_Z_y,prewhite = FALSE,lag=w))
  output$coefficients <- tail(lm_Z_coef[,"Estimate"],-1)
  output$tstats <- tail(lm_Z_coef[,"t value"],-1)
  output$r.squared <- summary(lm_Z_y)$r.squared
  return(output)
}

add.significance.stars2 <- function (p) 
{
  if (inherits(p, c("matrix", "data.frame")) && length(dim(p)) == 
      2) {
    apply(p, c(1, 2), add.significance.stars2)
  }
  else {
    if (length(p) > 1) {
      sapply(p, add.significance.stars2)
    }
    else {
      ifelse(p > 0.1,"",ifelse(p > 0.05, "*", ifelse(p > 0.01, "**", "***")))
    }
  }
}
see.stars <- function(tbl,bees=NULL,pees=NULL){
  if(!length(bees)==length(pees)){stop("need one beta column for each p value colunm")
  } else {
    for(b in 1:length(bees)){
      tpm = paste0(tbl[,bees[b]],add.significance.stars2(as.numeric(tbl[,pees[b]])))
      tbl[,bees[b]] = matrix(tpm,ncol = 1)
    }
  }
  return(tbl)
}
see.stars.tstats <- function(tbl,bees=NULL,tees=NULL,side="one",df=NULL,exp.positive=FALSE){
  if(!length(bees)==length(tees)){stop("need one beta column for each t stat colunm")
  } else if(is.null(df)){stop("must provide degrees of freedom")
  } else if(!side%in%c("one","two")){stop("side must either be 'one' or 'two'")
  } else {
    if(length(df)==1){df = rep(df,length(bees))}
    for(b in 1:length(bees)){
      # print(paste0("b is ",b))
      teesb = as.numeric(tbl[,tees[b]])
      if(side=="two"){s = 2} else{s=1}
      # print(paste0("s is ",s))
      if(exp.positive){
        s = 1
        pees = 1 - s*pt(teesb,df[b])
      } else{pees = s*pt(-abs(teesb),df[b])}
      # print("P values are ")
      # print(pees)
      tpm = paste0(tbl[,bees[b]],add.significance.stars2(pees))
      tbl[,bees[b]] = matrix(tpm,ncol = 1)
      # print(tbl)
    }
  }
  return(tbl)
}

enc.new = function(u1,u2){
  P = length(u1)
  return(P * ( sum(u1^2-u1*u2) / sum(u2^2)))
}

enc.t = function(u1,u2){
  P = length(u1)
  c = u1^2 - u1*u2
  cbar = (1/P) * sum(c)
  den = sqrt((1/P)* sum((c-cbar)^2))
  return(sqrt(P-1)*(cbar/den))
}

enc.hln = function(u1,u2){
  n = length(u1)
  u_hi3 = u1
  u_i = u2
  U = u_hi3 - u_i
  hln = lm(u_hi3 ~ 0 + U)
  lam = max(0,min(hln$coefficients[1],1,na.rm=T),na.rm=T)
  d_til = U * u_hi3
  n = length(d_til)
  d_bar = mean(d_til,na.rm=T)
  Hvect = 0
  f = vector(length = length(Hvect))
  for(j in 1:length(Hvect)){
    tau = Hvect[j]
    f[j] = sum((d_til[(abs(tau)+1):n]-d_bar)*(d_til[1:(n-abs(tau))]-d_bar))
  }
  Q2 = ((n)^-1)*sum(f)
  R = sqrt(n)*(Q2^(-.5))*d_bar
  HLN_mod = (n^(-.5))*(n+1-(2))^.5
  stat = HLN_mod * R
  return(c(lambda = lam, t.stat = stat))
}

enc.cvs = fread(input = 'encCV.csv',header = TRUE)
enc.cvs = melt(enc.cvs,id.vars = c("k2","p"),variable.name = "pi",value.name = "stat")
enc.cvs[, pi := as.numeric(as.character(pi))]
setkey(enc.cvs,k2,pi)


enc.new_plookup = function(encstat,df,mu){
  return(min(1 - enc.cvs[k2==df & pi >= min((1-mu)/mu,5) & encstat >= stat,]$p,1))
}

oos.R2 = function(u1,u2){
  return(1 - mean(u2^2)/mean(u1^2))
}

oos.tstat = function(u1,u2){
  d_til = u1^2 - u2^2 + (u1 - u2)^2
  sig_model <- lm(formula = d_til ~ 1)
  correct_sig <- coeftest(sig_model,vcov. = vcovHAC(sig_model))
  return(correct_sig[3])
}

mse.t = function(u1,u2){
  P = length(u1)
  dhat = u1^2 - u2^2
  dbar = (1/P) * sum(dhat)
  sdd = ar(sqrt(P)*dhat,order.max = 1)$asy.var.coef
  if(is.null(sdd)){ sdd = var(sqrt(P)*dhat)}
  return(sqrt(P)*(dbar/sqrt(sdd)))
}

mse.f = function(u1,u2){
  P = length(u1)
  dhat = u1^2 - u2^2
  dbar = sum(dhat)
  return(P * (dbar/sum(u2^2)))
  
}

lm.oos = function(formula = NULL, data = NULL, bench = NULL,train = NULL){
  formula = as.formula(formula)
  y = lhs(formula)
  end = as.integer(str_extract(y,pattern = "[:digit:]+"))
  nr = nrow(data) - end
  if(is.null(train)){
    train = floor(.15*nr)
  }
  pred = rep(NA_real_,nr-(train-1))
  benchmark = rep(NA_real_,nr-(train-1))
  for(n in train:nr){
    tmp_lm = lm(formula,data[1:(n-1)])
    pred[n-(train-1)] = predict(tmp_lm,data[n])
    if(is.null(bench)){
      f = as.formula(paste0(c(y)," ~ 1"))
      ben_lm = lm(f,data[1:(n-1)])
    } else {
      f = as.formula(paste0(c(y)," ~ ",bench))
      ben_lm = lm(f, data[1:(n-1)])
    }
    benchmark[n-(train-1)] = predict(ben_lm,data[n])
  }
  hist = unlist(data[train:(nr), eval(y)])
  e1 = hist - benchmark
  e2 = hist - pred
  k2 = length(rhs(formula)) - max(length(bench),1) + 1
  mu = (nrow(data) - train) / train 
  oosR2.stat = oos.R2(e1,e2)
  oosR2.tstat = oos.tstat(e1,e2)
  oosR = see.stars.tstats(tbl = round(as.matrix(data.table(oosR2.stat*100,oosR2.tstat)),3),bees = 1,tees = 2,
                          exp.positive = TRUE,df = length(e1) - k2)[1]
  enc_new = enc.new(e1,e2)
  enc_new.pvalue = enc.new_plookup(enc_new,k2,mu)
  encnew = see.stars(tbl = round(as.matrix(data.table(enc_new,enc_new.pvalue)),3),bees = 1,pees = 2)[1]
  dmtest = dm.test(e1,e2,alternative = "greater",h = 1,power = 2)
  # dm.stat = dmtest$statistic
  # dm.pval = dmtest$p.value
  # dmstat = see.stars(tbl = round(as.matrix(data.table(dm.stat,dm.pval)),3),bees = 1,pees = 2)[1]
  ENCHLN = enc.hln(e1,e2)
  enchln = see.stars.tstats(tbl = round(matrix(ENCHLN,nrow=1),3),bees = 1,tees = 2,exp.positive = TRUE,df = length(e1)-k2)[1]
  MSEF = mse.f(e1,e2)
  return(list(forecast_values = pred, K2 = k2,MU = mu, formula = formula, bench = bench, bench_values = benchmark, hist_values = hist,
              Statistics = c(oosR,MSEF,encnew,enchln)))
}

### Rossi Robust Stats ###
# clark and mccraken 2001 ENC-NEW
enc.newRobust = function(formula = NULL, data = NULL, bench = NULL, lowR = NULL, highR = NULL){
  # formula = as.formula(formula)
  # y = lhs(formula)
  # if(is.null(lowR)){
  #   lowR = floor(nrow(data) * .15)
  # }
  # if(is.null(highR)){
  #   highR = ceiling(nrow(data) * .85)
  # }
  # enc_vec1 = rep(NA_real_,highR-lowR)
  # enc_vec2 = rep(NA_real_,highR-lowR)
  # for(Rhat in lowR:highR){
  #   pred1 = rep(NA_real_,nrow(data)-Rhat)
  #   pred2 = rep(NA_real_,nrow(data)-Rhat)
  #   bench1 = rep(NA_real_,nrow(data)-Rhat)
  #   bench2 = rep(NA_real_,nrow(data)-Rhat)
  #   for(d in Rhat:(nrow(data)-1)){
  #     # expanding
  #     tmplm1 = lm(formula , data = data[1:(d-1)])
  #     pred1[d-Rhat+1] = predict(tmplm1,newdata = data[d])
  #     if(is.null(bench)){
  #       f = as.formula(paste0(c(y)," ~ 1"))
  #     } else {
  #       f = as.formula(paste0(c(y)," ~ ",bench))
  #       }
  #     tmplm2 = lm(f, data = data[1:(d-1)])
  #     bench1[d-Rhat+1] = predict(tmplm2,newdata = data[d])
  #     # rolling
  #     tmplm3 = lm(formula , data = data[(d-Rhat+1):(d-1)])
  #     pred2[d-Rhat+1] = predict(tmplm3,newdata = data[d])
  #     tmplm4 = lm(f, data = data[(d-Rhat+1):(d-1)])
  #     bench2[d-Rhat+1] = predict(tmplm4,newdata = data[d])
  #   }
  #   hist = data[(Rhat):(nrow(data)-1), eval(y)]
  #   u1 = (hist-bench1)
  #   u2 = (hist-pred1)
  #   enc_vec1[Rhat-lowR+1] = enc.new(u1,u2)
  #   u3 = (hist-bench2)
  #   u4 = (hist-pred2)
  #   enc_vec2[Rhat-lowR+1] = enc.new(u3,u4)
  # }
  # exp_RET = max(enc_vec1)
  # exp_AET = (1/(highR-lowR+1))*sum(enc_vec1)
  # roll_RET = max(enc_vec2)
  # roll_AET = (1/(highR-lowR+1))*sum(enc_vec2)
  # k2 = length(rhs(formula)) - max(length(bench),1) + 1
  # mu = lowR / nrow(data)
  # return(list(Exp_AET = exp_AET, Exp_RET = exp_RET, Roll_AET = roll_AET, Roll_RET = roll_RET, K2 = k2, MU = mu))
  # # add pvalues to function
  
  # parallel version
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
  mcoptions <- list(preschedule=FALSE, set.seed=FALSE)
  enc_vec = foreach(Rhat = lowR:highR,.combine = c,.options.multicore=mcoptions,.inorder = FALSE) %dopar% {
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
  enc_vec1 = enc_vec[names(enc_vec)=="exp"]
  enc_vec2 = enc_vec[names(enc_vec)=="roll"]
  exp_RET = max(enc_vec1)
  exp_AET = (1/(highR-lowR+1))*sum(enc_vec1)
  roll_RET = max(enc_vec2)
  roll_AET = (1/(highR-lowR+1))*sum(enc_vec2)
  k2 = length(rhs(formula)) - max(length(bench),1) + 1
  mu = (nrow(data) - lowR) / lowR
  return(list(Exp_AET = exp_AET, Exp_RET = exp_RET, Roll_AET = roll_AET, Roll_RET = roll_RET, K2 = k2, MU = mu))
  # add pvalues to function
}

# clark and west 2007 t-stat oosR2 
mspe.adj = function(e1,e2,y1,y2){
  return(e1^2-e2^2-(y1^2-y2^2))
}

mspe.adjRobust = function(formula = NULL, data = NULL, bench = NULL, lowR = NULL, highR = NULL){
  formula = as.formula(formula)
  y = lhs(formula)
  if(is.null(lowR)){
    lowR = floor(nrow(data) * .15)
  }
  if(is.null(highR)){
    highR = ceiling(nrow(data) * .85)
  }
  # mspe_vec1 = rep(NA_real_,highR-lowR)
  # mspe_vec2 = rep(NA_real_,highR-lowR)
  # for(Rhat in lowR:highR){
  #   pred1 = rep(NA_real_,nrow(data)-Rhat-1)
  #   pred2 = rep(NA_real_,nrow(data)-Rhat-1)
  #   bench1 = rep(NA_real_,nrow(data)-Rhat-1)
  #   bench2 = rep(NA_real_,nrow(data)-Rhat-1)
  #   for(d in Rhat:(nrow(data)-1)){
  #     # expanding
  #     tmplm1 = lm(formula , data = data[1:(d-1)])
  #     pred1[d-Rhat+1] = predict(tmplm1,newdata = data[d])
  #     if(is.null(bench)){
  #       f = as.formula(paste0(c(y)," ~ 1"))
  #     } else {
  #       f = as.formula(paste0(c(y)," ~ ",bench))
  #     }
  #     tmplm2 = lm(f, data = data[1:(d-1)])
  #     bench1[d-Rhat+1] = predict(tmplm2,newdata = data[d])
  #     # rolling
  #     tmplm3 = lm(formula , data = data[(d-Rhat+1):(d-1)])
  #     pred2[d-Rhat+1] = predict(tmplm3,newdata = data[d])
  #     tmplm4 = lm(f, data = data[(d-Rhat+1):(d-1)])
  #     bench2[d-Rhat+1] = predict(tmplm4,newdata = data[d])
  #   }
  #   hist = data[(Rhat+1):(nrow(data)), get(y)]
  #   u1 = (hist-bench1)
  #   u2 = (hist-pred1)
  #   mspe_vec1[Rhat-lowR+1] = mspe.adj(u1,u2)
  #   u3 = (hist-bench2)
  #   u4 = (hist-pred2)
  #   mspe_vec2[Rhat-lowR+1] = mspe.adj(u3,u4)
  # }
  kores = detectCores()-2
  cl = makeCluster(kores,type = "FORK")
  registerDoParallel(cl)
  mcoptions <- list(preschedule=FALSE, set.seed=FALSE)
  mspe_vec = foreach(Rhat = lowR:highR,.combine = c,.options.multicore=mcoptions,.inorder = FALSE,.verbose=TRUE) %dopar% {
    pred1 = rep(NA_real_,nrow(data)-Rhat-1)
    pred2 = rep(NA_real_,nrow(data)-Rhat-1)
    bench1 = rep(NA_real_,nrow(data)-Rhat-1)
    bench2 = rep(NA_real_,nrow(data)-Rhat-1)
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
    v1 = mspe.adj(u1,u2,bench1,pred1)
    v2 = mspe.adj(u3,u4,bench2,pred2)
    P = length(v1)
    return(c(exp = sqrt(P)*sum(v1) ,roll = sqrt(P)*sum(v2))) 
  }
  stopCluster(cl)
  gc()
  mspe_vec1 = mspe_vec[names(mspe_vec)=="exp"]
  mspe_vec2 = mspe_vec[names(mspe_vec)=="roll"]
  df = highR - lowR + 1
  omega1 = sum(ar(mspe_vec1)$asy.var.coef)
  omega2 = sum(ar(mspe_vec2)$asy.var.coef)
  # stat1 = t(mspe_vec1) * (1/omega1) %*% mspe_vec1
  stat1 = sum(mspe_vec1^2*(1/omega1))
  # stat2 = t(mspe_vec2) * (1/omega2) %*% mspe_vec2
  stat2 = sum(mspe_vec1^2*(1/omega2))
  stat1 = see.stars(round(as.matrix(data.table(stat1*100,pchisq(stat1, df))),3),bees = 1,pees = 2)[1]
  stat2 = see.stars(round(as.matrix(data.table(stat2*100,pchisq(stat2, df))),3),bees = 1,pees = 2)[1]
  return(list(exp_CWT = stat1, roll_CWT = stat2))
}

# Diebold and Mariano 1995 robust
dm.test.Robust = function(formula = NULL, data = NULL, bench = NULL, lowR = NULL, highR = NULL){
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
  # for(Rhat in lowR:highR){
  #   pred1 = rep(NA_real_,nrow(data)-Rhat)
  #   pred2 = rep(NA_real_,nrow(data)-Rhat)
  #   bench1 = rep(NA_real_,nrow(data)-Rhat)
  #   bench2 = rep(NA_real_,nrow(data)-Rhat)
  #   for(d in Rhat:(nrow(data)-1)){
  #     # expanding
  #     tmplm1 = lm(formula , data = data[1:(d-1)])
  #     pred1[d-Rhat+1] = predict(tmplm1,newdata = data[d])
  #     if(is.null(bench)){
  #       f = as.formula(paste0(c(y)," ~ 1"))
  #     } else {
  #       f = as.formula(paste0(c(y)," ~ ",bench))
  #     }
  #     tmplm2 = lm(f, data = data[1:(d-1)])
  #     bench1[d-Rhat+1] = predict(tmplm2,newdata = data[d])
  #     # rolling
  #     tmplm3 = lm(formula , data = data[(d-Rhat+1):(d-1)])
  #     pred2[d-Rhat+1] = predict(tmplm3,newdata = data[d])
  #     tmplm4 = lm(f, data = data[(d-Rhat+1):(d-1)])
  #     bench2[d-Rhat+1] = predict(tmplm4,newdata = data[d])
  #   }
  #   hist = data[(Rhat):(nrow(data)-1), eval(y)]
  #   u1 = (hist-bench1)
  #   u2 = (hist-pred1)
  #   enc_vec1[Rhat-lowR+1] = dm.test(u1,u2,h = 1,power = 2)$statistic
  #   u3 = (hist-bench2)
  #   u4 = (hist-pred2)
  #   enc_vec2[Rhat-lowR+1] = dm.test(u3,u4,h = 1,power = 2)$statistic
  # }
  kores = detectCores()-2
  cl = makeCluster(kores,type = "FORK")
  registerDoParallel(cl)
  mcoptions <- list(preschedule=FALSE, set.seed=FALSE)
  foreach(Rhat = lowR:highR,.combine = c,.options.multicore=mcoptions,.inorder = FALSE) %dopar% {
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
    return(c(exp=dm.test(u1,u2,h = 1,power = 2)$statistic,roll=dm.test(u3,u4,h = 1,power = 2)$statistic))
  }
  stopCluster(cl)
  gc()
  enc_vec1 = enc_vec[names(enc_vec)=="exp"]
  enc_vec2 = enc_vec[names(enc_vec)=="roll"]
  exp_RET = max(abs(enc_vec1))
  exp_AET = (1/(highR-lowR+1))*sum(abs(enc_vec1))
  roll_RET = max(abs(enc_vec2))
  roll_AET = (1/(highR-lowR+1))*sum(abs(enc_vec2))
  mu = (nrow(data) - lowR) / lowR
  return(list(Exp_AET = exp_AET, Exp_RET = exp_RET, Roll_AET = roll_AET, Roll_RET = roll_RET,MU = mu))
  # add pvalues to function
}

# Diebold and Mariano 1995 robust
enc.hln.Robust = function(formula = NULL, data = NULL, bench = NULL, lowR = NULL, highR = NULL){
  formula = as.formula(formula)
  y = lhs(formula)
  if(is.null(lowR)){
    lowR = floor(nrow(data) * .15)
  }
  if(is.null(highR)){
    highR = ceiling(nrow(data) * .85)
  }
  kores = detectCores()-2
  cl = makeCluster(kores,type = "FORK")
  registerDoParallel(cl)
  mcoptions <- list(preschedule=FALSE, set.seed=FALSE)
  enc_vec = foreach(Rhat = lowR:highR,.combine = c,.options.multicore=mcoptions,.inorder = FALSE) %dopar% {
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
    
    n = length(u1)
    u_hi3 = u1
    u_i = u2
    U = u_hi3 - u_i
    hln = lm(u_hi3 ~ 0 + U)
    #lam = max(0,min(hln$coefficients[1],1,na.rm=T),na.rm=T)
    d_til = U * u_hi3
    n = length(d_til)
    d_bar = mean(d_til,na.rm=T)
    Hvect = 0
    f = vector(length = length(Hvect))
    for(j in 1:length(Hvect)){
      tau = Hvect[j]
      f[j] = sum((d_til[(abs(tau)+1):n]-d_bar)*(d_til[1:(n-abs(tau))]-d_bar))
    }
    Q2 = ((n)^-1)*sum(f)
    #R = sqrt(n)*(Q2^(-.5))*d_bar
    #HLN_mod = (n^(-.5))*(n+1-(2))^.5
    #stat = HLN_mod * R
    WT1 = n * (d_til * (1/Q2) * d_til)
    
    n = length(u3)
    u_hi3 = u3
    u_i = u4
    U = u_hi3 - u_i
    hln = lm(u_hi3 ~ 0 + U)
    #lam = max(0,min(hln$coefficients[1],1,na.rm=T),na.rm=T)
    d_til = U * u_hi3
    n = length(d_til)
    d_bar = mean(d_til,na.rm=T)
    Hvect = 0
    f = vector(length = length(Hvect))
    for(j in 1:length(Hvect)){
      tau = Hvect[j]
      f[j] = sum((d_til[(abs(tau)+1):n]-d_bar)*(d_til[1:(n-abs(tau))]-d_bar))
    }
    Q2 = ((n)^-1)*sum(f)
    #R = sqrt(n)*(Q2^(-.5))*d_bar
    #HLN_mod = (n^(-.5))*(n+1-(2))^.5
    #stat = HLN_mod * R
    WT2 = n * (d_til * (1/Q2) * d_til)
    
    return(c(exp=WT1,roll=WT2))
  }
  stopCluster(cl)
  gc()
  enc_vec1 = enc_vec[names(enc_vec)=="exp"]
  enc_vec2 = enc_vec[names(enc_vec)=="roll"]
  exp_RET = max(abs(enc_vec1))
  exp_AET = (1/(highR-lowR+1))*sum((enc_vec1))
  roll_RET = max(abs(enc_vec2))
  roll_AET = (1/(highR-lowR+1))*sum((enc_vec2))
  mu = (nrow(data) - lowR) / lowR
  return(list(Exp_AET = exp_AET, Exp_RET = exp_RET, Roll_AET = roll_AET, Roll_RET = roll_RET,MU = mu))
  # add pvalues to function
}

get_ac = function(x,lag=NULL){
  return(ar(x,order.max = lag)$ar)
}

oos_wraper = function(bench,estm,hist,subs){
  e1 = hist - bench
  e2 = hist - estm
  e1 = e1[subs]
  e2 = e2[subs]
  oosR2.stat = oos.R2(e1,e2)
  oosR2.tstat = oos.tstat(e1,e2)
  oosR = see.stars.tstats(tbl = round(as.matrix(data.table(oosR2.stat*100,oosR2.tstat)),3),bees = 1,tees = 2,
                          exp.positive = TRUE,df = length(e1) - 1)[1]
  return(oosR)
}

encnew_wraper = function(bench,estm,hist,subs){
  e1 = hist - bench
  e2 = hist - estm
  e1 = e1[subs]
  e2 = e2[subs]
  enc_new = enc.new(e1,e2)
  enc_new.pvalue = enc.new_plookup(enc_new,1,(.85/.15))
  encnew = see.stars(tbl = round(as.matrix(data.table(enc_new,enc_new.pvalue)),3),bees = 1,pees = 2)[1]
  return(encnew)
}

msef_wraper = function(bench,estm,hist,subs){
  e1 = hist - bench
  e2 = hist - estm
  e1 = e1[subs]
  e2 = e2[subs]
  return(mse.f(e1,e2))
}

enchln_wraper = function(bench,estm,hist,subs){
  e1 = hist - bench
  e2 = hist - estm
  e1 = e1[subs]
  e2 = e2[subs]
  ENCHLN = enc.hln(e1,e2)
  enchln = see.stars.tstats(tbl = round(matrix(ENCHLN,nrow=1),3),bees = 1,tees = 2,exp.positive = TRUE,df = length(e1)-1)[1]
}

sortinoR = function(R,annualize = FALSE, freq = c("monthly","quarterly")){
  if(annualize){
    if(freq == "monthly"){
      R = R * 12
    } else {R = R * 4}
  }
  return(SortinoRatio(R))
}

ratio.test = function(rets1,rets2,n_samples = 1000,ratio = c("sortinoR","Kappa0","UpsidePotentialRatio","genRachev.ratio")){
  ratios = rep(NA_real_,length(rets1))
  ratios2 = rep(NA_real_,length(rets2))
  for(n in 1:n_samples){
    r1 = sample(rets1,replace = TRUE,size = length(adj_m_av_returns))
    r2 = sample(rets2,replace = TRUE,size = length(adj_m_vol_returns))
    ratios[n] = do.call(what = ratio,args = list(R = r1))
    ratios2[n] = do.call(what = ratio,args = list(R = r2))
  }
  np_dt1 = data.table(sr = c(ratios,ratios2), strat = as.factor(c(rep("r1",length(ratios)),rep("r2",length(ratios2)))))
  ktest = kruskal.test(formula = sr ~ strat, data = np_dt1)
  same = (ktest$p.value >= .1)
  if(!same){
    w = wilcox.test(ratios,ratios2,alternative = "less")$p.value
  } else {w = NULL}
  return(c("Ratio" = ratio, "k_p.value" = ktest$p.value, "w_p.value" = w))
}

genRachev.ratio = function(R){
  etl1 = ETL(R,method = "historical")
  etl2 = ETL(-1*R,method = "historical")
  return(c(genRachev = etl2/etl1))
}

Kappa0 = function(R){
  return(Kappa(R,0,4))
}

Sratio = function(x,annualize=FALSE,freq = NULL){
  if(annualize){s = sqrt(freq)} else {s = 1}
  return(mean(x,na.rm = TRUE)/sd(x,na.rm = TRUE) * s)
}

Sratio_diff = function(x){
  if(!is.matrix(x)){stop("x must be a matrix with two columns of return data")}
  s1 = Sratio(x[,1])
  s2 = Sratio(x[,2])
  return(s2-s1)
}

Sratio_diff_CI = function(x,l){
  vstar = c(mean(x[,1]),mean(x[,2],sd(x[,1]),sd(x[,2])))
  gv = grad_tic(vstar)
  l_b = floor(nrow(x)/l) 
  ystar = rep(list(vector("numeric",4)),nrow(x))
  zetaj = rep(list(vector("numeric",4)),l_b)
  Zeta = matrix(nrow = 4,ncol = 4)
  for(t in 1:nrow(x)){
    dm1 = x[t,1] - mean(x[,1])
    dm2 = x[t,2] - mean(x[,2])
    dv1 = x[t,1]^2 - var(x[,1])
    dv2 = x[t,2]^2 - var(x[,2])
    ystar[[t]] = c(dm1,dm2,dv1,dv2)
  }
  for(j in 1:l_b){
    zetaj[[t]] = (1/sqrt(l)) * reduce('+',ystar[((j-1)*l+1):((j-1)*l+l)])
    Zeta = Zeta + crossprod(t(zetaj[j]))
  }
  PSIstar = (1/l_b) * Zeta
  
  se = sqrt(gv%*%PSIstar%*%t(gv) / nrow(x))
  return(se)
}

grad_tic = function(x){
  if(!length(x)==4){stop("x must be a numeric vector of length 4")}
  g1 = x[3] / (x[3] - x[1]^2)^1.5
  g2 = x[4] / (x[4] - x[2]^2)^1.5
  g3 = -1*.5*(x[1] / (x[1] - x[3]^2)^1.5)
  g4 = -1*.5*(x[2] / (x[2] - x[4]^2)^1.5)
  return(c(g1,g2,g3,g4))
}

# boot_sr_diff = function(x,nsim = 1000, block = 12){
#   tmpx = matrix(nrow = nrow(x),ncol = 2)
#   nt = ceiling(nrow(x)/block)
#   SRd = rep(NA_real_,nsim)
#   SEd = rep(NA_real_,nsim)
#   for(n in 1:nsim){
#     for(t in 1:nt){
#       st = sample(1:nrow(x),1)
#       tmpx[t:(t+block-1)] = x[st:st+(block-1),2:3]
#     }
#     tmpx = head(tmpx,nrow(x))
#     SRd[n] = Sratio_diff(tmpx[,1],tmpx[,2])
#     
#   }
#   # if(!is.matrix(x)){stop("x must be a matrix")}
#   # if(!is.numeric(x)){stop("x must be numeric")}
#   # if(!ncol(x) == 3){stop("x must have two columns")}
#   # sr_dff_ts = apply(x[,2:3],MARGIN = 1,FUN = Sratio_diff)
#   
# }