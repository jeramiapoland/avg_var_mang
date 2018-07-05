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

unbiased_lm <- function(X,y,alt_y,lag,expSig,tstat,w){
  # following the Amihud and Hurvich 2004 residual inclusion style bias reduction estimator
  # check the dimensions of X and find the residual vector v
  X = as.matrix(X)
  X = X[!is.na(y),]
  alt_y = alt_y[!is.na(y)]
  y = na.omit(y)
  N <- 10000 # number of simulations
  pv = vector("numeric",length(expSig))
  names(pv) = colnames(X)
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
    for(i in 1:length(expSig)){
      if(expSig[i] > 0){
        pv[i] = sum(tstat_vec[, i] >= tstat[i])/N
      } else {
        pv[i] = sum(tstat_vec[, i] <= tstat[i])/N
      }
    }
    # if(reg %in% c(1,2)){
    #   mc_p <- sum(tstat_vec[,1] <= tstat[1])/N
    #   dp_p <- sum(tstat_vec[,2] >= tstat[2])/N
    # } else if (reg %in% c(4,5,6)){
    #   mc_p <- sum(tstat_vec[,1] <= tstat[1])/N
    #   dp_p <- sum(tstat_vec[,2] <= tstat[2])/N
    # } else {
    #   mc_p <- sum(tstat_vec[,1] >= tstat[1])/N
    #   dp_p <- sum(tstat_vec[,2] >= tstat[2])/N
    # }
    # names(mc_p) <- "MC_pval"
    # names(dp_p) <- "DP_pval"
    outp <- c(cor_betas,tstat,pv)
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
    for(i in 1:length(expSig)){
      if(expSig[i] > 0){
        pv[i] = sum(tstat_vec[, i] >= tstat[i])/N
      } else {
        pv[i] = sum(tstat_vec[, i] <= tstat[i])/N
      }
    }
    outp <- c(cor_betas,tstat,pv)
  }
  return(outp)
}

unbiased_lm2 = function(X,expSig,N = 10000,lag=NULL,aic=TRUE){
  # X = lm_object$model
  x_names = tail(names(X),-1)
  y_name = names(X)[1]
  Y = X[,1, drop = FALSE]
  X = X[,2:ncol(X), drop = FALSE]
  pv = vector("numeric",length(expSig))
  names(pv) = colnames(X)
  v_matrix = matrix(data = NA_real_, nrow = nrow(X), ncol = ncol(X))
  phi_matrix_0 = matrix(data = NA_real_, nrow = ncol(X), ncol = ncol(X))
  I = diag(ncol(X))
  var_model = ar(X,aic,order.max = lag,var.method = "yule-walker",demean = T)
  if(ncol(X)==1){
    phi_matrix_0[,i] = as.vector(var_model$ar[1])
  } else {
    for(i in 1:ncol(X)){
      phi_matrix_0[,i] = as.vector(var_model$ar[1,i,])
    }
  }
  v_matrix = as.matrix(var_model$resid)
  v_matrix[1,] = rep(0,ncol(X))
  t_phi = t(phi_matrix_0)
  e_values = eigen(t_phi)$values
  sigma_lam = matrix(data = 0,ncol = ncol(X),nrow = ncol(X))
  for(l in 1:length(e_values)){
    sigma_lam = sigma_lam + e_values[l]*solve((I-e_values[l]*t_phi))
  }
  sigma_v_0 = cov(v_matrix)
  sigma_x = cov(X)
  in_bracket = solve(I-t_phi) + t_phi%*%solve((I-(t_phi%*%t_phi))) +  sigma_lam
  b = sigma_v_0 %*% in_bracket %*% solve(sigma_x)
  phi_matrix_1 = phi_matrix_0 + b/nrow(X)
  est_X = matrix(data = NA_real_, nrow = nrow(X), ncol = ncol(X))
  est_X[1,] = as.matrix(X[1,,drop=FALSE])
  est_X[2:nrow(X),] = as.matrix(X[1:(nrow(X)-1),,drop=FALSE])%*%phi_matrix_1 + colMeans(X)
  v_matrix_2 = X - est_X
  for(c in 1:ncol(v_matrix_2)){
    v_matrix_2[,c] = Imzap(v_matrix_2[,c])
  }
  if(sum(colSums(apply(v_matrix_2,2,Im))) <= .Machine$double.eps){
    v_matrix_2 = apply(v_matrix_2,2,as.numeric)
  }
  # sigma_v_1 = cov(v_matrix_2)
  # t_phi = t(phi_matrix_1)
  # e_values = eigen(t_phi)$values
  # sigma_lam = matrix(data = 0,ncol = ncol(X),nrow = ncol(X))
  # for(l in 1:length(e_values)){
  #   sigma_lam = sigma_lam + e_values[l]*solve((I-e_values[l]*t_phi))
  # }
  # in_bracket = solve(I-t_phi) + t_phi%*%solve((I-(t_phi%*%t_phi))) +  sigma_lam
  # b = sigma_v_1 %*% in_bracket %*% solve(sigma_x)
  # phi_matrix_2 = phi_matrix_0 + b/nrow(X)
  # est_X = matrix(data = NA_real_, nrow = nrow(X), ncol = ncol(X))
  # est_X[1,] = as.matrix(X[1,,drop=FALSE])
  # est_X[2:nrow(X),] = as.matrix(X[1:(nrow(X)-1),,drop=FALSE])%*%phi_matrix_2 + colMeans(X)
  # est_X = apply(est_X,2,Imzap)
  # v_matrix_3 = X - est_X
  # v_matrix_3 = apply(v_matrix_3,2,Imzap)
  # if(sum(colSums(apply(v_matrix_3,2,Im))) <= .Machine$double.eps){
  #   v_matrix_3 = apply(v_matrix_3,2,as.numeric)
  # }
  v_matrix_3 = as.matrix(v_matrix_2)
  aug_X = cbind(X[1:(nrow(X)-1),],v_matrix_3[2:nrow(X),])
  colnames(aug_X) = c(colnames(X),paste0("V",1:ncol(X)))
  y_var = Y[1:(nrow(X)-1),]
  cor_model = lm(y_var~as.matrix(aug_X))
  cor_betas = cor_model$coefficients[2:(ncol(X)+1)]
  tstat = coef(summary(cor_model))[2:(ncol(X)+1), "t value"]
  tstat_vec = matrix(data = NA_real_,nrow = N,ncol = ncol(X))
  ols_res = cor_model$residuals
  y_bar = mean(y_var)
  for(i in 1:N){
    draw = rnorm(length(ols_res))
    y_hat = y_bar + (ols_res * draw)
    x_hat = est_X[1:(nrow(X)-1),] + (v_matrix_3[1:(nrow(X)-1),]*draw)
    temp_lm = lm(y_hat ~ x_hat)
    tstat_vec[i,] = tail(coef(summary(temp_lm))[, "t value"],-1)
  }
  for(i in 1:length(expSig)){
    if(expSig[i] > 0){
      pv[i] = sum(tstat_vec[, i] >= tstat[i])/(N-1)
    } else {
      pv[i] = sum(tstat_vec[, i] <= tstat[i])/(N-1)
    }
  }
  names(cor_betas) = x_names
  names(tstat) = x_names
  outp = c(coefficients = cor_betas,t.stats = tstat,p.values = pv,r.square = summary(cor_model)$r.squared,
           adj.r.squared = summary(cor_model)$adj.r.squared)
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

# enc.cvs = fread(input = 'encCV.csv',header = TRUE)
# enc.cvs = melt(enc.cvs,id.vars = c("k2","p"),variable.name = "pi",value.name = "stat")
# enc.cvs[, pi := as.numeric(as.character(pi))]
# setkey(enc.cvs,k2,pi)


# enc.new_plookup = function(encstat,df,mu){
#   if(mu > 5){
#     tmp = enc.cvs[k2==df & pi == 0 & encstat >= stat,]
#   } else {
#     tmp = enc.cvs[k2==df & pi >= mu & encstat >= stat,]
#   }
#   return(min(1 - $p,1))
# }

# oos.R2 = function(u1,u2){
#   return(1 - mean(u2^2)/mean(u1^2))
# }

# oos.tstat = function(u1,u2){
#   d_til = u1^2 - u2^2 + (u1 - u2)^2
#   sig_model <- lm(formula = d_til ~ 1)
#   correct_sig <- coeftest(sig_model,vcov. = vcovHAC(sig_model))
#   return(correct_sig[3])
# }

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

msef.cvs = fread(input = 'msef.csv',header = TRUE)
msef.cvs = melt(msef.cvs,id.vars = c("k2","p","model"),variable.name = "pi",value.name = "stat")
msef.cvs[, pi := as.numeric(as.character(pi))]
setkey(msef.cvs,k2,pi)

msef_plookup = function(msefstat,df,mu,type="recursive"){
  if(mu > 2){
    tmp = msef.cvs[k2==df & pi == 0.0 & msefstat >= stat & model == type,]
  } else {
    tmp = msef.cvs[k2==df & pi >= mu & msefstat >= stat & model == type,]
  }
  tmp = head(tmp,1)
  return(min(tmp$p,1))
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
  k2 = length(rhs(formula)) + max(length(bench),1)
  mu = (nrow(data) - train) / train 
  # oosR2.stat = oos.R2(e1,e2)
  # oosR2.pval = bootts(e1,e2,stat="oosR2",N=10000)
  # # oosR2.tstat = oos.tstat(e1,e2)
  # oosR = see.stars.(tbl = round(as.matrix(data.table(oosR2.stat*100,oosR2.pval)),3),bees = 1,bees = 2)[1]
  # enc_new = enc.new(e1,e2)
  # 
  # enc_new.pvalue = enc.new_plookup(enc_new,k2,mu)
  # encnew = see.stars(tbl = round(as.matrix(data.table(enc_new,enc_new.pvalue)),3),bees = 1,pees = 2)[1]
  eVAR = VAR(data.table(e1,e2),p = 1)
  u1 = eVAR$varresult$e1$residuals
  u2 = eVAR$varresult$e2$residuals
  dmtest = dm.test(u1,u2,alternative = "greater",h = 1,power = 2)
  dm.stat = dmtest$statistic
  # dmt = bootts(u1,u2,stat="DM",N=10000,type="recursive",mu)
  # dm.pval = t.test((dmt-dm.stat)/c(sqrt(std_err(dmt))),alternative = "less")$p.value
  dm.pval = dmtest$p.value
  dm.stat = see.stars(tbl = round(as.matrix(data.table(dm.stat,dm.pval)),3),bees = 1,pees = 2)[1]
  #d.adj.MSPE = mspe.adj(e1,e2,hist,pred) - mspe.adj(e1,e2,hist,pred)
  ENCHLN = enc.hln(u1,u2)
  enchln.tval = ENCHLN[2]
  # enct = bootts(u1,u2,stat="ENC-HLN",N=10000,type="recursive",mu)
  # enc.pval = t.test((enct-ENCHLN[2])/c(sqrt(std_err(enct))),alternative = "less")$p.value
  enchln = see.stars.tstats(tbl = round(as.matrix(data.table(ENCHLN,enchln.tval)),3),bees = 1,tees = 2,exp.positive = TRUE,
                            df = length(u1) - 1)[1]
  MSEF = mse.f(u1,u2)
  # MSEFt = bootts(u1,u2,stat="MSE-F",N=10000,type="recursive",mu)
  # MSEF.pval = t.test((MSEF-MSEFt)/c(sqrt(std_err(MSEFt))),alternative = "less")$p.value
  MSEF.pval = msef_plookup(MSEF,df = k2,type = "recursive",mu)
  MSEF = see.stars(tbl = round(as.matrix(data.table(MSEF,MSEF.pval)),3),bees = 1,pees = 2)[1]
  return(list(forecast_values = pred, K2 = k2,MU = mu, formula = formula, bench = bench, bench_values = benchmark, hist_values = hist,
              Statistics = c(dm.stat,MSEF,enchln),
              pvals = c(dm.pval,MSEF.pval,enchln.tval)))
}

bootts = function(e1,e2,stat=c("DM","MSE-F","ENC-HLN"),N=1000,type="recursive",mu){
  E = data.table(e1,e2)
  if(stat=="DM"){
    statfun = function(tsobj){
      e1 = tsobj[,1]
      e2 = tsobj[,2]
      return(dm.test(e1,e2,alternative = "greater",h = 1,power = 2)$statistic)
    }
  } else if(stat=="MSE-F"){
    statfun = function(tsobj){
      e1 = tsobj[,1]
      e2 = tsobj[,2]
      stat = mse.f(e1,e2)
      return(stat)
    }
  } else if (stat=="ENC-HLN"){
    statfun = function(tsobj){
      e1 = tsobj[,1]
      e2 = tsobj[,2]
      return(enc.hln(e1,e2)[2])
    }
  }
  kores = detectCores()-2
  cl = makeCluster(kores,type = "FORK")
  registerDoParallel(cl)
  bootsamples = tsboot(as.ts(E),statistic = statfun,R = N,l = 3, sim = "geom",orig.t = FALSE,
         parallel = "multicore",cl = clust)$t
  stopCluster(cl)
  gc()
  return(bootsamples)
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
  return(mean(e1^2-e2^2+(y1-y2)^2))
}

# innoe rossie adju
adj.mspe.adj = function(e1,e2,y1,y2){
  return(2*sum(e1^2-e2^2+y1^2-y2^2))
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
  kores = detectCores()-2
  cl = makeCluster(kores,type = "FORK")
  registerDoParallel(cl)
  mcoptions <- list(preschedule=FALSE, set.seed=FALSE)
  mspe_vec = foreach(Rhat = lowR:(highR-1),.combine = c,.options.multicore=mcoptions,.inorder = FALSE,
                     .verbose=TRUE) %dopar% {
    pred1 = rep(NA_real_,nrow(data)-highR+1)
    pred2 = rep(NA_real_,nrow(data)-highR+1)
    bench1 = rep(NA_real_,nrow(data)-highR+1)
    bench2 = rep(NA_real_,nrow(data)-highR+1)
    hist = data[(highR-1):(nrow(data)-1), eval(y)]
    # expanding
    tmplm1 = lm(formula , data = data[1:Rhat])
    pred1 = predict(tmplm1,newdata = data[(highR-1):(nrow(data)-1)])
    if(is.null(bench)){
      f = as.formula(paste0(c(y)," ~ 1"))
    } else {
      f = as.formula(paste0(c(y)," ~ ",bench))
    }
    tmplm2 = lm(f, data = data[1:Rhat])
    bench1 = predict(tmplm2,newdata = data[(highR-1):(nrow(data)-1)])
    # rolling
    tmplm3 = lm(formula , data = data[(Rhat-lowR+1):Rhat])
    pred2 = predict(tmplm3,newdata = data[(highR-1):(nrow(data)-1)])
    tmplm4 = lm(f, data = data[(Rhat-lowR+1):Rhat])
    bench2 = predict(tmplm4,newdata = data[(highR-1):(nrow(data)-1)])
    u1 = (hist-bench1)
    u2 = (hist-pred1)
    u3 = (hist-bench2)
    u4 = (hist-pred2)
    P = length(u1)
    return(c(v1 = (1/sqrt(P)) * adj.mspe.adj(u1,u2,bench1,pred1), v2 = (1/sqrt(P)) * adj.mspe.adj(u3,u4,bench2,pred2)))
  }
  stopCluster(cl)
  gc()
  mspe_vec1 = mspe_vec[names(mspe_vec)=="v1"]
  mspe_vec2 = mspe_vec[names(mspe_vec)=="v2"]
  df = highR - lowR + 1
  # omega1 = c(getLongRunVar(u = as.matrix(mspe_vec1),kernel = "qs")$Delta)
  omega1 = lrvar(mspe_vec1, type = "Newey-West", prewhite = TRUE, adjust = TRUE, lag = NULL)
  # omega2 = c(getLongRunVar(u = as.matrix(mspe_vec2),kernel = "qs")$Delta)
  omega2 = lrvar(mspe_vec2, type = "Newey-West", prewhite = TRUE, adjust = TRUE, lag = NULL)
  P = length(mspe_vec1)
  stat1 = sum(mspe_vec1^2*(1/omega1))
  stat2 = sum(mspe_vec2^2*(1/omega2))
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
  kores = detectCores()-2
  cl = makeCluster(kores,type = "FORK")
  registerDoParallel(cl)
  mcoptions <- list(preschedule=FALSE, set.seed=FALSE)
  dmvec = foreach(Rhat = lowR:highR,.combine = c,.options.multicore=mcoptions,.inorder = FALSE) %dopar% {
    pred1 = rep(NA_real_,nrow(data)-Rhat)
    pred2 = rep(NA_real_,nrow(data)-Rhat)
    bench1 = rep(NA_real_,nrow(data)-Rhat)
    bench2 = rep(NA_real_,nrow(data)-Rhat)
    for(d in Rhat:(nrow(data)-1)){
      # expanding
      tmplm1 = lm(formula , data = data[1:(d-1)])
      pred1[d-Rhat+1] = predict(tmplm1,newdata = data[d])
      if(is.null(bench)){
        f = as.formula(paste0(y," ~ 1"))
      } else {
        f = as.formula(paste0(y," ~ ",bench))
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
    return(c(exp=dm.test.alt(u1,u2,h = 1,power = 2,alternative = "greater")$statistic,
             roll=dm.test.alt(u3,u4,h = 1,power = 2,alternative = "greater")$statistic))
  }
  stopCluster(cl)
  gc()
  dmvec1 = dmvec[names(dmvec)=="exp.DM"]
  dmvec2 = dmvec[names(dmvec)=="roll.DM"]
  exp_RET = max(abs(dmvec1))
  exp_AET = (1/(highR-lowR+1))*sum(abs(dmvec1))
  roll_RET = max(abs(dmvec2))
  roll_AET = (1/(highR-lowR+1))*sum(abs(dmvec2))
  mu = (nrow(data) - highR) / nrow(data)
  return(list(Exp_AET = exp_AET, Exp_RET = exp_RET, Roll_AET = roll_AET, Roll_RET = roll_RET,MU = mu))
  # add pvalues to function
}

# Harvey, Lebourne and Newbold (1998) robust
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
    # hln = lm(u_hi3 ~ 0 + U)
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
    omega1 = ((n)^-1)*sum(f)
    #R = sqrt(n)*(Q2^(-.5))*d_bar
    #HLN_mod = (n^(-.5))*(n+1-(2))^.5
    #stat = HLN_mod * R
    # WT1 = (1/n) * (d_til %*% d_til * (1/Q2))
    L1 = (1/sqrt(n))* sum(d_til)
    # omega1 = c(getLongRunVar(d_til,kernel = "qs",bandwidth = "and")$Omega)
    # omega1 = lrvar(d_til, type = "Newey-West", prewhite = TRUE, adjust = TRUE, lag = NULL)
    WT1 = L1 * L1 * (1/omega1)
    
    n = length(u3)
    u_hi3 = u3
    u_i = u4
    U = u_hi3 - u_i
    # hln = lm(u_hi3 ~ 0 + U)
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
    omega2 = ((n)^-1)*sum(f)
    #R = sqrt(n)*(Q2^(-.5))*d_bar
    #HLN_mod = (n^(-.5))*(n+1-(2))^.5
    #stat = HLN_mod * R
    # WT2 = (1/n) * (d_til %*% d_til * (1/Q2))
    L2 = (1/sqrt(n))* sum(d_til)
    # omega2 = c(getLongRunVar(d_til,kernel = "qs",bandwidth = "and")$Omega)
    # omega2 = lrvar(d_til, type = "Newey-West", prewhite = TRUE, adjust = TRUE, lag = NULL)
    WT2 = L2 * L2 * (1/omega2)
    
    return(c(exp=WT1,roll=WT2))
  }
  stopCluster(cl)
  gc()
  enc_vec1 = enc_vec[str_detect(names(enc_vec),"exp")]
  enc_vec2 = enc_vec[str_detect(names(enc_vec),"roll")]
  exp_RET = max(enc_vec1)
  exp_AET = (1/(highR-lowR+1))*sum((enc_vec1))
  roll_RET = max(enc_vec2)
  roll_AET = (1/(highR-lowR+1))*sum((enc_vec2))
  mu = lowR / nrow(data)
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

sortinoR = function(R,annualize = TRUE, freq = 12){
  if(annualize){
    R = R * freq
  }
  return(SortinoRatio(R))
}


ratios_x = function(x,r_method){
  rx1 = do.call(what = r_method,args = list(R = x[,1]))
  rx2 = do.call(what = r_method,args = list(R = x[,2]))
  return(c(rx1,rx2))
}

ratio.test1 = function(rets1,rets2,n_samples = 10000){
  rx1 = Sratio(rets1)
  rx2 = Sratio(rets2)
  sample_diff = rx1 - rx2
  ts_obj = as.ts(data.table(rets1,rets2))
  clust = makeCluster(10,type = "FORK")
  registerDoParallel(clust)
  ts_ratios = tsboot(tseries = ts_obj,statistic = Sratio_diff,R = n_samples,l = 3,sim = "geom",
                     parallel = "multicore",cl = clust)$t
  stopCluster(clust)
  registerDoSEQ()
  pv = t.test(ts_ratios,alternative = "less")$p.value
  return(pv)
}

ratio.test2 = function(rets1,rets2,n_samples = 1000,
                      ratio = c("Sratio","sortinoR","Kappa03","Kappa04")){
  ts_obj = as.ts(data.table(rets1,rets2))
  if(ratio %in% c("Sratio","sortinoR","Kappa03","Kappa04")){
    df = as.xts(data.table(seq(as.Date("1926-08-28"),by = "month",length.out = length(rets1)),rets1,rets2))
  } else { df = data.table(rets1,rets2)}
  o_ratio = do.call(what = ratio,args = list(df))
  o_ratio_diff = o_ratio[1] - o_ratio[2]
  o_se = ratio_diff_se(rets1,rets2,ratio)
  stand_oratio = c(o_ratio_diff / o_se)
  clust = makeCluster(10,type = "FORK")
  registerDoParallel(clust)
  ts_ratios = tsboot(tseries = ts_obj,statistic = stand_ratio,R = n_samples,l = 3,sim = "geom",
         parallel = "multicore",cl = clust,ratio=ratio,o_diff = o_ratio_diff)$t
  stopCluster(clust)
  registerDoSEQ()
  gc()
  pv = (sum(ts_ratios >= stand_oratio)+1)/(n_samples+1)
  return(pv)
}

genRachev.ratio = function(R){
  if(length(dim(R))>1){
    return(apply(X = R,MARGIN = 2,FUN = genRachev.ratio))
  }
  etl1 = ETL(R,method = "historical")
  etl2 = ETL(-1*R,method = "historical")
  return(c(genRachev = etl2/etl1))
}

Kappa03 = function(R){
  if(length(dim(R))>1){
    return(apply(X = R,MARGIN = 2,FUN = Kappa03))
  }
  return(Kappa(R,0,3))
}
Kappa04 = function(R){
  if(length(dim(R))>1){
    return(apply(X = R,MARGIN = 2,FUN = Kappa04))
  }
  return(Kappa(R,0,4))
}

Sratio = function(x,annualize=TRUE,freq = 12){
  if(length(dim(x))>1){
    return(apply(X = x,MARGIN = 2,FUN = Sratio))
  }
  if(annualize){s = sqrt(freq)} else {s = 1}
  return(mean(x,na.rm = TRUE)/sd(x,na.rm = TRUE) * s)
}

Sratio_diff = function(r1,r2=NULL){
  if(dim(r1)[2]==2 & !is.null(r2)){
    s1 = Sratio(r1)
    s2 = Sratio(r2)
  } else {
    s1 = Sratio(r1[,1])
    s2 = Sratio(r1[,2])
  }
  return(s1-s2)
}

# Sratio_diff_CI = function(x,l,mu){
#   vstar = c(mean(x[,1]),mean(x[,2]),sd(x[,1]),sd(x[,2]))
#   gv = grad_tic(vstar)
#   l_b = floor(nrow(x)/l)
#   ystar = rep(list(vector("numeric",4)),nrow(x))
#   zetaj = rep(list(vector("numeric",4)),l_b)
#   Zeta = matrix(data = 0, nrow = 4,ncol = 4)
#   for(t in 1:nrow(x)){
#     dm1 = x[t,1] - mean(x[,1])
#     dm2 = x[t,2] - mean(x[,2])
#     dv1 = x[t,1]^2 - var(x[,1])
#     dv2 = x[t,2]^2 - var(x[,2])
#     ystar[[t]] = c(dm1,dm2,dv1,dv2)
#   }
#   
#   for(j in 1:l_b){
#     zetaj[[j]] = (1/sqrt(l)) * Reduce('+',ystar[((j-1)*l+1):((j-1)*l+l)])
#     Zeta = Zeta + crossprod(t(zetaj[[j]]))
#   }
#   PSIstar = (1/l_b) * Zeta
#   
#   se = c(sqrt(t(gv)%*%PSIstar%*%(gv) / nrow(x)))
#   L = ((x[,1] - x[,2]) - mu) / se
#   diff_dist = ecdf(L)
#   qts = quantile(diff_dist,probs = c(.9,.95,.99))
#   lows = mu - (qts *se)
#   highs = mu + (qts * se)
#   return()
# }

delta_hat = function(x){
  if(!length(x)==4){stop("x must be a numeric vector of length 4")}
  g1 = x[1] / (x[3] - x[1]^2)^.5
  g2 = x[2] / (x[4] - x[2]^2)^.5
  return(g1-g2)
}

grad_tic = function(x){
  if(!length(x)==4){stop("x must be a numeric vector of length 4")}
  g1 = x[3] / (x[3] - x[1]^2)^1.5
  g2 = x[4] / (x[4] - x[2]^2)^1.5
  g3 = -1*.5*(x[1] / (x[3] - x[1]^2)^1.5)
  g4 = -1*.5*(x[2] / (x[4] - x[2]^2)^1.5)
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

dm.test.alt = function (e1, e2, alternative = c("two.sided", "less", "greater"), 
          h = 1, power = 2) 
{
  alternative <- match.arg(alternative)
  d <- c(abs(e1))^power - c(abs(e2))^power
  # maxlag = ceiling(12*(length(d)/100)^(.25))
  # d = c(prewhiten(tser = as.ts(d),AR.max = maxlag)$prew_ar)
  # LRV = c(getLongRunVar(d,bandwidth = "nw")$Omega)
  LRV = lrvar(d, type = "Newey-West", prewhite = TRUE, adjust = TRUE, lag = NULL)
  d.var <- 2 * pi *  LRV/ length(d)
  dv <- d.var
  if (dv > 0) 
    STATISTIC <- mean(d, na.rm = TRUE)/sqrt(dv)
  else if (h == 1) 
    stop("Variance of DM statistic is zero")
  else {
    warning("Variance is negative, using horizon h=1")
    return(dm.test.alt(e1, e2, alternative, h = 1, power))
  }
  n <- length(d)
  k <- ((n + 1 - 2 * h + (h/n) * (h - 1))/n)^(1/2)
  STATISTIC <- STATISTIC * k
  names(STATISTIC) <- "DM"
  if (alternative == "two.sided") 
    PVAL <- 2 * pt(-abs(STATISTIC), df = n - 1)
  else if (alternative == "less") 
    PVAL <- pt(STATISTIC, df = n - 1)
  else if (alternative == "greater") 
    PVAL <- pt(STATISTIC, df = n - 1, lower.tail = FALSE)
  PARAMETER <- c(h, power)
  names(PARAMETER) <- c("Forecast horizon", "Loss function power")
  structure(list(statistic = STATISTIC, parameter = PARAMETER, 
                 alternative = alternative, p.value = PVAL, method = "Diebold-Mariano Test", 
                 data.name = c(deparse(substitute(e1)), deparse(substitute(e2)))), 
            class = "htest")
}

cer = function(rets,gamma,annualize = TRUE, freq = 12){
  CER = mean(rets) - .5*var(rets)*(1/gamma)
  if(annualize){
    CER = CER * 12
  }
  return(CER * 100)
}

hac_t.test = function(x,y=NULL,paired=FALSE,alternative=c("two-sided","less","greater"),
                                                          var.equal = FALSE){
  if(!is.null(y)){
    z = x - y
    if(paired){
      nom = mean(z)
      LRV = lrvar(z, type = "Newey-West", prewhite = TRUE, adjust = TRUE, lag = NULL)
      den = sqrt(LRV/length(z))
      tstat = nom / den
      if(alternative=="less"){
        pval = pt(q = tstat, df = length(z) - 1,lower.tail = TRUE)
      } else if(alternative == "greater"){
        pval = pt(q = tstat, df = length(z) - 1,lower.tail = FALSE)
      } else {
        pval = pt(q = tstat, df = length(z) - 1) * 2
      }
      return(list(t.stat = tstat,p.val = pval))
    } else { 
      nom = mean(x) - mean(y)
      if(var.equal){
        LRV1 = lrvar(x, type = "Newey-West", prewhite = TRUE, adjust = TRUE, lag = NULL)
        LRV2 = lrvar(y, type = "Newey-West", prewhite = TRUE, adjust = TRUE, lag = NULL)
        SLRV = (((length(x)-1)*LRV1) + ((length(y)-1)*LRV2)) / (length(x)+length(y)-2)
        den = sqrt(SLRV)*sqrt((1/length(x)+(1/length(y))))
        tstat = nom / den
        if(alternative=="less"){
          pval = pt(q = tstat, df = length(z) - 1,lower.tail = TRUE)
        } else if(alternative == "greater"){
          pval = pt(q = tstat, df = length(z) - 1,lower.tail = FALSE)
        } else {
          pval = pt(q = tstat, df = length(z) - 1) * 2
        }
      } else {
        LRV1 = lrvar(x, type = "Newey-West", prewhite = TRUE, adjust = TRUE, lag = NULL)
        LRV2 = lrvar(y, type = "Newey-West", prewhite = TRUE, adjust = TRUE, lag = NULL)
        den = sqrt((LRV1/length(x)+(LRV2/length(y))))
        tstat = nom / den
        if(alternative=="less"){
          pval = pt(q = tstat, df = length(z) - 1,lower.tail = TRUE)
        } else if(alternative == "greater"){
          pval = pt(q = tstat, df = length(z) - 1,lower.tail = FALSE)
        } else {
          pval = pt(q = tstat, df = length(z) - 1) * 2
        }
      }
      return(list(t.stat = tstat,p.val = pval))
      }
  }
  nom = mean(x)
  LRV1 = lrvar(x, type = "Newey-West", prewhite = TRUE, adjust = TRUE, lag = NULL)
  den = sqrt(LRV1/length(x))
  tstat = nom / den
  if(alternative=="less"){
    pval = pt(q = tstat, df = length(z) - 1,lower.tail = TRUE)
  } else if(alternative == "greater"){
    pval = pt(q = tstat, df = length(z) - 1,lower.tail = FALSE)
  } else {
    pval = pt(q = tstat, df = length(z) - 1) * 2
  }
  return(list(t.stat = tstat,p.val = pval))
}


moments = c(2,2,3,4)
names(moments) = c("Sratio","sortinoR","Kappa03","Kappa04")
ratio_diff_se = function(x1,x2,ratio){
  if(ratio=="genRachev.ratio"){
    etl11 = c(ETL(x1,method = "historical"))
    etl12 = c(ETL(-1*x1,method = "historical"))
    etl21 = c(ETL(x2,method = "historical"))
    etl22 = c(ETL(-1*x2,method = "historical"))
    etlx1 = tail(sort(x1),floor(.05*length(x1)))
    etlx2 = tail(sort(x2),floor(.05*length(x1)))
    etlx3 = head(sort(x1),floor(.05*length(x1)))
    etlx4 = head(sort(x2),floor(.05*length(x1)))
    y = c(etl12,etl22,etl11,etl21)
    Ldt = data.table(etl12 - etlx3,etl22 - etlx4,etlx1 - etl11,etlx2 - etl21)
  } else if(ratio %in% c("sortinoR","Kappa03","Kappa04")){
    n = moments[ratio]
    LP1 = lpm(R = as.xts(data.table(seq(as.Date("1926-08-28"),by = "month",length.out = length(x1)),x1))
              ,n,threshold = 0)
    LP2 = lpm(R = as.xts(data.table(seq(as.Date("1926-08-28"),by = "month",length.out = length(x2)),x2))
              ,n,threshold = 0)
    y = c(mean(x1),mean(x2),LP1,LP2)
    Lx1 = c(x1)
    Lx1[Lx1 > 0] = 0
    Lx2 = c(x2)
    Lx2[Lx2 > 0] = 0
    Ldt = data.table(x1-mean(x1),x2-mean(x2),
                     Lx1^2-LP1,
                     Lx2^2-LP2)
  } else {
    y = c(mean(x1),mean(x2),sd(x1),sd(x2))
    Ldt = data.table(x1-mean(x1),x2-mean(x2),x1^2-var(x1),x2^2-var(x2))
    }
  f = function(y){
    return((y[1]/y[3] - y[2]/y[4]))
  }
  grad_f = gradient(f,y)
  LRV = tryCatch(expr = {lrvar(as.matrix(Ldt), type = "Andrews", prewhite = TRUE, adjust = TRUE, lag = NULL)},
                 error = tryCatch(expr= {lrvar(as.matrix(Ldt), type = "Newey-West", prewhite = FALSE, 
                                               adjust = TRUE, lag = 0)},
                                  error = cov(Ldt)))
  se = tryCatch(expr = {sqrt((grad_f %*% LRV %*% t(grad_f))/length(x1))}, error = {sd(x1) / sqrt(length(x1))})
  return(se)
}

stand_ratio = function(tsobj,ratio,o_diff){
  x1 = tsobj[,1]
  x2 = tsobj[,2]
  ratio1 = do.call(what = ratio,args = list(x1))
  ratio2 = do.call(what = ratio,args = list(x2))
  diff = ratio1 - ratio2
  se = ratio_diff_se(x1,x2,ratio)
  num = diff - o_diff
  return(num/se)
}

scale.na = function(x){
  na.idx2 = which(!is.na(x))
  new.x = scale(na.omit(x))
  x[na.idx2] = new.x
  return(x)
}
ivreg2 <- function(form,endog,iv,data,digits=3){
  # library(MASS)
  # model setup
  r1 <- lm(form,data)
  y <- r1$fitted.values+r1$resid
  x <- model.matrix(r1)
  aa <- rbind(endog == colnames(x),1:dim(x)[2])  
  z <- cbind(x[,aa[2,aa[1,]==0]],data[,iv])  
  colnames(z)[(dim(z)[2]-length(iv)+1):(dim(z)[2])] <- iv  
  # iv coefficients and standard errors
  z <- as.matrix(z)
  pz <- z %*% (solve(crossprod(z))) %*% t(z)
  biv <- solve(crossprod(x,pz) %*% x) %*% (crossprod(x,pz) %*% y)
  sigiv <- crossprod((y - x %*% biv),(y - x %*% biv))/(length(y)-length(biv))
  vbiv <- as.numeric(sigiv)*solve(crossprod(x,pz) %*% x)
  res <- cbind(biv,sqrt(diag(vbiv)),biv/sqrt(diag(vbiv)),(1-pnorm(biv/sqrt(diag(vbiv))))*2)
  res <- matrix(as.numeric(sprintf(paste("%.",paste(digits,"f",sep=""),sep=""),res)),nrow=dim(res)[1])
  rownames(res) <- colnames(x)
  colnames(res) <- c("Coef","S.E.","t-stat","p-val")
  # First-stage F-test
  y1 <- data[,endog]
  z1 <- x[,aa[2,aa[1,]==0]]
  bet1 <- solve(crossprod(z)) %*% crossprod(z,y1)
  bet2 <- solve(crossprod(z1)) %*% crossprod(z1,y1)
  rss1 <- sum((y1 - z %*% bet1)^2)
  rss2 <- sum((y1 - z1 %*% bet2)^2)
  p1 <- length(bet1)
  p2 <- length(bet2)
  n1 <- length(y)
  fs <- abs((rss2-rss1)/(p2-p1))/(rss1/(n1-p1))
  firststage <- c(fs)
  firststage <- matrix(as.numeric(sprintf(paste("%.",paste(digits,"f",sep=""),sep=""),firststage)),ncol=length(firststage))
  colnames(firststage) <- c("First Stage F-test")
  # Hausman tests
  bols <- solve(crossprod(x)) %*% crossprod(x,y) 
  sigols <- crossprod((y - x %*% bols),(y - x %*% bols))/(length(y)-length(bols))
  vbols <- as.numeric(sigols)*solve(crossprod(x))
  sigml <- crossprod((y - x %*% bols),(y - x %*% bols))/(length(y))
  x1 <- x[,!(colnames(x) %in% "(Intercept)")]
  z1 <- z[,!(colnames(z) %in% "(Intercept)")]
  pz1 <- z1 %*% (solve(crossprod(z1))) %*% t(z1)
  biv1 <- biv[!(rownames(biv) %in% "(Intercept)"),]
  bols1 <- bols[!(rownames(bols) %in% "(Intercept)"),]
  # Durbin-Wu-Hausman chi-sq test:
  # haus <- t(biv1-bols1) %*% ginv(as.numeric(sigml)*(solve(crossprod(x1,pz1) %*% x1)-solve(crossprod(x1)))) %*% (biv1-bols1)
  # hpvl <- 1-pchisq(haus,df=1)
  # Wu-Hausman F test
  resids <- NULL
  resids <- cbind(resids,y1 - z %*% solve(crossprod(z)) %*% crossprod(z,y1))
  x2 <- cbind(x,resids)
  bet1 <- solve(crossprod(x2)) %*% crossprod(x2,y)
  bet2 <- solve(crossprod(x)) %*% crossprod(x,y)
  rss1 <- sum((y - x2 %*% bet1)^2)
  rss2 <- sum((y - x %*% bet2)^2)
  p1 <- length(bet1)
  p2 <- length(bet2)
  n1 <- length(y)
  fs <- abs((rss2-rss1)/(p2-p1))/(rss1/(n1-p1))
  fpval <- 1-pf(fs, p1-p2, n1-p1)
  #hawu <- c(haus,hpvl,fs,fpval)
  hawu <- c(fs,fpval)
  hawu <- matrix(as.numeric(sprintf(paste("%.",paste(digits,"f",sep=""),sep=""),hawu)),ncol=length(hawu))
  #colnames(hawu) <- c("Durbin-Wu-Hausman chi-sq test","p-val","Wu-Hausman F-test","p-val")
  colnames(hawu) <- c("Wu-Hausman F-test","p-val")  
  # Sargan Over-id test
  ivres <- y - (x %*% biv)
  oid <- solve(crossprod(z)) %*% crossprod(z,ivres)
  sstot <- sum((ivres-mean(ivres))^2)
  sserr <- sum((ivres - (z %*% oid))^2)
  rsq <- 1-(sserr/sstot)
  sargan <- length(ivres)*rsq
  spval <- 1-pchisq(sargan,df=length(iv)-1)
  overid <- c(sargan,spval)
  overid <- matrix(as.numeric(sprintf(paste("%.",paste(digits,"f",sep=""),sep=""),overid)),ncol=length(overid))
  colnames(overid) <- c("Sargan test of over-identifying restrictions","p-val")
  if(length(iv)-1==0){
    overid <- t(matrix(c("No test performed. Model is just identified")))
    colnames(overid) <- c("Sargan test of over-identifying restrictions")
  }
  full <- list(results=res, weakidtest=firststage, endogeneity=hawu, overid=overid)
  return(full)
}
