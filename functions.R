alt_var = function(x,timep){
  return((var(x) + (1 + mean(x))^2)^timep - (1 + mean(x))^(2*timep))
}

adj_cor <- function(r,nl){
  out = r * Re(hypergeo::hypergeo(A=(1/2),B=(1/2),C=((nl-1)/2),z = (1-r^2),maxiter = 1000))
  return(out)
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
  m_tr = apply(X = tmp_m,MARGIN = 2,FUN = var)
  weight = unique(mtrx2,by=c("PERMNO"))$weight
  avg_var = (m_tr %*% weight) * timep
  diag(c_m) = 0
  avg_cor = crossprod(weight,crossprod(c_m,weight))
  avg_cor_ta = avg_cor / (1 - sum(weight^2))
  mkt_r = unique(mtrx3,by="date")$vwretd.daily
  mkt_var = var(mkt_r) * timep
  return(c(avg_var,avg_cor,mkt_var,avg_cor_ta))
}
