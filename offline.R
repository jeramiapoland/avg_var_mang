paks <- c("Quandl","RCurl","data.table","lubridate","ggplot2","stringr","sandwich","stargazer","pracma","RColorBrewer",
          "CADFtest","complexplus","nlme","readxl","reshape2","quantmod","xlsx","tikzDevice","MASS","timeSeries","plm", "reshape2",
          "PortfolioAnalytics","PerformanceAnalytics","backtest","tidyr","broom","stringdist","BH","parallel","doMC","foreach",
          "doParallel","lfe","lmtest","hypergeo","abind","pcaMethods","pls","lubridate") 
# note: tikzDevice requires a working latex installation
# and xlsx require rJava so a properly configured java (try javareconf)
for (p in paks){
  require(p,character.only = TRUE) || {install.packages(p) 
    require(p,character.only = TRUE)}
}

### functions within data.table ####
cor_var = function(mtrx) {
  mtrx1 = subset(mtrx,select = c("date","PERMNO","RET"))
  mtrx2 = subset(mtrx,select = c("weight","PERMNO"))
  mtrx3 = subset(mtrx,select = c("date","vwretd"))
  tmp_m = dcast(data = mtrx1, formula = date ~ ..., value.var = "RET")
  tmp_m = as.data.table(tmp_m)
  tmp_m[, date := NULL]
  c_m = cor(tmp_m,use = "pairwise.complete.obs")
  v_m = cov(tmp_m,use = "pairwise.complete.obs")
  m_tr = diag(v_m)
  weight = unique(mtrx2,by=c("PERMNO"))$weight
  avg_var = (m_tr %*% weight) 
  unweighted_avg_var = m_tr / nrow(v_m)
  diag(c_m) = 0
  avg_cor = crossprod(weight,crossprod(c_m,weight)) 
  unweighted_avg_cor = avg_cor / (1 - sum(weight^2))
  mkt_r = unique(mtrx3,by="date")$vwretd
  mkt_var = var(mkt_r)
  return(c(avg_var,avg_cor,mkt_var,unweighted_avg_var,unweighted_avg_cor))
}

data[all_month, c("avg_var","avg_cor","mkt_var","unweighted_avg_var","unweighted_avg_cor") := as.list(cor_var(.SD)), 
     .SDcols = c("date","PERMNO","RET","weight","vwretd"), by = c("year","month")]

data[all_quarter, c("q_avg_var","q_avg_cor","q_mkt_var","q_unweighted_avg_var","q_unweighted_avg_cor") := as.list(cor_var(.SD)), 
     .SDcols = c("date","PERMNO","RET","q_weight","vwretd"), by = c("year","quarter")]