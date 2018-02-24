paks <- c("RCurl","data.table","lubridate","ggplot2","stringr","sandwich","stargazer","pracma","RColorBrewer",
          "CADFtest","complexplus","readxl","reshape2","quantmod","xlsx","tikzDevice","MASS","timeSeries",
          "PortfolioAnalytics","PerformanceAnalytics","backtest","tidyr","broom","stringdist","BH","parallel","doMC","foreach",
          "doParallel","lmtest","hypergeo") 
# note: tikzDevice requires a working latex installation
# and xlsx require rJava so a properly configured java (try javareconf)
for (p in paks){
  require(p,character.only = TRUE) || {install.packages(p) 
    require(p,character.only = TRUE)}
}

### functions within data.table ####
cor_var = function(mtrx,freq) {
  if(freq=="monthly"){
    X = "weight"
  } else { X = "q_weight"}
  mtrx1 = subset(mtrx,select = c("date","PERMNO","RET"))
  mtrx2 = subset(mtrx,select = c(X,"PERMNO"))
  mtrx3 = subset(mtrx,select = c("date","vwretd.daily"))
  tmp_m = dcast(data = mtrx1, formula = date ~ ..., value.var = "RET")
  tmp_m = as.data.table(tmp_m)
  tmp_m[, date := NULL]
  c_m = cor(tmp_m,use = "pairwise.complete.obs")
  m_tr = apply(X = tmp_m,MARGIN = 2,FUN = var,na.rm=TRUE)
  weight = unlist(unique(mtrx2,by=c("PERMNO"))[, X, with = FALSE])
  avg_var = (m_tr %*% weight) 
  diag(c_m) = 0
  avg_cor = crossprod(weight,crossprod(c_m,weight)) 
  avg_cor_ta = avg_cor / (1 - sum(weight^2))
  mkt_r = unique(mtrx3,by="date")$vwretd.daily
  mkt_var = var(mkt_r)
  return(c(avg_var,avg_cor,mkt_var,avg_cor_ta))
}

setkey(data,date,PERMNO)

data[, c("avg_var","avg_cor","mkt_var","avg_cor_ta") := as.list(cor_var(.SD,"monthly")), 
     .SDcols = c("date","PERMNO","RET","weight","vwretd.daily"), by = c("year","month")]

p_check = unique(data[all_month==1 & not_zero == 1 & is.na(avg_cor), .(year,month,PERMNO)], by = c("year","month"))

data[all_quarter == 1 & q_not_zero == 1, c("q_avg_var","q_avg_cor","q_mkt_var","q_avg_cor_ta") := as.list(cor_var(.SD,"quarterly")), 
     .SDcols = c("date","PERMNO","RET","q_weight","vwretd.daily"), by = c("year","quarter")]

q_p_check = unique(data[all_quarter==1 & q_not_zero == 1 & is.na(q_avg_cor), .(year,quarter,PERMNO)], by = c("year","quarter"))
