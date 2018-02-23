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
# source(file = 'functions.R')
tmp_crsp = fread(input = '../data/CRSP/crsp_daily_long.csv',colClasses = "character")
setkey(tmp_crsp,date,PERMNO)
tmp_crsp[, date := as.Date(as.character(date),format="%Y%m%d")]
setkey(tmp_crsp,date,PERMNO)
# tmp_crsp = subset(tmp_crsp,subset = year(date) >= 1961)
tmp_crsp[, year := year(date)]
tmp_crsp[, month := month(date)]
tmp_crsp[, quarter := quarter(date)]
tmp_crsp[, PRC := as.numeric(PRC)]
tmp_crsp[PRC < 0, PRC := abs(PRC)]
tmp_crsp[, RET := as.numeric(RET)]
setkey(tmp_crsp,PERMNO,date)
tmp_crsp[, ALTRET := ROC(PRC,type = "discrete"), by = PERMNO]
tmp_crsp[is.na(RET)&!is.na(ALTRET)] # no simple fix for missing returns
tmp_crsp[, RETX := as.numeric(RETX)]
tmp_crsp[, vwretd := as.numeric(vwretd)]
tmp_crsp[, vwretx := as.numeric(vwretx)]
tmp_crsp[, tdays := length(unique(date)), by=c("year","month")]
m_tday = tmp_crsp[, .(tdays = unique(tdays)), by =c("year","month")]
tmp_crsp[, asset_tdays := length(na.omit(PRC)), by=c("year","month","PERMNO")]
tmp_crsp[, all_month := tdays==asset_tdays]
tmp_crsp[, tdays := length(unique(date)), by=c("year","quarter")]
q_tday = tmp_crsp[, .(tdays = unique(tdays)), by =c("year","quarter")]
tmp_crsp[, asset_tdays := length(na.omit(PRC)), by=c("year","quarter","PERMNO")]
tmp_crsp[, all_quarter := tdays==asset_tdays]
setkey(tmp_crsp,date,PERMNO)
setkey(tmp_crsp,year,month)
crsp_permnos = tmp_crsp[, .(PERMNO = unique(PERMNO), num_PERMNO = length(unique(PERMNO))), by = c("year","month")]

tmp_mcap = fread(input = '9cfd0c24e4fad219.csv',colClasses = "character")
tmp_mcap[, date := as.Date(date,format="%Y%m%d")]
setkey(tmp_mcap,date,PERMNO)
# tmp_mcap = subset(tmp_mcap,subset = year(date) >= 1961)
tmp_mcap[, PRC := as.numeric(PRC)]
tmp_mcap[, ALTPRC := as.numeric(ALTPRC)]
tmp_mcap[, SHROUT := as.integer(SHROUT)]
tmp_mcap[is.na(PRC), PRC := ALTPRC]
tmp_mcap[, PRC := abs(PRC)]
tmp_mcap[, month_mcap := PRC * SHROUT]
tmp_mcap[, year := year(date)]
tmp_mcap[, month := month(date)]
tmp_mcap[, quarter := quarter(date)]
tmp_mcap = tmp_mcap[!is.na(month_mcap)]
tmp_mcap = setorder(setDT(tmp_mcap), year,month, -month_mcap)[, indx := seq_len(.N), by = c("year","month")][indx <= 1500]
tmp_mcap = merge(tmp_mcap,subset(crsp_permnos,select=c("year","month","PERMNO")),by=c("year","month","PERMNO"))
tmp_mcap[, weight := month_mcap / sum(month_mcap), by = c("year","month")]
tmp_mcap[, q_weight := mean(weight,na.rm=TRUE), by = c("year","quarter","PERMNO")] # quarter mcap mean of monthly mcaps
asset_count = tmp_mcap[, .(count = length(unique(PERMNO))), by = c("year","month")]
max_weight = tmp_mcap[, .(maxw = max(weight)), by = c("year","month")]
min_weight = tmp_mcap[, .(minw = min(weight)), by = c("year","month")]

data = merge(subset(tmp_crsp,select = c("PERMNO","date","year","month","quarter","SICCD","TSYMBOL","PRC","RET","RETX","all_month","all_quarter",
                                        "vwretd")),
             subset(tmp_mcap,select = c("PERMNO","year","month","quarter","weight")),by=c("year","month","quarter","PERMNO"))
setkey(data,date,PERMNO)
tmp_crsp = NULL
tmp_mcap = NULL
crsp_permnos = NULL
gc()



# monthly index returns
m_crsp = fread(input = 'a85c415bc7ca7abb.csv')
m_crsp[, year := as.integer(substr(as.character(DATE),1,4))]
m_crsp[, month := as.integer(substr(as.character(DATE),5,6))]
m_crsp[, c("vwretd.tp1","vwretx.tp1","sprtrn.tp1") := lapply(.SD,shift,type="lead"), .SDcols = c("vwretd","vwretx","sprtrn")]
m_crsp[, quarter := quarter(as.Date(as.character(DATE),format="%Y%m%d"))]
m_crsp[!is.na(vwretd.tp1), c("vwretd.tp3","vwretx.tp3") := lapply(.SD,function(x){
  exp(shift(runSum(log1p(x),n = 3),type="lead",n=2)) - 1
}), .SDcol = c("vwretd.tp1","vwretx.tp1")]
data = merge(data,m_crsp,by=c("year","month","quarter"),all.x=TRUE,suffixes = c(".daily",".monthly"))

gc()

#### offline cor calculations ####
### functions within data.table ####
# cor_var = function(mtrx) {
#   mtrx1 = subset(mtrx,select = c("date","PERMNO","RET"))
#   mtrx2 = subset(mtrx,select = c("weight","PERMNO"))
#   mtrx3 = subset(mtrx,select = c("date","vwretd.daily"))
#   tmp_m = dcast(data = mtrx1, formula = date ~ ..., value.var = "RET")
#   tmp_m = as.data.table(tmp_m)
#   tmp_m[, date := NULL]
#   c_m = cor(tmp_m,use = "pairwise.complete.obs")
#   v_m = cov(tmp_m,use = "pairwise.complete.obs")
#   m_tr = diag(v_m)
#   weight = unique(mtrx2,by=c("PERMNO"))$weight
#   avg_var = (m_tr %*% weight) 
#   unweighted_avg_var = m_tr / nrow(v_m)
#   diag(c_m) = 0
#   avg_cor = crossprod(weight,crossprod(c_m,weight)) 
#   unweighted_avg_cor = avg_cor / (1 - sum(weight^2))
#   mkt_r = unique(mtrx3,by="date")$vwretd.daily
#   mkt_var = var(mkt_r)
#   return(c(avg_var,avg_cor,mkt_var,unweighted_avg_var,unweighted_avg_cor))
# }
#
# setkey(data,date,PERMNO)
# data[all_month==1, c("avg_var","avg_cor","mkt_var","unweighted_avg_var","unweighted_avg_cor") := as.list(cor_var(.SD)), 
#      .SDcols = c("date","PERMNO","RET","weight","vwretd"), by = c("year","month")]
# 
# data[all_quarter==1, c("q_avg_var","q_avg_cor","q_mkt_var","q_unweighted_avg_var","q_unweighted_avg_cor") := as.list(cor_var(.SD)), 
#      .SDcols = c("date","PERMNO","RET","q_weight","vwretd"), by = c("year","quarter")]


m_data = unique(subset(data,select = c("year","month","avg_var","avg_cor","mkt_var","q_avg_var","q_avg_cor",
                                       "q_mkt_var","q_unweighted_avg_var","q_unweighted_avg_cor")),by=c("year","month"))
q_data = subset(m_data,subset = month %in% c(12,3,6,9),select = c("year","quarter","vwretd","vwretx","q_avg_var","q_avg_cor","q_mkt_var"))

# ff_data = fread(input = '../value_momentum_spread/F-F_Research_Data_Factors.CSV')
# ff_data[, V1 := as.character(V1)]
# ff_data[, year := as.integer(substr(V1,1,4))]
# ff_data[, month := as.integer(substr(V1,5,6))]
# ff_data[, Mkt_RF := `Mkt-RF`]
# ff_data[, `Mkt-RF` := NULL]
# ff_data[, Mkt_RF := Mkt_RF / 100]
# ff_data[, SMB := SMB / 100]
# ff_data[, HML := HML / 100]
# ff_data[, RF := RF / 100]
# ff_data[, RF_lag := shift(RF)]
# 
# m_data = merge(m_data,subset(ff_data,select=c("year","month","SMB","HML","RF","RF_lag","Mkt_RF")),by=c("year","month"),all.x=TRUE)
# m_data[, xlogret.tp1 := log1p(vwretd.tp1) - log1p(RF_lag)]
# m_data[, mkt_var.tp1 := shift(mkt_var,type = "lead")]
# m_data[, avg_var_p := avg_var*100]
# m_data[, mkt_var_p := mkt_var*100]
# m_data[, mkt_var_p.tp1 := shift(mkt_var_p,type = "lead")]
summary(m_data$avg_var_p)
avg_var_p_ar = ar(m_data$avg_var_p,order.max = 1)
avg_var_p_ar$ar
avg_var_p_ar_free = ar(m_data$avg_var_p)
