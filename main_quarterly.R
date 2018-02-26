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
# source(file = 'functions.R')
# tmp_crsp = fread(input = '../data/CRSP/crsp_daily_long.csv',colClasses = "character")
# setkey(tmp_crsp,date,PERMNO)
# tmp_crsp[, date := as.Date(as.character(date),format="%Y%m%d")]
# setkey(tmp_crsp,date,PERMNO)
# # tmp_crsp = subset(tmp_crsp,subset = year(date) >= 1961)
# tmp_crsp[, year := year(date)]
# tmp_crsp[, quarter := quarter(date)]
# tmp_crsp[, PRC := as.numeric(PRC)]
# tmp_crsp[PRC < 0, PRC := abs(PRC)]
# tmp_crsp[, RET := as.numeric(RET)]
# setkey(tmp_crsp,PERMNO,date)
# tmp_crsp[, ALTRET := ROC(PRC,type = "discrete"), by = PERMNO]
# tmp_crsp[is.na(RET)&!is.na(ALTRET)] # no simple fix for missing returns
# tmp_crsp[, RETX := as.numeric(RETX)]
# tmp_crsp[, vwretd := as.numeric(vwretd)]
# tmp_crsp[, vwretx := as.numeric(vwretx)]
tmp_crsp = fread(input = 'all_tmp_crsp.csv',colClasses = c("character","Date","character","character","character","numeric",
                                                           "numeric","character","numeric","numeric","numeric","character",
                                                           "character","character","numeric","numeric","numeric" ))
setkey(tmp_crsp,date,PERMNO)
tmp_crsp[, quarter := quarter(date)]
tmp_crsp[, q_tdays := length(unique(date)), by=c("year","quarter")]
q_tday = tmp_crsp[, .(q_tdays = unique(q_tdays)), by =c("year","quarter")]
tmp_crsp[, q_asset_tdays := length(na.omit(RET)), by=c("year","quarter","PERMNO")]
tmp_crsp[, q_not_zero := (!sum(RET==0)==length(RET)),by=c("year","quarter","PERMNO")]
tmp_crsp[, all_quarter := q_tdays==q_asset_tdays]
tmp_crsp = subset(tmp_crsp,subset= (all_quarter & q_not_zero))
gc()
setkey(tmp_crsp,date,PERMNO)
setkey(tmp_crsp,year,quarter)
q_crsp_permnos = tmp_crsp[, .(PERMNO = unique(PERMNO), num_PERMNO = length(unique(PERMNO))), by = c("year","quarter")]

# tmp_mcap = fread(input = '9cfd0c24e4fad219.csv',colClasses = "character")
# tmp_mcap[, date := as.Date(date,format="%Y%m%d")]
# setkey(tmp_mcap,date,PERMNO)
# tmp_mcap[, PRC := as.numeric(PRC)]
# tmp_mcap[, ALTPRC := as.numeric(ALTPRC)]
# tmp_mcap[, SHROUT := as.integer(SHROUT)]
# tmp_mcap[is.na(PRC), PRC := ALTPRC]
# tmp_mcap[, PRC := abs(PRC)]
# tmp_mcap[, month_mcap := PRC * SHROUT]
# tmp_mcap[, year := year(date)]
# tmp_mcap[, quarter := quarter(date)]
tmp_mcap = fread(input = 'all_tmp_mcap.csv', colClasses = c("character","Date","character","character","character","numeric",
                                                            "integer","numeric","numeric","numeric","numeric"))
setkey(tmp_mcap,date,PERMNO)
tmp_mcap[, quarter := quarter(date)]
tmp_mcap[, quarter_mcap := mean(month_mcap, na.rm = TRUE), by = c("year","quarter","PERMNO")]
tmp_mcap = tmp_mcap[!is.na(quarter_mcap)]
tmp_mcap = merge(tmp_mcap,subset(q_crsp_permnos,select=c("year","quarter","PERMNO")),by=c("year","quarter","PERMNO"))
tmp_mcap = unique(tmp_mcap,by=c("year","quarter","PERMNO"))
tmp_mcap = setorder(setDT(tmp_mcap), year,quarter, -quarter_mcap)[, indx := seq_len(.N), by = c("year","quarter")][indx <= 500]
q_crsp_permnos = NULL

gc()

tmp_crsp = subset(tmp_crsp,select = c("PERMNO","date","year","quarter","SICCD","TSYMBOL","PRC","RET","RETX","vwretd",
                                      "vwretx"))
tmp_mcap = subset(tmp_mcap,select = c("PERMNO","year","quarter","quarter_mcap"))

gc()

data = merge(tmp_crsp,tmp_mcap,by=c("year","quarter","PERMNO"))
setkey(data,date,PERMNO)
data[, weight := quarter_mcap / sum(unique(quarter_mcap)), by = c("year","quarter")]
q_asset_count = data[, (num_assets = length(unique(PERMNO))), by = c("year","quarter")]
q_min_weight = data[, (min_w = min(weight)), by = c("year","quarter")]
q_max_weight = data[, (max_w = max(weight)), by = c("year","quarter")]
tmp_crsp = NULL
tmp_mcap = NULL
gc()

m_crsp = fread(input = 'a85c415bc7ca7abb.csv')
m_crsp[, DATE := as.character(DATE)]
m_crsp[, DATE := as.Date(DATE,format="%Y%m%d")]
m_crsp[, quarter := quarter(DATE)]
m_crsp[, year := year(DATE)]
m_crsp[, c("vwretd","vwretx","sprtrn") := lapply(.SD,log1p), .SDcols = c("vwretd","vwretx","sprtrn")]
m_crsp[, c("vwretd.tp1","vwretx.tp1","sprtrn.tp1") := lapply(.SD,shift,type="lead"), .SDcols = c("vwretd","vwretx","sprtrn")]
m_crsp[!is.na(vwretd.tp1), c("vwretd.tp3","vwretx.tp3") := lapply(.SD,function(x){
  shift(runSum(x,n = 3),type="lead",n=2)}), .SDcol = c("vwretd.tp1","vwretx.tp1")]
setorder(m_crsp,-DATE)
m_crsp = unique(m_crsp,by=c("year","quarter"))
data = merge(data,m_crsp,by=c("year","quarter"),suffixes = c(".daily",".quarterly"))

setkey(data,date,PERMNO)
setkey(data,year,quarter)

data[, c("avg_var","avg_cor","mkt_var","avg_cor_ta") := as.list(cor_var(.SD)), 
     .SDcols = c("date","PERMNO","RET","weight","vwretd.daily"), by = c("year","quarter")]
data[, month := month(DATE)]

q_data = unique(subset(data,subset = month %in% c(12,3,6,9),
                       select = c("year","month","quarter","avg_var","avg_cor","mkt_var","avg_cor_ta",
                                  "vwretd.tp3","vwretx.tp3")),by=c("year","quarter"))
data = NULL
gc()

tbill3 = fread(input = 'TB3MS.csv')
tbill3[, TB3MS := log1p(TB3MS/100)]
tbill3[, DATE := as.Date(DATE,format="%Y-%m-%d")]
tbill3[, year := year(DATE)]
tbill3[, month := month(DATE)]
tbill3[, DATE := NULL]

q_data = merge(q_data,tbill3,by=c("year","month"),all.x=TRUE)
q_data[, TB3_lag := shift(TB3MS)]
q_data[, logxret.tp3 := vwretd.tp3 - TB3_lag]
