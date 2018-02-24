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
tmp_crsp[, q_tdays := length(unique(date)), by=c("year","quarter")]
q_tday = tmp_crsp[, .(tdays = unique(tdays)), by =c("year","quarter")]
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
tmp_mcap[, quarter := quarter(date)]
tmp_mcap[, quarter_mcap := mean(month_mcap, na.rm = TRUE), by = c("year","quarter","PERMNO")]
tmp_mcap = tmp_mcap[!is.na(quarter_mcap)]
tmp_mcap = merge(tmp_mcap,subset(q_crsp_permnos,select=c("year","month","PERMNO")),by=c("year","month","PERMNO"))

gc()


data[, q_tdays := length(unique(date)), by=c("year","quarter")]
q_tday = data[, .(tdays = unique(tdays)), by =c("year","quarter")]
data[, q_asset_tdays := length(na.omit(RET)), by=c("year","quarter","PERMNO")]
data[, q_not_zero := (!sum(RET==0)==length(RET)),by=c("year","quarter","PERMNO")]
data[, all_quarter := q_tdays==q_asset_tdays]
data[, weight := month_mcap / sum(month_mcap), by = c("year","month")]
data[, q_weight := mean(weight,na.rm=TRUE), by = c("year","quarter","PERMNO")] # quarter mcap mean of monthly mcaps
asset_count = data[, .(count = length(unique(PERMNO))), by = c("year","month")]
max_weight = data[, .(maxw = max(weight)), by = c("year","month")]
min_weight = data[, .(minw = min(weight)), by = c("year","month")]