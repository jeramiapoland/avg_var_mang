paks <- c("RCurl","data.table","tis","lubridate","ggplot2","stringr","sandwich","stargazer","pracma","RColorBrewer",
          "CADFtest","complexplus","readxl","reshape2","quantmod","xlsx","tikzDevice","MASS","timeSeries","vars","PortfolioEffectHFT",
          "PortfolioAnalytics","PerformanceAnalytics","backtest","tidyr","broom","stringdist","BH","parallel","doMC","foreach",
          "doParallel","lmtest","hypergeo","strucchange","formula.tools","multiwave","outliers","forecast","SharpeR","fastmatch",
          "bvarsv","boot","goftest","DescTools","dunn.test","generalCorr") 
# note: tikzDevice requires a working latex installation
# and xlsx require rJava so a properly configured java (try javareconf)
for (p in paks){
  require(p,character.only = TRUE) || {install.packages(p) 
    require(p,character.only = TRUE)}
}
source(file = 'functions.R')

#### Quarterly Data ####
# tmp_crsp = fread(input = '../../data/CRSP/crsp_daily_long.csv',colClasses = "character")
tmp_crsp = fread(input = '../../data/CRSP/ced98c65aea26fb3.csv', colClasses = "character")
setkey(tmp_crsp,date,PERMNO)
tmp_crsp[, date := as.Date(as.character(date),format="%Y%m%d")]
setkey(tmp_crsp,date,PERMNO)
tmp_crsp[, year := year(date)]
tmp_crsp = tmp_crsp[year <= 2016]
tmp_crsp[, month := month(date)]
tmp_crsp[, PRC := as.numeric(PRC)]
tmp_crsp[PRC < 0, PRC := abs(PRC)]
tmp_crsp[, RET := as.numeric(RET)]
setkey(tmp_crsp,PERMNO,date)
# tmp_crsp[, ALTRET := ROC(PRC,type = "discrete"), by = PERMNO]
# tmp_crsp[is.na(RET)&!is.na(ALTRET)] # no simple fix for missing returns
tmp_crsp[, RETX := as.numeric(RETX)]
tmp_crsp[, DLRET := as.numeric(DLRET)]
tmp_crsp[, DLRETX := as.numeric(DLRETX)]
tmp_crsp[is.na(RET), RET := DLRET]
tmp_crsp[is.na(RETX), RETX := DLRETX]
tmp_crsp[, vwretd := as.numeric(vwretd)]
tmp_crsp[, vwretx := as.numeric(vwretx)]
d_returns = unique(subset(tmp_crsp,select=c("date","vwretd","vwretx")),by="date")
fwrite(tmp_crsp,file = 'all_tmp_crsp.csv')
setkey(tmp_crsp,date,PERMNO)
tmp_crsp[, quarter := quarter(date)]
tmp_crsp[, q_tdays := length(unique(date)), by=c("year","quarter")]
q_tday = tmp_crsp[, .(q_tdays = unique(q_tdays)), by =c("year","quarter")]
avg_qtdays = mean(q_tday$q_tdays)
qd_adj = round(avg_qtdays)
tmp_crsp[, q_asset_tdays := length(na.omit(RET)), by=c("year","quarter","PERMNO")]
tmp_crsp[, q_not_zero := (!sum(RET==0)==length(RET)),by=c("year","quarter","PERMNO")]
tmp_crsp[, all_quarter := q_tdays==q_asset_tdays]
tmp_crsp = subset(tmp_crsp,subset= (all_quarter & q_not_zero))
gc()
setkey(tmp_crsp,date,PERMNO)
setkey(tmp_crsp,year,quarter)
q_crsp_permnos = tmp_crsp[, .(PERMNO = unique(PERMNO), num_PERMNO = length(unique(PERMNO))), by = c("year","quarter")]

# bring monthly market caps
tmp_mcap = fread(input = '9cfd0c24e4fad219.csv',colClasses = "character")
tmp_mcap[, date := as.Date(date,format="%Y%m%d")]
setkey(tmp_mcap,date,PERMNO)
tmp_mcap[, PRC := as.numeric(PRC)]
tmp_mcap[, ALTPRC := as.numeric(ALTPRC)]
tmp_mcap[, SHROUT := as.integer(SHROUT)]
tmp_mcap[is.na(PRC), PRC := ALTPRC]
tmp_mcap[, PRC := abs(PRC)]
tmp_mcap[, month_mcap := PRC * SHROUT]
tmp_mcap[, year := year(date)]
tmp_mcap[, month := month(date)]
fwrite(x = tmp_mcap,file = 'all_tmp_mcap.csv')
tmp_mcap = tmp_mcap[!is.na(month_mcap)]
setkey(tmp_mcap,year,month,PERMNO)
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

# monthly returns
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
q_crsp = unique(m_crsp,by=c("year","quarter"))
data = merge(data,q_crsp,by=c("year","quarter"),suffixes = c(".daily",".quarterly"))

setkey(data,date,PERMNO)
setkey(data,year,quarter)
data[, month := month(DATE)]

#### quarterly cor ####
data[, c("avg_var","avg_cor","mkt_var") := as.list(cor_var(.SD)), .SDcols = c("date","PERMNO","RET","weight","vwretd.daily"), 
     by = c("year","quarter")]


q_data = unique(subset(data,subset = month %in% c(12,3,6,9),
                       select = c("year","month","quarter","avg_var","avg_cor","mkt_var","vwretd.tp3","vwretx.tp3")),
                by=c("year","quarter"))
data = NULL
gc()

# quarterly risk free rate 
tbill3 = fread(input = 'TB3MS.csv')
tbill3[, TB3MS := log1p(TB3MS/100)/4]
tbill3[, DATE := as.Date(DATE,format="%Y-%m-%d")]
tbill3[, year := year(DATE)]
tbill3[, month := month(DATE)]
tbill3[, DATE := NULL]

q_data = merge(q_data,tbill3,by=c("year","month"),all.x=TRUE)
q_data[, TB3_lag := shift(TB3MS)]
q_data[, logxret.tp3 := vwretd.tp3 - TB3_lag]

# q_data = merge(q_data,subset(m_data,select=c("year","month","avg_cor3m","avg_var3m","mkt_var3m","vwretd.tp1","vwretx.tp1",
#                                              "logxret.tp1"),by=c("year","month")),all.x=TRUE,
#                suffixes = c(".quarterly",".monthly"))
#### Quarterly data file ####
fwrite(q_data,file = 'q_data.csv')

#### Monthly Data ####
# bring crsp back
tmp_crsp = fread(input = 'all_tmp_crsp.csv',colClasses = c("character","Date","character","character","character","numeric",
                                                           "numeric","numeric","numeric","character","numeric","numeric","numeric",
                                                           "character","character","character","numeric","numeric")
)
setkey(tmp_crsp,year,month,date,PERMNO)
market_daily = unique(subset(tmp_crsp,select=c("year","month","vwretd","vwretx","ewretd","ewretx","sprtrn"),by=c("year","month")))
tmp_crsp[, tdays := length(unique(date)), by=c("year","month")]
m_tday = tmp_crsp[, .(tdays = unique(tdays)), by =c("year","month")]
tmp_crsp[, asset_tdays := length(na.omit(RET)), by=c("year","month","PERMNO")]
tmp_crsp[, all_month := tdays==asset_tdays]
tmp_crsp[, not_zero := (!sum(RET==0)==length(RET)),by=c("year","month","PERMNO")]
# tmp_crsp = subset(tmp_crsp,subset= (all_month & not_zero))
tmp_crsp[, all_traded := (all_month & not_zero)]
gc()
# setkey(tmp_crsp,date,PERMNO)
# setkey(tmp_crsp,year,month)
# m_crsp_permnos = tmp_crsp[all_traded==1, .(PERMNO = unique(PERMNO), num_PERMNO = length(unique(PERMNO))), by = c("year","month")]

setkey(tmp_crsp,PERMNO,date)
#tmp_crsp[, rVar := c_var_run(RET,qd_adj), by = PERMNO]
setkey(tmp_crsp,PERMNO,year,month)
#tmp_crsp[, rVar := if(anyNA(rVar)){NA_real_} else {rVar}, by = c("PERMNO","year","month")]
m_assets = tmp_crsp[all_traded == 1, (num_assets = length(unique(PERMNO))), by = c("year","month")]
m_crsp_permnos = tmp_crsp[all_traded == 1, .(PERMNO = unique(PERMNO), num_PERMNO = length(unique(PERMNO))), by = c("year","month")]


# monthly market cap #
tmp_mcap = fread(input = 'all_tmp_mcap.csv', colClasses = c("character","Date","character","character","character","numeric",
                                                            "integer","numeric","numeric","numeric","numeric"))
setkey(tmp_mcap,date,PERMNO)
tmp_mcap = tmp_mcap[!is.na(month_mcap)]
setkey(tmp_mcap,year,month,PERMNO)
m_tmp_mcap = merge(tmp_mcap,subset(m_crsp_permnos,select=c("year","month","PERMNO")),by=c("year","month","PERMNO"))
m_tmp_mcap = setorder(setDT(m_tmp_mcap), year,month, -month_mcap)[, indx := seq_len(.N), by = c("year","month")][indx <= 500]
tmp_mcap = unique(tmp_mcap,by=c("year","month","PERMNO"))
gc()
tmp_crsp = subset(tmp_crsp,select = c("PERMNO","date","year","month","SICCD","TSYMBOL","PRC","RET","RETX","vwretd",
                                      "vwretx"))
m_tmp_mcap = subset(m_tmp_mcap,select = c("PERMNO","year","month","month_mcap"))
gc()

# merge
data = merge(tmp_crsp,m_tmp_mcap,by=c("year","month","PERMNO"))
m_assets = data[, (num_assets = length(unique(PERMNO))), by = c("year","month")]
data[, weight := month_mcap / sum(unique(month_mcap)), by = c("year","month")]
setkey(data,date,PERMNO)
# m_asset_count = data[, (num_assets = length(unique(PERMNO))), by = c("year","month")]
# m_min_weight = data[, (min_w = min(weight)), by = c("year","month")]
# m_max_weight = data[, (max_w = max(weight)), by = c("year","month")]
tmp_crsp = NULL
tmp_mcap = NULL
m_tmp_mcap = NULL
m_crsp_permnos = NULL
gc()

# monthly index returns
m_crsp = fread(input = 'a85c415bc7ca7abb.csv')
m_crsp[, year := as.integer(substr(as.character(DATE),1,4))]
m_crsp[, month := as.integer(substr(as.character(DATE),5,6))]
m_crsp[, c("vwretd","vwretx","sprtrn") := lapply(.SD,log1p), .SDcols = c("vwretd","vwretx","sprtrn")]
m_crsp[, c("vwretd.tp1","vwretx.tp1","sprtrn.tp1") := lapply(.SD,shift,type="lead"), .SDcols = c("vwretd","vwretx","sprtrn")]
m_crsp[, quarter := quarter(as.Date(as.character(DATE),format="%Y%m%d"))]
m_crsp[!is.na(vwretd.tp1), c("vwretd.tp3","vwretx.tp3") := lapply(.SD,function(x){
  shift(runSum(x,n = 3),type="lead",n=2)}), .SDcol = c("vwretd.tp1","vwretx.tp1")]
m_crsp[!is.na(vwretd.tp1), c("vwretd.tp6","vwretx.tp6") := lapply(.SD,function(x){
  shift(runSum(x,n = 6),type="lead",n=5)}), .SDcol = c("vwretd.tp1","vwretx.tp1")]
m_crsp[!is.na(vwretd.tp1), c("vwretd.tp12","vwretx.tp12") := lapply(.SD,function(x){
  shift(runSum(x,n = 12),type="lead",n=11)}), .SDcol = c("vwretd.tp1","vwretx.tp1")]
data = merge(data,m_crsp,by=c("year","month"),all.x=TRUE,suffixes = c(".daily",".monthly"))

gc()

#### monthly cor ####
setkey(data,PERMNO,date)

# data = subset(data,subset = !is.na(rVar))
# data[, rVar := last(rVar[rVar>0]), by = c("PERMNO","year","month")]

# u_d  = unique(subset(data,select = c("year","month","date","vwretd.daily")),by="date")
# setkey(u_d,date)
# u_d[, rMVar := c_var_run(vwretd.daily,qd_adj)]
# u_d[, mkt_var3m := last(rMVar[rMVar>0]) * qd_adj, by = c("year","month")]
# u_d = unique(subset(u_d,select=c("year","month","mkt_var3m")),by=c("year","month"))
# 
# data = merge(data,subset(u_d,select = c("year","month","mkt_var3m")),by=c("year","month"))

# u_mcap = unique(subset(data,select = c("year","month","PERMNO","month_mcap")), by = c("year","month","PERMNO"))
# u_mcap = setorder(setDT(u_mcap), year,month, -month_mcap)[, indx := seq_len(.N), by = c("year","month")][indx <= 500]
# 
# data = merge(data,subset(u_mcap,select=c("year","month","PERMNO")),by=c("year","month","PERMNO"))
# 
# data[, weight := month_mcap / sum(unique(month_mcap)), by = c("year","month")]
# m3_min_weight = data[, (min_w = min(weight)), by = c("year","month")]
# m3_max_weight = data[, (max_w = max(weight)), by = c("year","month")]
# m3_assets = data[,.(assets = length(unique(PERMNO))),by = c("year","month")]

setkey(data,date,PERMNO)
setkey(data,year,month,PERMNO)
#### 1 month variance statistics ####
data[, c("avg_var1m","avg_cor1m","mkt_var1m") := as.list(cor_var(.SD)), .SDcols = c("date","PERMNO","RET","weight","vwretd.daily"), 
                             by = c("year","month")]

data = unique(data,by=c("year","month","PERMNO"))
# data[, avg_var3m := sum(weight * rVar) * qd_adj,by = c("year","month")]


m_data = unique(subset(data,select = c("year","month","avg_var1m","avg_cor1m","mkt_var1m","vwretd.monthly","vwretd.tp1",
                                       "vwretx.tp1","month_mcap")),by=c("year","month"))


ff_data = fread(input = '../../value_momentum_spread/F-F_Research_Data_Factors.CSV')
ff_data[, V1 := as.character(V1)]
ff_data[, year := as.integer(substr(V1,1,4))]
ff_data[, month := as.integer(substr(V1,5,6))]
ff_data[, SMB := SMB / 100]
ff_data[, HML := HML / 100]
ff_data[, RF := RF / 100]
ff_data[, RF_lag := shift(RF)]
ff_data[, Mkt_RF := `Mkt-RF` / 100]
ff_data[, `Mkt-RF` := NULL]
ff_data[, c("Mkt_RF","SMB","HML","RF","RF_lag") := lapply(.SD,log1p), .SDcols = c("Mkt_RF","SMB","HML","RF","RF_lag")]

m_data = merge(m_data,subset(ff_data,select=c("year","month","Mkt_RF","SMB","HML","RF","RF_lag")),by=c("year","month"))
m_data[, logxret.tp1 := vwretd.tp1 - RF_lag]
m_data[, logxret := vwretd.monthly - RF_lag]
# m_data[, avg_cor3m := mkt_var3m / avg_var3m]
# m_data[, avg_cor3m := aprox_adj_cor(avg_cor3m,qd_adj)]
#### Monthly data file ####
fwrite(x = m_data,file = 'm_data.csv')
data = NULL
gc()


#### summary stats ####
pw_start = which(q_data$year == 1962 & q_data$quarter == 2)
pw_end = which(q_data$year == 2007 & q_data$quarter == 1)
s1 = q_data[pw_start:pw_end, .(RET = logxret.tp3 * 100, AC = avg_cor, AV = avg_var * 100, SV = mkt_var * 100)]
stargazer(s1,summary = TRUE,out = 'summary1.tex',out.header = FALSE)
s1_auto = s1[, .(autoC = lapply(.SD,get_ac,1)), .SDcols = colnames(s1)]

paper_q_start = pw_start
paper_m_start = which(m_data$year==1926&m_data$month==7)
paper_1962_start = which(m_data$year==1962&m_data$month==6)

# s2 = q_data[paper_q_start:nrow(q_data), .(RET = logxret.tp3 * 100, AC = avg_cor, AV = avg_var * 100, SV = mkt_var * 100)]
# stargazer(s2,summary = TRUE,out = 'summary2.tex',out.header = FALSE)
# s2_auto = s2[!is.na(RET), .(autoC = lapply(.SD,get_ac,1)), .SDcols = colnames(s2)]

m_data[1:(nrow(m_data)-1), logxret.tp3 := shift(runSum(logxret.tp1,n=3),n=2,type="lead")]
m_data[1:(nrow(m_data)-1), logxret.tp6 := shift(runSum(logxret.tp1,n=6),n=5,type="lead")]
m_data[1:(nrow(m_data)-1), logxret.tp12 := shift(runSum(logxret.tp1,n=12),n=11,type="lead")]
s3 = m_data[(paper_m_start+1):nrow(m_data), .("RET" = logxret * 100,AC = avg_cor1m, AV = avg_var1m * 100, SV = mkt_var1m * 100)]
stargazer(s3,summary = TRUE,out = 'summary3.tex')
s3_auto = s3[!is.na(RET), .(autoC = lapply(.SD,get_ac,1)), .SDcols = colnames(s3)]

s4 = m_data[paper_1962_start:nrow(m_data), .("RET" = logxret * 100,AC = avg_cor1m, AV = avg_var1m * 100, SV = mkt_var1m * 100)]
stargazer(s4,summary = TRUE,out = 'summary4.tex')
s4_auto = s4[!is.na(RET), .(autoC = lapply(.SD,get_ac,1)), .SDcols = colnames(s3)]


#### time series plots ####
tmpPalette = c(brewer.pal(name = "Set1",3))
cbPalette = c(tmpPalette[1:2],rep(tmpPalette[1:3],2))
cbPalette = c("#000000",cbPalette,tail(cbPalette,3))
names(cbPalette) = c("market","vol_mang","av_mang","mkt_var","avg_var","avg_cor",
                     "mkt_var3m","avg_var3m","avg_cor3m","mkt_var1m","avg_var1m","avg_cor1m")
linePalette <- c("solid", "dotted", "longdash","longdash","dotdash","dotted","longdash","dotdash","dotted",
                 "longdash","dotdash","dotted")
names(linePalette) <- c("market","vol_mang","av_mang","avg_var","avg_cor","mkt_var",
                        "avg_var3m","avg_cor3m","mkt_var3m","avg_var1m","avg_cor1m","mkt_var1m")
plotq = melt(q_data[paper_q_start:nrow(q_data)],id.vars = c("year","month","quarter"),measure.vars = c("avg_var","avg_cor","mkt_var"),
             variable.name = "stat")
plotq[, date := as.Date(paste0(year,"-",month,"-28"))]
plotq[stat=="avg_var", value := value * 100]
plotq[stat=="mkt_var", value := value * 100]
y_min = min(plotq$value)*.9
y_max = max(plotq$value)*1.1

q_plot = ggplot(data=plotq) + geom_line(mapping = aes(x=date,y=value,color=stat,linetype=stat)) +
  labs(title = "Quarterly Measures of Daily Return Statistics\n", x = "", y = "") + 
  scale_x_date(date_breaks = "5 year", date_labels =  "%Y") +
  scale_colour_manual(name = "Statistic",values=cbPalette,labels =c("AV","AC","SV")) + 
  scale_linetype_manual(name = "Statistic",values=linePalette,labels =c("AV","AC","SV")) +
  theme(text = element_text(size=11),panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line( size=.1, color="black"))+ theme_bw()
q_plot = tis::nberShade(q_plot,xrange = c(min(plotq$date), max(plotq$date)),openShade = FALSE)
tikz("q_plot.tex",width = 5.90551, height = 3,sanitize = FALSE)
plot(q_plot)
dev.off()

# plotm = melt(m_data[paper_m_start:nrow(m_data)],id.vars = c("year","month"),measure.vars = c("avg_var3m","avg_cor3m","mkt_var3m"),
#              variable.name = "stat")
# plotm[, date := as.Date(paste0(year,"-",month,"-28"))]
# plotm[stat=="avg_var3m", value := value * 100]
# plotm[stat=="mkt_var3m", value := value * 100]
# m_plot = ggplot(data=plotm) + geom_line(mapping = aes(x=date,y=value,color=stat,linetype=stat)) +
#   labs(title = "Rolling 3 Month Measures of Daily Return Statistics\n", x = "", y = "") + 
#   scale_x_date(date_breaks = "5 year", date_labels =  "%Y") +
#   scale_colour_manual(name = "Statistic",values=cbPalette,labels =c("AV","AC","SV")) + 
#   scale_linetype_manual(name = "Statistic",values=linePalette,labels =c("AV","AC","SV")) +
#   theme(text = element_text(size=11),panel.grid.major.x = element_blank(),
#         panel.grid.major.y = element_line( size=.1, color="black")) +
#   #coord_cartesian(ylim = c(-.03,22)) + 
#   theme_bw()
# m_plot = tis::nberShade(m_plot,xrange = c(min(plotm$date), max(plotm$date)),openShade = FALSE)
# tikz("m_plot.tex",width = 5.90551, height = 3, sanitize = FALSE)
# plot(m_plot)
# dev.off()

plotm2 = melt(m_data[paper_m_start:nrow(m_data)],id.vars = c("year","month"),measure.vars = c("avg_var1m","avg_cor1m","mkt_var1m"),
             variable.name = "stat")
plotm2[, date := as.Date(paste0(year,"-",month,"-28"))]
plotm2[stat=="avg_var1m", value := value * 100]
plotm2[stat=="mkt_var1m", value := value * 100]
m_plot2 = ggplot(data=plotm2) + geom_line(mapping = aes(x=date,y=value,color=stat,linetype=stat)) +
  labs(title = "Monthly Measures of Daily Return Statistics\n", x = "", y = "") + 
  scale_x_date(date_breaks = "5 year", date_labels =  "%Y") +
  scale_colour_manual(name = "Statistic",values=cbPalette,labels =c("AV","AC","SV")) + 
  scale_linetype_manual(name = "Statistic",values=linePalette,labels =c("AV","AC","SV")) +
  theme(text = element_text(size=11),panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line( size=.1, color="black"))+ theme_bw()
m_plot2 = tis::nberShade(m_plot2,xrange = c(min(plotm2$date), max(plotm2$date)),openShade = FALSE)
  # ylim((min(plotq$value)-.1*min(plotq$value)), (max(plotq$value)+.1*max(plotq$value)))
tikz("m_plot2.tex",width = 5.90551, height = 3, sanitize = FALSE)
plot(m_plot2)
dev.off()

#### regressions ####
q_data[, mkt_var.tp1 := shift(mkt_var,type = "lead")]
q_data[, avg_var.tp1 := shift(avg_var,type = "lead")]
#m_data[, mkt_var3m.tp1 := shift(mkt_var3m,type = "lead")]
#m_data[, mkt_var3m.tp3 := shift(mkt_var3m,type = "lead",n=3)]
m_data[, mkt_var1m.tp1 := shift(mkt_var1m,type = "lead")]
#m_data[, avg_var3m.tp1 := shift(avg_var3m,type = "lead")]
#m_data[, avg_var3m.tp3 := shift(avg_var3m,type = "lead",n=3)]
m_data[, avg_var1m.tp1 := shift(avg_var1m,type = "lead")]
# market variance
# replication
q_var_in1 = lm(mkt_var ~ avg_cor,q_data[pw_start:pw_end])
q_var_in2 = lm(mkt_var ~ avg_var,q_data[pw_start:pw_end])
q_var_in3 = lm(mkt_var ~ avg_cor + avg_var,q_data[pw_start:pw_end])
q_var_in4 = lm(mkt_var ~ (avg_cor:avg_var),q_data[pw_start:pw_end])
stargazer(q_var_in1,q_var_in2,q_var_in3,q_var_in4,out.header = FALSE,covariate.labels = c("AC","AV","AC * AV"),
          dep.var.labels = "SV",
          out = 'tab_var_rep_1.tex')
# q_var_in1 = lm(mkt_var ~ avg_cor,q_data[paper_q_start:nrow(q_data)])
# q_var_in2 = lm(mkt_var ~ avg_var,q_data[paper_q_start:nrow(q_data)])
# q_var_in3 = lm(mkt_var ~ avg_cor + avg_var,q_data[paper_q_start:nrow(q_data)])
# q_var_in4 = lm(mkt_var ~ (avg_cor:avg_var),q_data[paper_q_start:nrow(q_data)])
# stargazer(q_var_in1,q_var_in2,q_var_in3,q_var_in4,out.header = FALSE,covariate.labels = c("AC","AV","AC * AV"),
#           dep.var.labels = "SV",
#           out = 'tab_var_rep_2.tex')
# q_var_in5 = lm(avg_var ~ avg_cor,q_data[pw_start:pw_end])
# q_var_in6 = lm(avg_var ~ mkt_var,q_data[pw_start:pw_end])
# q_var_in7 = lm(avg_var ~ avg_cor + mkt_var,q_data[pw_start:pw_end])
# q_var_in8 = lm(avg_var ~ (avg_cor:mkt_var),q_data[pw_start:pw_end])
# stargazer(q_var_in5,q_var_in6,q_var_in7,q_var_in8,out.header = FALSE,covariate.labels = c("AC","SV","AC * SV"),
#           dep.var.labels = "SV",
#           out = 'tab_var_rep_3.tex')
# # expansion
# q_var_in5 = lm(mkt_var ~ avg_cor,q_data)
# q_var_in6 = lm(mkt_var ~ avg_var,q_data)
# q_var_in7 = lm(mkt_var ~ avg_cor + avg_var,q_data)
# q_var_in8 = lm(mkt_var ~ (avg_cor:avg_var),q_data)
# stargazer(q_var_in5,q_var_in6,q_var_in7,q_var_in8,out.header = FALSE,covariate.labels = c("AC","AV","AC * AV"),
#           dep.var.labels = "SV",
#           out = 'tab_var2.tex')
# monthly
# m_var_in1 = lm(mkt_var3m ~ avg_cor3m,m_data[paper_m_start:nrow(m_data)])
# m_var_in2 = lm(mkt_var3m ~ avg_var3m,m_data[paper_m_start:nrow(m_data)])
# m_var_in3 = lm(mkt_var3m ~ avg_cor3m + avg_var3m,m_data[paper_m_start:nrow(m_data)])
# m_var_in4 = lm(mkt_var3m ~ (avg_cor:avg_var),m_data[paper_m_start:nrow(m_data)])
# stargazer(m_var_in1,m_var_in2,m_var_in3,m_var_in4,out.header = FALSE,covariate.labels = c("AC","AV","AC * AV"),
#           dep.var.labels = "SV",
#           out = 'tab_var3.tex')
# market variance (t+1)
# replication
q_var_in9 = lm(mkt_var.tp1 ~ avg_cor,q_data[pw_start:pw_end])
q_var_in10 = lm(mkt_var.tp1 ~ avg_var,q_data[pw_start:pw_end])
q_var_in11 = lm(mkt_var.tp1 ~ avg_cor + avg_var,q_data[pw_start:pw_end])
#q_var_in12 = lm(mkt_var.tp1 ~ (avg_cor:avg_var),q_data[pw_start:pw_end])
q_var_in13 = lm(mkt_var.tp1 ~ mkt_var,q_data[pw_start:pw_end])
q_var_in20 = lm(mkt_var.tp1 ~ mkt_var + avg_var,q_data[pw_start:pw_end])
stargazer(q_var_in9,q_var_in10,q_var_in11,q_var_in13,q_var_in20,out.header = FALSE,
          covariate.labels = c("AC$_{t}$","AV$_{t}$","SV$_{t}$"),
          dep.var.labels = "SV$_{t+1}$",
          out = 'tab_var_rep_4.tex')
q_var_in19 = lm(avg_var.tp1 ~ avg_cor,q_data[pw_start:pw_end])
q_var_in110 = lm(avg_var.tp1 ~ avg_var,q_data[pw_start:pw_end])
q_var_in111 = lm(avg_var.tp1 ~ avg_cor + avg_var,q_data[pw_start:pw_end])
#q_var_in12 = lm(avg_var.tp1 ~ (avg_cor:avg_var),q_data[pw_start:pw_end])
q_var_in113 = lm(avg_var.tp1 ~ avg_var,q_data[pw_start:pw_end])
q_var_in120 = lm(avg_var.tp1 ~ mkt_var + avg_var,q_data[pw_start:pw_end])
stargazer(q_var_in19,q_var_in110,q_var_in113,q_var_in113,q_var_in120,out.header = FALSE,
          covariate.labels = c("AC$_{t}$","AV$_{t}$","SV$_{t}$"),
          dep.var.labels = "AV$_{t+1}$",
          out = 'tab_var_rep_5.tex')
# expansion
# q_var_in14 = lm(mkt_var.tp1 ~ avg_cor,q_data)
# q_var_in15 = lm(mkt_var.tp1 ~ avg_var,q_data)
# q_var_in16 = lm(mkt_var.tp1 ~ avg_cor + avg_var,q_data)
# # q_var_in17 = lm(mkt_var.tp1 ~ (avg_cor:avg_var),q_data)
# q_var_in18 = lm(mkt_var.tp1 ~ mkt_var,q_data)
# q_var_in22 = lm(mkt_var.tp1 ~ mkt_var + avg_var,q_data)
# stargazer(q_var_in14,q_var_in15,q_var_in18,q_var_in22,q_var_in18,out.header = FALSE,
#           covariate.labels = c("AC$_{t}$","AV$_{t}$","SV$_{t}$"),
#           dep.var.labels = "SV$_{t+1}$",
#           out = 'tab_var5.tex')
# q_var_in114 = lm(avg_var.tp1 ~ avg_cor,q_data)
# q_var_in115 = lm(avg_var.tp1 ~ avg_var,q_data)
# q_var_in116 = lm(avg_var.tp1 ~ avg_cor + avg_var,q_data)
# # q_var_in117 = lm(avg_var.tp1 ~ (avg_cor:avg_var),q_data)
# q_var_in118 = lm(avg_var.tp1 ~ avg_var,q_data)
# q_var_in122 = lm(avg_var.tp1 ~ mkt_var + avg_var,q_data)
# stargazer(q_var_in114,q_var_in115,q_var_in118,q_var_in122,q_var_in118,out.header = FALSE,
#           covariate.labels = c("AC$_{t}$","AV$_{t}$","SV$_{t}$"),
#           dep.var.labels = "AV$_{t+1}$",
#           out = 'tab_var6.tex')
# monthly
# m_var_in5 = lm(mkt_var3m.tp1 ~ avg_cor3m,m_data)
# m_var_in6 = lm(mkt_var3m.tp1 ~ avg_var3m,m_data)
# # m_var_in7 = lm(mkt_var3m.tp1 ~ avg_cor + avg_var3,,m_data)
# # m_var_in8 = lm(mkt_var3m.tp1 ~ (avg_cor:avg_var3,),m_data)
# m_var_in9 = lm(mkt_var3m.tp1 ~ mkt_var3m,m_data)
# m_var_in24 = lm(mkt_var3m.tp1 ~ mkt_var3m + avg_var3m,m_data)
# # m_var_in44 = lm(mkt_var3m.tp1 ~ mkt_var1m,m_data)
# # m_var_in45 = lm(mkt_var3m.tp1 ~ mkt_var1m + avg_var3m,m_data)
# stargazer(m_var_in5,m_var_in6,m_var_in9,m_var_in24,out.header = FALSE,
#           covariate.labels = c("AC$_{t}$","AV$_{t}$","SV$_{t}$","$SV^{1}_{t}$"),
#           dep.var.labels = "SV$_{t+1}$",
#           out = 'tab_var7.tex')
# monthly
m_var_in5 = lm(mkt_var1m.tp1 ~ avg_cor1m,m_data[paper_1962_start:nrow(m_data)])
m_var_in6 = lm(mkt_var1m.tp1 ~ avg_var1m,m_data[paper_1962_start:nrow(m_data)])
m_var_in7 = lm(mkt_var1m.tp1 ~ avg_cor1m + avg_var1m,m_data[paper_1962_start:nrow(m_data)])
# m_var_in8 = lm(mkt_var1m.tp1 ~ (avg_cor:avg_var3,),m_data)
m_var_in9 = lm(mkt_var1m.tp1 ~ mkt_var1m,m_data[paper_1962_start:nrow(m_data)])
m_var_in24 = lm(mkt_var1m.tp1 ~ mkt_var1m + avg_var1m,m_data[paper_1962_start:nrow(m_data)])
stargazer(m_var_in5,m_var_in6,m_var_in9,m_var_in7,m_var_in24,out.header = FALSE,
          covariate.labels = c("AC$_{t}$","AV$_{t}$","SV$_{t}$"),
          dep.var.labels = "SV$_{t+1}$",
          out = 'tab_var8.tex')

m_var_in75 = lm(avg_var1m.tp1 ~ avg_cor1m,m_data[paper_1962_start:nrow(m_data)])
m_var_in76 = lm(avg_var1m.tp1 ~ avg_var1m,m_data[paper_1962_start:nrow(m_data)])
m_var_in77 = lm(avg_var1m.tp1 ~ avg_cor1m + avg_var1m,m_data[paper_1962_start:nrow(m_data)])
# m_var_in8 = lm(mkt_var1m.tp1 ~ (avg_cor:avg_var3,),m_data)
m_var_in79 = lm(avg_var1m.tp1 ~ mkt_var1m,m_data[paper_1962_start:nrow(m_data)])
m_var_in724 = lm(avg_var1m.tp1 ~ mkt_var1m + avg_var1m,m_data[paper_1962_start:nrow(m_data)])
stargazer(m_var_in75,m_var_in76,m_var_in79,m_var_in77,m_var_in724,out.header = FALSE,
          covariate.labels = c("AC$_{t}$","AV$_{t}$","SV$_{t}$"),
          dep.var.labels = "AV$_{t+1}$",
          out = 'tab_var78.tex')

# monthly
m_var_in05 = lm(mkt_var1m.tp1 ~ avg_cor1m,m_data[paper_m_start:paper_1962_start])
m_var_in06 = lm(mkt_var1m.tp1 ~ avg_var1m,m_data[paper_m_start:paper_1962_start])
# m_var_in7 = lm(mkt_var1m.tp1 ~ avg_cor + avg_var3,,m_data)
# m_var_in8 = lm(mkt_var1m.tp1 ~ (avg_cor:avg_var3,),m_data)
m_var_in09 = lm(mkt_var1m.tp1 ~ mkt_var1m,m_data[paper_m_start:paper_1962_start])
m_var_in024 = lm(mkt_var1m.tp1 ~ mkt_var1m + avg_var1m,m_data[paper_m_start:paper_1962_start])
stargazer(m_var_in05,m_var_in06,m_var_in09,m_var_in024,out.header = FALSE,
          covariate.labels = c("AC$_{t}$","AV$_{t}$","SV$_{t}$","$SV^{1}_{t}$"),
          dep.var.labels = "SV$_{t+1}$",
          out = 'tab_var08.tex')

# monthly
m_var_in65 = lm(mkt_var1m.tp1 ~ avg_cor1m,m_data)
m_var_in66 = lm(mkt_var1m.tp1 ~ avg_var1m,m_data)
m_var_in67 = lm(mkt_var1m.tp1 ~ avg_cor1m + avg_var1m,m_data)
# m_var_in8 = lm(mkt_var1m.tp1 ~ (avg_cor:avg_var3,),m_data)
m_var_in69 = lm(mkt_var1m.tp1 ~ mkt_var1m,m_data)
m_var_in624 = lm(mkt_var1m.tp1 ~ mkt_var1m + avg_var1m,m_data)
stargazer(m_var_in65,m_var_in66,m_var_in69,m_var_in624,out.header = FALSE,
          covariate.labels = c("AC$_{t}$","AV$_{t}$","SV$_{t}$","$SV^{1}_{t}$"),
          dep.var.labels = "SV$_{t+1}$",
          out = 'tab_var68.tex')

# m_var_in35 = lm(mkt_var3m.tp3 ~ avg_cor3m,m_data)
# m_var_in36 = lm(mkt_var3m.tp3 ~ avg_var3m,m_data)
# # m_var_in7 = lm(mkt_var3m.tp3 ~ avg_cor + avg_var3,,m_data)
# # m_var_in8 = lm(mkt_var3m.tp3 ~ (avg_cor:avg_var3,),m_data)
# m_var_in39 = lm(mkt_var3m.tp3 ~ mkt_var3m,m_data)
# m_var_in324 = lm(mkt_var3m.tp3 ~ mkt_var3m + avg_var3m,m_data)
# m_var_in344 = lm(mkt_var3m.tp3 ~ mkt_var1m,m_data)
# m_var_in345 = lm(mkt_var3m.tp3 ~ mkt_var1m + avg_var3m,m_data)
# stargazer(m_var_in35,m_var_in36,m_var_in39,m_var_in324,out.header = FALSE,
#           covariate.labels = c("AC$_{t}$","AV$_{t}$","SV$_{t}$"),
#           dep.var.labels = "SV$_{t+1}$",
#           out = 'tab_var7.tex')


#market returns (t+1)
# replication
q_ret_in1 = lm(logxret.tp3 ~ avg_cor,q_data[pw_start:pw_end])
q_ret_in2 = lm(logxret.tp3 ~ avg_var,q_data[pw_start:pw_end])
q_ret_in3 = lm(logxret.tp3 ~ avg_cor + avg_var,q_data[pw_start:pw_end])
# q_ret_in4 = lm(logxret.tp3 ~ (avg_cor:avg_var),q_data[pw_start:pw_end])
q_ret_in5 = lm(logxret.tp3 ~ mkt_var,q_data[pw_start:pw_end])
q_ret_in25 = lm(logxret.tp3 ~ mkt_var + avg_var,q_data[pw_start:pw_end])
stargazer(q_ret_in1,q_ret_in2,q_ret_in3,q_ret_in5,q_ret_in25,out.header = FALSE,
          covariate.labels = c("AC$_{t}$","AV$_{t}$","SV$_{t}$"),
          dep.var.labels = "RET$_{t+1}",
          out = 'tab_ret_rep1.tex')

# expansion
# q_ret_in6 = lm(logxret.tp3 ~ avg_cor,q_data)
# q_ret_in7 = lm(logxret.tp3 ~ avg_var,q_data)
# #q_ret_in8 = lm(logxret.tp3 ~ avg_cor + avg_var,q_data)
# #q_ret_in9 = lm(logxret.tp3 ~ (avg_cor:avg_var),q_data)
# q_ret_in10 = lm(logxret.tp3 ~ mkt_var,q_data)
# q_ret_in26 = lm(logxret.tp3 ~ mkt_var + avg_var,q_data)
# stargazer(q_ret_in6,q_ret_in7,q_ret_in10,q_ret_in26,out.header = FALSE,
#           covariate.labels = c("AC$_{t}$","AV$_{t}$","SV$_{t}$"),
#           dep.var.labels = "RET$_{t+1}$",
#           out = 'tab_ret2.tex')
# # monthly
# m_ret_in1 = lm(logxret.tp1 ~ avg_cor3m,m_data[paper_m_start:nrow(m_data)])
# m_ret_in2 = lm(logxret.tp1 ~ avg_var3m,m_data[paper_m_start:nrow(m_data)])
# m_ret_in3 = lm(logxret.tp1 ~ avg_cor3m + avg_var3m,m_data[paper_m_start:nrow(m_data)])
# #m_ret_in4 = lm(logxret.tp1 ~ (avg_cor:avg_var3,),m_data)
# m_ret_in5 = lm(logxret.tp1 ~ mkt_var3m,m_data[paper_m_start:nrow(m_data)])
# m_ret_in25 = lm(logxret.tp1 ~ mkt_var3m + avg_var3m,m_data[paper_m_start:nrow(m_data)])
# stargazer(m_ret_in1,m_ret_in2,m_ret_in5,m_ret_in25,out.header = FALSE,
#           covariate.labels = c("AC$_{t}$","AV$_{t}$","SV$_{t}$"),
#           dep.var.labels = "RET$_{t+1}$",
#           out = 'tab_ret3.tex')

m_ret_in11 = lm(logxret.tp1 ~ avg_cor1m,m_data[paper_1962_start:nrow(m_data)])
m_ret_in12 = lm(logxret.tp1 ~ avg_var1m,m_data[paper_1962_start:nrow(m_data)])
m_ret_in13 = lm(logxret.tp1 ~ avg_cor1m + avg_var1m,m_data[paper_1962_start:nrow(m_data)])
#m_ret_in4 = lm(logxret.tp1 ~ (avg_cor:avg_var3,),m_data[paper_1962_start:nrow(m_data)])
m_ret_in15 = lm(logxret.tp1 ~ mkt_var1m,m_data[paper_1962_start:nrow(m_data)])
m_ret_in125 = lm(logxret.tp1 ~ mkt_var1m + avg_var1m,m_data[paper_1962_start:nrow(m_data)])
stargazer(m_ret_in11,m_ret_in12,m_ret_in15,m_ret_in13,m_ret_in125,out.header = FALSE,
          covariate.labels = c("AC$_{t}$","AV$_{t}$","SV$_{t}$"),
          dep.var.labels = "RET$_{t+1}$",
          out = 'tab_ret3.tex')
m_ret_in21 = lm(logxret.tp3 ~ avg_cor1m,m_data[paper_1962_start:nrow(m_data)])
m_ret_in22 = lm(logxret.tp3 ~ avg_var1m,m_data[paper_1962_start:nrow(m_data)])
m_ret_in23 = lm(logxret.tp3 ~ avg_cor1m + avg_var1m,m_data[paper_1962_start:nrow(m_data)])
#m_ret_in4 = lm(logxret.tp1 ~ (avg_cor:avg_var3,),m_data[paper_1962_start:nrow(m_data)])
m_ret_in25 = lm(logxret.tp3 ~ mkt_var1m,m_data[paper_1962_start:nrow(m_data)])
m_ret_in225 = lm(logxret.tp3 ~ mkt_var1m + avg_var1m,m_data[paper_1962_start:nrow(m_data)])
stargazer(m_ret_in21,m_ret_in22,m_ret_in25,m_ret_in225,out.header = FALSE,
          covariate.labels = c("AC$_{t}$","AV$_{t}$","SV$_{t}$"),
          dep.var.labels = "RET$_{t+3}$",
          out = 'tab_ret4.tex')
m_ret_in31 = lm(logxret.tp6 ~ avg_cor1m,m_data[paper_1962_start:nrow(m_data)])
m_ret_in32 = lm(logxret.tp6 ~ avg_var1m,m_data[paper_1962_start:nrow(m_data)])
m_ret_in33 = lm(logxret.tp6 ~ avg_cor1m + avg_var1m,m_data[paper_1962_start:nrow(m_data)])
#m_ret_in4 = lm(logxret.tp1 ~ (avg_cor:avg_var3,),m_data[paper_1962_start:nrow(m_data)])
m_ret_in35 = lm(logxret.tp6 ~ mkt_var1m,m_data[paper_1962_start:nrow(m_data)])
m_ret_in325 = lm(logxret.tp6 ~ mkt_var1m + avg_var1m,m_data[paper_1962_start:nrow(m_data)])
stargazer(m_ret_in31,m_ret_in32,m_ret_in35,m_ret_in325,out.header = FALSE,
          covariate.labels = c("AC$_{t}$","AV$_{t}$","SV$_{t}$"),
          dep.var.labels = "RET$_{t+6}$",
          out = 'tab_ret5.tex')
m_ret_in41 = lm(logxret.tp12 ~ avg_cor1m,m_data[paper_1962_start:nrow(m_data)])
m_ret_in42 = lm(logxret.tp12 ~ avg_var1m,m_data[paper_1962_start:nrow(m_data)])
m_ret_in43 = lm(logxret.tp12 ~ avg_cor1m + avg_var1m,m_data[paper_1962_start:nrow(m_data)])
#m_ret_in4 = lm(logxret.tp1 ~ (avg_cor:avg_var3,),m_data[paper_1962_start:nrow(m_data)])
m_ret_in45 = lm(logxret.tp12 ~ mkt_var1m,m_data[paper_1962_start:nrow(m_data)])
m_ret_in425 = lm(logxret.tp12 ~ mkt_var1m + avg_var1m,m_data[paper_1962_start:nrow(m_data)])
stargazer(m_ret_in41,m_ret_in42,m_ret_in45,m_ret_in425,out.header = FALSE,
          covariate.labels = c("AC$_{t}$","AV$_{t}$","SV$_{t}$"),
          dep.var.labels = "RET$_{t+12}$",
          out = 'tab_ret6.tex')

stargazer(m_ret_in12,m_ret_in15,m_ret_in125,m_ret_in22,m_ret_in25,m_ret_in225,m_ret_in32,m_ret_in35,m_ret_in325,
          m_ret_in42,m_ret_in45,m_ret_in425,out.header = FALSE,
          covariate.labels = c("AV$_{t}$","SV$_{t}$"),
          dep.var.labels = c("RET$_{t+1}$","RET$_{t+3}$","RET$_{t+6}$","RET$_{t+12}$"),
          out = 'tab_ret7.tex')

m_ret1962_in11 = lm(logxret.tp1 ~ avg_cor1m,m_data[paper_m_start:paper_1962_start])
m_ret1962_in12 = lm(logxret.tp1 ~ avg_var1m,m_data[paper_m_start:paper_1962_start])
m_ret1962_in13 = lm(logxret.tp1 ~ avg_cor1m + avg_var1m,m_data[paper_m_start:paper_1962_start])
#m_ret1962_in4 = lm(logxret.tp1 ~ (avg_cor:avg_var3,),m_data[paper_m_start:paper_1962_start])
m_ret1962_in15 = lm(logxret.tp1 ~ mkt_var1m,m_data[paper_m_start:paper_1962_start])
m_ret1962_in125 = lm(logxret.tp1 ~ mkt_var1m + avg_var1m,m_data[paper_m_start:paper_1962_start])
stargazer(m_ret1962_in11,m_ret1962_in12,m_ret1962_in15,m_ret1962_in125,out.header = FALSE,
          covariate.labels = c("AC$_{t}$","AV$_{t}$","SV$_{t}$"),
          dep.var.labels = "RET$_{t+1}$",
          out = 'tab_ret1962_3.tex')
m_ret1962_in21 = lm(logxret.tp3 ~ avg_cor1m,m_data[paper_m_start:paper_1962_start])
m_ret1962_in22 = lm(logxret.tp3 ~ avg_var1m,m_data[paper_m_start:paper_1962_start])
m_ret1962_in23 = lm(logxret.tp3 ~ avg_cor1m + avg_var1m,m_data[paper_m_start:paper_1962_start])
#m_ret1962_in4 = lm(logxret.tp1 ~ (avg_cor:avg_var3,),m_data[paper_m_start:paper_1962_start])
m_ret1962_in25 = lm(logxret.tp3 ~ mkt_var1m,m_data[paper_m_start:paper_1962_start])
m_ret1962_in225 = lm(logxret.tp3 ~ mkt_var1m + avg_var1m,m_data[paper_m_start:paper_1962_start])
stargazer(m_ret1962_in21,m_ret1962_in22,m_ret1962_in25,m_ret1962_in225,out.header = FALSE,
          covariate.labels = c("AC$_{t}$","AV$_{t}$","SV$_{t}$"),
          dep.var.labels = "RET$_{t+3}$",
          out = 'tab_ret1962_4.tex')
m_ret1962_in31 = lm(logxret.tp6 ~ avg_cor1m,m_data[paper_m_start:paper_1962_start])
m_ret1962_in32 = lm(logxret.tp6 ~ avg_var1m,m_data[paper_m_start:paper_1962_start])
m_ret1962_in33 = lm(logxret.tp6 ~ avg_cor1m + avg_var1m,m_data[paper_m_start:paper_1962_start])
#m_ret1962_in4 = lm(logxret.tp1 ~ (avg_cor:avg_var3,),m_data[paper_m_start:paper_1962_start])
m_ret1962_in35 = lm(logxret.tp6 ~ mkt_var1m,m_data[paper_m_start:paper_1962_start])
m_ret1962_in325 = lm(logxret.tp6 ~ mkt_var1m + avg_var1m,m_data[paper_m_start:paper_1962_start])
stargazer(m_ret1962_in31,m_ret1962_in32,m_ret1962_in35,m_ret1962_in325,out.header = FALSE,
          covariate.labels = c("AC$_{t}$","AV$_{t}$","SV$_{t}$"),
          dep.var.labels = "RET$_{t+6}$",
          out = 'tab_ret1962_5.tex')
m_ret1962_in41 = lm(logxret.tp12 ~ avg_cor1m,m_data[paper_m_start:paper_1962_start])
m_ret1962_in42 = lm(logxret.tp12 ~ avg_var1m,m_data[paper_m_start:paper_1962_start])
m_ret1962_in43 = lm(logxret.tp12 ~ avg_cor1m + avg_var1m,m_data[paper_m_start:paper_1962_start])
#m_ret1962_in4 = lm(logxret.tp1 ~ (avg_cor:avg_var3,),m_data[paper_m_start:paper_1962_start])
m_ret1962_in45 = lm(logxret.tp12 ~ mkt_var1m,m_data[paper_m_start:paper_1962_start])
m_ret1962_in425 = lm(logxret.tp12 ~ mkt_var1m + avg_var1m,m_data[paper_m_start:paper_1962_start])
stargazer(m_ret1962_in41,m_ret1962_in42,m_ret1962_in45,m_ret1962_in425,out.header = FALSE,
          covariate.labels = c("AC$_{t}$","AV$_{t}$","SV$_{t}$"),
          dep.var.labels = "RET$_{t+12}$",
          out = 'tab_ret1962_6.tex')

stargazer(m_ret1962_in12,m_ret1962_in15,m_ret1962_in125,m_ret1962_in22,m_ret1962_in25,m_ret1962_in225,m_ret1962_in32,m_ret1962_in35,m_ret1962_in325,
          m_ret1962_in42,m_ret1962_in45,m_ret1962_in425,out.header = FALSE,
          covariate.labels = c("AV$_{t}$","SV$_{t}$"),
          dep.var.labels = c("RET$_{t+1}$","RET$_{t+3}$","RET$_{t+6}$","RET$_{t+12}$"),
          out = 'tab_ret1962_7.tex')


#### out of sample regressions ####
# q_start = floor(.25 * nrow(q_data))
# m_start = floor(.25 * nrow(m_data))

q_start = floor(.15 * nrow(q_data[paper_q_start:nrow(q_data)]))
m_start = floor(.15 * nrow(m_data[paper_m_start:nrow(m_data)]))
train = floor(.15 * nrow(m_data[paper_1962_start:nrow(m_data)]))

q_data[, date := as.Date(paste0(year,"-",month,"-","28"),format="%Y-%m-%d")]
q_st_date = q_data[paper_q_start:nrow(q_data)]$date[q_start]
m_data[, date := as.Date(paste0(year,"-",month,"-","28"),format="%Y-%m-%d")]
m_st_date = m_data[paper_m_start:nrow(m_data)]$date[m_start]
m_out_st_date = m_data[paper_1962_start:nrow(m_data)]$date[train]

y_list = list(quarterly = c("avg_var.tp1","logxret.tp3"), monthly = c("mkt_var1m.tp1","avg_var1m.tp1","logxret.tp1","logxret.tp3"))
y_names = c(avg_var.tp1 = "AV$_{t+1}$",mkt_var1m.tp1 = "SV$_{t+1}$",avg_var1m.tp1 = "AV$_{t+1}$",logxret.tp3 = "RET$_{t+3}$", logxret.tp1 = "RET$_{t+1}$")
x_vars = c(quarterly = "avg_var", monthly = "avg_var1m")
sp = c("Monthly")
freq = c("monthly")
names(freq) = sp
b_freq = c(quarterly = "mkt_var",monthly = "mkt_var1m")

oos_table = data.table(variable = c(rep("SV$_{t+1}$",2),rep("AV$_{t+1}$",2),c("RET$_{t+3}$","RET$_{t+1}$","RET$_{t+3}$")), Sample = "Monthly")
oos_table2 = data.table(variable = c(rep("SV$_{t+1}$",2),rep("AV$_{t+1}$",2),c("RET$_{t+3}$","RET$_{t+1}$","RET$_{t+3}$")), Sample = "Monthly")
# oos_table2b = data.table(variable = c(rep("SV$_{t+1}$",3),c("RET3$_{t+1}$","RET3$_{t+1}$","RET$_{t+1}$")),Sample = rep(sp,2))
# oos_table3 = data.table(variable = c(rep("AV$_{t+1}$",1),c("RET$_{t+1}$","RET$_{t+3}$")),Sample = rep(sp[2],3))


for(s in sp){
  fq = freq[s]
  y_vars = y_list[[fq]]
  x = x_vars[fq]
  for(y in y_vars){
    yn = y_names[y]
    f = as.formula(paste0(y,"~",x))
    if(fq=="quarterly"){
      dt = q_data[paper_q_start:nrow(q_data)]
    } else {dt = m_data[paper_1962_start:nrow(m_data)]}
    if(s == "1983Q2:2007Q1"){
      dt = q_data[pw_start:pw_end]
      train = 81
    } else {
      train = floor(.15 * nrow(dt))
    }
    oos_table[variable == yn & Sample == s, 
              c("$R^{2}_{oos}$","MSE-F","ENC-NEW","ENC-HLN") := as.list(lm.oos(f,dt,bench=NULL,train)$Statistics)]
    oos_table2[variable == yn & Sample == s, 
               c("$R^{2}_{oos}$","MSE-F","ENC-NEW","ENC-HLN") := as.list(lm.oos(f,dt,bench=b_freq[fq],train)$Statistics)]
    # if(fq=="monthly"){
    #   #newf = paste0("logxret.tp3","~",rhs(f))
    #   #newf = as.formula(newf)
    #   #oos_table2b[variable == yn & Sample == s, 
    #   #            c("$R^{2}_{oos}$","MSE-F","ENC-NEW","ENC-HLN") := as.list(lm.oos(newf,dt,bench=b_freq)$Statistics)]
    #   oos_table3[, c("$R^{2}_{oos}$","MSE-F","ENC-NEW","ENC-HLN") := as.list(lm.oos(f,dt,bench="mkt_var1m")$Statistics)]
    #}
  }
}
oos_table[, `MSE-F` :=  round(as.numeric(`MSE-F`),3)]
oos_table2[, `MSE-F` :=  round(as.numeric(`MSE-F`),3)]

oos_table = rbind(oos_table,oos_table2)

stargazer(oos_table,summary = FALSE,out = 'tab_oos.tex',rownames = FALSE,
          column.labels = c("","Sample",c("$R^{2}_{oos}$","MSE-F","ENC-NEW","ENC-HLN")))

## first half oos ##

oos_table = data.table(variable = c(rep("SV$_{t+1}$",2),rep("AV$_{t+1}$",2),c("RET$_{t+3}$","RET$_{t+1}$","RET$_{t+3}$")), Sample = c(sp,sp,"Monthly"))
oos_table2 = data.table(variable = c(rep("SV$_{t+1}$",2),rep("AV$_{t+1}$",2),c("RET$_{t+3}$","RET$_{t+1}$","RET$_{t+3}$")), Sample = c(sp,sp,"Monthly"))
#oos_table2b = data.table(variable = c(rep("SV$_{t+1}$",3),c("RET3$_{t+1}$","RET3$_{t+1}$","RET$_{t+1}$")),Sample = rep(sp,2))
# oos_table3 = data.table(variable = c(rep("AV$_{t+1}$",1),c("RET$_{t+1}$","RET$_{t+3}$")),Sample = rep(sp[2],3))


for(s in sp){
  fq = freq[s]
  y_vars = y_list[[fq]]
  x = x_vars[fq]
  for(y in y_vars){
    yn = y_names[y]
    f = as.formula(paste0(y,"~",x))
    if(fq=="quarterly"){
      dt = q_data[paper_q_start:nrow(q_data)]
    } else {dt = m_data[paper_m_start:paper_1962_start]}
    if(s == "1983Q2:2007Q1"){
      dt = q_data[pw_start:pw_end]
      train = 81
    } else {
      train = floor(.15 * nrow(dt))
    }
    oos_table[variable == yn & Sample == s, 
              c("$R^{2}_{oos}$","MSE-F","ENC-NEW","ENC-HLN") := as.list(lm.oos(f,dt,bench=NULL,train)$Statistics)]
    oos_table2[variable == yn & Sample == s, 
               c("$R^{2}_{oos}$","MSE-F","ENC-NEW","ENC-HLN") := as.list(lm.oos(f,dt,bench=b_freq[fq],train)$Statistics)]
    # if(fq=="monthly"){
    #   #newf = paste0("logxret.tp3","~",rhs(f))
    #   #newf = as.formula(newf)
    #   #oos_table2b[variable == yn & Sample == s, 
    #   #            c("$R^{2}_{oos}$","MSE-F","ENC-NEW","ENC-HLN") := as.list(lm.oos(newf,dt,bench=b_freq)$Statistics)]
    #   oos_table3[, c("$R^{2}_{oos}$","MSE-F","ENC-NEW","ENC-HLN") := as.list(lm.oos(f,dt,bench="mkt_var1m")$Statistics)]
    #}
  }
}
oos_table[, `MSE-F` :=  round(as.numeric(`MSE-F`),3)]
oos_table2[, `MSE-F` :=  round(as.numeric(`MSE-F`),3)]

oos_table = rbind(oos_table,oos_table2)

stargazer(oos_table,summary = FALSE,out = 'tab_oos2.tex',rownames = FALSE,
          column.labels = c("","Sample",c("$R^{2}_{oos}$","MSE-F","ENC-NEW","ENC-HLN")))

# stargazer(oos_table2,summary = FALSE,out = 'tab_oos.tex',rownames = FALSE,
#        column.labels = c("","Sample",c("$R^{2}_{oos}$","MSE-F","DM","ENC-NEW","ENC-HLN")))

# oos_table5 = data.table(Sample = c(rep("Quarterly",4),rep("Monthly",4)),Stat = rep(c("$R^{2}_{oos}$","MSE-F","ENC-NEW","ENC-HLN"),2))
# 
# stats = c("$R^{2}_{oos}$","MSE-F","ENC-NEW","ENC-HLN")
# stats_fun = c("oos_wraper","msef_wraper","encnew_wraper","enchln_wraper")
# 
# y_vars = c(Quarterly = "mkt_var.tp1",Monthly = "mkt_var1m.tp1")
# names(x_vars) = sp
# names(b_freq) = sp
# names(stats_fun) = stats
# for(s in sp[2]){
#   for(v in stats){
#     x = x_vars[s]
#     y = y_vars[s]
#     f = as.formula(paste0(y,"~",x))
#     if(s=="Quarterly"){
#       dt = q_data
#     } else {dt = m_data}
#     train = floor(.15 * nrow(dt))
#     m1 = lm.oos(f,dt,bench=NULL,train)
#     m2 = lm.oos(f,dt,bench=b_freq[s],train)
#     c = dt[floor((nrow(dt)*.15)+1):(nrow(dt))]$date %fin% contractions 
#     oos_table5[Sample == s & Stat == v, 
#                c("Contractions","Expansions",paste0(c("Contractions","Expansions"),2)) := 
#                  as.list(
#                    c(
#                      do.call(what = stats_fun[v], args = list(bench = m1$bench_values,estm = m1$forecast_values,
#                                                               hist = m1$hist_values, sub = c)),
#                      do.call(what = stats_fun[v], args = list(bench = m1$bench_values,estm = m1$forecast_values,
#                                                               hist = m1$hist_values, sub = !c)),
#                      do.call(what = stats_fun[v], args = list(m2$bench_values,m2$forecast_values,m2$hist_values,c)),
#                      do.call(what = stats_fun[v], args = list(m2$bench_values,m2$forecast_values,m2$hist_values,!c))
#                    ))]
#   }
# }
# stargazer(oos_table5,summary = FALSE,out = 'tab_oos_sub.tex')

#### out of sample variance robust stats ####
y_vars = c("avg_var1m.tp1","logxret.tp1")
x_vars = "avg_var1m"
b_freq = "mkt_var1m"

robust_table = data.table(Stat = c(rep("$R_{T}$",2),rep("$A_{T}$",2)), Variable = rep(c("AV$_{t+1}$","RET_{t+1}"),2))
# robust_table2 = data.table(Stat = c(rep("$R_{T}$",2),rep("$A_{T}$",2)), Sample = rep(c("Quarterly","Monthly"),2))


for(y in y_vars){
  f = as.formula(paste0(y," ~ ",x_vars))
  dt = m_data[paper_m_start:nrow(m_data)]
  lowR = floor(.15 * nrow(dt))
  highR = ceiling(.85*nrow(dt))
  # m1 = mspe.adjRobust(formula = f,data = dt,bench = b_freq[fq],lowR,highR)
  m2 = enc.newRobust(formula = f,data = dt,bench = b_freq,lowR,highR)
  m3 = enc.hln.Robust(formula = f,data = dt,bench = b_freq,lowR,highR)
  robust_table[Sample == fq & Stat == "$R_{T}$", 
               c("ENC-NEW","ENC-HLN",paste0(c("ENC-NEW","ENC-HLN"),"2")) := as.list(c(
                 m2$Exp_RET, m3$Exp_RET, m2$Roll_RET, m3$Roll_RET))]
  robust_table[Sample == fq &  Stat == "$A_{T}$", 
                c("ENC-NEW","ENC-HLN",paste0(c("ENC-NEW","ENC-HLN"),"2")) := as.list(
                  c(m2$Exp_AET, m3$Exp_AET, m2$Roll_AET, m3$Roll_AET))]
}

#### asset allocation ####
# quarterly 
# q_bh_returns = q_data$logxret.tp3[q_start:(nrow(q_data)-1)]
# target_sd = sd(q_bh_returns)
# q_vol_weights = (1/q_data[q_start:nrow(q_data)]$mkt_var)
# q_vol_returns = q_vol_weights * q_bh_returns
# vol_sd = sd(q_vol_returns)
# q_c_adj = target_sd / vol_sd
# adj_q_vol_weights = q_c_adj * q_vol_weights
# adj_q_vol_returns = adj_q_vol_weights * q_bh_returns
# 
# sd(adj_q_vol_returns)
# sd(q_bh_returns) # check
# 
# q_av_weights = (1/q_data[q_start:nrow(q_data)]$avg_var)
# q_av_returns = q_av_weights * q_bh_returns
# av_sd = sd(q_av_returns)
# q_c_adj_av = target_sd / av_sd
# adj_q_av_weights = q_c_adj_av * q_av_weights
# adj_q_av_returns = adj_q_av_weights * q_bh_returns
# 
# ann_q_av_ret = mean(adj_q_av_returns) * 400
# q_sr_test = sr_test(adj_q_vol_returns,adj_q_av_returns,ope = 4,paired = TRUE)
# 
# q_bh_sortino = sortinoR(q_bh_returns,annualize = TRUE,freq = "quarterly")
# q_vol_sortino = sortinoR(adj_q_vol_returns,annualize = TRUE,freq = "quarterly")
# q_av_sortino = sortinoR(adj_q_av_returns,annualize = TRUE,freq = "quarterly")



# monthly
#m_start = (paper_1962_start-1) + floor(.15*length((paper_1962_start-1):nrow(m_data)))
# train = floor(.15 * length(paper_1962_start:nrow(m_data)))
m_bh_returns = m_data$logxret.tp1[(paper_m_start+1):(nrow(m_data))]
tar_sd = sd(m_bh_returns)
m_vol_weights = (1/m_data[(paper_m_start+1):(nrow(m_data))]$mkt_var1m)
m_vol_returns = m_vol_weights * m_bh_returns
vol_sd = sd(m_vol_returns)
m_c_adj = tar_sd / vol_sd
adj_m_vol_weights = m_c_adj * m_vol_weights
adj_m_vol_returns = adj_m_vol_weights * m_bh_returns
sd(adj_m_vol_returns)

m_av_weights = (1/m_data[(paper_m_start+1):(nrow(m_data))]$avg_var1m)
m_av_returns = m_av_weights * m_bh_returns
av_sd = sd(m_av_returns)
m_c_adj_av = tar_sd / av_sd
adj_m_av_weights = m_c_adj_av * m_av_weights
adj_m_av_returns = adj_m_av_weights * m_bh_returns
sd(adj_m_av_returns)

weights_table = data.table(vol_mang = adj_m_vol_weights,av_mang = adj_m_av_weights)
stargazer(weights_table,out = "tab_weights.tex")
weights_table = data.table(m_data$date[(paper_m_start+1):(nrow(m_data))],weights_table)
weights_dt = melt(weights_table,id.vars = "V1",variable.name = "Strategy",value.name = "Weight")

weight_plot2 = ggplot(data=weights_dt) + geom_line(aes(x=V1,y=Weight,color=Strategy,linetype=Strategy)) + 
  labs(title = "Strategy Investment Weight", x = "", y = "") + 
  scale_x_date(date_breaks = "5 year", date_labels =  "%Y") +
  scale_colour_manual(name = "Strategy",values=cbPalette,labels =c("SV","AV")) + 
  scale_linetype_manual(name = "Strategy",values=linePalette,labels =c("SV","AV")) +
  theme(text = element_text(size=11),panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line( size=.1, color="black"))+ theme_bw()
weight_plot2 = nberShade(weight_plot2,xrange = c(min(weights_dt$V1), max(weights_dt$V1)),openShade = FALSE)
tikz("weight_plot2.tex",width = 5.90551, height = 3, sanitize = FALSE)
plot(weight_plot2)
dev.off()

m_av_annRET = mean(adj_m_av_returns)*1200
m_vol_annRET = mean(adj_m_vol_returns)*1200
m_sr_test = sr_test(adj_m_vol_returns,adj_m_av_returns,ope = 12,paired = TRUE,alternative = "less")

m_bh_sortino = sortinoR(m_bh_returns,annualize = TRUE,freq = "monthly")
m_av_sortino = sortinoR(adj_m_av_returns,annualize = TRUE,freq = "monthly")
m_vol_sortino = sortinoR(adj_m_vol_returns,annualize = TRUE,freq = "monthly")

# var1 = VAR(y = data.table(adj_m_av_returns,adj_m_vol_returns),p = 1,type = "none", season = 12)
# return_resid = data.table(var1$varresult$adj_m_av_returns$residuals,var1$varresult$adj_m_vol_returns$residuals)
# tboot1 = tsboot(as.ts(return_resid),statistic = Sratio_diff,R = 1000,l = 12,sim = "fixed")
# p.sr = t.test(tboot1$t)$p.value

#### performance table ####
var1 = VAR(y = data.table(adj_m_av_returns,adj_m_vol_returns),p = 1,type = "none", season = 12)
return_resid = data.table(var1$varresult$adj_m_av_returns$residuals,var1$varresult$adj_m_vol_returns$residuals)
perf_dt = data.table(Strategy = rep(c("BH","SV","AV")))
fq = 12
r0 = m_bh_returns
r1 = adj_m_vol_returns
r2 = adj_m_av_returns

SR1 = mean(r1)/sd(r1) * sqrt(12)
SR2 = mean(r2)/sd(r2) * sqrt(12)
SOR1 = SortinoRatio(r1) * sqrt(12)
SOR2 = SortinoRatio(r2) * sqrt(12)
K1 = Kappa0(r1)
K2 = Kappa0(r2)
UP1 = UpsidePotentialRatio(r1)
UP2 = UpsidePotentialRatio(r2)
gR1 = genRachev.ratio(r1)
gR2 = genRachev.ratio(r2)

perf_dt[Strategy == "BH", RET := as.character(round(mean(r0)*fq*100,3))]
ar.p = t.test(r2,r1,alternative = "less",paired = TRUE,var.equal = TRUE)$p.value
sp = data.table(avg = round(mean(r1)*fq*100,3), pval = ar.p)
perf_dt[Strategy == "SV", RET := see.stars(round(as.matrix(sp),3),1,2)[1]]
ar.p = t.test(r1,r2,alternative = "less",paired = TRUE,var.equal = TRUE)$p.value
sp = data.table(avg = round(mean(r2)*fq*100,3), pval = ar.p)
perf_dt[Strategy == "AV", RET := see.stars(round(as.matrix(sp),3),1,2)[1]]

# r1 = unlist(return_resid[,2])
# r2 = unlist(return_resid[,1])

perf_dt[Strategy == "BH", Sharpe := as.character(round((mean(r0)/sd(r0))* sqrt(12),3))]
perf_dt[Strategy == "BH", Sortino :=  as.character(round(SortinoRatio(r0)* sqrt(12),3))]
perf_dt[Strategy == "BH", Kappa := as.character(round(Kappa0(r0),3))]
perf_dt[Strategy == "BH", UpsidePotential := as.character(round(UpsidePotentialRatio(r0),3))]
perf_dt[Strategy == "BH", Rachev := as.character(round(genRachev.ratio(r0),3))]


sharpep = ratio.test1(r2,r1)
sp = data.table(SR1, pval = sharpep)
sp = round(matrix(as.numeric(sp),nrow=1),3)
perf_dt[Strategy == "SV", Sharpe := see.stars(round(as.matrix(sp),3),1,2)[1]]
sp = data.table(SR = SOR1, pval = ratio.test2(r2,r1,ratio = "sortinoR"))
sp = round(matrix(as.numeric(sp),nrow=1),3)
perf_dt[Strategy == "SV", Sortino := see.stars(round(as.matrix(sp),3),1,2)[1]]
sp = data.table(SR = K1, pval = ratio.test2(r2,r1,ratio = "Kappa0"))
sp = round(matrix(as.numeric(sp),nrow=1),3)
perf_dt[Strategy == "SV", Kappa := see.stars(round(as.matrix(sp),3),1,2)[1]]
sp = data.table(SR = UP1, pval = ratio.test2(r2,r1,ratio = "UpsidePotentialRatio"))
sp = round(matrix(as.numeric(sp),nrow=1),3)
perf_dt[Strategy == "SV", UpsidePotential := see.stars(round(as.matrix(sp),3),1,2)[1]]
sp = data.table(SR = gR1, pval = ratio.test2(r2,r1,ratio = "genRachev.ratio"))
sp = round(matrix(as.numeric(sp),nrow=1),3)
perf_dt[Strategy == "SV", Rachev := see.stars(round(as.matrix(sp),3),1,2)[1]]


sharpep = ratio.test1(r1,r2,24)
sp = data.table(SR2, pval = sharpep)
sp = round(matrix(as.numeric(sp),nrow=1),3)
perf_dt[Strategy == "AV", Sharpe := see.stars(round(as.matrix(sp),3),1,2)[1]]
sp = data.table(SR = SOR2, pval = ratio.test2(r1,r2,ratio = "sortinoR"))
sp = round(matrix(as.numeric(sp),nrow=1),3)
perf_dt[Strategy == "AV", Sortino := see.stars(round(as.matrix(sp),3),1,2)[1]]
sp = data.table(SR = K2, pval = ratio.test2(r1,r2,ratio = "Kappa0"))
sp = round(matrix(as.numeric(sp),nrow=1),3)
perf_dt[Strategy == "AV", Kappa := see.stars(round(as.matrix(sp),3),1,2)[1]]
sp = data.table(SR = UP2, pval = ratio.test2(r1,r2,ratio = "UpsidePotentialRatio"))
sp = round(matrix(as.numeric(sp),nrow=1),3)
perf_dt[Strategy == "AV", UpsidePotential := see.stars(round(as.matrix(sp),3),1,2)[1]]
sp = data.table(SR = gR2, pval = ratio.test2(r1,r2,ratio = "genRachev.ratio"))
sp = round(matrix(as.numeric(sp),nrow=1),3)
perf_dt[Strategy == "AV", Rachev := see.stars(round(as.matrix(sp),3),1,2)[1]]

stargazer(perf_dt,summary = FALSE,out = 'tab_perf.tex')

## performance table 1962##
# var1 = VAR(y = data.table(adj_m_av_returns,adj_m_vol_returns),p = 1,type = "none", season = 12)
# return_resid = data.table(var1$varresult$adj_m_av_returns$residuals,var1$varresult$adj_m_vol_returns$residuals)
perf2_dt = data.table(Strategy = rep(c("BH","SV","AV")))
fq = 12
r0 = m_bh_returns[(paper_1962_start-1):length(m_bh_returns)]
r1 = adj_m_vol_returns[(paper_1962_start-1):length(m_bh_returns)]
r2 = adj_m_av_returns[(paper_1962_start-1):length(m_bh_returns)]

SR1 = mean(r1)/sd(r1) * sqrt(12)
SR2 = mean(r2)/sd(r2) * sqrt(12)
SOR1 = SortinoRatio(r1) * sqrt(12)
SOR2 = SortinoRatio(r2) * sqrt(12)
K1 = Kappa0(r1)
K2 = Kappa0(r2)
UP1 = UpsidePotentialRatio(r1)
UP2 = UpsidePotentialRatio(r2)
gR1 = genRachev.ratio(r1)
gR2 = genRachev.ratio(r2)

perf2_dt[Strategy == "BH", RET := as.character(round(mean(r0)*fq*100,3))]
ar.p = t.test(r2,r1,alternative = "less",paired = TRUE,var.equal = TRUE)$p.value
sp = data.table(avg = round(mean(r1)*fq*100,3), pval = ar.p)
perf2_dt[Strategy == "SV", RET := see.stars(round(as.matrix(sp),3),1,2)[1]]
ar.p = t.test(r1,r2,alternative = "less",paired = TRUE,var.equal = TRUE)$p.value
sp = data.table(avg = round(mean(r2)*fq*100,3), pval = ar.p)
perf2_dt[Strategy == "AV", RET := see.stars(round(as.matrix(sp),3),1,2)[1]]

# r1 = unlist(return_resid[,2])
# r2 = unlist(return_resid[,1])

perf2_dt[Strategy == "BH", Sharpe := as.character(round((mean(r0)/sd(r0))* sqrt(12),3))]
perf2_dt[Strategy == "BH", Sortino :=  as.character(round(SortinoRatio(r0)* sqrt(12),3))]
perf2_dt[Strategy == "BH", Kappa := as.character(round(Kappa0(r0),3))]
perf2_dt[Strategy == "BH", UpsidePotential := as.character(round(UpsidePotentialRatio(r0),3))]
perf2_dt[Strategy == "BH", Rachev := as.character(round(genRachev.ratio(r0),3))]


sharpep = ratio.test1(r2,r1)
sp = data.table(SR1, pval = sharpep)
sp = round(matrix(as.numeric(sp),nrow=1),3)
perf2_dt[Strategy == "SV", Sharpe := see.stars(round(as.matrix(sp),3),1,2)[1]]
sp = data.table(SR = SOR1, pval = ratio.test2(r2,r1,ratio = "sortinoR"))
sp = round(matrix(as.numeric(sp),nrow=1),3)
perf2_dt[Strategy == "SV", Sortino := see.stars(round(as.matrix(sp),3),1,2)[1]]
sp = data.table(SR = K1, pval = ratio.test2(r2,r1,ratio = "Kappa0"))
sp = round(matrix(as.numeric(sp),nrow=1),3)
perf2_dt[Strategy == "SV", Kappa := see.stars(round(as.matrix(sp),3),1,2)[1]]
sp = data.table(SR = UP1, pval = ratio.test2(r2,r1,ratio = "UpsidePotentialRatio"))
sp = round(matrix(as.numeric(sp),nrow=1),3)
perf2_dt[Strategy == "SV", UpsidePotential := see.stars(round(as.matrix(sp),3),1,2)[1]]
sp = data.table(SR = gR1, pval = ratio.test2(r2,r1,ratio = "genRachev.ratio"))
sp = round(matrix(as.numeric(sp),nrow=1),3)
perf2_dt[Strategy == "SV", Rachev := see.stars(round(as.matrix(sp),3),1,2)[1]]


sharpep = ratio.test1(r1,r2,24)
sp = data.table(SR2, pval = sharpep)
sp = round(matrix(as.numeric(sp),nrow=1),3)
perf2_dt[Strategy == "AV", Sharpe := see.stars(round(as.matrix(sp),3),1,2)[1]]
sp = data.table(SR = SOR2, pval = ratio.test2(r1,r2,ratio = "sortinoR"))
sp = round(matrix(as.numeric(sp),nrow=1),3)
perf2_dt[Strategy == "AV", Sortino := see.stars(round(as.matrix(sp),3),1,2)[1]]
sp = data.table(SR = K2, pval = ratio.test2(r1,r2,ratio = "Kappa0"))
sp = round(matrix(as.numeric(sp),nrow=1),3)
perf2_dt[Strategy == "AV", Kappa := see.stars(round(as.matrix(sp),3),1,2)[1]]
sp = data.table(SR = UP2, pval = ratio.test2(r1,r2,ratio = "UpsidePotentialRatio"))
sp = round(matrix(as.numeric(sp),nrow=1),3)
perf2_dt[Strategy == "AV", UpsidePotential := see.stars(round(as.matrix(sp),3),1,2)[1]]
sp = data.table(SR = gR2, pval = ratio.test2(r1,r2,ratio = "genRachev.ratio"))
sp = round(matrix(as.numeric(sp),nrow=1),3)
perf2_dt[Strategy == "AV", Rachev := see.stars(round(as.matrix(sp),3),1,2)[1]]

stargazer(perf2_dt,summary = FALSE,out = 'tab_perf2.tex')

#### return plots ####
p1_dt = data.table(Date = rep(m_data[(paper_m_start+1):(nrow(m_data))]$date,3), 
                Strategy = c(rep("avg_var1m",length(adj_m_av_returns)),
                             rep("mkt_var1m",length(adj_m_vol_returns)),
                             rep("market",length(m_bh_returns))),
                Return = c(adj_m_av_returns,
                           adj_m_vol_returns,
                           m_bh_returns))
p1_dt[, Return := cumsum(Return), by = Strategy]

m_ret_plot = ggplot(data=p1_dt) + geom_line(aes(x=Date,y=Return,color=Strategy,linetype=Strategy)) + 
  labs(title = "Cummulative Excess Log Returns - Monthly", x = "", y = "") + 
  scale_x_date(date_breaks = "5 year", date_labels =  "%Y") +
  scale_colour_manual(name = "Strategy",values=cbPalette,labels =c("AV","Market","SV")) + 
  scale_linetype_manual(name = "Strategy",values=linePalette,labels =c("AV","Market","SV")) +
  theme(text = element_text(size=11),panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line( size=.1, color="black"))+ theme_bw()
m_ret_plot = nberShade(m_ret_plot,xrange = c(min(p1_dt$Date), max(p1_dt$Date)),openShade = FALSE)
tikz("m_ret_plot2.tex",width = 5.90551, height = 3, sanitize = FALSE)
plot(m_ret_plot)
dev.off()

p2 = data.table(Date = rep(m_data[paper_m_start:(nrow(m_data)-1)]$date,3), 
                Strategy = c(rep("avg_var1m",length(m_av_returns)),rep("mkt_var1m",length(m_vol_returns)),rep("market",length(m_bh_returns))),
                Return = c(m_av_returns,m_vol_returns,m_bh_returns))
p2[, Return := cumsum(Return), by = Strategy]

m_ret_plot2 = ggplot(data=p2) + geom_line(aes(x=Date,y=Return,color=Strategy,linetype=Strategy)) + 
  labs(title = "Cummulative Excess Log Returns - Monthly", x = "", y = "") + 
  scale_x_date(date_breaks = "5 year", date_labels =  "%Y") +
  scale_colour_manual(name = "Strategy",values=cbPalette,labels =c("AV","Market","SV")) + 
  scale_linetype_manual(name = "Strategy",values=linePalette,labels =c("AV","Market","SV")) +
  theme(text = element_text(size=11),panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line( size=.1, color="black"))+ theme_bw()
m_ret_plot2 = nberShade(m_ret_plot2,xrange = c(min(p2$Date), max(p2$Date)),openShade = FALSE)
tikz("m_ret_plot3.tex",width = 5.90551, height = 3, sanitize = FALSE)
plot(m_ret_plot2)
dev.off()

#### drawdowns ####
dd_ts = as.timeSeries(data.table(market = m_bh_returns,vol_mang = adj_m_vol_returns,
                                 av_mang = adj_m_av_returns))
dd_ts = exp(dd_ts) - 1
dd_dt = data.table(Date = m_data[paper_m_start:nrow(m_data)]$date,drawdowns(dd_ts))
dd_dt[, keep.rownames := NULL]
dd_dt = melt(dd_dt,measure.vars = c("market","vol_mang","av_mang"),value.name = "Return",
             id.vars = "Date",variable.name = "Strategy")

m_dd_plot = ggplot(data=dd_dt) + geom_line(aes(x=Date,y=Return,color=Strategy,linetype=Strategy)) + 
  labs(title = "Cummulative Drawdown - Monthly", x = "", y = "") + 
  scale_x_date(date_breaks = "5 year", date_labels =  "%Y") +
  scale_colour_manual(name = "Strategy",values=cbPalette,labels =c("Market","SV","AV")) + 
  scale_linetype_manual(name = "Strategy",values=linePalette,labels =c("Market","SV","AV")) +
  theme(text = element_text(size=11),panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line( size=.1, color="black"))+ theme_bw()
m_dd_plot = nberShade(m_dd_plot,xrange = c(min(dd_dt$Date), max(dd_dt$Date)),openShade = FALSE)
tikz("dd_plot.tex",width = 5.90551, height = 3, sanitize = FALSE)
plot(m_dd_plot)
dev.off()


dds_bh = as.data.table(drawdownsStats(as.timeSeries(exp(m_bh_returns)-1)))
dds_av = as.data.table(drawdownsStats(as.timeSeries(exp(adj_m_av_returns)-1)))
dds_vol = as.data.table(drawdownsStats(as.timeSeries(exp(adj_m_vol_returns)-1)))

dd_table = data.table(Strategy = c("BH","SV","AV"))
dd_table[Strategy == "BH", 
         c("N","Max DD","Avg DD","Max Length","Avg Length","Max Recovery","Avg Recovery") := 
           list(nrow(dds_bh[Length > 1]),min(dds_bh$Depth),mean(dds_bh[Length > 1]$Depth),
                max(dds_bh$Length),mean(dds_bh[Length > 1]$Length),
                max(dds_bh$Recovery),mean(dds_bh[Length > 1]$Recovery))]
dd_table[Strategy == "SV", 
         c("N","Max DD","Avg DD","Max Length","Avg Length","Max Recovery","Avg Recovery") := 
           list(nrow(dds_vol[Length > 1]),min(dds_vol$Depth),mean(dds_vol[Length > 1]$Depth),
                max(dds_vol$Length),mean(dds_vol[Length > 1]$Length),
                max(dds_vol$Recovery),mean(dds_vol[Length > 1]$Recovery))]
dd_table[Strategy == "AV", 
         c("N","Max DD","Avg DD","Max Length","Avg Length","Max Recovery","Avg Recovery") := 
           list(nrow(dds_av[Length > 1]),min(dds_av$Depth),mean(dds_av[Length > 1]$Depth),
                max(dds_av$Length),mean(dds_av[Length > 1]$Length),
                max(dds_av$Recovery),mean(dds_av[Length > 1]$Recovery))]

dd_table[, c("Max DD","Avg DD") := list(`Max DD`*100,`Avg DD`*100)]
dd_table[,2:ncol(dd_table)] = round(dd_table[,2:ncol(dd_table)],3)
stargazer(dd_table,summary = FALSE,out = "dd_stats.tex")

# p2 = data.table(Date = rep(q_data[q_start:nrow(q_data)]$date,3), 
#                 Strategy = c(rep("avg_var,",length(adj_q_av_returns)),rep("mkt_var",length(adj_q_vol_returns)),rep("market",length(q_bh_returns))),
#                 Return = c(adj_q_av_returns,adj_q_vol_returns,q_bh_returns))
# p2[, Return := cumsum(Return), by = Strategy]
# q_ret_plot = ggplot(data=p2) + geom_line(aes(x=Date,y=Return,color=Strategy,linetype=Strategy)) + 
#   labs(title = "Cummulative Excess Log Returns - Quarterly", x = "", y = "") + 
#   scale_x_date(date_breaks = "5 year", date_labels =  "%Y") +
#   scale_colour_manual(name = "Strategy",values=cbPalette,labels =c("AV","Market","SV")) + 
#   scale_linetype_manual(name = "Strategy",values=linePalette,labels =c("AV","Market","SV")) +
#   theme(text = element_text(size=11),panel.grid.major.x = element_blank(),
#         panel.grid.major.y = element_line( size=.1, color="black"))+ theme_bw()
# q_ret_plot = nberShade(q_ret_plot,xrange = c(min(p2$Date), max(p2$Date)),openShade = FALSE)

#### Business Cycle Subsets ####
# m_data[, date := as.Date(paste0(year,"-",month,"-28"))]
# q_data[, date := as.Date(paste0(year,"-",month,"-28"))]
nberD = nberDates()
nberD = as.data.table(nberD)
nberD[, Start := as.Date(as.character(Start),format="%Y%m%d")]
nberD[, End := as.Date(as.character(End),format="%Y%m%d")]
nberD = subset(nberD,subset = Start >= m_data[paper_m_start]$date)
contractions = seq(from = nberD[1,Start],to = nberD[1,End], by = "1 month")
for(n in 2:nrow(nberD)){
  contractions = c(contractions,seq(from = nberD[n,Start],to = nberD[n,End], by = "1 month"))
}
contractions = str_replace(contractions,"-01","-28")
contractions = m_data$date[m_data$date %fin% as.Date(contractions,format = "%Y-%m-%d")]

subs_dt = data.table(Date = rep(m_data[paper_m_start:nrow(m_data)]$date,3), 
                     Strategy = c(rep("avg_var1m",length(adj_m_av_returns)),rep("mkt_var1m",length(adj_m_vol_returns)),rep("market",length(m_bh_returns))),
                     Return = c(adj_m_av_returns,adj_m_vol_returns,m_bh_returns))
subs_dt[Date %in% contractions, contraction := 1]
subs_dt[is.na(contraction), contraction := 0]

subs_dt = dcast(subs_dt,formula = Date + contraction ~ ...,value.var = "Return")

subs_table = 

#### VAR tests ####
# var_model_q = VAR(data.table(subset(q_data[q_start:nrow(q_data)],
#                                     select = c("avg_var","avg_cor.quarterly","mkt_var.quarterly","logxret.tp3"))))
# irf_q = irf(var_model_q)
# var_dt = data.table(subset(q_data,select = c("avg_var","avg_cor.quarterly","","logxret.tp3")))
# 
# var_model_q3 = VAR(data.table(subset(q_data[q_start:nrow(q_data)],select = c("avg_var","mkt_var.quarterly"))))
# 
# var_model_q2 = bvar.sv.tvp(Y = as.matrix(var_dt[!is.na(logxret.tp3)]),tau = q_start-33,thinfac = 5,nf = 8)
# 
# var_dt_m = data.table(logxret = shift(m_data$logxret.tp1)[m_start:nrow(m_data)],
#                       subset(m_data[m_start:nrow(m_data)],select = c("avg_cor","mkt_var","avg_var")))
# var_model_m = VAR(var_dt_m,type = "none")
# irf_m = irf(var_model_m)
# 
# # scaled irf plot
# irf_melt = as.data.table(melt(data.frame(time = seq(nrow(irf_m$irf$logxret)),
#                                       irf_m$irf$logxret),id.vars = "time",variable.name = "variable",value.name = "estimate"))
# irf_melt[, plot := "logxret"]
# irf_melt2 = as.data.table(melt(data.frame(time = seq(nrow(irf_m$irf$avg_cor)),
#                                       irf_m$irf$avg_var),id.vars = "time",variable.name = "variable",value.name = "estimate"))
# irf_melt2[, plot := "avg_cor"]
# irf_melt3 = as.data.table(melt(data.frame(time = seq(nrow(irf_m$irf$mkt_var)),
#                                        irf_m$irf$mkt_var),id.vars = "time",variable.name = "variable",value.name = "estimate"))
# irf_melt3[, plot := "mkt_var"]
# irf_melt4 = as.data.table(melt(data.frame(time = seq(nrow(irf_m$irf$avg_var)),
#                                           irf_m$irf$avg_var),id.vars = "time",variable.name = "variable",value.name = "estimate"))
# irf_melt4[, plot := "avg_var"]
# 
# irf_plot_dt = rbindlist(list(irf_melt,irf_melt2,irf_melt3,irf_melt4))
# 
# ggplot() + geom_line(data = irf_plot_dt,mapping = aes(x=time,y=estimate,linetype=variable,color=variable)) + 
#   facet_grid(variable ~ plot,scales = "free_y")

# bayesian monthly
m_data[, logxret := shift(logxret.tp1)]
m_data[, av_weight := m_c_adj_av * 1/avg_var3m]
m_data[, sv_weight := m_c_adj * 1/mkt_var]
cape_dt = read_excel(path = 'ie_data.xls',sheet = "Data",skip = 7,col_types = c("text",rep("numeric",10)),na = "NA")
cape_dt = as.data.table(cape_dt)
cape_dt[, year := as.integer(substr(Date,1,4))]
cape_dt[, month := as.integer(substr(Date,6,7))]
m_data = merge(m_data,subset(cape_dt,select = c("year","month","CAPE")))
# m_data[, c("avg_var3m","mkt_var","avg_cor","logxret") := ]
bvar_m_dt = as.matrix(subset(m_data[!is.na(logxret)],select = c("CAPE","logxret","av_weight","avg_var3m"))) # c("avg_var3m","mkt_var","avg_cor","logxret"))) # 
m_var_model = bvar.sv.tvp(Y = bvar_m_dt,tau = (m_start-2),nf = 18,p = 2)

# impulse avg_var3m
# avg_var3m response
avg_var_avg_var = impulse.responses(m_var_model,impulse.variable = 4,response.variable = 4,nhor = 24,draw.plot = FALSE,scenario = 3)
av_av = colMeans(avg_var_avg_var$irf)
av_av_sd = colSds(avg_var_avg_var$irf)
av_sd = sd(m_data[m_start:nrow(m_data)]$avg_var3m)
av_av_95 = qnorm(.95,mean = av_av, sd = av_av_sd)
av_av_5 = qnorm(.05,mean = av_av, sd = av_av_sd)
av_av_dt = data.table(time = seq_along(av_av),avg_var3, estimate = av_av, lower = av_av_5, upper = av_av_95, impulse = "avg_var3m", response = "avg_var3m")
melt_av_av_dt = melt(av_av_dt,id.vars = c("time","impulse","response"),variable.name = "line",value.name = "estimate")

cbPalette = c(upper = "#000000", lower = "#000000",cbPalette)
linePalette <- c(upper = "4C88C488",lower = "4C88C488",linePalette)
av_av_plot = ggplot() + geom_line(data = melt_av_av_dt, mapping = aes(x = time, y = estimate, color = line, linetype = line)) + 
  scale_x_continuous(name = "Month", breaks = 1:24,labels = 1:24) + 
  labs(title = "Average Variance Response", x = "", y = "") +
  scale_colour_manual(name = "Strategy",values=cbPalette,guide=FALSE) + 
  scale_linetype_manual(name = "Strategy",values=linePalette,guide=FALSE) +
  theme(text = element_text(size=11),panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line( size=.1, color="black"))+ theme_bw()
tikz("av_av_plot.tex",width = 5.90551, height = 3,sanitize = FALSE)
plot(av_av_plot)
dev.off()


# return response
avg_var_logxret = impulse.responses(m_var_model,impulse.variable = 4,response.variable = 2,nhor = 24,draw.plot = FALSE,scenario = 3)
av_logxret = colMeans(avg_var_logxret$irf)
av_logxret_sd = colSds(avg_var_logxret$irf)
av_logxret_95 = qnorm(.95,mean = av_logxret, sd = av_logxret_sd)
av_logxret_5 = qnorm(.05,mean = av_logxret, sd = av_logxret_sd)
av_logxret_dt = data.table(time = seq_along(av_logxret),logxret = av_logxret, lower = av_logxret_5, upper = av_logxret_95, impulse = "avg_var3m", response = "logxret")
melt_av_logxret_dt = melt(av_logxret_dt,id.vars = c("time","impulse","response"),variable.name = "line",value.name = "estimate")

cbPalette = c(logxret = "darkviolet",cbPalette)
linePalette <- c(logxret = "solid",linePalette)

av_logxret_plot = ggplot() + geom_line(data = melt_av_logxret_dt, mapping = aes(x = time, y = estimate, color = line, linetype = line)) + 
  scale_x_continuous(name = "Month", breaks = 1:24,labels = 1:24) + 
  labs(title = "Excess Log Return Response", x = "", y = "") +
  scale_colour_manual(values=cbPalette,guide=FALSE) + 
  scale_linetype_manual(values=linePalette,guide=FALSE) +
  theme(text = element_text(size=11),panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line( size=.1, color="black"))+ theme_bw()
tikz("av_logxret_plot.tex",width = 5.90551, height = 3,sanitize = FALSE)
plot(av_logxret_plot)
dev.off()

# investment response
avg_var_av_weight = impulse.responses(m_var_model,impulse.variable = 4,response.variable = 3,nhor = 24,draw.plot = FALSE,scenario = 3)
av_av_weight = colMeans(avg_var_av_weight$irf)
av_av_weight_sd = colSds(avg_var_av_weight$irf)
av_av_weight_95 = qnorm(.95,mean = av_av_weight, sd = av_av_weight_sd)
av_av_weight_5 = qnorm(.05,mean = av_av_weight, sd = av_av_weight_sd)
av_av_weight_dt = data.table(time = seq_along(av_av_weight),av_weight = av_av_weight, lower = av_av_weight_5, upper = av_av_weight_95, impulse = "avg_var3m", response = "av_weight")
melt_av_av_weight_dt = melt(av_av_weight_dt,id.vars = c("time","impulse","response"),variable.name = "line",value.name = "estimate")

cbPalette = c(av_weight = "green4",cbPalette)
linePalette <- c(av_weight = "solid",linePalette)

av_av_weight_plot = ggplot() + geom_line(data = melt_av_av_weight_dt, mapping = aes(x = time, y = estimate, color = line, linetype = line)) + 
  scale_x_continuous(name = "Month", breaks = 1:24,labels = 1:24) + 
  labs(title = "Investment Weight Response", x = "", y = "") +
  scale_colour_manual(values=cbPalette,guide=FALSE) + 
  scale_linetype_manual(values=linePalette,guide=FALSE) +
  theme(text = element_text(size=11),panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line( size=.1, color="black"))+ theme_bw()
tikz("av_av_weight_plot.tex",width = 5.90551, height = 3,sanitize = FALSE)
plot(av_av_weight_plot)
dev.off()

# mkt_var impulse
# mkt_var response
bvar_m_dt2 = as.matrix(subset(m_data[!is.na(logxret)],select = c("CAPE","logxret","sv_weight","mkt_var3m"))) # c("avg_var","mkt_var3m","avg_cor","logxret"))) # 
m_var_model2 = bvar.sv.tvp(Y = bvar_m_dt2,tau = (m_start-2),nf = 18,p = 2)
mkt_var_mkt_var = impulse.responses(m_var_model2,impulse.variable = 4,response.variable = 4,nhor = 24,draw.plot = FALSE)
sv_sv = colMeans(mkt_var_mkt_var$irf)
sv_sv_sd = colSds(mkt_var_mkt_var$irf)
sv_sv_95 = qnorm(.95,mean = sv_sv, sd = sv_sv_sd)
sv_sv_5 = qnorm(.05,mean = sv_sv, sd = sv_sv_sd)
sv_sv_dt = data.table(time = seq_along(sv_sv),mkt_var3m = sv_sv, lower = sv_sv_5, upper = sv_sv_95, impulse = "mkt_var3m", response = "mkt_var3m")
melt_sv_sv_dt = melt(sv_sv_dt,id.vars = c("time","impulse","response"),variable.name = "line",value.name = "estimate")

sv_sv_plot = ggplot() + geom_line(data = melt_sv_sv_dt, mapping = aes(x = time, y = estimate, color = line, linetype = line)) + 
  scale_x_continuous(name = "Month", breaks = 1:24,labels = 1:24) + 
  labs(title = "Market Variance Response", x = "", y = "") +
  scale_colour_manual(values=cbPalette,guide=FALSE) + 
  scale_linetype_manual(values=linePalette,guide=FALSE) +
  theme(text = element_text(size=11),panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line( size=.1, color="black"))+ theme_bw()
tikz("sv_sv_plot.tex",width = 5.90551, height = 3,sanitize = FALSE)
plot(sv_sv_plot)
dev.off()

# excess return response
mkt_var_logxret = impulse.responses(m_var_model2,impulse.variable = 4,response.variable = 2,nhor = 24,draw.plot = FALSE,scenario = 3)
sv_logxret = colMeans(mkt_var_logxret$irf)
sv_logxret_sd = colSds(mkt_var_logxret$irf)
sv_logxret_95 = qnorm(.95,mean = sv_logxret, sd = sv_logxret_sd)
sv_logxret_5 = qnorm(.05,mean = sv_logxret, sd = sv_logxret_sd)
sv_logxret_dt = data.table(time = seq_along(sv_logxret),mkt_var3m = sv_logxret, lower = sv_logxret_5, upper = sv_logxret_95, impulse = "mkt_var3m", response = "mkt_var3m")
melt_logxretmkt_var_dt = melt(sv_logxret_dt,id.vars = c("time","impulse","response"),variable.name = "line",value.name = "estimate")

sv_logxret_plot = ggplot() + geom_line(data = melt_logxretmkt_var_dt, mapping = aes(x = time, y = estimate, color = line, linetype = line)) + 
  scale_x_continuous(name = "Month", breaks = 1:24,labels = 1:24) + 
  labs(title = "Excess Log Return Response", x = "", y = "") +
  scale_colour_manual(values=cbPalette,guide=FALSE) + 
  scale_linetype_manual(values=linePalette,guide=FALSE) +
  theme(text = element_text(size=11),panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line( size=.1, color="black"))+ theme_bw()
tikz("sv_logxret_plot.tex",width = 5.90551, height = 3,sanitize = FALSE)
plot(sv_logxret_plot)
dev.off()

# sv investment response # 
mkt_var_sv_weight = impulse.responses(m_var_model2,impulse.variable = 4,response.variable = 3,nhor = 24,draw.plot = FALSE,scenario = 3)
sv_sv_weight = colMeans(mkt_var_sv_weight$irf)
sv_sv_weight_sd = colSds(mkt_var_sv_weight$irf)
sv_sv_weight_95 = qnorm(.95,mean = sv_sv_weight, sd = sv_sv_weight_sd)
sv_sv_weight_5 = qnorm(.05,mean = sv_sv_weight, sd = sv_sv_weight_sd)
sv_sv_weight_dt = data.table(time = seq_along(sv_sv_weight),mkt_var3m = sv_sv_weight, lower = sv_sv_weight_5, upper = sv_sv_weight_95, impulse = "mkt_var3m", response = "mkt_var3m")
melt_sv_weightmkt_var_dt = melt(sv_sv_weight_dt,id.vars = c("time","impulse","response"),variable.name = "line",value.name = "estimate")

sv_sv_weight_plot = ggplot() + geom_line(data = melt_sv_weightmkt_var_dt, mapping = aes(x = time, y = estimate, color = line, linetype = line)) + 
  scale_x_continuous(name = "Month", breaks = 1:24,labels = 1:24) + 
  labs(title = "Investment Weight Response", x = "", y = "") +
  scale_colour_manual(values=cbPalette,guide=FALSE) + 
  scale_linetype_manual(values=linePalette,guide=FALSE) +
  theme(text = element_text(size=11),panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line( size=.1, color="black"))+ theme_bw()

tikz("sv_sv_weight_plot.tex",width = 5.90551, height = 3,sanitize = FALSE)
plot(sv_sv_weight_plot)
dev.off()

#### lottery vs leverage ####
dp = dcast(p1,formula = Date ~ ...,value.var = "Return")
dp = as.data.table(dp)
dp[, year := year(Date)]
dp[, month := month(Date)]

w_dt = data.table(date = dp$Date + months(1),adj_m_vol_weights,adj_m_av_weights)
w_dt[, year := year(date)]
w_dt[, month := month(date)]
w_dt[, date := NULL]
d_returns[, year := year(date)]
d_returns[, month := month(date)]
d_returns = merge(d_returns,w_dt,by=c("year","month"))
setkey(d_returns,year,month)
d_returns[, BH_MAX1 := max(vwretd), by = c("year","month")]
d_returns[, SV_MAX1 := max(vwretd*adj_m_vol_weights), by = c("year","month")]
d_returns[, AV_MAX1 := max(vwretd*adj_m_av_weights), by = c("year","month")]


d_returns[, BH_MAX5 := mean(sort(vwretd,decreasing = TRUE)[1:5]), by=c("year","month")]
d_returns[, SV_MAX5 := mean(sort(vwretd*adj_m_vol_weights,decreasing = TRUE)[1:5]), by=c("year","month")]
d_returns[, AV_MAX5 := mean(sort(vwretd*adj_m_av_weights,decreasing = TRUE)[1:5]), by=c("year","month")]


d_returns[, BH_var := var(vwretd), by = c("year","month")]
d_returns[, SV_var := var(vwretd*adj_m_vol_weights), by = c("year","month")]
d_returns[, AV_var := var(vwretd*adj_m_av_weights), by = c("year","month")]

d2 = unique(d_returns,by=c("year","month"))
d2[, BH_var.tm1 := shift(BH_var,type = "lag")]
d2[, SV_var.tm1 := shift(SV_var,type = "lag")]
d2[, AV_var.tm1 := shift(AV_var,type = "lag")]

d_returns = merge(d_returns,subset(d2,select = c("year","month","BH_var.tm1","SV_var.tm1","AV_var.tm1")),all.x=TRUE)
d_returns[, BH_MAX1_scale := BH_MAX1 / sqrt(BH_var.tm1)]
d_returns[, SV_MAX1_scale := SV_MAX1 / sqrt(SV_var.tm1)]
d_returns[, AV_MAX1_scale := BH_MAX1 / sqrt(AV_var.tm1)]

d_returns[, BH_MAX5_scale := BH_MAX5 / sqrt(BH_var.tm1)]
d_returns[, SV_MAX5_scale := SV_MAX5 / sqrt(SV_var.tm1)]
d_returns[, AV_MAX5_scale := BH_MAX5 / sqrt(AV_var.tm1)]

m_returns = unique(d_returns,by=c("year","month"))
m_returns[, market := m_bh_returns]
m_returns[, av_mang := adj_m_av_returns]
m_returns[, vol_mang := adj_m_vol_returns]


MAX1_dt = subset(m_returns[!is.na(BH_MAX1_scale)],select = c("BH_MAX1","SV_MAX1","AV_MAX1","BH_MAX1_scale",
                                                           "SV_MAX1_scale","AV_MAX1_scale","BH_MAX5","SV_MAX5",
                                                           "AV_MAX5","BH_MAX5_scale","SV_MAX5_scale","AV_MAX5_scale"))

# ecdf_bhmax = ecdf(x = max_dt$BH_MAX1)
# ecdf_bhmax_s = ecdf(x = max_dt$BH_MAX1_scale)
# ecdf_svmax_s = ecdf(x = max_dt$SV_MAX1_scale)
# ecdf_avmax_s = ecdf(x = max_dt$AV_MAX1_scale)
# ecdf_avmax = ecdf(x = max_dt$AV_MAX1)
# ecdf_svmax = ecdf(x = max_dt$SV_MAX1)
# 
# ecdf_bhmax = ecdf(x = max_dt$BH_MAX5)
# ecdf_bhmax_s = ecdf(x = max_dt$BH_MAX5_scale)
# ecdf_svmax_s = ecdf(x = max_dt$SV_MAX5_scale)
# ecdf_avmax_s = ecdf(x = max_dt$AV_MAX5_scale)
# ecdf_avmax = ecdf(x = max_dt$AV_MAX5)
# ecdf_svmax = ecdf(x = max_dt$SV_MAX5)

MAX1_stats=data.table(Strategy = c("BH","SV","AV"))
MAX1_stats[Strategy == "BH", c("Mean","Median","Sd","KS","Mean_s","Median_s","Sd_s","KS_s"):=
            list(mean(max_dt$BH_MAX1)*100,median(max_dt$BH_MAX1)*100,sd(max_dt$BH_MAX1)*100,NA_real_,
                 mean(max_dt$BH_MAX1_scale),median(max_dt$BH_MAX1_scale),sd(max_dt$BH_MAX1_scale),NA_real_)]
MAX1_stats[Strategy == "SV", c("Mean","Median","Sd","KS","Mean_s","Median_s","Sd_s","KS_s"):=
            list(mean(max_dt$SV_MAX1)*100,median(max_dt$SV_MAX1)*100,sd(max_dt$SV_MAX1)*100,
                 ks.test(max_dt$BH_MAX1,max_dt$SV_MAX1,alternative = "greater")$p.value,
                 mean(max_dt$SV_MAX1_scale),median(max_dt$SV_MAX1_scale),sd(max_dt$SV_MAX1_scale),
                 ks.test(max_dt$BH_MAX1_scale,max_dt$SV_MAX1_scale,alternative = "greater")$p.value)]
MAX1_stats[Strategy == "AV", c("Mean","Median","Sd","KS","Mean_s","Median_s","Sd_s","KS_s"):=
            list(mean(max_dt$AV_MAX1)*100,median(max_dt$AV_MAX1)*100,sd(max_dt$AV_MAX1)*100,
                 ks.test(max_dt$BH_MAX1,max_dt$AV_MAX1,alternative = "greater")$p.value,
                 mean(max_dt$AV_MAX1_scale),median(max_dt$AV_MAX1_scale),sd(max_dt$AV_MAX1_scale),
                 ks.test(max_dt$BH_MAX1_scale,max_dt$AV_MAX1_scale,alternative = "greater")$p.value)]

MAX1_stats[, 2:ncol(MAX1_stats)] = round(MAX1_stats[, 2:ncol(MAX1_stats)],3)

MAX5_stats=data.table(Strategy = c("BH","SV","AV"))
MAX5_stats[Strategy == "BH", c("Mean","Median","Sd","KS","Mean_s","Median_s","Sd_s","KS_s"):=
             list(mean(max_dt$BH_MAX5)*100,median(max_dt$BH_MAX5)*100,sd(max_dt$BH_MAX5)*100,NA_real_,
                  mean(max_dt$BH_MAX5_scale),median(max_dt$BH_MAX5_scale),sd(max_dt$BH_MAX5_scale),NA_real_)]
MAX5_stats[Strategy == "SV", c("Mean","Median","Sd","KS","Mean_s","Median_s","Sd_s","KS_s"):=
             list(mean(max_dt$SV_MAX5)*100,median(max_dt$SV_MAX5)*100,sd(max_dt$SV_MAX5)*100,
                  ks.test(max_dt$BH_MAX5,max_dt$SV_MAX5,alternative = "greater")$p.value,
                  mean(max_dt$SV_MAX5_scale),median(max_dt$SV_MAX5_scale),sd(max_dt$SV_MAX5_scale),
                  ks.test(max_dt$BH_MAX5_scale,max_dt$SV_MAX5_scale,alternative = "greater")$p.value)]
MAX5_stats[Strategy == "AV", c("Mean","Median","Sd","KS","Mean_s","Median_s","Sd_s","KS_s"):=
             list(mean(max_dt$AV_MAX5)*100,median(max_dt$AV_MAX5)*100,sd(max_dt$AV_MAX5)*100,
                  ks.test(max_dt$BH_MAX5,max_dt$AV_MAX5,alternative = "greater")$p.value,
                  mean(max_dt$AV_MAX5_scale),median(max_dt$AV_MAX5_scale),sd(max_dt$AV_MAX5_scale),
                  ks.test(max_dt$BH_MAX5_scale,max_dt$AV_MAX5_scale,alternative = "greater")$p.value)]

MAX5_stats[, 2:ncol(MAX5_stats)] = round(MAX5_stats[, 2:ncol(MAX5_stats)],3)
MAX_stats = rbind(MAX1_stats,MAX5_stats)
stargazer(MAX_stats,summary = FALSE,out = "tab_max_stats.tex")

ff_data[, year := as.integer(substr(V1,1,4))]
ff_data[, month := as.integer(substr(V1,5,6))]

m_returns = merge(m_returns,subset(ff_data,select = c("year","month","Mkt_RF","SMB","HML")))

ff_5 = fread(input = "F-F_Research_Data_5_Factors_2x3.CSV",header = TRUE,skip = 3)
ff_5[, year:= as.integer(substr(V1,1,4))]
ff_5[, month := as.integer(substr(V1,5,6))]
m_returns = merge(m_returns,subset(ff_5,select = c("year","month","RMW","CMA")),by=c("year","month"),all.x=TRUE)
# 
# 
# 
# lm_max_bh = lm(market ~ BH_MAX, data = m_returns)
# lm_max_sv = lm(adj_m_vol_returns ~ SV_MAX, data = m_returns)
# lm_max_av = lm(adj_m_av_returns ~ AV_MAX, data = m_returns)
# 
# hac_max_bh = coeftest(lm_max_bh,vcov. = vcovHAC(lm_max_bh))
# hac_max_sv = coeftest(lm_max_sv,vcov. = vcovHAC(lm_max_sv))
# hac_max_av = coeftest(lm_max_av,vcov. = vcovHAC(lm_max_av))

ks.test(adj_m_av_returns,m_bh_returns,alternative = "less")
dunn.test(list(av_mang = adj_m_av_returns,bh = m_bh_returns),method = "by")
wt_out = wtdpapb(xa = adj_m_av_returns,xb = m_bh_returns)
sd2_out = stochdom2(wt_out$dj,wt_out$wpa,wt_out$wpb)


## leverage ##

marg_req = fread(input = './margin_req.csv')
marg_req[, Date := as.Date(Effective,format="%m-%d-%Y")]
sign(diff(x = marg_req$Rate))
marg_req[2:nrow(marg_req), mr_change := diff(x = marg_req$Rate)]
marg_req[2:nrow(marg_req), mr_changeI := sign(diff(x = marg_req$Rate))]
marg_req[,c("year","month","day") := list(year(Date),month(Date),day(Date))]
marg_req[1, mr_change := 1]
marg_req[1, mr_changeI := 1]
# marg_req[day > 21, month := month + 1]


m_returns = merge(m_returns,subset(marg_req,select=c("year","month","mr_change","mr_changeI")),all.x=TRUE)
m_returns[is.na(mr_change), mr_change := 0]
m_returns[is.na(mr_changeI), mr_changeI := 0]

margin_data = fread(input = 'reproduce_data.csv')

m_returns = merge(m_returns,subset(margin_data,select = c("year","month","m_retvol","md","md_1984","mc","mc_1984","m_retvar","avg_var","avg_cor",
                                              "spls","icrf","aem_leverage_factor","bc_chg_p_nsa","vix",
                                              "bc_chg_p")),all.x=TRUE)

#### lending rates ####
bloom_call_money <- fread(input = '../../market downturn indicators/decline_predictors/data/bloomberg_call_money_rate.csv',sep = ",",header = TRUE,na.strings = "",skip = 1)
bloom_call_money <- bloom_call_money[complete.cases(bloom_call_money)]
intermediate_rate <- fread(input = '../../market downturn indicators/decline_predictors/data/call_rate.csv',sep = ",",header = TRUE,na.strings = "")
money_rate <- fread(input = '../../market downturn indicators/decline_predictors/data/money_rates.csv')
bloom_call_money$date <- as.Date(bloom_call_money$date,format = "%d/%m/%Y")
intermediate_rate$date <- as.Date(paste0("01-",intermediate_rate$date),format = "%d-%b-%Y")
setnames(money_rate,"#ADDIN?","date")
money_rate$date <- as.Date(money_rate$date,format = "%d/%m/%Y")
setnames(money_rate,c(2,3),c("call_money","bank_prime"))
intermediate_rate[, year := year(date)]
intermediate_rate[, month := month(date)]
bloom_call_money[, year := year(date)]
bloom_call_money[, month := month(date)]
money_rate[, year := year(date)]
money_rate[, month := month(date)]
intermediate_rate$date <- NULL
bloom_call_money$date <- NULL
money_rate$date <- NULL
rates <- merge(intermediate_rate,money_rate,by = c("year","month"),all.x = TRUE)
rates <- merge(rates,bloom_call_money, by = c("year","month"),all.x = TRUE)
setnames(rates,c("year","month","int_rate","call_money","bank_prime","rate"))
m_returns =  merge(m_returns,rates,by=c("year","month"),all.x = T)
# m_returns[, tbl_per := tbl*100] # change from fraction to percentage to match other rates

lm_aem3 = lm(av_mang~market*aem_leverage_factor+SMB+HML,data=m_returns)
lm_aem5 = lm(av_mang~market*aem_leverage_factor+SMB+HML+RMW+CMA,data=m_returns)
lm_icrf3 = lm(av_mang~market*icrf + SMB + HML, data = m_returns)
lm_icrf5 = lm(av_mang~market*icrf + SMB + HML + RMW + CMA, data = m_returns)
# lm_bc3 = lm(av_mang~market*bc_chg_p_nsa + SMB + HML, data = m_returns)
# lm_bc5 = lm(av_mang~market*bc_chg_p_nsa + SMB + HML + RMW + CMA, data = m_returns)
lm_bcp3 = lm(av_mang~market*bc_chg_p + SMB + HML, data = m_returns)
lm_bcp5 = lm(av_mang~market*bc_chg_p + SMB + HML + RMW + CMA, data = m_returns)

m_returns[, chg_md := shift(diff(md),type = "lag")]
m_returns[, chg_md_1984 := shift(diff(md_1984),type = "lag")]

lm_md3 = lm(av_mang~market*chg_md + SMB + HML, data = m_returns)
lm_md5 = lm(av_mang~market*chg_md + SMB + HML + RMW + CMA, data = m_returns)
lm_md_19843 = lm(av_mang~market*chg_md_1984 + SMB + HML, data = m_returns)
lm_md_19845 = lm(av_mang~market*chg_md_1984 + SMB + HML + RMW + CMA, data = m_returns)

lm_call_money3 = lm(av_mang~market*call_money + SMB + HML, data = m_returns)
lm_call_money5 = lm(av_mang~market*call_money + SMB + HML + RMW + CMA, data = m_returns)
lm_prime3 = lm(av_mang~market*bank_prime + SMB + HML, data = m_returns)
lm_prime5 = lm(av_mang~market*bank_prime + SMB + HML + RMW + CMA, data = m_returns)
lm_broker3 = lm(av_mang ~ market*rate + SMB + HML, data = m_returns)
lm_broker5 = lm(av_mang ~ market*rate + SMB + HML + RMW + CMA, data = m_returns)

stargazer(lm_aem3,lm_aem5,lm_icrf3,lm_icrf5,lm_bcp3,lm_bcp5,lm_md_19843,lm_md_19845,out = 'tab_levprox.tex')
stargazer(lm_broker3,lm_broker5,lm_call_money3,lm_call_money5,lm_prime3,lm_prime5, out = 'tab_rates.tex')

lm_margreq3 = lm(av_mang ~ market*mr_change+SMB+HML,data=m_returns[year>=1934&year<=1974])

## gambling ##
tmp_crsp = fread(input = '../../data/CRSP/ced98c65aea26fb3.csv', colClasses = "character")
setkey(tmp_crsp,date,PERMNO)
tmp_crsp[, date := as.Date(as.character(date),format="%Y%m%d")]
setkey(tmp_crsp,date,PERMNO)
tmp_crsp[, year := year(date)]
tmp_crsp = tmp_crsp[year <= 2016]
tmp_crsp[, month := month(date)]
tmp_crsp[, PRC := as.numeric(PRC)]
tmp_crsp[PRC < 0, PRC := abs(PRC)]
tmp_crsp[, RET := as.numeric(RET)]
setkey(tmp_crsp,PERMNO,date)
tmp_crsp[, RETX := as.numeric(RETX)]
tmp_crsp[, DLRET := as.numeric(DLRET)]
tmp_crsp[, DLRETX := as.numeric(DLRETX)]
tmp_crsp[is.na(RET), RET := DLRET]
tmp_crsp[is.na(RETX), RETX := DLRETX]
tmp_crsp[, vwretd := as.numeric(vwretd)]
tmp_crsp[, vwretx := as.numeric(vwretx)]
gaming_crsp = tmp_crsp[SICCD =="7999"]
tmp_crsp = NULL
gaming_crsp[, mcap := PRC * as.integer(SHROUT)]
setkey(tmp_crsp,PERMNO,year,month)
gaming_crsp[, month_mcap := mean(mcap,na.rm = TRUE), by = c("PERMNO","year","month")]
gaming_crsp = unique(gaming_crsp,by=c("PERMNO","year","month"))
gaming_crsp[, gaming_mcap := sum(month_mcap), by = c("year","month")]
gaming_crsp = unique(gaming_crsp, by=c("year","month"))

# monthly market caps
tmp_mcap = fread(input = 'all_tmp_mcap.csv')
tmp_mcap = tmp_mcap[!is.na(month_mcap)]
setkey(tmp_mcap,year,month,PERMNO)
tmp_mcap = setorder(setDT(tmp_mcap), year,month, -month_mcap)[, indx := seq_len(.N), by = c("year","month")][indx <= 500]
tmp_mcap[, mcap500 := sum(month_mcap), by = c("year","month")]
tmp_mcap = unique(tmp_mcap,by=c("year","month"))

gaming_mcap = merge(subset(gaming_crsp,select = c("year","month","gaming_mcap")),
                    subset(tmp_mcap,select = c("year","month","mcap500")),by=c("year","month"))
gaming_mcap[, gratio := gaming_mcap / mcap500]

m_returns = merge(m_returns,gaming_mcap,by=c("year","month"),all.x=TRUE)
lm_gaming3 = lm(av_mang~market*gaming_mcap+SMB+HML,data = m_returns)
lm_gaming5 = lm(av_mang~market*gaming_mcap+SMB+HML+RMW+CMA,data = m_returns)

lm_gratio3 = lm(av_mang~market*gratio+SMB+HML,data = m_returns)
lm_gratio5 = lm(av_mang~market*gratio+SMB+HML+RMW+CMA,data = m_returns)

stargazer(lm_gaming3,lm_gaming5,lm_gratio3,lm_gratio5,out = 'tab_gaminglm.tex')

# leverage restricted performance #
adj_rest_m_vol_weights = c(adj_m_vol_weights)
adj_rest_m_vol_weights[adj_rest_m_vol_weights > 1.5] = 1.5
adj_rest_m_vol_returns = adj_rest_m_vol_weights * m_bh_returns

adj_rest_m_av_weights = c(adj_m_av_weights)
adj_rest_m_av_weights[adj_rest_m_av_weights > 1.5] = 1.5
adj_rest_m_av_returns = adj_rest_m_av_weights * m_bh_returns
#### performance table ####
rest_perf_dt = data.table(Strategy = rep(c("BH","SV","AV")))
fq = 12
r0 = m_bh_returns
r1 = adj_rest_m_vol_returns
r2 = adj_rest_m_av_returns

SR1 = mean(r1)/sd(r1) * sqrt(12)
SR2 = mean(r2)/sd(r2) * sqrt(12)
SOR1 = SortinoRatio(r1) * sqrt(12)
SOR2 = SortinoRatio(r2) * sqrt(12)
K1 = Kappa0(r1)
K2 = Kappa0(r2)
UP1 = UpsidePotentialRatio(r1)
UP2 = UpsidePotentialRatio(r2)
gR1 = genRachev.ratio(r1)
gR2 = genRachev.ratio(r2)

rest_perf_dt[Strategy == "BH", RET := as.character(round(mean(r0)*fq*100,3))]
ar.p = t.test(r2,r1,alternative = "less",paired = TRUE,var.equal = TRUE)$p.value
sp = data.table(avg = round(mean(r1)*fq*100,3), pval = ar.p)
rest_perf_dt[Strategy == "SV", RET := see.stars(round(as.matrix(sp),3),1,2)[1]]
ar.p = t.test(r1,r2,alternative = "less",paired = TRUE,var.equal = TRUE)$p.value
sp = data.table(avg = round(mean(r2)*fq*100,3), pval = ar.p)
rest_perf_dt[Strategy == "AV", RET := see.stars(round(as.matrix(sp),3),1,2)[1]]

# r1 = unlist(return_resid[,2])
# r2 = unlist(return_resid[,1])

rest_perf_dt[Strategy == "BH", Sharpe := as.character(round((mean(r0)/sd(r0))* sqrt(12),3))]
rest_perf_dt[Strategy == "BH", Sortino :=  as.character(round(SortinoRatio(r0)* sqrt(12),3))]
rest_perf_dt[Strategy == "BH", Kappa := as.character(round(Kappa0(r0),3))]
rest_perf_dt[Strategy == "BH", UpsidePotential := as.character(round(UpsidePotentialRatio(r0),3))]
rest_perf_dt[Strategy == "BH", Rachev := as.character(round(genRachev.ratio(r0),3))]


sharpep = ratio.test1(r2,r1)
sp = data.table(SR1, pval = sharpep)
sp = round(matrix(as.numeric(sp),nrow=1),3)
rest_perf_dt[Strategy == "SV", Sharpe := see.stars(round(as.matrix(sp),3),1,2)[1]]
sp = data.table(SR = SOR1, pval = ratio.test2(r2,r1,ratio = "sortinoR"))
sp = round(matrix(as.numeric(sp),nrow=1),3)
rest_perf_dt[Strategy == "SV", Sortino := see.stars(round(as.matrix(sp),3),1,2)[1]]
sp = data.table(SR = K1, pval = ratio.test2(r2,r1,ratio = "Kappa0"))
sp = round(matrix(as.numeric(sp),nrow=1),3)
rest_perf_dt[Strategy == "SV", Kappa := see.stars(round(as.matrix(sp),3),1,2)[1]]
sp = data.table(SR = UP1, pval = ratio.test2(r2,r1,ratio = "UpsidePotentialRatio"))
sp = round(matrix(as.numeric(sp),nrow=1),3)
rest_perf_dt[Strategy == "SV", UpsidePotential := see.stars(round(as.matrix(sp),3),1,2)[1]]
sp = data.table(SR = gR1, pval = ratio.test2(r2,r1,ratio = "genRachev.ratio"))
sp = round(matrix(as.numeric(sp),nrow=1),3)
rest_perf_dt[Strategy == "SV", Rachev := see.stars(round(as.matrix(sp),3),1,2)[1]]


sharpep = ratio.test1(r1,r2,24)
sp = data.table(SR2, pval = sharpep)
sp = round(matrix(as.numeric(sp),nrow=1),3)
rest_perf_dt[Strategy == "AV", Sharpe := see.stars(round(as.matrix(sp),3),1,2)[1]]
sp = data.table(SR = SOR2, pval = ratio.test2(r1,r2,ratio = "sortinoR"))
sp = round(matrix(as.numeric(sp),nrow=1),3)
rest_perf_dt[Strategy == "AV", Sortino := see.stars(round(as.matrix(sp),3),1,2)[1]]
sp = data.table(SR = K2, pval = ratio.test2(r1,r2,ratio = "Kappa0"))
sp = round(matrix(as.numeric(sp),nrow=1),3)
rest_perf_dt[Strategy == "AV", Kappa := see.stars(round(as.matrix(sp),3),1,2)[1]]
sp = data.table(SR = UP2, pval = ratio.test2(r1,r2,ratio = "UpsidePotentialRatio"))
sp = round(matrix(as.numeric(sp),nrow=1),3)
rest_perf_dt[Strategy == "AV", UpsidePotential := see.stars(round(as.matrix(sp),3),1,2)[1]]
sp = data.table(SR = gR2, pval = ratio.test2(r1,r2,ratio = "genRachev.ratio"))
sp = round(matrix(as.numeric(sp),nrow=1),3)
rest_perf_dt[Strategy == "AV", Rachev := see.stars(round(as.matrix(sp),3),1,2)[1]]

adj_rest1_m_vol_weights = c(adj_m_vol_weights)
adj_rest1_m_vol_weights[adj_rest1_m_vol_weights > 1] = 1
adj_rest1_m_vol_returns = adj_rest1_m_vol_weights * m_bh_returns

adj_rest1_m_av_weights = c(adj_m_av_weights)
adj_rest1_m_av_weights[adj_rest1_m_av_weights > 1] = 1
adj_rest1_m_av_returns = adj_rest1_m_av_weights * m_bh_returns
#### performance table ####
rest1_perf_dt = data.table(Strategy = rep(c("BH","SV","AV")))
fq = 12
r0 = m_bh_returns
r1 = adj_rest1_m_vol_returns
r2 = adj_rest1_m_av_returns

SR1 = mean(r1)/sd(r1) * sqrt(12)
SR2 = mean(r2)/sd(r2) * sqrt(12)
SOR1 = SortinoRatio(r1) * sqrt(12)
SOR2 = SortinoRatio(r2) * sqrt(12)
K1 = Kappa0(r1)
K2 = Kappa0(r2)
UP1 = UpsidePotentialRatio(r1)
UP2 = UpsidePotentialRatio(r2)
gR1 = genRachev.ratio(r1)
gR2 = genRachev.ratio(r2)

rest1_perf_dt[Strategy == "BH", RET := as.character(round(mean(r0)*fq*100,3))]
ar.p = t.test(r2,r1,alternative = "less",paired = TRUE,var.equal = TRUE)$p.value
sp = data.table(avg = round(mean(r1)*fq*100,3), pval = ar.p)
rest1_perf_dt[Strategy == "SV", RET := see.stars(round(as.matrix(sp),3),1,2)[1]]
ar.p = t.test(r1,r2,alternative = "less",paired = TRUE,var.equal = TRUE)$p.value
sp = data.table(avg = round(mean(r2)*fq*100,3), pval = ar.p)
rest1_perf_dt[Strategy == "AV", RET := see.stars(round(as.matrix(sp),3),1,2)[1]]

# r1 = unlist(return_resid[,2])
# r2 = unlist(return_resid[,1])

rest1_perf_dt[Strategy == "BH", Sharpe := as.character(round((mean(r0)/sd(r0))* sqrt(12),3))]
rest1_perf_dt[Strategy == "BH", Sortino :=  as.character(round(SortinoRatio(r0)* sqrt(12),3))]
rest1_perf_dt[Strategy == "BH", Kappa := as.character(round(Kappa0(r0),3))]
rest1_perf_dt[Strategy == "BH", UpsidePotential := as.character(round(UpsidePotentialRatio(r0),3))]
rest1_perf_dt[Strategy == "BH", Rachev := as.character(round(genRachev.ratio(r0),3))]


sharpep = ratio.test1(r2,r1)
sp = data.table(SR1, pval = sharpep)
sp = round(matrix(as.numeric(sp),nrow=1),3)
rest1_perf_dt[Strategy == "SV", Sharpe := see.stars(round(as.matrix(sp),3),1,2)[1]]
sp = data.table(SR = SOR1, pval = ratio.test2(r2,r1,ratio = "sortinoR"))
sp = round(matrix(as.numeric(sp),nrow=1),3)
rest1_perf_dt[Strategy == "SV", Sortino := see.stars(round(as.matrix(sp),3),1,2)[1]]
sp = data.table(SR = K1, pval = ratio.test2(r2,r1,ratio = "Kappa0"))
sp = round(matrix(as.numeric(sp),nrow=1),3)
rest1_perf_dt[Strategy == "SV", Kappa := see.stars(round(as.matrix(sp),3),1,2)[1]]
sp = data.table(SR = UP1, pval = ratio.test2(r2,r1,ratio = "UpsidePotentialRatio"))
sp = round(matrix(as.numeric(sp),nrow=1),3)
rest1_perf_dt[Strategy == "SV", UpsidePotential := see.stars(round(as.matrix(sp),3),1,2)[1]]
sp = data.table(SR = gR1, pval = ratio.test2(r2,r1,ratio = "genRachev.ratio"))
sp = round(matrix(as.numeric(sp),nrow=1),3)
rest1_perf_dt[Strategy == "SV", Rachev := see.stars(round(as.matrix(sp),3),1,2)[1]]


sharpep = ratio.test1(r1,r2,24)
sp = data.table(SR2, pval = sharpep)
sp = round(matrix(as.numeric(sp),nrow=1),3)
rest1_perf_dt[Strategy == "AV", Sharpe := see.stars(round(as.matrix(sp),3),1,2)[1]]
sp = data.table(SR = SOR2, pval = ratio.test2(r1,r2,ratio = "sortinoR"))
sp = round(matrix(as.numeric(sp),nrow=1),3)
rest1_perf_dt[Strategy == "AV", Sortino := see.stars(round(as.matrix(sp),3),1,2)[1]]
sp = data.table(SR = K2, pval = ratio.test2(r1,r2,ratio = "Kappa0"))
sp = round(matrix(as.numeric(sp),nrow=1),3)
rest1_perf_dt[Strategy == "AV", Kappa := see.stars(round(as.matrix(sp),3),1,2)[1]]
sp = data.table(SR = UP2, pval = ratio.test2(r1,r2,ratio = "UpsidePotentialRatio"))
sp = round(matrix(as.numeric(sp),nrow=1),3)
rest1_perf_dt[Strategy == "AV", UpsidePotential := see.stars(round(as.matrix(sp),3),1,2)[1]]
sp = data.table(SR = gR2, pval = ratio.test2(r1,r2,ratio = "genRachev.ratio"))
sp = round(matrix(as.numeric(sp),nrow=1),3)
rest1_perf_dt[Strategy == "AV", Rachev := see.stars(round(as.matrix(sp),3),1,2)[1]]

restricted_perf = rbind(rest_perf_dt,rest1_perf_dt)
stargazer(restricted_perf,summary = FALSE,out = 'tab_rest_perf.tex')
