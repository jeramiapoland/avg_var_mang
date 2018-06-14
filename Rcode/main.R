paks <- c("RCurl","data.table","tis","lubridate","ggplot2","stringr","sandwich","stargazer","pracma","RColorBrewer",
          "CADFtest","complexplus","readxl","reshape2","quantmod","xlsx","tikzDevice","MASS","timeSeries","vars","PortfolioEffectHFT",
          "PortfolioAnalytics","PerformanceAnalytics","backtest","tidyr","broom","stringdist","BH","parallel","doMC","foreach",
          "doParallel","lmtest","hypergeo","strucchange","formula.tools","multiwave","outliers","forecast","SharpeR","fastmatch",
          "bvarsv","boot","goftest","DescTools","dunn.test","generalCorr","cointReg","psd","plot3D","rootSolve",
          "fitdistrplus","conics") 
# note: tikzDevice requires a working latex installation
# and xlsx require rJava so a properly configured java (try javareconf)
for (p in paks){
  require(p,character.only = TRUE) || {install.packages(p) 
    require(p,character.only = TRUE)}
}
source(file = 'functions.R')

#### Quarterly Data ####
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

q_data[, mkt_var.tp1 := shift(mkt_var,type = "lead")]
q_data[, avg_var.tp1 := shift(avg_var,type = "lead")]
m_data[, mkt_var1m.tp1 := shift(mkt_var1m,type = "lead")]
m_data[, avg_var1m.tp1 := shift(avg_var1m,type = "lead")]
m_data[, avg_cor1m.tp1 := shift(avg_cor1m,type = "lead")]

#### Monthly data file ####
fwrite(x = m_data,file = 'm_data.csv')
data = NULL
gc()


#### summary stats ####
pw_start = which(q_data$year == 1963 & q_data$quarter == 2)
pw_end = which(q_data$year == 2007 & q_data$quarter == 1)
s1 = q_data[pw_start:pw_end, .(RET = logxret.tp3 * 100, AC = avg_cor, AV = avg_var * 100, SV = mkt_var * 100)]
stargazer(s1,summary = TRUE,out = './tables/summary/summary1.tex',out.header = FALSE)
s1_auto = s1[, .(autoC = lapply(.SD,get_ac,1)), .SDcols = colnames(s1)]

paper_q_start = pw_start
paper_m_start = which(m_data$year==1926&m_data$month==7)
paper_1962_start = which(m_data$year==1962&m_data$month==6)


m_data[1:(nrow(m_data)-1), logxret.tp3 := shift(runSum(logxret.tp1,n=3),n=2,type="lead")]
m_data[1:(nrow(m_data)-1), logxret.tp6 := shift(runSum(logxret.tp1,n=6),n=5,type="lead")]
m_data[1:(nrow(m_data)-1), logxret.tp12 := shift(runSum(logxret.tp1,n=12),n=11,type="lead")]
s3 = m_data[(paper_m_start+1):nrow(m_data), .("RET" = logxret * 100,AC = avg_cor1m, AV = avg_var1m * 100, SV = mkt_var1m * 100)]
stargazer(s3,summary = TRUE,out = './tables/summary/summary3.tex')
s3_auto = s3[!is.na(RET), .(autoC = lapply(.SD,get_ac,1)), .SDcols = colnames(s3)]

s4 = m_data[paper_1962_start:nrow(m_data), .("RET" = logxret * 100,AC = avg_cor1m, AV = avg_var1m * 100, SV = mkt_var1m * 100)]
stargazer(s4,summary = TRUE,out = './tables/summary/summary4.tex')
s4_auto = s4[!is.na(RET), .(autoC = lapply(.SD,get_ac,1)), .SDcols = colnames(s3)]

#### correlation table ####
mmat = matrix(NA_real_,8,8)
mmat[] = 1
mmat[upper.tri(mmat)] = 0
s3_alt = data.table(s3)
s3_alt[,paste0(colnames(s3),".tp1") := lapply(.SD,FUN = shift,type="lead"),
       .SDcols = colnames(s3)]
cors3 = cor(na.omit(s3_alt)) * mmat
stargazer(round(cors3,3),summary = FALSE,float = FALSE,out.header = FALSE,
          out = "tables/summary/tab_correlation.tex")

s4_alt = data.table(s4)
s4_alt[,paste0(colnames(s4),".tp1") := lapply(.SD,FUN = shift,type="lead"),
       .SDcols = colnames(s4)]
cors4 = cor(na.omit(s4_alt)) * mmat
stargazer(round(cors4,3),summary = FALSE,float = FALSE,out.header = FALSE,
          out = "tables/summary/tab_correlation_short.tex")


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
tikz("./figures/q_plot.tex",width = 5.90551, height = 3,sanitize = FALSE)
plot(q_plot)
dev.off()


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
tikz("./figures/m_plot2.tex",width = 5.90551, height = 3, sanitize = FALSE)
plot(m_plot2)
dev.off()

#### in sample regressions ####

y_list = list(quarterly = c("avg_var.tp1","logxret.tp3"), monthly = c("avg_cor1m.tp1","mkt_var1m.tp1","avg_var1m.tp1","logxret.tp1"))
y_names = c(avg_cor1m.tp1 = "AC$_{t+1}$",avg_var.tp1 = "AV$_{t+1}$",
            mkt_var1m.tp1 = "SV$_{t+1}$",avg_var1m.tp1 = "AV$_{t+1}$",
            logxret.tp3 = "RET$_{t+3}$", logxret.tp1 = "RET$_{t+1}$")
x_list = list(quarterly = "avg_var", monthly = c("avg_var1m","mkt_var1m","avg_cor1m"))
x_names = c(avg_var = "AV$_{t}$", avg_var1m = "AV$_{t}$", mkt_var1m = "SV$_{t}$")
sp = c("Monthly")
freq = c("monthly")
names(freq) = sp
b_freq = c(quarterly = "mkt_var",monthly = "mkt_var1m")

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


spans = list("full" = m_data$date,
             "pre1962" = m_data[date <= "1962-06-28"]$date,
             "post1962" = m_data[date >= "1962-06-28"]$date,
             "contractions" = contractions,
             "expansions" = m_data[!(date %fin% contractions)]$date)

Xsign = data.table(Y = y_list[["monthly"]])
Xsign[Y == "avg_cor1m.tp1", c("avg_var1m","mkt_var1m","avg_cor1m") := list(1,1,1)]
Xsign[Y == "mkt_var1m.tp1", c("avg_var1m","mkt_var1m","avg_cor1m") := list(1,1,1)]
Xsign[Y == "avg_var1m.tp1", c("avg_var1m","mkt_var1m","avg_cor1m") := list(1,1,1)]
Xsign[Y == "logxret.tp1", c("avg_var1m","mkt_var1m","avg_cor1m") := list(-1,1,1)]
setkey(Xsign,Y)

RHS = list("avg_var1m","avg_cor1m","mkt_var1m",c("avg_var1m","avg_cor1m"),c("avg_var1m","mkt_var1m"))
# corrected_dt = data.table(Y = y_list[["monthly"]])
for(sp in 1:length(spans)){
  dt = subset(m_data,subset = date %fin% spans[[sp]])
  dtcols = colnames(dt)
  dtidx = unlist(lapply(dt,class))=="numeric"
  dt[, dtcols[dtidx] := lapply(.SD,scale.na), .SDcols = dtcols[dtidx]]
  for(y in y_list[["monthly"]]){
    forms = list(f1 = as.formula(paste0(y," ~ avg_var1m")),
                 f2 = as.formula(paste0(y," ~ avg_cor1m")),
                 f3 = as.formula(paste0(y," ~ mkt_var1m")),
                 f4 = as.formula(paste0(y," ~ avg_var1m + avg_cor1m")),
                 f5 = as.formula(paste0(y," ~ avg_var1m + mkt_var1m")) )
    clabs = c("AV","AC","SV")
    for(f in 1:length(forms)){
      assign(paste0("lm",f),lm(formula = forms[[f]],data = dt))
    }
    stargazer(lapply(X = 1:length(forms),FUN = function(n){get(paste0("lm",n))}),
              out = paste0("./tables/insample/tab_in_",y,"_",names(spans[sp]),".tex"),
              covariate.labels = clabs,
              dep.var.caption = "",out.header = FALSE,style = "qje",
              dep.var.labels = y_names[y],df = FALSE,digits = 3,float = FALSE,
              keep.stat = c("n","rsq","adj.rsq"),star.char = "*",digits.extra = 2,initial.zero = TRUE,
              model.numbers = FALSE,report = "vc*p")
    # Amihud and Hurvich 2004 reduced bias regressions
    corrected_dt = data.table(Y = rep(eval(y),length(forms)), 
                              Form = unlist(lapply(forms,FUN = as.character)))
    for(R in 1:length(RHS)){
      Rvec = RHS[[R]]
      dt2 = subset(m_data,subset = date %fin% spans[[sp]],select=c(eval(y),Rvec))
      dt2 = dt2[complete.cases(dt2)]
      dtcols2 = colnames(dt2)
      dtidx2 = unlist(lapply(dt2,class))=="numeric"
      dt2[, dtcols2[dtidx2] := lapply(.SD,scale), .SDcols = dtcols2[dtidx2]]
      xsig = unlist(Xsign[Y == eval(y), Rvec, with = FALSE])
      corrected_dt[Form == as.character(forms[[R]]), 
                   c(Rvec,paste0("t.stat.",Rvec),paste0("p.val.",Rvec),"r.squared","adj.r.squared") :=
                     as.list(unbiased_lm2(as.matrix(dt2),expSig = xsig, 
                                              N = 10000, lag = 1, aic = FALSE))]
    }
    corrected_dt[, 3:ncol(corrected_dt)] = round(corrected_dt[, 3:ncol(corrected_dt)],3)
    setcolorder(corrected_dt,
                neworder = c(colnames(corrected_dt)[!colnames(corrected_dt) %in% c("r.squared","adj.r.squared")],
                             c("r.squared","adj.r.squared")))
    stargazer(corrected_dt,summary = FALSE,float = FALSE,out.header = FALSE,
              out = paste0("./tables/insample/tab_in_",y,"_",names(spans[sp]),"_biascorrected.tex"))
  }
}


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

for(sp in 1:length(spans)){
  dt = m_data[date %fin% spans[[sp]]]
  # dtcols = colnames(dt)
  # dtidx = unlist(lapply(dt,class))=="numeric"
  # dt[, dtcols[dtidx] := lapply(.SD,scale.na), .SDcols = dtcols[dtidx]]
  y_vars = y_list[["monthly"]]
  x_vars = x_list[["monthly"]]
  for(x in x_vars){
    oos_table = data.table(variable = c("AC$_{t+1}$","SV$_{t+1}$","AV$_{t+1}$","RET$_{t+1}$"))
    oos_table2 = data.table(variable = c("AC$_{t+1}$","SV$_{t+1}$","AV$_{t+1}$","RET$_{t+1}$"))
    for(y in y_vars){
      yn = y_names[y]
      f = as.formula(paste0(y,"~",x))
      # if(fq=="quarterly"){
      #   dt = q_data[paper_q_start:nrow(q_data)]
      # } else {dt = m_data[paper_1962_start:nrow(m_data)]}
      # if(s == "1983Q2:2007Q1"){
      #   dt = q_data[pw_start:pw_end]
      #   train = 81
      # } else {
      train = floor(.15 * nrow(dt))
      # }
      oos_table[variable == yn, 
                c("DM","MSE-F","ENC-HLN") := as.list(lm.oos(f,dt,bench=NULL,train)$Statistics)]
      if(!x==b_freq["monthly"]){
        oos_table2[variable == yn, 
                   c("DM","MSE-F","ENC-HLN") := as.list(lm.oos(f,dt,bench=b_freq["monthly"],train)$Statistics)]
      }
    }
    if(!x==b_freq["monthly"]){
      oos_table = rbind(oos_table,oos_table2)
    }
    # stargazer latex names workaround #
    star = stargazer(oos_table,summary = FALSE,rownames = FALSE,
                     column.labels = c("","Sample",c("DM","MSE-F","ENC-HLN")))
    cat(star, sep = '\n', file = paste0('./tables/outsample/tab_oos_',x,'_',names(spans[sp]),'.tex'))
  }
}
# oos_table = data.table(variable = c(rep("AC$_{t+1}$",2),rep("SV$_{t+1}$",2),rep("AV$_{t+1}$",2),c("RET$_{t+3}$","RET$_{t+1}$","RET$_{t+3}$")), Sample = "Monthly")
# oos_table2 = data.table(variable = c(rep("AC$_{t+1}$",2),rep("SV$_{t+1}$",2),rep("AV$_{t+1}$",2),c("RET$_{t+3}$","RET$_{t+1}$","RET$_{t+3}$")), Sample = "Monthly")
# 
# for(s in sp){
#   fq = freq[s]
#   y_vars = y_list[[fq]]
#   x = x_vars[fq]
#   for(y in y_vars){
#     yn = y_names[y]
#     f = as.formula(paste0(y,"~",x))
#     if(fq=="quarterly"){
#       dt = q_data[paper_q_start:nrow(q_data)]
#     } else {dt = m_data[paper_1962_start:nrow(m_data)]}
#     if(s == "1983Q2:2007Q1"){
#       dt = q_data[pw_start:pw_end]
#       train = 81
#     } else {
#       train = floor(.15 * nrow(dt))
#     }
#     oos_table[variable == yn & Sample == s, 
#               c("$R^{2}_{oos}$","MSE-F","ENC-NEW","ENC-HLN") := as.list(lm.oos(f,dt,bench=NULL,train)$Statistics)]
#     oos_table2[variable == yn & Sample == s, 
#                c("$R^{2}_{oos}$","MSE-F","ENC-NEW","ENC-HLN") := as.list(lm.oos(f,dt,bench=b_freq[fq],train)$Statistics)]
#   }
# }
# oos_table[, `MSE-F` :=  round(as.numeric(`MSE-F`),3)]
# oos_table2[, `MSE-F` :=  round(as.numeric(`MSE-F`),3)]
# 
# oos_table = rbind(oos_table,oos_table2)
# 
# stargazer(oos_table,summary = FALSE,out = 'tab_oos.tex',rownames = FALSE,
#           column.labels = c("","Sample",c("$R^{2}_{oos}$","MSE-F","ENC-NEW","ENC-HLN")))

## first half oos ##

# oos_table = data.table(variable = c(rep("AC$_{t+1}$",2),rep("SV$_{t+1}$",2),rep("AV$_{t+1}$",2),c("RET$_{t+3}$","RET$_{t+1}$")), Sample = c(sp,sp,"Monthly"))
# oos_table2 = data.table(variable = c(rep("AC$_{t+1}$",2),rep("SV$_{t+1}$",2),rep("AV$_{t+1}$",2),c("RET$_{t+3}$","RET$_{t+1}$")), Sample = c(sp,sp,"Monthly"))
# 
# for(s in sp){
#   fq = freq[s]
#   y_vars = y_list[[fq]]
#   x = x_vars[fq]
#   for(y in y_vars){
#     yn = y_names[y]
#     f = as.formula(paste0(y,"~",x))
#     if(fq=="quarterly"){
#       dt = q_data[paper_q_start:nrow(q_data)]
#     } else {dt = m_data[paper_m_start:paper_1962_start]}
#     if(s == "1983Q2:2007Q1"){
#       dt = q_data[pw_start:pw_end]
#       train = 81
#     } else {
#       train = floor(.15 * nrow(dt))
#     }
#     oos_table[variable == yn & Sample == s, 
#               c("$R^{2}_{oos}$","MSE-F","ENC-NEW","ENC-HLN") := as.list(lm.oos(f,dt,bench=NULL,train)$Statistics)]
#     oos_table2[variable == yn & Sample == s, 
#                c("$R^{2}_{oos}$","MSE-F","ENC-NEW","ENC-HLN") := as.list(lm.oos(f,dt,bench=b_freq[fq],train)$Statistics)]
#   }
# }
# oos_table[, `MSE-F` :=  round(as.numeric(`MSE-F`),3)]
# oos_table2[, `MSE-F` :=  round(as.numeric(`MSE-F`),3)]
# 
# oos_table = rbind(oos_table,oos_table2)
# 
# stargazer(oos_table,summary = FALSE,out = 'tab_oos2.tex',rownames = FALSE,
#           column.labels = c("","Sample",c("$R^{2}_{oos}$","MSE-F","ENC-NEW","ENC-HLN")))

#### out of sample robust stats ####
y_vars = y_list[["monthly"]]
robust_table = data.table(Stat = c(rep("$R_{T}$",4),rep("$A_{T}$",4)), 
                          Variable = rep(c("AC$_{t+1}$","SV$_{t+1}$","AV$_{t+1}$","RET$_{t+1}$"),2))

for(y in y_vars){
  yn = y_names[y]
  f = as.formula(paste0(y," ~ ","avg_var1m"))
  lowR = floor(.15 * nrow(m_data))
  highR = ceiling(.85*nrow(m_data))
  m1 = dm.test.Robust(formula = f,data = m_data,bench = "mkt_var1m",lowR,highR)
  # m2 = mspe.adjRobust(formula = f,data = m_data,bench = "mkt_var1m",lowR,highR)
  m3 = enc.hln.Robust(formula = f,data = m_data,bench = "mkt_var1m",lowR,highR)
  robust_table[Variable == yn & Stat == "$R_{T}$", 
               c("DM","ENC-HLN",paste0(c("DM","ENC-HLN"),"2")) := as.list(c(
                 m1$Exp_RET, m3$Exp_RET, m1$Roll_RET, m3$Roll_RET))]
  robust_table[Variable == yn &  Stat == "$A_{T}$", 
                c("DM","ENC-HLN",paste0(c("DM","ENC-HLN"),"2")) := as.list(
                  c(m1$Exp_AET, m3$Exp_AET,m1$Roll_AET, m3$Roll_AET))]
  # robust_table[Variable == yn &  Stat == "$CW_{T}$", c("adj.MSPE","adj.MSPE2") := as.list(
  #                c(m2$exp_CWT,m2$roll_CWT))]
}

write.csv(robust_table,file = "tables/robust.csv")

#### asset allocation ####

# monthly
#m_start = (paper_1962_start-1) + floor(.15*length((paper_1962_start-1):nrow(m_data)))
# train = floor(.15 * length(paper_1962_start:nrow(m_data)))
m_bh_returns = m_data$logxret.tp1[(paper_m_start+1):(nrow(m_data))]

tar_sds = c(.029,.035,sd(m_bh_returns))
m_sv_weights = (1/m_data[(paper_m_start+1):(nrow(m_data))]$mkt_var1m)
m_sv_returns = m_sv_weights * m_bh_returns
sv_sd = sd(m_sv_returns)
m_av_weights = (1/m_data[(paper_m_start+1):(nrow(m_data))]$avg_var1m)
m_av_returns = m_av_weights * m_bh_returns
av_sd = sd(m_av_returns)
for(t in tar_sds){
  suf = str_extract(t,"[:digit:]{2,3}")
  assign(paste0("c_sv_",suf),(t / sv_sd))
  assign(paste0("sv_",suf,"_weights"), ((t / sv_sd) * m_sv_weights))
  assign(paste0("sv_",suf,"_returns"), ((t / sv_sd) * m_sv_weights) * m_bh_returns)
  assign(paste0("c_av_",suf),(t / av_sd))
  assign(paste0("av_",suf,"_weights"), ((t / av_sd) * m_av_weights))
  assign(paste0("av_",suf,"_returns"), ((t / av_sd) * m_av_weights) * m_bh_returns)
}

sufx = vapply(X = tar_sds,FUN = str_extract,"[:digit:]{2,3}",FUN.VALUE = "0")

#### All returns ####
upper_lev = seq(1,3,.1)
r_dates = m_data[(paper_m_start+1):(nrow(m_data))]$date
returns_dt = array(dim = c(2,3,length(upper_lev)+1,length(m_bh_returns)),
                   dimnames = list(c("av","sv"),sufx,c("NO",upper_lev),as.character(r_dates)))
for(s in sufx){
  returns_dt["av",s,"NO",] = get(paste0("av_",suf,"_returns"))
  returns_dt["sv",s,"NO",] = get(paste0("sv_",suf,"_returns"))
}

for(p in c("av","sv")){
  for(s in sufx){
    w = get(paste0(p,"_",s,"_weights"))
    for(u in upper_lev){
      tw = replace(w, w>=u, u)
      returns_dt[p,s,as.character(u),] = tw * m_bh_returns
    }
  }
}


weights_table = data.table(vol_mang = sv_053_weights, av_mang = av_053_weights,
                           vol_mang_10 = sv_029_weights, av_mang_10 = av_029_weights,
                           vol_mang_12 = sv_035_weights, av_mang_12 = av_035_weights)
stargazer(weights_table,out = "tables/performance/tab_weights.tex",
          summary.stat = c("n","mean","sd","min","p25","median","p75","max"))
weights_table = data.table(m_data$date[(paper_m_start+1):(nrow(m_data))],weights_table)
weights_table[, V1 := V1 + months(1)] # adjust date to the month weight is used
weights_dt = melt(subset(weights_table,select = c(V1,vol_mang,av_mang)),id.vars = "V1",variable.name = "Strategy",value.name = "Weight")

weight_plot2 = ggplot(data=weights_dt) + geom_line(aes(x=V1,y=Weight,color=Strategy,linetype=Strategy)) + 
  labs(title = "Strategy Investment Weight", x = "", y = "") + 
  scale_x_date(date_breaks = "5 year", date_labels =  "%Y") +
  scale_colour_manual(name = "Strategy",values=cbPalette,labels =c("SV","AV")) + 
  scale_linetype_manual(name = "Strategy",values=linePalette,labels =c("SV","AV")) +
  theme(text = element_text(size=11),panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line( size=.1, color="black"))+ theme_bw()
weight_plot2 = nberShade(weight_plot2,xrange = c(min(weights_dt$V1), max(weights_dt$V1)),openShade = FALSE)
tikz("figures/weight_plot2.tex",width = 5.90551, height = 3, sanitize = FALSE)
plot(weight_plot2)
dev.off()


#### performance table ####
perf_dt = array(dim = c(length(sufx),length(spans),3,6,3),dimnames = list(Target = sufx,Sample = names(spans),
                                                                          Strategy = c("BH","SV","AV"),
                                                                          Measure = c("Return","Sharpe","Sortino","Kappa$_{3}$",
                                                                                      "Kappa$_{4}$","Rachev"),
                                                                          Constraint = c("1.5","3","NO")))
m_bh_dates = m_data$date[(paper_m_start+1):(nrow(m_data))]
set.seed(123)
for(s in 1:length(spans)){
  sp = spans[[s]]
  fq = 12
  r0 = c(m_bh_returns[m_bh_dates%fin% sp])
  sp_logical = as.Date(dimnames(returns_dt)[[4]]) %fin% sp
  for(c in sufx){
    for(l in c("1.5","3","NO")){
      r1 = returns_dt["sv",c,l,sp_logical]
      r2 = returns_dt["av",c,l,sp_logical]
      var1 = VAR(y = data.table(r1,r2),p = 1,type = "none", season = fq)
      return_resid = data.table(r1 = var1$varresult$r1$residuals,r2 = var1$varresult$r2$residuals)
      rr1 = return_resid$r1
      rr2 = return_resid$r2
      perf_dt[c,s,"BH",,l]  = c(as.character(round(mean(r0)*fq*100,3)),
                                as.character(round((mean(r0)/sd(r0))* sqrt(fq),3)),
                                as.character(round(sortinoR(r0,annualize = TRUE,freq = fq),3)),
                                as.character(round(Kappa03(r0),3)),
                                as.character(round(Kappa04(r0),3)),
                                as.character(round(genRachev.ratio(r0),3)))
      svp_dt = c(as.character(round(mean(r1)*fq*100,3)),
                 hac_t.test(rr2,rr1,alternative = "less",
                            paired = TRUE,var.equal = TRUE)$p.val,
                 as.character(round(Sratio(r1,annualize = TRUE,freq = fq),3)),
                 ratio.test2(rets1 = rr1,rets2 = rr2,ratio = "Sratio"),
                 as.character(round(sortinoR(r1,annualize = TRUE,freq = fq),3)),
                 ratio.test2(rets1 = rr1,rets2 = rr2,ratio = "sortinoR"),
                 as.character(round(Kappa03(r1),3)),
                 ratio.test2(rets1 = rr1,rets2 = rr2,ratio = "Kappa03"),
                 as.character(round(Kappa04(r1),3)),
                 ratio.test2(rets1 = rr1,rets2 = rr2,ratio = "Kappa04"),
                 as.character(round(genRachev.ratio(r1),3)),
                 ratio.test2(rets1 = rr1,rets2 = rr2,ratio = "genRachev.ratio")
      )
      svp_dt[is.na(svp_dt)] = 0
      perf_dt[c,s,"SV",,l] = see.stars(matrix(as.numeric(svp_dt),nrow=1),seq(1,11,by=2),seq(2,12,by=2))[seq(1,11,by=2)]
      avp_dt = c(as.character(round(mean(r2)*fq*100,3)),
                 hac_t.test(rr1,rr2,alternative = "less",
                            paired = TRUE,var.equal = TRUE)$p.val,
                 as.character(round(Sratio(r2,annualize = TRUE,freq = fq),3)),
                 ratio.test2(rets1 = rr2,rets2 = rr1,ratio = "Sratio"),
                 as.character(round(sortinoR(r2,annualize = TRUE,freq = fq),3)),
                 ratio.test2(rets1 = rr2,rets2 = rr1,ratio = "sortinoR"),
                 as.character(round(Kappa03(r2),3)),
                 ratio.test2(rets1 = rr2,rets2 = rr1,ratio = "Kappa03"),
                 as.character(round(Kappa04(r2),3)),
                 ratio.test2(rets1 = rr2,rets2 = rr1,ratio = "Kappa04"),
                 as.character(round(genRachev.ratio(r2),3)),
                 ratio.test2(rets1 = rr2,rets2 = rr1,ratio = "genRachev.ratio")
      )
      avp_dt[is.na(avp_dt)] = 0
      perf_dt[c,s,"AV",,l] = see.stars(matrix(as.numeric(avp_dt),nrow=1),seq(1,11,by=2),seq(2,12,by=2))[seq(1,11,by=2)]
    }
  }
}


for(s in 1:length(spans)){
  for(c in sufx){
    out1 = cbind(perf_dt[c,s,,,"1.5"],perf_dt[c,s,,,"3"])
    assign(paste0("out",c),cbind(rep(c,3),c("BH","SV","AV"),out1))
    out2 = perf_dt[c,s,,,"NO"]
    assign(paste0("out2",c),cbind(rep(c,3),c("BH","SV","AV"),out1))
  }
  out1 = rbindlist(lapply(as.list(paste0("out",sufx)),function(x){as.data.frame(get(x))}))
  ltex = stargazer(out1,rownames = FALSE,summary = FALSE,out.header = FALSE)
  cat(x = ltex,sep = '\n',file = paste0("tables/performance/tab_perf_constrained_",names(spans)[s],".tex"))
  out2 = rbindlist(lapply(as.list(paste0("out2",sufx)),function(x){as.data.frame(get(x))}))
  ltex2 = stargazer(out2,rownames = FALSE,summary = FALSE,out.header = FALSE)
  cat(x = ltex2,sep = '\n',file = paste0("tables/performance/tab_perf_NO_",names(spans)[s],".tex"))
}


#### return plots ####
p1_dt = data.table(Date = rep(m_data[(paper_m_start+1):(nrow(m_data))]$date,3), 
                Strategy = c(rep("avg_var1m",dim(returns_dt)[4]),
                             rep("mkt_var1m",dim(returns_dt)[4]),
                             rep("market",dim(returns_dt)[4])),
                Return = c(returns_dt["av","053","1.5",],
                           returns_dt["sv","053","1.5",],
                           m_bh_returns),
                Constraint = rep("Leverage - 1.5",3*dim(returns_dt)[4]))
p1_dt[, Return := cumsum(Return), by = Strategy]
p2_dt = data.table(Date = rep(m_data[(paper_m_start+1):(nrow(m_data))]$date,3), 
                   Strategy = c(rep("avg_var1m",dim(returns_dt)[4]),
                                rep("mkt_var1m",dim(returns_dt)[4]),
                                rep("market",dim(returns_dt)[4])),
                   Return = c(returns_dt["av","053","3",],
                              returns_dt["sv","053","3",],
                              m_bh_returns),
                   Constraint = rep("Leverage - 3",3*dim(returns_dt)[4]))
p2_dt[, Return := cumsum(Return), by = Strategy]
p3_dt = data.table(Date = rep(m_data[(paper_m_start+1):(nrow(m_data))]$date,3), 
                   Strategy = c(rep("avg_var1m",dim(returns_dt)[4]),
                                rep("mkt_var1m",dim(returns_dt)[4]),
                                rep("market",dim(returns_dt)[4])),
                   Return = c(returns_dt["av","053","NO",],
                              returns_dt["sv","053","NO",],
                              m_bh_returns),
                   Constraint = rep("Leverage - Unconstrained",3*dim(returns_dt)[4]))
p3_dt[, Return := cumsum(Return), by = Strategy]

p1 = rbind(p1_dt,p2_dt,p3_dt)

m_ret_plot = ggplot(data=p1) + geom_line(aes(x=Date,y=Return,color=Strategy,linetype=Strategy)) + 
  labs(title = "Cummulative Excess Log Returns", x = "", y = "") + 
  scale_x_date(date_breaks = "5 year", date_labels =  "%Y") +
  scale_colour_manual(name = "Strategy",values=cbPalette,labels =c("AV","Market","SV")) + 
  scale_linetype_manual(name = "Strategy",values=linePalette,labels =c("AV","Market","SV")) +
  theme(text = element_text(size=11),panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line( size=.1, color="black")) + 
  facet_wrap(~Constraint,ncol = 1,nrow = 3,scales = "free_y") + 
  theme_bw()
m_ret_plot = nberShade(m_ret_plot,xrange = c(min(p1_dt$Date), max(p1_dt$Date)),openShade = FALSE)
tikz("figures/m_ret_plot2.tex",width = 5.90551, height = 9, sanitize = FALSE)
plot(m_ret_plot)
dev.off()

# p2 = data.table(Date = rep(m_data[paper_m_start:(nrow(m_data)-1)]$date,3), 
#                 Strategy = c(rep("avg_var1m",length(m_av_returns)),rep("mkt_var1m",length(m_vol_returns)),rep("market",length(m_bh_returns))),
#                 Return = c(m_av_returns,m_vol_returns,m_bh_returns))
# p2[, Return := cumsum(Return), by = Strategy]
# 
# m_ret_plot2 = ggplot(data=p2) + geom_line(aes(x=Date,y=Return,color=Strategy,linetype=Strategy)) + 
#   labs(title = "Cummulative Excess Log Returns - Monthly", x = "", y = "") + 
#   scale_x_date(date_breaks = "5 year", date_labels =  "%Y") +
#   scale_colour_manual(name = "Strategy",values=cbPalette,labels =c("AV","Market","SV")) + 
#   scale_linetype_manual(name = "Strategy",values=linePalette,labels =c("AV","Market","SV")) +
#   theme(text = element_text(size=11),panel.grid.major.x = element_blank(),
#         panel.grid.major.y = element_line( size=.1, color="black"))+ theme_bw()
# m_ret_plot2 = nberShade(m_ret_plot2,xrange = c(min(p2$Date), max(p2$Date)),openShade = FALSE)
# tikz("m_ret_plot3.tex",width = 5.90551, height = 3, sanitize = FALSE)
# plot(m_ret_plot2)
# dev.off()


#### CER ####
cerPalette = c("1" = "#000000", "1.5" = "#000000", "2" = "#E41A1C", "2.5" = "#E41A1C", "3" = "#377EB8", "5" = "#377EB8")
cerLinePalette = c("1" = "solid", "1.5" = "solid", "2" = "dotted", "2.5" = "dotted", "3" = "longdash", "5" = "#377EB8")

gammas = seq(1,5,by = .1)
cer_gains_dt = array(dim = c(3,length(seq(1,3,.1)),length(seq(1,5,.1))),dimnames = list(sufx,dimnames(returns_dt)[[3]][2:22],gammas))
for(g in gammas){
  for(c in sufx){
    tmp1 = t(returns_dt["av",c,2:(length(upper_lev)+1),])
    cer1 = colMeans(tmp1) - .5*apply(tmp1,MARGIN = 2,FUN = var)*(1/g)
    tmp2 = t(returns_dt["sv",c,2:(length(upper_lev)+1),])
    cer2 = colMeans(tmp2) - .5*apply(tmp2,MARGIN = 2,FUN = var)*(1/g)
    cer_gains_dt[c,,as.character(g)] = (cer1 - cer2)*1200
  }
}

# cer3D_plot = persp3D(x= seq(1,3,.1), y = gammas, z = cer_gains_dt["053",,],theta = 305, 
#         xlab = "Constraint", ylab = "Gamma", zlab = "CER Gain",  
#         clab = "CER GAIN %",resfac = 3, colkey = list(side=1,length=.3),
#         ticktype="detailed")
tikz("figures/cer3d_plot.tex",width = 5.90551, height = 3, sanitize = FALSE)
persp3D(x= seq(1,3,.1), y = gammas, z = cer_gains_dt["053",,],theta = 305, 
        xlab = "Constraint", ylab = "Gamma", zlab = "CER Gain",  
        clab = "Percent Gain",resfac = 3, colkey = list(side=1,length=.3),
        ticktype="detailed")
dev.off()


cer_plots_const = data.table(gain = cer_gains_dt["053","1.5",],constraint = "1.5",
                             gamma = as.numeric(dimnames(cer_gains_dt)[[3]]))
cer_plots_const = rbindlist(list(cer_plots_const, data.table(gain = cer_gains_dt["053","2",],constraint = "2",
                                                          gamma = as.numeric(dimnames(cer_gains_dt)[[3]])),
                              data.table(gain = cer_gains_dt["053","3",],constraint = "3",
                                         gamma = as.numeric(dimnames(cer_gains_dt)[[3]]))))
cer_const_plot = ggplot(data = cer_plots_const) + geom_path(mapping = aes(x = gamma,y = gain,color = constraint,linetype = constraint)) +
  scale_colour_manual(name = "Constraint",values=cerPalette,labels =c("1.5","2","3")) + 
  scale_linetype_manual(name = "Constraint",values=cerLinePalette,labels =c("1.5","2","3")) + 
  ylab("CER Gain") + theme_bw()
tikz("figures/cer_const_plot.tex",width = 5.90551, height = 3, sanitize = FALSE)
cer_const_plot
dev.off()

cer_plots_gamma = data.table(gain = cer_gains_dt["053",,"5"],constraint = as.numeric(dimnames(cer_gains_dt)[[2]]),
                             gamma = "5")
cer_plots_gamma = rbindlist(list(cer_plots_gamma, data.table(gain = cer_gains_dt["053",,"2.5"],
                                                             constraint = as.numeric(dimnames(cer_gains_dt)[[2]]),
                                                             gamma = "2.5"),
                                 data.table(gain = cer_gains_dt["053",,"1"],constraint = as.numeric(dimnames(cer_gains_dt)[[2]]),
                                            gamma = "1")))
cer_gamma_plot = ggplot(data = cer_plots_gamma) + geom_line(mapping = aes(x = constraint,y = gain,color = gamma,linetype = gamma)) +
  # scale_colour_manual(name = "Gamma",values=cerPalette,labels =c("1","2.5","5")) + 
  # scale_linetype_manual(name = "Gamma",values=cerLinePalette,labels =c("1.5","2.5","5")) + 
  ylab("CER Gain") + theme_bw()
tikz("figures/cer_gamma_plot.tex",width = 5.90551, height = 3, sanitize = FALSE)
cer_gamma_plot
dev.off()

#### drawdowns ####
dd_ts = as.timeSeries(data.table(market = m_bh_returns,vol_mang = returns_dt["sv","053","NO",],
                                 av_mang = returns_dt["av","053","NO",]))
dd_ts = exp(dd_ts) - 1
dd_dt = data.table(Date = m_data[paper_m_start:nrow(m_data)]$date,drawdowns(dd_ts),constraint = "NO")
dd_dt[, keep.rownames := NULL]
dd_dt = melt(dd_dt,measure.vars = c("market","vol_mang","av_mang"),value.name = "Return",
             id.vars = c("Date","constraint"),variable.name = "Strategy")
dd_ts2 = as.timeSeries(data.table(market = m_bh_returns,vol_mang = returns_dt["sv","053","3",],
                                 av_mang = returns_dt["av","053","3",]))
dd_ts2 = exp(dd_ts2) - 1
dd_dt2 = data.table(Date = m_data[paper_m_start:nrow(m_data)]$date,drawdowns(dd_ts2),constraint = "3")
dd_dt2[, keep.rownames := NULL]
dd_dt2 = melt(dd_dt2,measure.vars = c("market","vol_mang","av_mang"),value.name = "Return",
             id.vars = c("Date","constraint"),variable.name = "Strategy")
dd_ts3 = as.timeSeries(data.table(market = m_bh_returns,vol_mang = returns_dt["sv","053","1.5",],
                                  av_mang = returns_dt["av","053","1.5",]))
dd_ts3 = exp(dd_ts3) - 1
dd_dt3 = data.table(Date = m_data[paper_m_start:nrow(m_data)]$date,drawdowns(dd_ts3), constraint = "1.5")
dd_dt3[, keep.rownames := NULL]
dd_dt3 = melt(dd_dt3,measure.vars = c("market","vol_mang","av_mang"),value.name = "Return",
              id.vars = c("Date","constraint"),variable.name = "Strategy")
dd_dt = rbind(dd_dt3,dd_dt2,dd_dt)

m_dd_plot = ggplot(data=dd_dt[constraint=="NO"]) + geom_line(aes(x=Date,y=Return,color=Strategy,linetype=Strategy)) + 
  labs(title = "Cummulative Drawdown", x = "", y = "") + 
  scale_x_date(date_breaks = "5 year", date_labels =  "%Y") +
  scale_colour_manual(name = "Strategy",values=cbPalette,labels =c("Market","SV","AV")) + 
  scale_linetype_manual(name = "Strategy",values=linePalette,labels =c("Market","SV","AV")) +
  theme(text = element_text(size=11),panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line( size=.1, color="black")) +
  theme_bw()
m_dd_plot = nberShade(m_dd_plot,xrange = c(min(dd_dt$Date), max(dd_dt$Date)),openShade = FALSE)
tikz("figures/dd_plot.tex",width = 5.90551, height = 3, sanitize = FALSE)
plot(m_dd_plot)
dev.off()


dds_bh = as.data.table(drawdownsStats(as.timeSeries(exp(m_bh_returns)-1)))
dds_av = as.data.table(drawdownsStats(as.timeSeries(exp(returns_dt["av","053","NO",])-1)))
dds_vol = as.data.table(drawdownsStats(as.timeSeries(exp(returns_dt["sv","053","NO",])-1)))

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
stargazer(dd_table,summary = FALSE,out = "tables/performance/dd_stats.tex")

# knockout drawndown
#  “the probability of a 40% drawdown in one year is less than 0.1%.”
dd_dt_binom = dd_dt[constraint == "NO"]
dd_dt_binom[, dd40 := as.integer(Return <= -.45)]
dd_dt_binom[, dd40_gamma := sum(dd40) / (length(dd40) /12) , by = Strategy]
p40_bh = fitdist(dd_dt_binom[Strategy=="market"]$dd40,distr = "binom",method = "mle",fix.arg = list(size=12),
                 start=list(prob=(dd_dt_binom[Strategy=="market"]$dd40_gamma[1]/100)))$estimate * 100
p40_av = fitdist(dd_dt_binom[Strategy=="av_mang"]$dd40,distr = "binom",method = "mle",fix.arg = list(size=12),
                 start=list(prob=(dd_dt_binom[Strategy=="av_mang"]$dd40_gamma[1]/100)))$estimate * 100
p40_sv = fitdist(dd_dt_binom[Strategy=="vol_mang"]$dd40,distr = "binom",method = "mle",fix.arg = list(size=12),
                      start=list(prob=(dd_dt_binom[Strategy=="vol_mang"]$dd40_gamma[1]/100)))$estimate * 100


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

# subs_dt = data.table(Date = rep(m_data[paper_m_start:nrow(m_data)]$date,3), 
#                      Strategy = c(rep("avg_var1m",length(adj_m_av_returns)),rep("mkt_var1m",length(adj_m_vol_returns)),rep("market",length(m_bh_returns))),
#                      Return = c(adj_m_av_returns,adj_m_vol_returns,m_bh_returns))
# subs_dt[Date %in% contractions, contraction := 1]
# subs_dt[is.na(contraction), contraction := 0]
# 
# subs_dt = dcast(subs_dt,formula = Date + contraction ~ ...,value.var = "Return")
# 
# subs_table = 

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
dp = dcast(p1[Constraint == "Leverage - Unconstrained"],formula = Date + Constraint ~ ...,value.var = "Return")
dp = as.data.table(dp)
dp[, Constraint := NULL]
dp[, year := year(Date)]
dp[, month := month(Date)]

# w_dt = data.table(date = dp$Date + months(1),adj_m_vol_weights,adj_m_av_weights)
weights_dt[, year := year(V1)]
weights_dt[, month := month(V1)]
weights_dt[, V1 := NULL]
weights_dt = dcast(data = weights_dt,formula = year + month ~ ..., value.var = "Weight")
d_returns[, year := year(date)]
d_returns[, month := month(date)]
d_returns = merge(d_returns,weights_dt,by=c("year","month"))
setkey(d_returns,year,month)
d_returns[, BH_MAX1 := max(vwretd), by = c("year","month")]
d_returns[, SV_MAX1 := max(vwretd*vol_mang), by = c("year","month")]
d_returns[, AV_MAX1 := max(vwretd*av_mang), by = c("year","month")]


d_returns[, BH_MAX5 := mean(sort(vwretd,decreasing = TRUE)[1:5]), by=c("year","month")]
d_returns[, SV_MAX5 := mean(sort(vwretd*vol_mang,decreasing = TRUE)[1:5]), by=c("year","month")]
d_returns[, AV_MAX5 := mean(sort(vwretd*av_mang,decreasing = TRUE)[1:5]), by=c("year","month")]


d_returns[, BH_var := var(vwretd), by = c("year","month")]
d_returns[, SV_var := var(vwretd*vol_mang), by = c("year","month")]
d_returns[, AV_var := var(vwretd*av_mang), by = c("year","month")]

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

d_returns[, BH_skew := skewness(vwretd,method = "sample"), by = c("year","month")]
d_returns[, SV_skew := skewness(vwretd*vol_mang,method = "sample"), by = c("year","month")]
d_returns[, AV_skew := skewness(vwretd*av_mang,method = "sample"), by = c("year","month")]

m_returns = unique(d_returns,by=c("year","month"))
m_returns[, market_returns := m_bh_returns[2:(nrow(m_returns)+1)]]
m_returns[, av_returns := returns_dt["av","053","NO",2:(nrow(m_returns)+1)]]
m_returns[, vol_returns := returns_dt["sv","053","NO",2:(nrow(m_returns)+1)]]
setnames(weights_dt,old = c("vol_mang","av_mang"),new = c("vol_weight","av_weight"))
m_returns = merge(m_returns,weights_dt,by=c("year","month"),all.x=TRUE)




MAX1_dt = subset(m_returns[!is.na(BH_MAX1_scale)],select = c("BH_MAX1","SV_MAX1","AV_MAX1","BH_MAX1_scale",
                                                           "SV_MAX1_scale","AV_MAX1_scale","BH_MAX5","SV_MAX5",
                                                           "AV_MAX5","BH_MAX5_scale","SV_MAX5_scale","AV_MAX5_scale"))

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

m_returns = merge(m_returns,subset(ff_data,select = c("year","month","Mkt_RF","SMB","HML")),
                  by=c("year","month"),all.x=TRUE)

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
marg_req[, Effective := NULL]
marg_req = marg_req %>% pad %>% fill(Rate)
# marg_req[2:nrow(marg_req), mr_change := diff(x = marg_req$Rate)]
# marg_req[2:nrow(marg_req), mr_changeI := sign(diff(x = marg_req$Rate))]
marg_req[,c("year","month","day") := list(year(Date),month(Date),day(Date))]
setorderv(marg_req,cols = "Date",order = -1)
marg_req = unique(marg_req,by=c("year","month"))
marg_req[, Date := NULL]
# marg_req[1, mr_change := 1]
# marg_req[1, mr_changeI := 1]


m_returns = merge(m_returns,subset(marg_req,select=c("year","month","Rate")),all.x=TRUE,by=c("year","month"))
setnames(m_returns,old = "Rate",new = "mr_rate")
# m_returns[is.na(mr_change), mr_change := 0]
# m_returns[is.na(mr_changeI), mr_changeI := 0]

margin_data = fread(input = 'reproduce_data.csv')

m_returns = merge(m_returns,subset(margin_data,select = c("year","month","m_retvol","md","md_1984","mc","mc_1984","m_retvar","avg_var","avg_cor",
                                              "spls","icrf","aem_leverage_factor","bc_chg_p_nsa","vix",
                                              "bc_chg_p","Index")),by=c("year","month"),all.x=TRUE)
goyal_data = read_excel(path = 'PredictorData2017.xlsx',sheet = "Monthly",col_names = TRUE,na = "NaN")
goyal_data = as.data.table(goyal_data)
goyal_data[, year := as.integer(substr(yyyymm,1,4))]
goyal_data[, month := as.integer(substr(yyyymm,5,6))]
m_returns = merge(m_returns,subset(goyal_data,select = c("year","month","Index")),by=c("year","month"),all.x=TRUE)

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

lm_aem3 = lm(av_returns~market_returns*aem_leverage_factor+SMB+HML,data=m_returns)
lm_aem5 = lm(av_mang~market*aem_leverage_factor+SMB+HML+RMW+CMA,data=m_returns)
lm_icrf3 = lm(av_returns~market_returns*icrf + SMB + HML, data = m_returns)
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


#### weights, lending, leverage correlation ####
cor_lottery = cor(subset(m_returns,select = c(vol_mang,av_mang,BH_MAX1,BH_MAX5,BH_MAX5_scale,SV_MAX1,SV_MAX5,
                                SV_MAX5_scale,AV_MAX1,AV_MAX5,AV_MAX5_scale,gaming_mcap)))
cor_lending = cor(subset(m_returns,select = c(vol_mang,av_mang,icrf,aem_leverage_factor,bc_chg_p,call_money,
                                              bank_prime,rate,chg_md)))
#### preferences graphs ####
set.seed(111)
assets3_dt = data.table(time = seq(as.Date("1976-12-01"),length.out = 200,by = "week"),
                        a1 = rnorm(200,.005,sd=.05))
assets3_dt[, a2 := -1 *a1 + rnorm(200,.009,.08)]
assets3_dt[, a3 := a1]
assets3_dt[, a3 := a3 - .0025]
pspec = portfolioSpec(portfolio = list(riskFreeRate=.004,nFrontierPoints=100))
setSolver(pspec) = "solveRshortExact"
pf = portfolioFrontier(data = as.timeSeries(assets3_dt[,2:3]),spec = pspec)
pf_alt = rlang::duplicate(x = pf,shallow = FALSE)

pf_alt@portfolio@portfolio$targetReturn = pf@portfolio@portfolio$targetReturn - .001

eff_dt = data.table(Eret = pf@portfolio@portfolio$targetReturn[,1], std = pf@portfolio@portfolio$targetRisk[,1],
                    line = "curve1",shade = "grey",style = "solid",pref="lottery")
eff_dt = rbind(eff_dt,data.table(Eret = pf_alt@portfolio@portfolio$targetReturn[,1], std = pf_alt@portfolio@portfolio$targetRisk[,1],
                                 line = "curve2",shade = "black",style = "longdash",pref="lottery"))

sr1 = as.data.table(sharpeRatioLines(pf))
sr1 = unlist(sr1[y.norm==max(y.norm)])
sr2 = as.data.table(sharpeRatioLines(pf_alt))
sr2 = unlist(sr2[y.norm==max(y.norm)])
tangent_line = function(rf = .004,sr, std){
  return(rf + sr * std)
}
eff_dt = rbind(eff_dt,
               data.table(Eret = tangent_line(sr = (sr1[2]-.004) / sr1[1],std = seq(0,.1,.001)),
                          std = seq(0,.1,.001),
                          line = "tan1",shade = "grey",style = "solid",pref="lottery"))
eff_dt = rbind(eff_dt,
               data.table(Eret = tangent_line(sr = (0.008002177-.005) / sr2[1],std = seq(0,.1,.001)),
               std = seq(0,.1,.001),
               line = "tan2",shade = "black", style = "longdash", pref="lottery"))

eff_dt = rbind(eff_dt,data.table(Eret = pf@portfolio@portfolio$targetReturn[,1], std = pf@portfolio@portfolio$targetRisk[,1],
                                 line = "curve1",shade = "grey",style = "solid",pref="leverage"))
eff_dt = rbind(eff_dt, data.table(Eret = tangent_line(sr = (sr1[2]-.004) / sr1[1],std = seq(0,.1,.001)),
           std = seq(0,.1,.001),
           line = "tan1",shade = "grey",style = "solid",pref="leverage"))
pspec2 = portfolioSpec(portfolio = list(riskFreeRate=.005,nFrontierPoints=100))
setSolver(pspec2) = "solveRshortExact"
pf2 = portfolioFrontier(data = as.timeSeries(assets3_dt[,2:3]),spec = pspec2)
sr3 = as.data.table(sharpeRatioLines(pf2))
sr3 = unlist(sr3[y.norm==max(y.norm)])

# eff_dt = rbind(eff_dt,data.table(Eret = pf2@portfolio@portfolio$targetReturn[,1], std = pf2@portfolio@portfolio$targetRisk[,1],
#                                  line = "curve2",shade = "grey",style = "solid",pref="leverage"))
eff_dt = rbind(eff_dt, data.table(Eret = tangent_line(rf=.005,sr = (sr3[2]-.005) / sr3[1],std = seq(0,.1,.001)),
                                  std = seq(0,.1,.001),
                                  line = "tan2",shade = "black",style = "solid",pref="leverage"))



ggplot() + geom_path(mapping = aes(x = std, y = Eret, group = line, colour = shade, linetype = style),data = eff_dt) + 
  facet_wrap(~pref) #+ 
  # geom_point(mapping = aes(x=tp1@portfolio@portfolio$targetRisk[1],y=tp1@portfolio@portfolio$targetReturn[1]),size = 2) + 
  # geom_point(mapping = aes(x=tp2@portfolio@portfolio$targetRisk[1],y=tp2@portfolio@portfolio$targetReturn[1]),size = 2) +
  # geom_point(mapping = aes(x=tp1_levered_sd,y=tp1_levered_ret),size = 2) +
  # geom_point(mapping = aes(x=tp2_levered_sd,y=tp2_levered_ret),size = 2)
