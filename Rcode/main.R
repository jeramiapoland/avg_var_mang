paks <- c("RCurl","data.table","tis","lubridate","ggplot2","stringr","sandwich","stargazer","pracma","RColorBrewer",
          "CADFtest","complexplus","readxl","reshape2","quantmod","xlsx","tikzDevice","MASS","timeSeries","vars","PortfolioEffectHFT",
          "PortfolioAnalytics","PerformanceAnalytics","backtest","tidyr","broom","stringdist","BH","parallel","doMC","foreach",
          "doParallel","lmtest","hypergeo","strucchange","formula.tools","multiwave","outliers","forecast","SharpeR","fastmatch",
          "bvarsv","boot","goftest","DescTools","dunn.test","generalCorr","cointReg","psd","plot3D","rootSolve",
          "fitdistrplus","conics","panelvar") 
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
                                       "vwretx.tp1")),by=c("year","month"))


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
ff_mom = fread(input = 'F-F_Momentum_Factor.CSV')
ff_mom[, year:= as.integer(substr(V1,1,4))]
ff_mom[, month := as.integer(substr(V1,5,6))]
ff_data = merge(ff_data,ff_mom,by=c("year","month"),all.x=TRUE)
ff_data[, date := as.Date(paste0(year,"-",month,"-28"))]
ff_data[, Mom := log1p(Mom /100)]

m_data = merge(m_data,subset(ff_data,select=c("year","month","Mkt_RF","SMB","HML","RF","RF_lag")),by=c("year","month"),all.x=TRUE)
m_data[, logxret.tp1 := vwretd.tp1 - RF]
m_data[, logxret := vwretd.monthly - RF_lag]
ff_data[, Mkt_RF := NULL]
ff_data = merge(ff_data,subset(m_data,select = c("year","month","logxret")),by=c("year","month"),all.x=TRUE)
ff_data[, Mkt_RF := logxret]
ff_data[, logxret := NULL]

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

mdate = m_data[2:nrow(m_data)]$date
spans = list("full" = mdate,
             "pre1962" = mdate[mdate <= "1962-06-28"],
             "post1962" = mdate[mdate >= "1962-06-28"],
             "contractions" = mdate[mdate %fin% (contractions-months(1))],
             "expansions" = mdate[!(mdate %fin% (contractions-months(1)))],
             "cont_xGreat" = mdate[mdate %fin% contractions& !(mdate >= "1929-09-28" & mdate <= "1933-03-28")])

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
m_data[, date := as.Date(paste0(year,"-",month,"-28"))]
r_dates = m_data[(paper_m_start+1):(nrow(m_data))]$date + months(1)
returns_dt = array(dim = c(2,3,length(upper_lev)+1,length(m_bh_returns)),
                   dimnames = list(c("av","sv"),sufx,c("NO",upper_lev),as.character(r_dates)))
for(s in sufx){
  returns_dt["av",s,"NO",] = get(paste0("av_",s,"_returns"))
  returns_dt["sv",s,"NO",] = get(paste0("sv_",s,"_returns"))
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

upper_lev_long = seq(1,16.2,.1)
returns_long_dt = array(dim = c(2,3,length(upper_lev_long)+1,length(m_bh_returns)),
                   dimnames = list(c("av","sv"),sufx,c("NO",upper_lev_long),as.character(r_dates)))

for(s in sufx){
  returns_long_dt["av",s,"NO",] = get(paste0("av_",suf,"_returns"))
  returns_long_dt["sv",s,"NO",] = get(paste0("sv_",suf,"_returns"))
}

for(p in c("av","sv")){
  for(s in sufx){
    w = get(paste0(p,"_",s,"_weights"))
    for(u in upper_lev_long){
      tw = replace(w, w>=u, u)
      returns_long_dt[p,s,as.character(u),] = tw * m_bh_returns
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
weights_dt = melt(weights_table,id.vars = "V1",variable.name = "Strategy",value.name = "Weight")
weights_dt[, Strategy := as.character(Strategy)]
weights_dt[, Target := {if(substr(Strategy,nchar(Strategy)-1, nchar(Strategy))== "10"){"029"}else if(
  substr(Strategy,nchar(Strategy)-1, nchar(Strategy))== "12"){"035"}else{"053"}},by=1:nrow(weights_dt)]
weights_dt[, Strategy := str_extract(Strategy,"[a-z]{2,3}_mang")]

weight_plot2 = ggplot(data=weights_dt) + geom_line(aes(x=V1,y=Weight,color=Strategy,linetype=Strategy)) + 
  facet_wrap(~Target,nrow = 3) + 
  labs(title = "Strategy Investment Weight", x = "", y = "") + 
  scale_x_date(date_breaks = "5 year", date_labels =  "%Y") +
  scale_colour_manual(name = "Strategy",values=cbPalette,labels =c("AV","SV")) + 
  scale_linetype_manual(name = "Strategy",values=linePalette,labels =c("AV","SV")) +
  theme(text = element_text(size=11),panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line( size=.1, color="black"))+ theme_bw()
weight_plot2 = nberShade(weight_plot2,xrange = c(min(weights_dt$V1), max(weights_dt$V1)),openShade = FALSE)
tikz("figures/weight_plot2.tex",width = 5.90551, height = 3, sanitize = FALSE)
plot(weight_plot2)
dev.off()

cast_weightsdt = data.table::dcast(weights_dt,formula = V1 ~ ...,value.var = "Weight")
cast_weightsdt[, diff := av_mang - vol_mang]
cast_weightsdt[, pos := diff > 0]
weight_plot3 = ggplot(data=cast_weightsdt,aes(x=as.Date(V1),y=diff)) + geom_line(aes(color=pos,group=1)) + 
  labs(title = "AV Investment Weight Minus SV Investment Weight", x = "", y = "") + 
  scale_color_manual(name = "",labels =c("Negative","Positive"),values=c("#bab8b8","#000000")) +
  scale_x_date(date_breaks = "5 year", date_labels =  "%Y") +
  theme(text = element_text(size=11),panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line( size=.1, color="black"))+ theme_bw()
weight_plot3 = nberShade(weight_plot3,xrange = c("1926-08-28","2016-12-28"),openShade = FALSE)
tikz("figures/weight_plot3.tex",width = 5.90551, height = 3, sanitize = FALSE)
plot(weight_plot3)
dev.off()

#### performance table ####
perf_dt = array(dim = c(length(sufx),length(spans),3,7,3),dimnames = list(Target = sufx,Sample = names(spans),
                                                                          Strategy = c("BH","SV","AV"),
                                                                          Measure = c("Return","Sharpe","Sortino","Kappa$_{3}$",
                                                                                      "Kappa$_{4}$","alpha$_{FF5}$",
                                                                                      "alpha$_{FF5+Mom}$"),
                                                                          Constraint = c("1.5","3","NO")))
ff_data = ff_data[, c("SMB","HML","RF","RF_lag","Mkt_RF","Mom") := lapply(.SD,log1p), 
                  .SDcols =  c("SMB","HML","RF","RF_lag","Mkt_RF","Mom")]
m_bh_dates = m_data$date[(paper_m_start+1):(nrow(m_data))]
set.seed(123)
for(s in 1:length(spans)){
  sp = spans[[s]]
  fq = 12
  r0 = c(m_bh_returns[m_bh_dates%fin% sp])
  sp_logical = (as.Date(dimnames(returns_dt)[[4]])-months(1)) %fin% sp
  f5 = merge(data.table(date = sp),
             subset(ff_data,subset = ff_data$date%fin%sp,select = c("date","SMB","HML","Mkt_RF")),all.x=TRUE)
  f5[, date := NULL]
  f5 = as.matrix(f5)
  f6 = merge(data.table(date = sp),
             subset(ff_data,subset = ff_data$date%fin%sp,select = c("date","SMB","HML","Mkt_RF","Mom")),all.x=TRUE)
  f6[, date := NULL]
  f6 = as.matrix(f6)
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
                                as.character(round(alpha(r0,f5)*fq*100,3)),
                                as.character(round(alpha(r0,f6)*fq*100,3))
                                # as.character(round(genRachev.ratio(r0),3))
                                )
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
                 as.character(round(alpha(r1,f5)*fq*100,3)),
                 hac_t.test2(alpha(r2,f5),alpha(r1,f5),rr2,rr1,alternative = "less",
                            paired = TRUE,var.equal = TRUE)$p.val,
                 as.character(round(alpha(r1,f6)*fq*100,3)),
                 hac_t.test2(alpha(r2,f5),alpha(r1,f5),rr2,rr1,alternative = "less",
                             paired = TRUE,var.equal = TRUE)$p.val
                 # as.character(round(genRachev.ratio(r1),3)),
                 # ratio.test2(rets1 = rr1,rets2 = rr2,ratio = "genRachev.ratio")
      )
      svp_dt[is.na(svp_dt)] = 0
      perf_dt[c,s,"SV",,l] = see.stars(matrix(as.numeric(svp_dt),nrow=1),seq(1,13,by=2),seq(2,14,by=2))[seq(1,13,by=2)]
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
                 # as.character(round(genRachev.ratio(r2),3)),
                 # ratio.test2(rets1 = rr2,rets2 = rr1,ratio = "genRachev.ratio")
                 as.character(round(alpha(r2,f5)*fq*100,3)),
                 hac_t.test2(alpha(r1,f5),alpha(r2,f5),rr1,rr2,alternative = "less",
                             paired = TRUE,var.equal = TRUE)$p.val,
                 as.character(round(alpha(r2,f6)*fq*100,3)),
                 hac_t.test2(alpha(r1,f5),alpha(r2,f5),rr1,rr2,alternative = "less",
                             paired = TRUE,var.equal = TRUE)$p.val
      )
      avp_dt[is.na(avp_dt)] = 0
      perf_dt[c,s,"AV",,l] = see.stars(matrix(as.numeric(avp_dt),nrow=1),seq(1,13,by=2),seq(2,14,by=2))[seq(1,13,by=2)]
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

m_ret_plot1 = ggplot(data=p1[Constraint=="Leverage - 1.5"]) + geom_line(aes(x=Date,y=Return,color=Strategy,linetype=Strategy)) + 
  labs(title = "Cummulative Excess Log Returns: Leverage - 1.5", x = "", y = "") + 
  scale_x_date(date_breaks = "5 year", date_labels =  "%Y") +
  scale_colour_manual(name = "Strategy",values=cbPalette,labels =c("AV","Market","SV")) + 
  scale_linetype_manual(name = "Strategy",values=linePalette,labels =c("AV","Market","SV")) +
  theme(text = element_text(size=11),panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line( size=.1, color="black")) +   theme_bw()
m_ret_plot1 = nberShade(m_ret_plot1,xrange = c(min(p1_dt$Date), max(p1_dt$Date)),openShade = FALSE)
tikz("figures/m_ret_plot2_1.tex",width = 5.90551, height = 9, sanitize = FALSE)
plot(m_ret_plot1)
dev.off()

m_ret_plot2 = ggplot(data=p1[Constraint=="Leverage - 3"]) + geom_line(aes(x=Date,y=Return,color=Strategy,linetype=Strategy)) + 
  labs(title = "Cummulative Excess Log Returns: Leverage - 3", x = "", y = "") + 
  scale_x_date(date_breaks = "5 year", date_labels =  "%Y") +
  scale_colour_manual(name = "Strategy",values=cbPalette,labels =c("AV","Market","SV")) + 
  scale_linetype_manual(name = "Strategy",values=linePalette,labels =c("AV","Market","SV")) +
  theme(text = element_text(size=11),panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line( size=.1, color="black")) +   theme_bw()
m_ret_plot2 = nberShade(m_ret_plot2,xrange = c(min(p1_dt$Date), max(p1_dt$Date)),openShade = FALSE)
tikz("figures/m_ret_plot2_2.tex",width = 5.90551, height = 9, sanitize = FALSE)
plot(m_ret_plot2)
dev.off()

m_ret_plot3 = ggplot(data=p1[Constraint=="Leverage - Unconstrained"]) + geom_line(aes(x=Date,y=Return,color=Strategy,linetype=Strategy)) + 
  labs(title = "Cummulative Excess Log Returns: Leverage - Unconstrained", x = "", y = "") + 
  scale_x_date(date_breaks = "5 year", date_labels =  "%Y") +
  scale_colour_manual(name = "Strategy",values=cbPalette,labels =c("AV","Market","SV")) + 
  scale_linetype_manual(name = "Strategy",values=linePalette,labels =c("AV","Market","SV")) +
  theme(text = element_text(size=11),panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line( size=.1, color="black")) +   theme_bw()
m_ret_plot3 = nberShade(m_ret_plot3,xrange = c(min(p1_dt$Date), max(p1_dt$Date)),openShade = FALSE)
tikz("figures/m_ret_plot2_3.tex",width = 5.90551, height = 9, sanitize = FALSE)
plot(m_ret_plot3)
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

cer_loss_dt = array(dim = c(2,3,length(seq(1,16.2,.1)),length(seq(1,5,.1))),dimnames = list(c("av","sv"),sufx,tail(dimnames(returns_long_dt)[[3]],-1),gammas))
for(g in gammas){
  for(p in c("av","sv")){
    for(c in sufx){
      tmp1 = returns_long_dt[p,c,"NO",]
      cer1 = mean(tmp1) - .5*var(tmp1)*(1/g)
      tmp2 = t(returns_long_dt[p,c,2:(length(upper_lev_long)+1),])
      cer2 = colMeans(tmp2) - .5*apply(tmp2,MARGIN = 2,FUN = var)*(1/g)
      cer_loss_dt[p,c,,as.character(g)] = (cer1 - cer2)*1200
    }
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

cer_plots_loss = data.table(loss = cer_loss_dt["av","053",,"5"],constraint = as.numeric(dimnames(cer_loss_dt)[[3]]),
                             gamma = "5",Strategy= "av_mang")
cer_plots_loss = rbindlist(list(cer_plots_loss, data.table(loss = cer_loss_dt["av","053",,"2.5"],constraint = as.numeric(dimnames(cer_loss_dt)[[3]]),
                                                           gamma = "2.5",Strategy= "av_mang"),
                                cer_plots_loss, data.table(loss = cer_loss_dt["av","053",,"1"],constraint = as.numeric(dimnames(cer_loss_dt)[[3]]),
                                                           gamma = "1",Strategy= "av_mang"),
                                data.table(loss = cer_loss_dt["sv","053",,"5"],constraint = as.numeric(dimnames(cer_loss_dt)[[3]]),
                                                          gamma = "5",Strategy= "vol_mang"), data.table(loss = cer_loss_dt["sv","053",,"2.5"],constraint = as.numeric(dimnames(cer_loss_dt)[[3]]),
                                                           gamma = "2.5",Strategy= "vol_mang"),
                                cer_plots_loss, data.table(loss = cer_loss_dt["sv","053",,"1"],constraint = as.numeric(dimnames(cer_loss_dt)[[3]]),
                                                           gamma = "1",Strategy= "vol_mang")))
cer_plots_loss[, bk := if(constraint >=13 & constraint <= 16.2){-16}else if(constraint <= 8){-8},by=1:nrow(cer_plots_loss)]
setorderv(cer_plots_loss,cols = "bk",order = 1)
glabels = c("1" = "gamma = 1", "2.5" = "gamma = 2.5", "5" = "gamma = 5")
cer_loss_plot = ggplot(data = cer_plots_loss[!is.na(bk)]) + geom_line(mapping = aes(x = constraint,y = -1*loss,color = Strategy,linetype = Strategy)) +
  scale_x_reverse() +
  ylab("CER Loss") + theme_bw()  + xlab("Constraint") +
  scale_colour_manual(name = "Strategy",values=cbPalette,labels =c("AV","SV")) + 
  scale_linetype_manual(name = "Strategy",values=linePalette,labels =c("AV","SV")) + 
  facet_grid(gamma ~ bk,scales = "free_x",space="free", labeller = labeller(gamma = glabels)) + 
  theme(strip.text.x = element_blank())
tikz("figures/cer_loss_plot.tex",width = 5.90551, height = 3, sanitize = TRUE)
cer_loss_plot
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
stargazer(MAX_stats,summary = FALSE,out = "tables/tab_max_stats.tex")

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

ks.test(adj_m_av_returns,adj_m_vol_returns,alternative = "less")
dunn.test(list(av_mang = adj_m_av_returns,bh = adj_m_vol_returns),method = "by")
wt_out = wtdpapb(xa = adj_m_av_returns,xb = adj_m_vol_returns)
sd2_out = stochdom2(wt_out$dj,wt_out$wpa,wt_out$wpb)


smcap = as.timeSeries(data.table(adj_m_av_returns,adj_m_vol_returns))
summary(smcap)
round(basicStats(cbind(adj_m_av_returns,adj_m_vol_returns)),3)
matplot(cbind(adj_m_av_returns,adj_m_vol_returns), typ="l", main="Returns on AV and SV Managed Portfolios")
layout(matrix(1:4, nrow = 2, ncol = 2))
dgg=density(adj_m_vol_returns)
n=length(dgg$x)
plot(dgg, main="Kernel smooth density for SV")
dmkt=density(adj_m_av_returns)
plot(dmkt, main="Kernel smooth density for AV")
cdfgg=diffinv(dgg$y)
cdfmkt=diffinv(dmkt$y)
plot(dgg$x,cdfgg[2:(n+1)], typ="l",
     main="Cumulative density for SV",xlab="x", ylab="Cumulative density")
plot(dmkt$x,cdfmkt[2:(n+1)], typ="l",
     main="Cumulative density for AV",xlab="x", ylab="Cumulative density")
layout(matrix(1:1, nrow = 1, ncol = 1)) #back to normal plotting




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

##### costs and borrowing ####

logff = fread(input = 'log_ff.csv')
logff[, year := as.integer(substr(date,1,4))]
logff[, month := as.integer(substr(date,5,6))]
logff[, date := NULL]

m_returns = merge(m_returns,logff,by=c("year","month"),all.x=TRUE)

lmvol5 = lm(vol_returns ~ market_returns + log_SMB + log_HML + log_CMA + log_RMW, data = m_returns)
volA5 = coefficients(lmvol5)[1] * 1200
lmvol5mom = lm(vol_returns ~ market_returns + log_SMB + log_HML + log_CMA + log_RMW + log_mom, data = m_returns)
volA5mom = coefficients(lmvol5mom)[1] * 1200
dWeight = c(SV = mean(abs(diff(m_returns$vol_weight))), AV = mean(abs(diff(m_returns$av_weight))))
vol_be =  c(volA5/1200,volA5mom/1200) / dWeight[1]* 10000

lmav5 = lm(av_returns ~ market_returns + log_SMB + log_HML + log_CMA + log_RMW, data = m_returns)
avA5 = coefficients(lmav5)[1] * 1200
lmav5mom = lm(av_returns ~ market_returns + log_SMB + log_HML + log_CMA + log_RMW + log_mom, data = m_returns)
avA5mom = coefficients(lmav5mom)[1] * 1200
av_be = (c(avA5/1200,avA5mom/1200) / dWeight[2]) * 10000

month_call = (1+m_returns$rate/100)^(1/12)-1
sv_borrow = mean(pmax((m_returns$vol_weight - 1),0)*month_call,na.rm = TRUE) * 10000
av_borrow = mean(pmax((m_returns$av_weight - 1),0)*month_call,na.rm = TRUE) * 10000

lm_aem3 = lm(av_returns~market_returns*aem_leverage_factor+SMB+HML,data=m_returns)
lm_aem5 = lm(av_returns~markt_returns*aem_leverage_factor+SMB+HML+RMW+CMA,data=m_returns)
lm_icrf3 = lm(av_returns~market_returns*icrf + SMB + HML, data = m_returns)
lm_icrf5 = lm(av_returns~markt_returns*icrf + SMB + HML + RMW + CMA, data = m_returns)
# lm_bc3 = lm(av_returns~markt_returns*bc_chg_p_nsa + SMB + HML, data = m_returns)
# lm_bc5 = lm(av_returns~markt_returns*bc_chg_p_nsa + SMB + HML + RMW + CMA, data = m_returns)
lm_bcp3 = lm(av_returns~markt_returns*bc_chg_p + SMB + HML, data = m_returns)
lm_bcp5 = lm(av_returns~markt_returns*bc_chg_p + SMB + HML + RMW + CMA, data = m_returns)

m_returns[, chg_md := shift(diff(md),type = "lag")]
m_returns[, chg_md_1984 := shift(diff(md_1984),type = "lag")]

lm_md3 = lm(av_returns~markt_returns*chg_md + SMB + HML, data = m_returns)
lm_md5 = lm(av_returns~markt_returns*chg_md + SMB + HML + RMW + CMA, data = m_returns)
lm_md_19843 = lm(av_returns~markt_returns*chg_md_1984 + SMB + HML, data = m_returns)
lm_md_19845 = lm(av_returns~markt_returns*chg_md_1984 + SMB + HML + RMW + CMA, data = m_returns)

lm_call_money3 = lm(av_returns~markt_returns*call_money + SMB + HML, data = m_returns)
lm_call_money5 = lm(av_returns~markt_returns*call_money + SMB + HML + RMW + CMA, data = m_returns)
lm_prime3 = lm(av_returns~markt_returns*bank_prime + SMB + HML, data = m_returns)
lm_prime5 = lm(av_returns~markt_returns*bank_prime + SMB + HML + RMW + CMA, data = m_returns)
lm_broker3 = lm(av_returns ~ markt_returns*rate + SMB + HML, data = m_returns)
lm_broker5 = lm(av_returns ~ markt_returns*rate + SMB + HML + RMW + CMA, data = m_returns)

stargazer(lm_aem3,lm_aem5,lm_icrf3,lm_icrf5,lm_bcp3,lm_bcp5,lm_md_19843,lm_md_19845,out = 'tab_levprox.tex')
stargazer(lm_broker3,lm_broker5,lm_call_money3,lm_call_money5,lm_prime3,lm_prime5, out = 'tab_rates.tex')

lm_margreq3 = lm(av_returns ~ markt_returns*mr_change+SMB+HML,data=m_returns[year>=1934&year<=1974])

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
# gaming_crsp = tmp_crsp[SICCD %in% c("7999","7011")]
gaming_tsymbols = c("ASCA","BYI","BYD","CZR","CPHC","CNTY","CHDN","DDE","ERI","NYNY","FLL","GLPI","IGT","IKGH","LVS",
                    "LACO","MPEL","MGM","MCRI","MNTG","MGAM","UWN","PENN","PNK","SGMS","WYNN")
gaming_crsp = tmp_crsp[TSYMBOL %in% gaming_tsymbols]
tmp_crsp = NULL
setkey(gaming_crsp,PERMNO,year,month)
gaming_crsp[, mcap := PRC * as.integer(SHROUT)]
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
gaming_mcap = gaming_mcap[complete.cases(gaming_mcap)]
gaming_mcap[, gaming_mcap := scale(gaming_mcap)]
gaming_mcap[, gratio := scale(gratio)]
m_returns = merge(m_returns,gaming_mcap,by=c("year","month"),all.x=TRUE)
# m_returns[, gaming_mcap := scale(na.omit(gaming_mcap))]
lm_gaming3 = lm(av_returns~market_returns*shift(gaming_mcap)+SMB+HML,data = m_returns)
lm_gaming5 = lm(av_returns~market_returns*gaming_mcap+SMB+HML+RMW+CMA,data = m_returns)

lm_gratio3 = lm(av_returns~market_returns*gratio+SMB+HML,data = m_returns)
lm_gratio5 = lm(av_returns~market_returns*gratio+SMB+HML+RMW+CMA,data = m_returns)

lm_max5 = lm(av_returns~market_returns*BH_MAX5+SMB+HML,data = m_returns)

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
                    style = "base",line="curve",pref="lottery")
eff_dt = rbind(eff_dt,data.table(Eret = pf_alt@portfolio@portfolio$targetReturn[,1], std = pf_alt@portfolio@portfolio$targetRisk[,1],
                                 style = "alt",line="curve",pref="lottery"))
frontierPlot(pf)
sr1 = as.data.table(sharpeRatioLines(pf))
sr1 = unlist(sr1[y.norm==max(y.norm)])
sr2 = as.data.table(sharpeRatioLines(pf_alt))
sr2 = unlist(sr2[y.norm==max(y.norm)])
tangent_line = function(rf = .004,sr, std){
  return(rf + sr * std)
}
eff_dt = rbind(eff_dt,
               data.table(Eret = tangent_line(sr = (sr1[2]-.004) / sr1[1],std = seq(0,.1,.001)),
                          std = seq(0,.1,.001), style = "base",line="tangent",pref="lottery"))
eff_dt = rbind(eff_dt,
               data.table(Eret = tangent_line(sr = (0.008002177-.005) / sr2[1],std = seq(0,.1,.001)),
               std = seq(0,.1,.001),style = "alt",line="tangent", pref="lottery"))

eff_dt = rbind(eff_dt,data.table(Eret = pf@portfolio@portfolio$targetReturn[,1], std = pf@portfolio@portfolio$targetRisk[,1],
                                 style = "base",line="curve",pref="leverage"))
# eff_dt = rbind(eff_dt, data.table(Eret = tangent_line(sr = (sr1[2]-.004) / sr1[1],std = seq(0,.1,.001)),
#            std = seq(0,.1,.001),
#            style = "base",line="curve",pref="leverage"))
pspec2 = portfolioSpec(portfolio = list(riskFreeRate=.005,nFrontierPoints=100))
setSolver(pspec2) = "solveRshortExact"
pf2 = portfolioFrontier(data = as.timeSeries(assets3_dt[,2:3]),spec = pspec2)
sr3 = as.data.table(sharpeRatioLines(pf2))
sr3 = unlist(sr3[y.norm==max(y.norm)])

# eff_dt = rbind(eff_dt,data.table(Eret = pf2@portfolio@portfolio$targetReturn[,1], std = pf2@portfolio@portfolio$targetRisk[,1],
#                                  line = "curve2",shade = "grey",style = "solid",pref="leverage"))
eff_dt = rbind(eff_dt, data.table(Eret = tangent_line(rf=.005,sr = (sr1[2]-.005) / sr1[1],std = seq(0,.1,.001)),
                                  std = seq(0,.1,.001),
                                  style = "alt",line="tangent",pref="leverage"))
eff_dt = rbind(eff_dt,
               data.table(Eret = tangent_line(sr = (sr1[2]-.004) / sr1[1],std = seq(0,.1,.001)),
                          std = seq(0,.1,.001), style = "base",line="tangent",pref="leverage"))
# eff_dt = rbind(eff_dt,data.table(Eret = pf@portfolio@portfolio$targetReturn[,1], std = pf@portfolio@portfolio$targetRisk[,1],
#                     style = "base",line="curve",pref="leverage"))



# ggplot() + geom_path(mapping = aes(x = std, y = Eret, group = line, colour = shade, linetype = style),data = eff_dt) + 
#   facet_wrap(~pref) +
#   geom_point(mapping = aes(x=tp1@portfolio@portfolio$targetRisk[1],y=tp1@portfolio@portfolio$targetReturn[1]),size = 2) +
#   geom_point(mapping = aes(x=tp2@portfolio@portfolio$targetRisk[1],y=tp2@portfolio@portfolio$targetReturn[1]),size = 2) +
#   geom_point(mapping = aes(x=tp1_levered_sd,y=tp1_levered_ret),size = 2) +
#   geom_point(mapping = aes(x=tp2_levered_sd,y=tp2_levered_ret),size = 2)

prefPalette = c("base" = "#A9A9A9", "alt" = "#000000")
prefLinePalatee = c("base" = "solid", "alt" = "longdash")
eff_dt[, line := paste0(line,style)]
sr2[2] = 0.007002177
lottery_plot = ggplot() + geom_path(mapping = aes(x = std, y = Eret, group = line, colour = style, linetype = style,group=line),
                     data = eff_dt[pref=="lottery"]) +
  geom_point(mapping = aes(x=sr1[1],y=sr1[2]),size = 2) +
  geom_point(mapping = aes(x=sr2[1],y=sr2[2]),size = 2) + 
  scale_colour_manual(values=prefPalette,guide=FALSE) + 
  scale_linetype_manual(values=prefLinePalatee,guide=FALSE) + theme_bw() + 
  theme(axis.text=element_blank(),axis.ticks=element_blank()) + xlab("Standard Deviation") + ylab("Expected Return")
leverage_plot = ggplot() + geom_path(mapping = aes(x = std, y = Eret, group = line, colour = style, linetype = style,group=line),
                                     data = eff_dt[pref=="leverage"]) +
  geom_point(mapping = aes(x=sr1[1],y=sr1[2]),size = 2) +
  scale_colour_manual(values=prefPalette,guide=FALSE) + 
  scale_linetype_manual(values=prefLinePalatee,guide=FALSE) + theme_bw() + 
  theme(axis.text=element_blank(),axis.ticks=element_blank()) + xlab("Standard Deviation") + ylab("Expected Return")
tikz("figures/lottery_plot.tex",width = 5.90551, height = 3,sanitize = FALSE)
plot(lottery_plot)
dev.off()
tikz("figures/leverage_plot.tex",width = 5.90551, height = 3,sanitize = FALSE)
plot(leverage_plot)
dev.off()

#### proxy regressions ####
m_returns2 = data.table(m_returns)
scale_vars = c("av_returns","market_returns","icrf","aem_leverage_factor","md_1984","chg_md_1984","call_money",
               "bank_prime","mr_rate","bc_chg_p","gaming_mcap","gratio","BH_MAX5","BH_MAX5_scale")
for(n in 37:nrow(m_returns2)){
  m_returns2[, eval(scale_vars) := lapply(.SD,scale), .SDcols = scale_vars]
  lmb = lm(av_returns~market_returns, data = m_returns2[(n-36):(n-1)])
  m_returns2[n, estBeta := coefficients(lmb)[2]]
  lm4 = lm(av_returns~shift(scale(av_weight),type = "lead"),m_returns2[(n-36):(n-1)])
  m_returns2[n, c("alpha2","beta2") := as.list(coefficients(lm4)[1:2])]
}
for(n in 72:nrow(m_returns2)){
  lm3 = lm(av_returns~estBeta,data = m_returns2[(n-36):(n-1)])
  m_returns2[n, c("alpha1","beta1") := as.list(coefficients(lm3)[1:2])]
}

#### lending regressions ####
lm_alpha_call = lm(alpha1 ~ market_returns + shift(call_money) , data = m_returns2)
lm_alpha_bank = lm(alpha1 ~ market_returns + shift(bank_prime) , data = m_returns2)
lm_alpha_aem = lm(alpha1 ~ market_returns + shift(aem_leverage_factor), data = m_returns2)
lm_alpha_icrf = lm(alpha1 ~ market_returns + shift(scale(icrf)), data = m_returns2)
lm_alpha_md_1984 = lm(alpha1 ~ market_returns + shift(md_1984), data = m_returns2)
lm_alpha_bc_chg_p = lm(alpha1 ~ market_returns + shift(bc_chg_p), data = m_returns2)
lm_alpha_bc_chg_p_nsa = lm(alpha1 ~ market_returns + shift(scale(bc_chg_p_nsa)), data = m_returns2)

lm_beta_call = lm(beta1 ~ market_returns + shift(call_money) , data = m_returns2)
lm_beta_bank = lm(beta1 ~ market_returns + shift(bank_prime) , data = m_returns2)
lm_beta_aem = lm(beta1 ~ market_returns + shift(aem_leverage_factor), data = m_returns2)
lm_beta_icrf = lm(beta1 ~ market_returns + shift(scale(icrf)), data = m_returns2)
lm_beta_md_1984 = lm(beta1 ~ market_returns + shift(md_1984), data = m_returns2)
lm_beta_bc_chg_p = lm(beta1 ~ market_returns + shift(bc_chg_p), data = m_returns2)
lm_beta_bc_chg_p_nsa = lm(beta1 ~ market_returns + shift(scale(bc_chg_p_nsa)), data = m_returns2)

#### lottery regressions ####
lm_alpha_gaming = lm(alpha1 ~ market_returns + shift(gaming_mcap), data = m_returns2)
lm_alpha_gratio = lm(alpha1 ~ market_returns + shift(gratio), data = m_returns2)
lm_alpha_bhmax5 = lm(alpha1 ~ market_returns + shift(BH_MAX5), data = m_returns2)
lm_alpha_bhmax5_scale = lm(alpha1 ~ market_returns + shift(BH_MAX5_scale), data = m_returns2)

lm_beta_gaming = lm(beta1 ~ market_returns + shift(gaming_mcap), data = m_returns2)
lm_beta_gratio = lm(beta1 ~ market_returns + shift(gratio), data = m_returns2)
lm_beta_bhmax5 = lm(beta1 ~ market_returns + shift(BH_MAX5), data = m_returns2)
lm_beta_bhmax5_scale = lm(beta1 ~ market_returns + shift(BH_MAX5_scale), data = m_returns2)

#### international ####
sbf120 = read_excel('sbf120.xlsx')
sbf120 = as.data.table(sbf120)
setorderv(sbf120,"Date",1)
sbf120[, retd := ROC(sbf120_tr,type = "discrete")]
sbf120[, retx := ROC(sbf120, type = "discrete")]
sbf120[, dp := sbf120_dps12/sbf120]
sbf120[, year := year(Date)]
sbf120[, month := month(Date)]
sbf120[, m_tr := tail(sbf120_tr,1), by = c("year","month")]
sbf120[, var1m := var(retd), by = c("year","month")]
setorderv(sbf120,"Date",-1)
sub_sbf120 = unique(sbf120,by=c("year","month"))
setorderv(sub_sbf120,"Date",1)
sub_sbf120[, mretd := ROC(m_tr,type = "discrete")]
sub_sbf120[, dg := ROC(sbf120_dps12, type = "discrete")]
sub_sbf120 = sub_sbf120[!is.na(dg)]
sub_sbf120[, var1m.tm1 := shift(var1m)]
setnames(sub_sbf120,"sbf120_dps12","dps12")
sub_sbf120 = subset(sub_sbf120,select=c("year","month","dp","dps12","var1m","mretd","dg","var1m.tm1"))
sub_sbf120[, Country := "france"]


indices = fread('bb_data.csv')
indices[, datadate := as.Date(datadate,format="%d/%m/%Y")]
setorderv(indices,"datadate",1)
indices[, germany.retd := ROC(hdax_tr,type = "discrete")]
indices[, year := year(datadate)]
indices[, month := month(datadate)]
indices[, m_tr := tail(hdax_tr,1), by = c("year","month")]
indices[, germany.var1m := var(germany.retd), by = c("year","month")]
indices[, hdax_div := momentum(hdax_dps12)]
hdax = fread(input = 'hdax.csv')
hdax[, date := as.Date(date,format="%d/%m/%y")]
setorderv(hdax,"date",1)
hdax[, germany.retx := ROC(hdax_price,type = "discrete")]
indices = merge(indices,hdax,by.x="datadate",by.y="date",all.x=TRUE)

indices2 = fread(input = 'some_indices.csv')
indices2[, V1 := NULL]
indices2[, Dates := as.Date(Dates,format="%Y-%m-%d")]

german_indices = merge(subset(indices,select = c("datadate","hdax_tr","hdax_dps12","germany.retd","hdax_price","germany.retx")),
                              subset(indices2,select = c("Dates","germany_retx","germany_tr")),all.x=TRUE,by.x="datadate",by.y="Dates")
# gvar = VAR(german_indices[complete.cases(german_indices),-"datadate"])
rhs = c(tail(colnames(german_indices),-1)[-c(4,5)],paste0(tail(colnames(german_indices),-1)[-c(4,5)],".tm1"))
german_indices[, paste0(tail(colnames(german_indices),-1)[-c(4,5)],".tm1") := lapply(.SD,shift), .SDcols = tail(colnames(german_indices),-1)[-c(4,5)]]
gform = paste0("hdax_price~",paste0(rhs,collapse = "+"))
gform = as.formula(gform)
gretxlm = lm(gform,data=german_indices)

gpred = predict(gretxlm,newdata = german_indices[is.na(hdax_price)&!is.na(germany_retx)])
german_indices[is.na(hdax_price)&!is.na(germany_retx), hdax_price := gpred]
indices[, hdax_price := NULL]
indices = merge(indices,subset(german_indices,select = c("datadate","hdax_price")),by="datadate",all.x=TRUE)
indices[, germany.retx := ROC(hdax_price,type = "discrete")]
indices[, germany.dp := hdax_dps12 / hdax_price]
indices[, germany.dg := ROC(hdax_dps12,type = "discrete")]

setorderv(indices,"datadate",-1)
setnames(indices,c("hdax_dps12","germany.var1m","germany.dp"),c("dps12","var1m","dp"))
sub_indices = unique(indices,by=c("year","month"))
setorderv(sub_indices,"datadate",1)
sub_indices[, mretd := ROC(m_tr,type = "discrete")]
sub_indices[, dg := ROC(dps12, type = "discrete")]
sub_indices = sub_indices[!is.na(dg)]
sub_indices[, var1m.tm1 := shift(var1m)]
sub_indices[, Country := "germany"]

bigI = rbind(sub_sbf120,subset(sub_indices,select = colnames(sub_sbf120)))
indices2[, germany_retx := NULL]
indices2[, germany_tr := NULL]
setnames(indices2,old = c("nifty_tr","nifty_retx","brazil_retx","brazil_tr","us_retx","us_tr","spchina_tr","spchina_retx","uk_retx",
                          "uk_ret"),new = c("india.retd","india.retx","brazil.retx","brazil.retd","us.retx","us.retd",
                                            "china.retd","china.retx","uk.retx","uk.retd"))
indices2[, year := year(Dates)]
indices2[, month := month(Dates)]
setkey(indices2,year,month)
# for(i in 1:nrow(indices2)){
#   indices2[, india.retd12 := ]
# }
indices2[, india.dp := ((1 + india.retd) / (1 + india.retx)) - 1]
indices2[!is.na(india.retd), india.mretd := prod(1+india.retd) - 1, by = c("year","month")]
indices2[!is.na(india.retx), india.mretx := prod(1+india.retx) - 1, by = c("year","month")]

i3 = c("india","brazil","china","uk","aus","can","worldD","world","hong_kong","skorea","taiwan","japan2","china2","japan","italy")
indi_list = vector(mode = "list",length = length(i3))

for(i in 1:length(i3)){
  tmpxl = read_excel(path = 'all_I2.xlsx',sheet = i,skip = 1,col_names = TRUE)
  tmpxl = as.data.table(tmpxl)
  setnames(tmpxl,c("datadate","price","tr","sma15","dps12"))
  tmpxl[, sma15 := NULL]
  setorderv(tmpxl,"datadate",1)
  tmpxl = tmpxl[complete.cases(tmpxl)]
  tmpxl[, retx := ROC(price,type = "discrete")]
  tmpxl[, retd := ROC(tr,type = "discrete")]
  tmpxl[, dp := dps12 / price]
  # tmpxl[, dg := ROC(dps12,type="discrete")]
  tmpxl[, country := i3[i]]
  indi_list[[i]] = tmpxl[!is.na(price)&!is.na(tr)]
}

indices3 = rbindlist(indi_list)
indices3[, year := year(datadate)]
indices3[, month := month(datadate)]
indices3[, mretx := prod(1+na.omit(retx))-1, by = c("country","year","month")]
indices3[, mretd := prod(1+na.omit(retd))-1, by = c("country","year","month")]
indices3[, var1m := var(retd,na.rm = TRUE), by = c("country","year","month")]
setorderv(indices3,c("country","datadate"),c(1,-1))
sub_indices3 = unique(subset(indices3,select = c("year","month","country","price","tr","dps12","dp","mretd","mretx","var1m")),
                      by = c("country","year","month"))
setorderv(sub_indices3,c("country","year","month"),c(1,1,1))
sub_indices3[, var1m.tm1 := shift(var1m), by = c("country")]
sub_indices3[, mretx12 := ROC(price,n = 12,type = "discrete"), by = country]
sub_indices3[, mretd12 := ROC(tr,n = 12,type = "discrete"), by = country]
sub_indices3[is.na(dp)&!is.na(mretx12)&!is.na(mretd12), dp := ((1+mretd12) / (1+mretx12)) -1 ]
sub_indices3[, dg := ROC(dps12,type = "discrete"), by = "country"]
setnames(sub_indices3,"country","Country")
# sub_indices3 = data.table::dcast(data = sub_indices3,formula = year + month ~ country+..., 
#                   value.var = list("price","tr","dps12","dp","dg","mretx","mretd","var1m","var1m.tm1","mretx12","mretd12"))
# new_names = unlist(lapply(str_split(colnames(sub_indices3)[3:ncol(sub_indices3)],"_"),function(x){paste0(x[2],".",x[1])}))
# setnames(sub_indices3,colnames(sub_indices3)[3:ncol(sub_indices3)],new_names)

# bigI = merge(bigI,sub_indices3,by=c("year","month"),all = TRUE)
bigI = rbind(bigI,subset(sub_indices3,select=colnames(bigI)))
setnames(goyal_data,c("us.mretd","us.mretx","us.dps12","us.price"),new = c("mretd","mretx","dps12","price"))
# bigI = merge(bigI,subset(goyal_data,select = c("year","month","us.mretd","us.mretx","us.dps12","Rfree")),all.x=TRUE)
goyal_data[, dp := dps12 / price]
gdata = subset(goyal_data,select=c("year","month","mretd","dps12","dp"))
gdata[, Country := "usa"]
# bigI = merge(bigI,subset(m_data,select = c("year","month","mkt_var1m","avg_var1m","avg_cor1m")))
# setnames(bigI,c("mkt_var1m","avg_var1m","avg_cor1m"),c("us.var1m","us.avar1m","us.acorr1m"))
gdata = merge(gdata,subset(m_data,select = c("year","month","mkt_var1m","avg_cor1m","avg_var1m")),by=c("year","month"))
setnames(gdata,c("mkt_var1m","avg_cor1m","avg_var1m"),c("var1m","acorr1m","avar1m"))
gdata[, var1m.tm1 := shift(var1m)]
gdata[, avar1m.tm1 := shift(avar1m)]
gdata[, acorr1m.tm1 := shift(acorr1m)]
gdata[, quater := quarter(as.Date(paste0(year,"-",month,"-01")))]

# # japan indicies #
# j_index = read_excel(path = 'japan.xlsx',col_names = TRUE,skip = 1)
# j_index = as.data.table(j_index)
# setnames(j_index,c("datadate","price","tr","sma15","dps12"))
# setorderv(j_index,"datadate",1)
# j_index[, sma15 := NULL]
# j_index = j_index[!is.na(price)]
# j_index[, retd := ROC(tr,type = "discrete")]
# j_index[, retx := ROC(price,type="discrete")]
# j_index[, year := year(datadate)]
# j_index[, month := month(datadate)]
# j_index[, var1m := var(retd,na.rm = TRUE), by = c("year","month")]
# j_index[, mretd := prod(1+retd)-1, by = c("year","month")]
# j_index[, mretx := prod(1+retx)-1, by = c("year","month")]
# setorderv(j_index,"datadate",-1)
# sub_jindex = unique(j_index,by=c("year","month"))
# setorderv(sub_jindex,c("year","month"),c(1,1))
# sub_jindex[, dg := ROC(dps12,type = "discrete")]
# sub_jindex[, dp := dps12 /price]
# sub_jindex[, var1m.tm1 := shift(var1m)]
# sub_jindex[, Country := "japan"]
# # setnames(sub_jindex,c("mretd","mretx","dg","dp","var1m","var1m.tm1","price","tr"),
# #          paste0("japan.",c("mretd","mretx","dg","dp","var1m","var1m.tm1","price","tr")))
# 
# # bigI = merge(bigI,
# #              subset(sub_jindex,select = c("year","month",
# #                                           paste0("japan.",c("mretd","mretx","dg","dp","var1m","var1m.tm1","price","tr")))),
# #              all.x=TRUE,by=c("year","month"))
# 
# bigI = rbind(bigI,subset(sub_jindex,select = colnames(bigI)))

# countries = c("japan","uk","china","aus","germany","france","brazil","india")
all_daily_returns = fread(input = 'all_daily.csv',header = TRUE,stringsAsFactors = FALSE,
                          drop = c(2,6,7,11),
                          colClasses = c(rep("character",3),"integer",rep("numeric",2),
                                         "integer",rep("character",3),"integer"),
                          key = c("gvkey","datadate"))
all_daily_returns[is.na(fic), fic := loc]


many_daily_returns = all_daily_returns[fic %fin% c("AUS","BRA","CHN","DEU","FRA","GBR",
                                                   "HKG","IND","ITA","JPN","KOR","TWN")]
all_daily_returns = NULL
gc()
# many_daily_returns = many_daily_returns[prccd > 1]
many_daily_returns = many_daily_returns[!(is.na(prccd)|is.na(cshoc))]
many_daily_returns[, gvkey2 := paste(gvkey,isin,sep = "_")]
setkey(many_daily_returns,datadate,gvkey2)
many_daily_returns = unique(many_daily_returns,by=c("datadate","gvkey2"))
creturns = c("aus","brazil","china","germany","france","uk","hong_kong","india","italy","japan","skorea","taiwan")
setkey(many_daily_returns,fic,gvkey2,datadate)
names(creturns) = unique(many_daily_returns$fic)
# c("india","brazil","china","uk","aus","can","worldD","world","hong_kong","skorea","taiwan","japan2","china2","japan")
many_daily_returns[, datadate := as.Date(datadate,format="%Y%m%d")]
many_daily_returns[, c("year","month") := list(year(datadate),month(datadate))]
many_daily_returns[, Country := creturns[fic]]
setkey(many_daily_returns,Country,year,month)
many_daily_returns[, fic := NULL]
many_daily_returns[, loc := NULL]
gc()
# return_files = c(japan = 'japan_daily.csv', uk = 'uk_daily.csv', china = 'china_daily.csv', aus = 'aus_daily.csv',
#                  germany = 'germany_daily.csv', france = 'fra_daily.csv',brazil = 'bra_daily.csv', india = 'india_daily.csv')
index_depth = c(japan = 255, uk = 100, china = 300, aus = 200, germany = 110, france = 120, brazil = 60, india = 50, hong_kong = 50,
                skorea = 200, taiwan = 50, italy = 40)

indi_list = vector("list",length = length(return_files))
for(f in 1:length(return_files)){
  tmpdt = fread(eval(return_files[f]))
  tmpdt[, datadate := as.character(datadate)]
  tmpdt[, datadate := as.Date(datadate,format("%Y%m%d"))]
  setkey(tmpdt,gvkey,datadate)
  tex = table(tmpdt$exchg)
  if(length(tex) > 3){
    stex = sort(tex,decreasing = TRUE)[1:3]
    nstex = names(stex)
    tmpdt = tmpdt[exchg %fin% nstex]
  }
  tmpdt = unique(tmpdt,by=c("datadate","gvkey"))
  tmpdt[, year := year(datadate)]
  tmpdt[, month := month(datadate)]
  tmpdt[, wkday := weekdays(datadate)]
  tmpdt = tmpdt[wkday %fin% c("Monday","Tuesday","Wednesday","Thursday","Friday")]
  tmpdt[, tdays := length(unique(datadate)), by=c("year","month","exchg")]
  tmpdt[, asset_tdays := length(na.omit(prccd)), by=c("year","month","gvkey")]
  tmpdt = tmpdt[asset_tdays > 14]
  tmpdt[, all_month := .75 * tdays <=  asset_tdays]
  tmpdt[, RET := ROC(prccd,type = "discrete"), by = c("gvkey")]
  tmpdt[, not_zero := (!sum(RET==0)==length(RET)),by=c("year","month","gvkey")]
  # tmpdt[, RET_sd := sd(RET,na.rm = TRUE), by = c("gvkey2","year","month")]
  # tmpdt = tmpdt[!is.na(RET_sd)]
  # cut = min(summary(tmpdt$RET_sd)[3:4])
  # tmpdt = tmpdt[all_month & not_zero & RET_sd >= cut]
  tmpdt = tmpdt[all_month & not_zero]
  setkey(tmpdt,gvkey,year,month)
  tmpdt[, mprccd := tail(na.omit(prccd),1), by = c("gvkey","year","month")]
  tmpdt[, mcsho := tail(na.omit(cshoc),1), by = c("gvkey","year","month")]
  # tmpdt[, mmcap := mprccd * mcsho]
  sub_tmpdt = subset(unique(tmpdt,by=c("year","month","gvkey")),select=c("year","month","gvkey","mprccd","mcsho"))
  sub_tmpdt[, mmcap := mcsho*mprccd]
  sub_tmpdt = sub_tmpdt[!is.na(mmcap)]
  depth = index_depth[f]
  sub_tmpdt = setorder(setDT(sub_tmpdt), year,month, -mmcap)[, indx := seq_len(.N), by = c("year","month")][indx <= depth]
  sub_tmpdt[, weight := mmcap / sum(mmcap), by = c("year","month")]
  tmpdt = merge(tmpdt,subset(sub_tmpdt,select = c("year","month","gvkey","weight")),by=c("year","month","gvkey"))
  # tmpdt = unique(tmpdt,by=c("datadate","gvkey"))
  tmpdt[, c("avar1m","acorr1m") := as.list(cor_var3(.SD)), .SDcols = c("datadate","gvkey","RET","weight"),
       by = c("year","month")]
  c = countries[f+1]
  tmpdt = unique(tmpdt,by=c("year","month"))
  sub_tmpdt = subset(tmpdt,select=c("year","month","avar1m","acorr1m"))
  sub_tmpdt[, avar1m.tm1 := shift(avar1m)]
  sub_tmpdt[, acorr1m.tm1 := shift(acorr1m)]
  sub_tmpdt[, country := c]
  indi_list[[f]] = sub_tmpdt
}
bigAV2 = rbindlist(indi_list)
bigAV2 = as.data.table(bigAV2)
setnames(bigAV2,"country","Country")
# bigAV = data.table::dcast(bigAV,formula = year + month ~ country + ...,value.var=c("avar1m","avar1m.tm1","acorr1m","acorr1m.tm1"))
# new_names = unlist(lapply(str_split(colnames(bigAV)[3:ncol(bigAV)],"_"),function(x){paste0(x[2],".",x[1])}))
# setnames(bigAV,colnames(bigAV)[3:ncol(bigAV)],new_names)
# many_daily_returns[, Country := creturns[fic]]
# many_daily_returns[, c("year","month") := list(year(datadate),month(datadate))]
setkey(many_daily_returns,Country,gvkey2,year,month)
many_daily_returns[, tdays := length(unique(datadate)), by=c("Country","year","month")]
many_daily_returns[, asset_tdays := length(prccd), by=c("gvkey2","year","month")]
many_daily_returns[, all_month := .75 * tdays <=  asset_tdays]
many_daily_returns[, RET := ROC(prccd,type = "discrete"), by = c("gvkey2")]
many_daily_returns[, not_zero := (!sum(RET==0)==length(RET)),by=c("year","month","gvkey2")]
many_daily_returns = many_daily_returns[asset_tdays > 14&all_month&not_zero&!is.na(RET)]
gc()
many_daily_returns[, mprccd := tail(na.omit(prccd),1), by = c("gvkey2","year","month")]
many_daily_returns[, mcsho := tail(na.omit(cshoc),1), by = c("gvkey2","year","month")]
sub_many_daily_returns = subset(unique(many_daily_returns,by=c("year","month","gvkey2")),select=c("year","month","gvkey2","mprccd","mcsho","Country"))
sub_many_daily_returns[, mmcap := mcsho*mprccd]
sub_many_daily_returns = sub_many_daily_returns[!is.na(mmcap)]
sub_many_daily_returns[, depth := index_depth[Country]]
sub_many_daily_returns = setorder(setDT(sub_many_daily_returns),Country,year,month, -mmcap)[, indx := seq_len(.N), by = c("Country","year","month")][indx <= depth]
nassts = sub_many_daily_returns[, .(num_assts = length(unique(gvkey2))), by = c("Country","year","month")]
sub_many_daily_returns = merge(sub_many_daily_returns,nassts,by=c("Country","year","month"))
# sub_many_daily_returns = sub_many_daily_returns[num_assts >= .6 * depth]
sub_many_daily_returns[, weight := mmcap / sum(mmcap), by = c("year","month")]
many_daily_returns = merge(many_daily_returns,subset(sub_many_daily_returns,
                                                     select = c("Country","year","month","gvkey2","weight","num_assts","depth")),
                           by=c("Country","year","month","gvkey2"))
gc()
many_daily_returns[, c("avar1m","acorr1m") := as.list(cor_var2(.SD)), .SDcols = c("datadate","gvkey2","RET","weight"), by = c("Country","year","month")]
sub_many_daily_returns = unique(subset(many_daily_returns,
                                       select = c("Country","year","month","num_assts","depth","avar1m","acorr1m")),
                                       by=c("Country","year","month"))

bigData = merge(bigI,sub_many_daily_returns,by=c("year","month","Country"),all.x=TRUE)
bigData2 = merge(bigI,bigAV2,by=c("year","month","Country"),all.x=TRUE)
setkey(bigData,Country,year,month)
bigData = bigData[!Country=="china2"]
jpdata = bigData[Country=="japan"]
jpdata[, Country := "japan2"]
jpdata = subset(jpdata,select = c("Country","year","month","num_assts","depth","avar1m","acorr1m"))
setnames(jpdata,old = c("num_assts","depth","avar1m","acorr1m"),new=paste0(c("num_assts","depth","avar1m","acorr1m"),".2"))
bigData = merge(bigData,jpdata,by=c("Country","year","month"),all.x=TRUE)
bigData[Country=="japan2", c("num_assts","depth","avar1m","acorr1m") := 
          list(num_assts.2,depth.2,avar1m.2,acorr1m.2)]
bigData[, paste0(c("num_assts","depth","avar1m","acorr1m"),".2") := NULL]
IntSummary = bigData[!is.na(dg), .(Start = paste(head(year,1),"-",head(month,1)), N = length(.I),
                      Avg_Assests = round(mean(num_assts,na.rm = TRUE),1)), by = "Country"]

bigData2 = bigData2[!Country%fin%c("japan2","china2")]
IntSummary2 = bigData2[!is.na(dg), .(Start = paste(head(year,1),"-",head(month,1)), N = length(.I)), by = "Country"]
# # melt(bigData,id.vars = c("year","month","country"))
# bigData2 = data.table(bigData)
# bigData2[, Rfree := NULL]
# bigData2[, Rf_lag := NULL]
# bigData2 = data.table::melt(bigData2,id.vars = c("year","month"))
# bigData2 = bigData2 %>%
#   separate(variable, c("Country", "new_old"), sep = "(?<=^[:alpha:]{1,8})\\.") %>% 
#   spread(new_old, value)
# 
# bigData2 = as.data.table(bigData2)
# setkey(bigData,Country,year,month)
genc = function(X,c="av"){
  tsd = sd(X$xlogret,na.rm = TRUE)
  sd1 = sd(X$xlogret / X$avar1m.tm1,na.rm = TRUE)
  c_av = tsd / sd1
  sd2 = sd(X$xlogret / X$var1m.tm1,na.rm = TRUE)
  c_sv = tsd / sd2
  if(c=="av"){
    return(c_av)
  } else {return(c_sv)}
  # return(list(c_av,c_sv))
}

# littleData = bigData[Country %in% countries]
setkey(bigData,Country,year,month)
setorderv(bigData,cols = c("Country","year","month"),order = c(1,1,1))
bigData[, avar1m.tm1 := shift(avar1m), by = Country]
bigData[, acorr1m.tm1 := shift(acorr1m), by = Country]
gdata[, dg := ROC(dps12,type = "discrete")]
gdata[, depth := 500]
gdata[, num_assts := 500]
bigData = rbind(bigData,gdata)
bigData = merge(bigData,subset(goyal_data,select = c("year","month","Rfree")),by=c("year","month"))
setkey(bigData,Country,year,month)
bigData[, Rf_lag := shift(Rfree), by = "Country"]
bigData[, xlogret := log1p(mretd) - log1p(Rf_lag)]
# bigData[, ]
bigData[!is.na(dg), c_av := genc(.SD,"av"), .SDcols = c("xlogret","avar1m.tm1","var1m.tm1"), by=c("Country")]
bigData[!is.na(dg), c_sv := genc(.SD,"sv"), .SDcols = c("xlogret","avar1m.tm1","var1m.tm1"), by=c("Country")]
bigData[, "av_weight" := c_av/avar1m.tm1]
bigData[, "sv_weight" := c_sv/var1m.tm1]
bigData[, av_return := av_weight * xlogret]
bigData[, sv_return := sv_weight * xlogret]

results_table2 = bigData[!is.na(dg)&!(is.na(av_weight)|is.na(sv_weight)), 
                        .(av_return = mean(av_weight*xlogret)*1200, 
                          av_sharpe = mean(av_weight*xlogret)*sqrt(12)/sd(av_weight*xlogret),
                          sv_return = mean(sv_weight*xlogret)*1200, 
                          sv_sharpe = mean(sv_weight*xlogret)*sqrt(12)/sd(sv_weight*xlogret),
                          bh_return = mean(xlogret)*1200,
                          bh_sharpe = mean(xlogret)*sqrt(12)/sd(xlogret)
                          ), 
                        by = Country]

bigData2 = merge(bigData2,subset(goyal_data,select = c("year","month","Rfree")),by=c("year","month"))
setkey(bigData2,Country,year,month)
bigData2[, Rf_lag := shift(Rfree), by = "Country"]
bigData2[, xlogret := log1p(mretd) - log1p(Rf_lag)]
# bigData2[, ]
bigData2[!is.na(dg), c_av := genc(.SD,"av"), .SDcols = c("xlogret","avar1m.tm1","var1m.tm1"), by=c("Country")]
bigData2[!is.na(dg), c_sv := genc(.SD,"sv"), .SDcols = c("xlogret","avar1m.tm1","var1m.tm1"), by=c("Country")]
bigData2[, "av_weight" := c_av/avar1m.tm1]
bigData2[, "sv_weight" := c_sv/var1m.tm1]
bigData2[, av_return := av_weight * xlogret]
bigData2[, sv_return := sv_weight * xlogret]

results_table3 = bigData2[!is.na(dg)&!(is.na(av_weight)|is.na(sv_weight)), 
        .(av_return = mean(av_weight*xlogret)*1200, 
          av_sharpe = mean(av_weight*xlogret)*sqrt(12)/sd(av_weight*xlogret),
          sv_return = mean(sv_weight*xlogret)*1200, 
          sv_sharpe = mean(sv_weight*xlogret)*sqrt(12)/sd(sv_weight*xlogret),
          bh_return = mean(xlogret)*1200,
          bh_sharpe = mean(xlogret)*sqrt(12)/sd(xlogret)
        ), 
        by = Country]
IntData = rbind(bigData[Country%fin%c("aus","brazil","china","france","italy","japan")],
                bigData2[Country%fin%c("germany","india","uk")],fill = TRUE)
stargazer(IntSummary[Country%fin%unique(IntData$Country)],summary = FALSE,out = 'tables/tab_intSummary.tex')

IntData = IntData %>%
  mutate(Country =  factor(Country, 
                           levels = c("aus","brazil","china","germany","france","india",
                                      "italy","japan","uk","usa"))) %>%
  arrange(Country) 
IntData = as.data.table(IntData)
IntPerformance1 = IntData[!is.na(dg)&!(is.na(av_weight)|is.na(sv_weight)), 
                          .(av_return = mean(av_weight*xlogret)*1200, 
                            av_returnp = hac_t.test(resReturns(av_weight*xlogret,
                                                               sv_weight*xlogret),
                                                    resReturns(sv_weight*xlogret,
                                                               av_weight*xlogret),alternative = "greater",
                                                    paired = TRUE,var.equal = TRUE)$p.val,
                            av_sharpe = mean(av_weight*xlogret)*sqrt(12)/sd(av_weight*xlogret),
                            av_sharpep = ratio.test2(rets1 = 
                                                       resReturns(av_weight*xlogret,
                                                                  sv_weight*xlogret),
                                                     rets2 = resReturns(sv_weight*xlogret,
                                                                av_weight*xlogret),ratio = "Sratio"),
                            sv_return = mean(sv_weight*xlogret)*1200, 
                            sv_sharpe = mean(sv_weight*xlogret)*sqrt(12)/sd(sv_weight*xlogret),
                            bh_return = mean(xlogret)*1200,
                            bh_sharpe = mean(xlogret)*sqrt(12)/sd(xlogret)
                          ), 
                          by = Country]



IntPerformance1 = cbind(c("AUS","BRA","CHN","FRA","ITA","JPN","DEU","IND","UK","USA"),round(IntPerformance1[, -"Country"],digits = 3))
setnames(IntPerformance1,"V1","Country")
tmpI1 = see.stars(as.matrix(IntPerformance1[, -"Country"],nrow=nrow(IntPerformance1)),c(1,3),c(2,4))[,c(1,3,5:(ncol(IntPerformance1)-1))]
IntPerformance1 = cbind(c("AUS","BRA","CHN","FRA","ITA","JPN","DEU","IND","UK","USA"),tmpI1)
# setorderv(IntPerformance1,"Country",1)
stargazer(IntPerformance1,summary = FALSE,out = 'tables/performance/tab_intPerf1.tex')

intlist = vector(mode = "list",length = 3)

for(i in 1:length(c("av_return","sv_return","xlogret"))){
  r = c("av_return","sv_return","xlogret")[i]
  tmp = IntData[!is.na(dg)&!(is.na(av_weight)|is.na(sv_weight)), 
          as.list(ddsummary2(get(r))), 
          by = Country]
  tmp = round(tmp[, -"Country"],digits = 3)
  intlist[[i]] = tmp
}
IntPerformance2 = cbind(intlist[[1]],cbind(intlist[[2]],intlist[[3]]))
setnames(IntPerformance2,rep(c("Avg DD","Avg Length","Avg Recovery"),3))
IntPerformance2 = cbind(c("AUS","BRA","CHN","FRA","ITA","JPN","DEU","IND","UK"),IntPerformance2)
setnames(IntPerformance2,"V1","Country")
setorderv(IntPerformance2,"Country",1)
stargazer(IntPerformance2,summary = FALSE,out = 'tables/performance/tab_intPerf2.tex')

IntPerformance3 = IntData[!is.na(dg)&!(is.na(av_weight)|is.na(sv_weight)),
                           .(abs_change = round(mean(abs(diff(av_weight)),na.rm = T),3),
                             Break_even = round((mean(av_return,na.rm = T)-mean(xlogret,na.rm = T)) / mean(abs(diff(av_weight)),na.rm = T)*10000,3),
                             abs_change = round(mean(abs(diff(sv_weight)),na.rm = T),3),
                             Break_even = round((mean(sv_return,na.rm = T)-mean(xlogret,na.rm = T)) / mean(abs(diff(sv_weight)),na.rm = T)*10000,3)
                           )
                          , by = Country]
IntPerformance3 = cbind(c("AUS","BRA","CHN","FRA","ITA","JPN","DEU","IND","UK"),IntPerformance3[,2:5])
setnames(IntPerformance3,"V1","Country")
setorderv(IntPerformance3,"Country",1)
IntPerformance3 = cbind(IntPerformance1[,1:2],IntPerformance3[,2:3],IntPerformance1[,4],IntPerformance3[,4:5],IntPerformance1[,6])

stargazer(IntPerformance3,summary = FALSE,out = 'tables/performance/tab_intPerf3.tex')

# world avar1m #
ratios = fread('fredgraph_mcap2gdp.csv')
ratios = melt(ratios,id.vars = "DATE",value.name = "mkt2gdp",variable.name = "Country")
ratios[, mkt2gdp := mkt2gdp / 100]

usGDP = fread(input = 'usGDP.csv')
usGDP[, year := year(as.Date(DATE,format="%Y-%m-%d"))]
usGDP[, quarter := quarter(as.Date(DATE,format="%Y-%m-%d"))]
ausGDP = fread('australia_gdp.csv')
ausGDP[, year := year(as.Date(DATE,format="%Y-%m-%d"))]
gdp = merge(subset(usGDP,select = c("year","quarter","us")),subset(ausGDP,select = c("year","australia")),by=c("year"))
ausmkt2gdp = fread(input = 'australia.csv')
ratios[, year := year(as.Date(DATE,format="%Y-%m-%d"))]
ratios[, DATE := NULL]
ratios = rbind(ratios,ausmkt2gdp)

files = list.files(pattern = ".*\\_gdp\\.csv")
files = files[2:length(files)]
for(f in files){
  tmp = fread(input = f)
  tmp[, year := year(as.Date(DATE,format="%Y-%m-%d"))]
  tmp[, DATE := NULL]
  gdp = merge(gdp,tmp,by=c("year"),all.x=TRUE)
}
setnames(gdp,"us","usa")
gdp = melt(gdp,id.vars = c("year","quarter"),variable.name = "Country",value.name = "gdp")
ratios[, year := as.numeric(year)]
mktvalue = merge(gdp,ratios,by=c("year","Country"))
setkey(mktvalue,year,quarter)
mktvalue = mktvalue[mkt2gdp > 0]
mktvalue[, mktcap := gdp * mkt2gdp]
mktvalue[, weight := mktcap / sum(mktcap), by = c("year","quarter")]
gdata = merge(gdata,subset(goyal_data,select = c("year","month","Rfree")), by=c("year","month"))
gdata[, Rf_lag := shift(Rfree)]
gdata[, xlogret := log1p(mretd) - log1p(Rf_lag)]
gdata[!is.na(dg), c_av := genc(.SD,"av"), .SDcols = c("xlogret","avar1m.tm1","var1m.tm1")]
gdata[!is.na(dg), c_sv := genc(.SD,"sv"), .SDcols = c("xlogret","avar1m.tm1","var1m.tm1")]
gdata[, "av_weight" := c_av/avar1m.tm1]
gdata[, "sv_weight" := c_sv/var1m.tm1]
gdata[, av_return := av_weight * xlogret]
gdata[, sv_return := sv_weight * xlogret]
IntData = rbind(IntData,subset(gdata,select = colnames(gdata)[colnames(gdata)%in%colnames(IntData)]))
IntData[, quarter := quarter(as.Date(paste0(year,"-",month,"-01")))]
world_avar = merge(subset(mktvalue,select = c("year","quarter","weight","Country")),IntData,
                   by=c("year","quarter","Country"),all.y=TRUE)
world_avar = world_avar[!is.na(weight)]
world_avar[, worldavar1m := sum(avar1m*weight), by = c("year","month")]
world_avar[, worldvar1m := sum(var1m*weight), by = c("year","month")]
worldData1 = bigData2[Country %in% c("world")]
world_avar = unique(world_avar,by=c("year","month","worldavar1m","worldvar1m"))
worldData1 = merge(worldData1,subset(world_avar,select=c("year","month","worldavar1m","worldvar1m")),by=c("year","month"))
worldData2 = bigData2[Country %in% c("worldD")]
worldData2 = merge(worldData2,subset(world_avar,select=c("year","month","worldavar1m","worldvar1m")),by=c("year","month"))
worldData = rbind(worldData1,worldData2)
worldData = merge(worldData,subset(goyal_data,select = c("year","month","Rfree")), by=c("year","month"))
worldData[, avar1m.tm1 := shift(worldavar1m), by = c("Country")]
worldData[, var1m.tm1 := shift(worldvar1m), by = c("Country")]
# worldData[, Rf_lag := shift(Rfree)]
# worldData[, xlogret := log1p(mretd) - log1p(Rf_lag)]
worldData[!is.na(dg), c_av := genc(.SD,"av"), .SDcols = c("xlogret","avar1m.tm1","var1m.tm1")]
worldData[!is.na(dg), c_sv := genc(.SD,"sv"), .SDcols = c("xlogret","avar1m.tm1","var1m.tm1")]
worldData[, "av_weight" := c_av/avar1m.tm1]
worldData[, "sv_weight" := c_sv/var1m.tm1]
worldData[, av_return := av_weight * xlogret]
worldData[, sv_return := sv_weight * xlogret]
worldData[!is.na(dg)&!(is.na(av_weight)|is.na(sv_weight)), 
          .(av_return = mean(av_weight*xlogret)*1200, 
            av_returnp = hac_t.test(resReturns(av_weight*xlogret,
                                               sv_weight*xlogret),
                                    resReturns(sv_weight*xlogret,
                                               av_weight*xlogret),alternative = "greater",
                                    paired = TRUE,var.equal = TRUE)$p.val,
            av_sharpe = mean(av_weight*xlogret)*sqrt(12)/sd(av_weight*xlogret),
            av_sharpep = ratio.test2(rets1 = 
                                       resReturns(av_weight*xlogret,
                                                  sv_weight*xlogret),
                                     rets2 = resReturns(sv_weight*xlogret,
                                                        av_weight*xlogret),ratio = "Sratio"),
            sv_return = mean(sv_weight*xlogret)*1200, 
            sv_sharpe = mean(sv_weight*xlogret)*sqrt(12)/sd(sv_weight*xlogret),
            bh_return = mean(xlogret)*1200,
            bh_sharpe = mean(xlogret)*sqrt(12)/sd(xlogret)
          ), 
          by = Country]
worldData[!is.na(dg)&!(is.na(av_weight)|is.na(sv_weight)),
        .(abs_change = round(mean(abs(diff(av_weight)),na.rm = T),3),
          Break_even = round((mean(av_return,na.rm = T)-mean(xlogret,na.rm = T)) / mean(abs(diff(av_weight)),na.rm = T)*10000,3),
          abs_change = round(mean(abs(diff(sv_weight)),na.rm = T),3),
          Break_even = round((mean(sv_return,na.rm = T)-mean(xlogret,na.rm = T)) / mean(abs(diff(sv_weight)),na.rm = T)*10000,3)
        )
        , by = Country]
intlist = vector(mode = "list",length = 3)
for(i in 1:length(c("av_return","sv_return","xlogret"))){
  r = c("av_return","sv_return","xlogret")[i]
  tmp = worldData[!is.na(dg)&!(is.na(av_weight)|is.na(sv_weight)), 
                as.list(ddsummary2(get(r))), 
                by = Country]
  tmp = round(tmp[, -"Country"],digits = 3)
  intlist[[i]] = tmp
}
worldPerformance2 = cbind(intlist[[1]],cbind(intlist[[2]],intlist[[3]]))
setnames(worldPerformance2,rep(c("Avg DD","Avg Length","Avg Recovery"),3))
worldPerformance2 = cbind(c("world","worldD"),worldPerformance2)
setnames(worldPerformance2,"V1","Country")
setorderv(worldPerformance2,"Country",1)
# stargazer(worldPerformance2,summary = FALSE,out = 'tables/performance/tab_intPerf2.tex')

#### currency carry ####
currency = fread(input = 'currency_factors.csv')
currency[, curr.ret.RX.paper := (1+(as.numeric(curr.ret.RX.paper)/100))]
currency[, curr.hml.paper := (1+(as.numeric(curr.hml.paper)/100))]
currency = merge(currency,
                 subset(m_data,
                        select = c("year","month","avg_var1m","avg_cor1m","mkt_var1m")),
                 by=c("year","month"),all.x=TRUE)
currency = merge(currency,subset(goyal_data,select=c("year","month","Rfree")))
currency = merge(currency,subset(worldData[Country=="worldD"],select = c("year","month","worldavar1m","worldvar1m","avar1m.tm1",
                                                                           "var1m.tm1")),by=c("year","month"))
currency[, xlogret := log(curr.hml.paper)]
currency[, c_av := genc(.SD,"av"), .SDcols = c("xlogret","avar1m.tm1","var1m.tm1")]
currency[, c_sv := genc(.SD,"sv"), .SDcols = c("xlogret","avar1m.tm1","var1m.tm1")]
currency[, "av_weight" := c_av/avar1m.tm1]
currency[, "sv_weight" := c_sv/var1m.tm1]
currency[, av_return := av_weight * xlogret]
currency[, sv_return := sv_weight * xlogret]
currency[!(is.na(av_return)|is.na(sv_return)), 
          .(av_return = mean(av_return)*1200, 
            av_sharpe = mean(av_return)*sqrt(12)/sd(av_return),
            sv_return = mean(sv_return)*1200, 
            sv_sharpe = mean(sv_return)*sqrt(12)/sd(sv_return),
            bh_return = mean(xlogret)*1200,
            bh_sharpe = mean(xlogret)*sqrt(12)/sd(xlogret)
          )]
currency[!(is.na(av_weight)|is.na(sv_weight)),
          .(abs_change = round(mean(abs(diff(av_weight)),na.rm = T),3),
            Break_even = round((mean(av_return,na.rm = T)-mean(xlogret,na.rm = T)) / mean(abs(diff(av_weight)),na.rm = T)*10000,3),
            abs_change = round(mean(abs(diff(sv_weight)),na.rm = T),3),
            Break_even = round((mean(sv_return,na.rm = T)-mean(xlogret,na.rm = T)) / mean(abs(diff(sv_weight)),na.rm = T)*10000,3)
          )]
intlist = vector(mode = "list",length = 3)
for(i in 1:length(c("av_return","sv_return","xlogret"))){
  r = c("av_return","sv_return","xlogret")[i]
  tmp = currency[!(is.na(av_weight)|is.na(sv_weight)), 
                  as.list(ddsummary2(get(r)))]
  tmp = round(tmp,digits = 3)
  intlist[[i]] = tmp
}
currencyPerformance2 = cbind(intlist[[1]],cbind(intlist[[2]],intlist[[3]]))
setnames(currencyPerformance2,rep(c("Avg DD","Avg Length","Avg Recovery"),3))
# currencyPerformance2 = cbind(c("currency","currencyD"),currencyPerformance2)
setnames(currencyPerformance2,"V1","Country")
setorderv(currencyPerformance2,"Country",1)


bbdollar = as.data.table(read_excel(path = 'moreI.xlsx',sheet = 1,skip = 1))
bbdollar[, c("year","month") := list(year(Date),month(Date))]
setorderv(bbdollar,cols = "Date",order = 1)
bbdollar[, bbdollar := ROC(`Last Price`,type = "discrete")]

reit = as.data.table(read_excel(path = 'moreI.xlsx',sheet = 2,skip = 1))
reit[, c("year","month") := list(year(Date),month(Date))]
setorderv(reit,cols = "Date",order = 1)
setnames(reit,c("Date","price","tr","sma15","dps","year","month"))
reit[, reit := ROC(tr,type = "discrete")]
alt_inv = merge(subset(bbdollar,select = c("year","month","bbdollar")),
                subset(reit,select = c("year","month","reit")),by=c("year","month"))
dbcurr = as.data.table(read_excel(path = 'moreI.xlsx',sheet = 3,skip = 1))
dbcurr[, c("year","month") := list(year(Date),month(Date))]
setorderv(dbcurr,cols = "Date",order = 1)
dbcurr[, dbcurr := ROC(`DBCRUSI Index - Last Price`,type="discrete")]
alt_inv = merge(alt_inv,subset(dbcurr,select=c("year","month","dbcurr")),
                by=c("year","month"),all.x=TRUE)

dbcarry = as.data.table(read_excel(path = 'moreI.xlsx',sheet = 4,skip = 1))
dbcarry[, c("year","month") := list(year(Date),month(Date))]
setorderv(dbcarry,cols = "Date",order = 1)
dbcarry[, dbcarry := ROC(`Mid Price`,type = "discrete")]
alt_inv = merge(alt_inv,subset(dbcarry,select=c("year","month","dbcarry")),
                by=c("year","month"),all.x=TRUE)

dbmom = as.data.table(read_excel(path = 'moreI.xlsx',sheet = 5,skip = 1))
dbmom[, c("year","month") := list(year(Date),month(Date))]
setorderv(dbmom,cols = "Date",order = 1)
dbmom[, dbmom := ROC(`Mid Price`,type="discrete")]
alt_inv = merge(alt_inv,subset(dbmom,select=c("year","month","dbmom")),
                by=c("year","month"),all.x=TRUE)

dbmom2 = as.data.table(read_excel(path = 'moreI.xlsx',sheet = 6,skip = 1))
dbmom2[, c("year","month") := list(year(Date),month(Date))]
setorderv(dbmom2,cols = "Date",order = 1)
dbmom2[, dbmom2 := ROC(`Mid Price`,type = "discrete")]
alt_inv = merge(alt_inv,subset(dbmom2,select=c("year","month","dbmom2")),
                by=c("year","month"),all.x=TRUE)

dbval = as.data.table(read_excel(path = 'moreI.xlsx',sheet = 7,skip = 1))
dbval[, c("year","month") := list(year(Date),month(Date))]
setorderv(dbval,cols = "Date",order = 1)
dbval[, dbval := ROC(`Mid Price`,type = "discrete")]
alt_inv = merge(alt_inv,subset(dbval,select=c("year","month","dbval")),
                by=c("year","month"),all.x=TRUE)

bonds = fread(input = 'bondI.csv')
bonds[, Date := as.Date(as.character(Date),format="%d/%m/%Y")]
bonds = bonds[!is.na(Date)]
bonds[, year := year(Date)]
bonds[, month := month(Date)]
bonds = unique(bonds,by=c("year","month"))
bonds[, Date := NULL]
setkey(bonds,year,month)
bonds[, bbUSuniv := ROC(bbusu_bond,type = "discrete")]
bonds[, bbUSagg := ROC(bbusa_bond,type = "discrete")]
alt_inv = merge(alt_inv,subset(bonds,select = c("year","month","bbUSuniv","bbUSagg")),
                by = c("year","month"),all.x=TRUE)

bcom = as.data.table(read_excel(path = 'commI.xlsx',sheet = 1,skip = 1))
bcom[, c("year","month") := list(year(Date),month(Date))]
bcom = unique(bcom,by=c("year","month"))
setorderv(bcom,cols = "Date",order = 1)
bcom[, bcom := ROC(`BCOM Index - Last Price`,type="discrete")]
alt_inv = merge(alt_inv,subset(bcom,select=c("year","month","bcom")),
                by=c("year","month"),all.x=TRUE)

gscom = as.data.table(read_excel(path = 'commI.xlsx',sheet = 3,skip = 1))
gscom[, c("year","month") := list(year(Date),month(Date))]
gscom = unique(gscom,by=c("year","month"))
setorderv(gscom,cols = "Date",order = 1)
gscom[, gscom := ROC(`SPGSESTR Index - Last Price`,type="discrete")]
alt_inv = merge(alt_inv,subset(gscom,select=c("year","month","gscom")),
                by=c("year","month"),all.x=TRUE)
alt_inv_m = melt(alt_inv,id.vars = c("year","month"),variable.name = "index",value.name = "RET") 
alt_inv_m = merge(alt_inv_m,subset(currency,
                                   select = c("year","month","Rfree","worldavar1m","worldvar1m",
                                              "avar1m.tm1","var1m.tm1","avg_var1m","avg_cor1m","mkt_var1m")),
                  by=c("year","month"),all.x=TRUE)
setkey(alt_inv_m,index,year,month)
alt_inv_m[, Rf_lag := shift(Rfree), by = index]
alt_inv_m[, xlogret := log1p(RET) - log1p(Rf_lag)]
alt_inv_m[!(is.na(avar1m.tm1)|is.na(var1m.tm1)), c_av := genc(.SD,"av"), .SDcols = c("xlogret","avar1m.tm1","var1m.tm1"), by = index]
alt_inv_m[!(is.na(avar1m.tm1)|is.na(var1m.tm1)), c_sv := genc(.SD,"sv"), .SDcols = c("xlogret","avar1m.tm1","var1m.tm1"), by = index]
alt_inv_m[, "av_weight" := c_av/avar1m.tm1]
alt_inv_m[, "sv_weight" := c_sv/var1m.tm1]
alt_inv_m[, av_return := av_weight * xlogret]
alt_inv_m[, sv_return := sv_weight * xlogret]
altPerf1 = alt_inv_m[!(is.na(av_return)|is.na(sv_return)), 
                     .(av_return = mean(av_weight*xlogret)*1200, 
                       av_returnp = hac_t.test(resReturns(av_weight*xlogret,
                                                          sv_weight*xlogret),
                                               resReturns(sv_weight*xlogret,
                                                          av_weight*xlogret),alternative = "greater",
                                               paired = TRUE,var.equal = TRUE)$p.val,
                       av_sharpe = mean(av_weight*xlogret)*sqrt(12)/sd(av_weight*xlogret),
                       av_sharpep = ratio.test2(rets1 = 
                                                  resReturns(av_weight*xlogret,
                                                             sv_weight*xlogret),
                                                rets2 = resReturns(sv_weight*xlogret,
                                                                   av_weight*xlogret),ratio = "Sratio"),
                       sv_return = mean(sv_weight*xlogret)*1200, 
                       sv_sharpe = mean(sv_weight*xlogret)*sqrt(12)/sd(sv_weight*xlogret),
                       bh_return = mean(xlogret)*1200,
                       bh_sharpe = mean(xlogret)*sqrt(12)/sd(xlogret)
                     ), 
                     by = index]
altPerf1 = cbind(altPerf1[,1],round(altPerf1[,2:ncol(altPerf1)],3))
# setnames(altPerf1,"V1","Index")
tmpI1 = see.stars(as.matrix(altPerf1[, -"index"],nrow=nrow(altPerf1)),c(1,3),c(2,4))[,c(1,3,5:(ncol(altPerf1)-1))]
altPerf1 = cbind(c("bbdollar","reit","dbcurr","dbcarry","dbmom","dbmom2","dbval","bcom","gscom","bbUSuniv","bbUSagg"),tmpI1)
# setorderv(IntPerformance1,"Country",1)
stargazer(IntPerformance1,summary = FALSE,out = 'tables/performance/tab_intPerf1.tex')

stargazer(altPerf1,summary = FALSE,out = 'tables/performance/tab_altPerf1.tex')
altPerf2 = alt_inv_m[!(is.na(av_return)|is.na(sv_return)),
                    .(av_return = mean(av_return)*1200,
                      abs_change = round(mean(abs(diff(av_weight)),na.rm = T),3),
                      Break_even = round((mean(av_return,na.rm = T)-mean(xlogret,na.rm = T)) / mean(abs(diff(av_weight)),na.rm = T)*10000,3),
                      sv_return = mean(sv_return)*1200,
                      abs_change = round(mean(abs(diff(sv_weight)),na.rm = T),3),
                      Break_even = round((mean(sv_return,na.rm = T)-mean(xlogret,na.rm = T)) / mean(abs(diff(sv_weight)),na.rm = T)*10000,3),
                      bh_return = mean(xlogret)*1200
                    ), by = index]
# altPerf1 = cbind(c("bbdollar","reit","dbcurr","dbcarry","dbmom","dbmom2","dbval","bcom","gscom","bbUSuniv","bbUSagg"),tmpI1)
# altPerf2 = round(altPerf2,3)
altPerf2 = cbind(altPerf2[,1],round(altPerf2[,2:ncol(altPerf2)],3))

stargazer(altPerf2,summary = FALSE,out = 'tables/performance/tab_altPerf2.tex')

intlist = vector(mode = "list",length = 3)
for(i in 1:length(c("av_return","sv_return","xlogret"))){
  r = c("av_return","sv_return","xlogret")[i]
  tmp = alt_inv_m[!(is.na(av_weight)|is.na(sv_weight)), 
                 as.list(ddsummary2(get(r))), by = index]
  tmp = round(tmp[, -"index"],digits = 3)
  intlist[[i]] = tmp
}
altPerf3 = cbind(intlist[[1]],cbind(intlist[[2]],intlist[[3]]))
setnames(altPerf3,rep(c("Avg DD","Avg Length","Avg Recovery"),3))
altPerf3 = cbind(c("bbdollar","reit","dbcurr","bdcarry","dbmom","dbmom2","dbval","bcom","gscom","bbUSuniv","bbUSagg"),altPerf3)
setnames(altPerf3,"V1","Index")
setorderv(altPerf3,"Index",1)
stargazer(altPerf3,summary = FALSE,out = 'tables/performance/tab_altPerf3.tex')
altSummary = alt_inv_m[!(is.na(av_return)|is.na(sv_return)), .(Start = paste(head(year,1),"-",head(month,1)), N = length(.I)), by = "index"]

#### borrowing quartiles ####
ff_data[, top4 := RF_lag >= .0042908]
ff_data[, bottom4 := RF_lag <= .0003]
borrowing_index = subset(ff_data,subset = ff_data$date%in%(spans[[1]]+months(1)),select = c("date","top4","bottom4"))
rtsdt = t(returns_dt[,"053","NO",])
rtsdt = as.data.table(rtsdt,keep.rownames=TRUE)
setnames(rtsdt,c("date","AV","SV"))
rtsdt[, date := as.Date(date,format="%Y-%m-%d")]
rtsdt = merge(rtsdt,borrowing_index,by="date")
perf_dt2 = array(dim = c(1,2,3,7,1),dimnames = list(Target = "053",Sample = c("top4","bottom4"),Strategy = c("BH","SV","AV"),
                                                    Measure = c("Return","Sharpe","Sortino","Kappa$_{3}$",
                                                                "Kappa$_{4}$","alpha$_{FF3}$","alpha$_{FF3+Mom}$"),
                                                    Constraint = "NO"))
set.seed(123)
for(s in 1:length(c("top4","bottom4"))){
  sub = c("top4","bottom4")[s]
  fq = 12
  spdt = subset(rtsdt,subset = get(sub)==TRUE,select = c("date",eval(sub)))
  r0 = c(m_bh_returns[m_bh_dates%fin% spdt$date])
  f3 = merge(subset(ff_data,select = c("date","SMB","HML","Mkt_RF")),subset(spdt,select = "date"),by="date")
  f3[, date := NULL]
  f3 = as.matrix(f3)
  f4 = merge(subset(ff_data,select = c("date","SMB","HML","Mkt_RF","Mom")),subset(spdt,select = "date"),by="date")
  f4[, date := NULL]
  f4 = as.matrix(f4)
  c = "053"
  l= "NO"
  sp_logical = unlist(rtsdt[, eval(sub), with = FALSE])
  r1 = returns_dt["sv",c,l,sp_logical]
  r2 = returns_dt["av",c,l,sp_logical]
  var1 = VAR(y = data.table(r1,r2),p = 1,type = "none", season = fq)
  return_resid = data.table(r1 = var1$varresult$r1$residuals,r2 = var1$varresult$r2$residuals)
  rr1 = return_resid$r1
  rr2 = return_resid$r2
  a3r1 = alpha(r1,f3)
  a3r2 = alpha(r2,f3)
  a4r1 = alpha(r1,f4)
  a4r2 = alpha(r2,f4)
  perf_dt2[c,s,"BH",,l]  = c(as.character(round(mean(r0)*fq*100,3)),
                            as.character(round((mean(r0)/sd(r0))* sqrt(fq),3)),
                            as.character(round(sortinoR(r0,annualize = TRUE,freq = fq),3)),
                            as.character(round(Kappa03(r0),3)),
                            as.character(round(Kappa04(r0),3)),
                            NA_real_,
                            NA_real_
                            #as.character(round(alpha(r0,f5)*fq*100,3)),
                            #as.character(round(alpha(r0,f6)*fq*100,3))
                            # as.character(round(genRachev.ratio(r0),3))
  )
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
             as.character(round(a3r1[[1]]*fq*100,3)),
             hac_t.test2(a3r2[[1]],a3r1[[1]],a3r2[[2]],a3r1[[2]],alternative = "less",
                         paired = TRUE,var.equal = TRUE)$p.val,
             as.character(round(a4r1[[1]]*fq*100,3)),
             hac_t.test2(a4r2[[1]],a4r1[[1]],a4r2[[2]],a4r1[[2]],alternative = "less",
                         paired = TRUE,var.equal = TRUE)$p.val
             # as.character(round(genRachev.ratio(r1),3)),
             # ratio.test2(rets1 = rr1,rets2 = rr2,ratio = "genRachev.ratio")
  )
  svp_dt[is.na(svp_dt)] = 0
  perf_dt2[c,s,"SV",,l] = see.stars(matrix(as.numeric(svp_dt),nrow=1),seq(1,13,by=2),seq(2,14,by=2))[seq(1,13,by=2)]
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
             # as.character(round(genRachev.ratio(r2),3)),
             # ratio.test2(rets1 = rr2,rets2 = rr1,ratio = "genRachev.ratio")
             as.character(round(a3r2[[1]]*fq*100,3)),
             hac_t.test2(a3r1[[1]],a3r2[[1]],a3r1[[2]],a3r2[[2]],alternative = "less",
                         paired = TRUE,var.equal = TRUE)$p.val,
             as.character(round(a4r2[[1]]*fq*100,3)),
             hac_t.test2(a4r1[[1]],a4r2[[1]],a4r1[[2]],a4r2[[2]],alternative = "less",
                         paired = TRUE,var.equal = TRUE)$p.val
  )
  avp_dt[is.na(avp_dt)] = 0
  perf_dt2[c,s,"AV",,l] = see.stars(matrix(as.numeric(avp_dt),nrow=1),seq(1,13,by=2),seq(2,14,by=2))[seq(1,13,by=2)]
}
out2 = rbind(perf_dt2["053",1,,,"NO"],perf_dt2["053",2,,,"NO"])
ltex2 = stargazer(out2,rownames = FALSE,summary = FALSE,out.header = FALSE)
cat(x = ltex2,sep = '\n',file = paste0("tables/performance/borrowing_performance.tex"))

#### stochastic dominance ####
kd_dt = data.table(AV = adj_m_av_returns, SV = adj_m_vol_returns)
attach(kd_dt)
layout(matrix(1:4, nrow = 2, ncol = 2))
dAV=density(AV)
n=length(dAV$x)
tikz("figures/kd_plots.tex",width = 5.90551, height = 3,sanitize = FALSE)
plot(dAV, main="Kernel smooth density for AV")
dmkt=density(SV)
plot(dmkt, main="Kernel smooth density for SV")
cdfAV=diffinv(dAV$y)
cdfmkt=diffinv(dmkt$y)
plot(dAV$x,cdfAV[2:(n+1)], typ="l",
     main="Cumulative density for AV",xlab="x", ylab="Cumulative density")
plot(dmkt$x,cdfmkt[2:(n+1)], typ="l",
     main="Cumulative density for SV",xlab="x", ylab="Cumulative density")
dev.off()
layout(matrix(1:1, nrow = 1, ncol = 1)) #back to normal plotting
detach(kd_dt)

ill_graph_dt = data.table(Wpa = NA_real_, p = seq(0,1,by=.001),
                          alpha=c(rep(0,length(seq(0,1,by=.001))),
                                  rep(.4,length(seq(0,1,by=.001))),
                                  rep(1,length(seq(0,1,by=.001)))))
ill_graph_dt[, Wpa := exp(-(-log(p))^alpha)]
ill_graph_dt[, alpha := as.factor(alpha)]
sd_ill_plot = ggplot(ill_graph_dt) + geom_line(aes(x=p,y=Wpa,color=alpha)) + theme_bw()
tikz("figures/sd_ill_plot.tex",width = 5.90551, height = 3,sanitize = FALSE)
plot(sd_ill_plot)
dev.off()
sd_table = round(compall(adj_m_av_returns,adj_m_vol_returns),3)
stargazer(sd_table,summary = FALSE,digits = 3,out = 'tables/performance/sd_utility.tex')

### other investment correlations ####
MD1 = rbind(subset(IntData,select = c("year","month","xlogret","Country")),
            subset(worldData[Country=="worldD"],select = c("year","month","xlogret","Country")))
MD2 = subset(alt_inv_m[index%in%c("bbdollar","reit","dbcurr","dbcarry","dbmom2","bcom","bbUSuniv","bbUSagg")],
             select = c("year","month","index","xlogret"))
setnames(MD2,"index","Country")
MD = rbind(MD1,MD2)
library(dplyr)
MD = MD %>%
  mutate(Country =  factor(Country, 
                           levels = c("aus","brazil","china","germany","france","india",
                                      "italy","japan","uk","usa","worldD","bbdollar","dbcurr",
                                      "dbcarry","dbmom2","reit","bcom","bbUSuniv","bbUSagg"))) %>%
  arrange(Country) 
MD3 = spread(data = MD,key = Country, value = xlogret) 
MD3 = as.data.table(MD3)
mmat = matrix(NA_real_,ncol(MD3)-2,ncol(MD3)-2)
mmat[] = 1
mmat[upper.tri(mmat)] = 0
cMD = cor(MD3[, -c("year","month"), with = FALSE], use = "pairwise.complete.obs")
cMD = round(cMD,3)
cMD = cMD * mmat
cMD[cMD == 0] = NA_real_
stargazer(cMD,summary = FALSE,out = 'tables/summary/other_invst_corr.tex')

#### mkt2gdp and int returns ####
pop_dt = fread(input = 'population.csv')
setnames(pop_dt,
         c("Country Code","AUS","BRA","CHN","DEU","FRA","GBR","IND","ITA",
           "JPN","USA")
         ,c("year","australia","brazil","china","germany","france","uk",
            "india","italy","japan","usa"))
pop_dt = melt(pop_dt,id.vars = "year",variable.name = "Country",value.name = "pop")
ratios = merge(ratios,pop_dt,by=c("year","Country"),all.x=TRUE)
ratios[, mkt2gdp2 := mkt2gdp * pop]
ratios[, count := length(unique(Country)), by = c("year")]
ratios[, top := NULL]
setorder(setDT(ratios), year, -mkt2gdp2)[, indx := seq_len(.N), by = year][indx <= count/2, top := 1]
# setorder(setDT(ratios), year, -mkt2gdp2)[, indx := seq_len(.N), by = year][indx >= count - 3, bottom := 1]
ratios[is.na(top), top := 0]
# ratios[is.na(bottom), bottom := 0]
ratios[Country=="uk"&year>=2012, top := 1]
ratios[Country=="brazil"&year>=2013, top := 0]
mxD = merge(IntData,ratios,by=c("year","Country"))
# mkt2gdp_port = mxD[year >= 2005, .(RET = mean(av_return*(top==1),na.rm = T) - mean(av_return*!(top==1),na.rm = T)), by = c("year","month")]
# mkt2gdp_retSharp = mkt2gdp_port[, .(ret = mean(RET)*1200, sharpe = mean(RET) / sd(RET) * sqrt(12))]

mxD_long = mxD[year >= 2005 & top==1, .(RET = mean(av_return,na.rm = T)), by = c("year","month")]
mkt2gdp_retSharp_long = mxD_long[, .(ret = mean(RET)*1200, sharpe = mean(RET) / sd(RET) * sqrt(12))]
mxD_short = mxD[year >= 2005 & top==0, .(RET = mean(av_return,na.rm = T)), by = c("year","month")]
mkt2gdp_retSharp_short = mxD_short[, .(ret = mean(RET)*1200, sharpe = mean(RET) / sd(RET) * sqrt(12))]

mkt2gdp_port = data.table(year = mxD_long$year, month = mxD_long$month ,RET = mxD_long$RET - mxD_short$RET)
mkt2gdp_retSharp = mkt2gdp_port[, .(ret = mean(RET)*1200, sharpe = mean(RET) / sd(RET) * sqrt(12))]

globalff = fread(input = 'Global_5_Factors.csv')
globalff[, year := as.integer(substr(V1,1,4))]
globalff[, month := as.integer(substr(V1,5,6))]
globalff[, c("SMB","HML","RMW","CMA","Mkt","RF_lag","Mom") := lapply(.SD/100,log1p), 
         .SDcols = c("SMB","HML","RMW","CMA","Mkt","RF_lag","Mom")]
globalff = merge(globalff,subset(worldData[Country=="worldD"],
                                 select = c("year","month","xlogret")),all.x=TRUE)
setnames(globalff,"xlogret","Mkt_RF2")
globalff[, Mkt_RF := Mkt - RF_lag]

a3s = alpha2(mxD_long$RET,as.matrix(globalff[, c("SMB","HML","Mkt_RF2")]))
a5s = alpha2(mxD_long$RET,as.matrix(globalff[, c("SMB","HML","Mkt_RF2","RMW","CMA")]))
a6s = alpha2(mxD_long$RET,as.matrix(globalff[, c("SMB","HML","Mkt_RF2","RMW","CMA","Mom")]))
mkt2gdp_strategyLong = c(mkt2gdp_retSharp_long,a3s[1]*1200,a3s[2],a5s[1]*1200,a5s[2],a6s[1]*1200,a6s[2])
mkt2gdp_strategyLong = unlist(mkt2gdp_strategyLong)

a3s = alpha2(mxD_short$RET,as.matrix(globalff[, c("SMB","HML","Mkt_RF2")]))
a5s = alpha2(mxD_short$RET,as.matrix(globalff[, c("SMB","HML","Mkt_RF2","RMW","CMA")]))
a6s = alpha2(mxD_short$RET,as.matrix(globalff[, c("SMB","HML","Mkt_RF2","RMW","CMA","Mom")]))
mkt2gdp_strategyshort = c(mkt2gdp_retSharp_short,a3s[1]*1200,a3s[2],a5s[1]*1200,a5s[2],a6s[1]*1200,a6s[2])
mkt2gdp_strategyshort = unlist(mkt2gdp_strategyshort)

a3s = alpha2(mkt2gdp_port$RET,as.matrix(globalff[, c("SMB","HML","Mkt_RF2")]))
a5s = alpha2(mkt2gdp_port$RET,as.matrix(globalff[, c("SMB","HML","Mkt_RF2","RMW","CMA")]))
a6s = alpha2(mkt2gdp_port$RET,as.matrix(globalff[, c("SMB","HML","Mkt_RF2","RMW","CMA","Mom")]))
mkt2gdp_strategy = c(mkt2gdp_retSharp,a3s[1]*1200,a3s[2],a5s[1]*1200,a5s[2],a6s[1]*1200,a6s[2])
mkt2gdp_strategy = unlist(mkt2gdp_strategy)
mkt2gdp2_results = rbind(mkt2gdp_strategyLong,mkt2gdp_strategyshort,mkt2gdp_strategy)

ratios[, top := NULL]

wb_ranking = fread(input = 'worldbank_ranking.csv',header = TRUE)
wb_melt = melt(wb_ranking,id.vars = "Country",variable.name = "year",value.name = "wbTop")
wb_melt[, year := as.integer(as.character(year))]
ratios = merge(ratios,wb_melt,by=c("Country","year"))

# setorder(setDT(ratios), year, -mkt2gdp)[, indx := seq_len(.N), by = year][indx <= count/2, top := 1]
# setorder(setDT(ratios), year, -mkt2gdp2)[, indx := seq_len(.N), by = year][indx >= count - 3, bottom := 1]
# ratios[is.na(top), top := 0]
# ratios[is.na(bottom), bottom := 0]
# ratios[Country=="uk"&year>=2013, top := 1]
# ratios[Country=="india"&year>=2013, top := 0]
mxD = merge(IntData,ratios,by=c("year","Country"))

mxD_long = mxD[year >= 2005 & wbTop==1, .(RET = mean(av_return,na.rm = T)), by = c("year","month")]
mkt2gdp_retSharp_long = mxD_long[, .(ret = mean(RET)*1200, sharpe = mean(RET) / sd(RET) * sqrt(12))]
mxD_short = mxD[year >= 2005 & wbTop==0, .(RET = mean(av_return,na.rm = T)), by = c("year","month")]
mkt2gdp_retSharp_short = mxD_short[, .(ret = mean(RET)*1200, sharpe = mean(RET) / sd(RET) * sqrt(12))]

mkt2gdp_port = data.table(year = mxD_long$year, month = mxD_long$month ,RET = mxD_long$RET - mxD_short$RET)
mkt2gdp_retSharp = mkt2gdp_port[, .(ret = mean(RET)*1200, sharpe = mean(RET) / sd(RET) * sqrt(12))]


a3s = alpha2(mxD_long$RET,as.matrix(globalff[, c("SMB","HML","Mkt_RF2")]))
a5s = alpha2(mxD_long$RET,as.matrix(globalff[, c("SMB","HML","Mkt_RF2","RMW","CMA")]))
a6s = alpha2(mxD_long$RET,as.matrix(globalff[, c("SMB","HML","Mkt_RF2","RMW","CMA","Mom")]))
mkt2gdp_strategyLong = c(mkt2gdp_retSharp_long,a3s[1]*1200,a3s[2],a5s[1]*1200,a5s[2],a6s[1]*1200,a6s[2])
mkt2gdp_strategyLong = unlist(mkt2gdp_strategyLong)

a3s = alpha2(mxD_short$RET,as.matrix(globalff[, c("SMB","HML","Mkt_RF2")]))
a5s = alpha2(mxD_short$RET,as.matrix(globalff[, c("SMB","HML","Mkt_RF2","RMW","CMA")]))
a6s = alpha2(mxD_short$RET,as.matrix(globalff[, c("SMB","HML","Mkt_RF2","RMW","CMA","Mom")]))
mkt2gdp_strategyshort = c(mkt2gdp_retSharp_short,a3s[1]*1200,a3s[2],a5s[1]*1200,a5s[2],a6s[1]*1200,a6s[2])
mkt2gdp_strategyshort = unlist(mkt2gdp_strategyshort)

a3s = alpha2(mkt2gdp_port$RET,as.matrix(globalff[, c("SMB","HML","Mkt_RF2")]))
a5s = alpha2(mkt2gdp_port$RET,as.matrix(globalff[, c("SMB","HML","Mkt_RF2","RMW","CMA")]))
a6s = alpha2(mkt2gdp_port$RET,as.matrix(globalff[, c("SMB","HML","Mkt_RF2","RMW","CMA","Mom")]))
mkt2gdp_strategy = c(mkt2gdp_retSharp,a3s[1]*1200,a3s[2],a5s[1]*1200,a5s[2],a6s[1]*1200,a6s[2])
mkt2gdp_strategy = unlist(mkt2gdp_strategy)
mkt2gdp2_results2 = rbind(mkt2gdp_strategyLong,mkt2gdp_strategyshort,mkt2gdp_strategy)

cs_wealth = fread(input = 'credit_suisse_wealth.csv',header = TRUE)
cs_wealth = melt(cs_wealth,id.vars = "Country",variable.name = "year",value.name = "wealth")
cs_wealth[, year := as.integer(as.character(year))]

cs_wealth = merge(cs_wealth,mktvalue[quarter==1],by=c("year","Country"))
cs_wealth[, mkt2wealth := mktcap / wealth]
setkey(cs_wealth,Country,year)
cs_wealth[, retMcap := ROC(mktcap),by = "Country"]
cs_wealth[, retCSWealth := ROC(wealth),by = "Country"]
cs_wealth[, retCSWealth := retMcap / retCSWealth]

ratios = merge(ratios,subset(cs_wealth,select = c("year","Country","retCSWealth")),by=c("Country","year"))

setorder(setDT(ratios), year, -retCSWealth)[, indx := seq_len(.N), by = year][indx <= count/2, CStop2 := 1]
# setorder(setDT(ratios), year, -mkt2gdp2)[, indx := seq_len(.N), by = year][indx >= count - 3, bottom := 1]
ratios[is.na(CStop2), CStop2 := 0]
# ratios[is.na(bottom), bottom := 0]
# ratios[Country=="uk"&year>=2013, top := 1]
# ratios[Country=="india"&year>=2013, top := 0]
mxD = merge(IntData,ratios,by=c("year","Country"))

mxD_long = mxD[year >= 2005 & CStop2==1, .(RET = mean(av_return,na.rm = T)), by = c("year","month")]
mkt2gdp_retSharp_long = mxD_long[, .(ret = mean(RET)*1200, sharpe = mean(RET) / sd(RET) * sqrt(12))]
mxD_short = mxD[year >= 2005 & CStop2==0, .(RET = mean(av_return,na.rm = T)), by = c("year","month")]
mkt2gdp_retSharp_short = mxD_short[, .(ret = mean(RET)*1200, sharpe = mean(RET) / sd(RET) * sqrt(12))]

mkt2gdp_port = data.table(year = mxD_long$year, month = mxD_long$month ,RET = mxD_long$RET - mxD_short$RET)
mkt2gdp_retSharp = mkt2gdp_port[, .(ret = mean(RET)*1200, sharpe = mean(RET) / sd(RET) * sqrt(12))]


a3s = alpha2(mxD_long$RET,as.matrix(globalff[, c("SMB","HML","Mkt_RF")]))
a5s = alpha2(mxD_long$RET,as.matrix(globalff[, c("SMB","HML","Mkt_RF","RMW","CMA")]))
a6s = alpha2(mxD_long$RET,as.matrix(globalff[, c("SMB","HML","Mkt_RF","RMW","CMA","Mom")]))
mkt2gdp_strategyLong = c(mkt2gdp_retSharp_long,a3s[[1]][1]*1200,a3s[[1]][2],a5s[[1]][1]*1200,a5s[[1]][2],a6s[[1]][1]*1200,a6s[[1]][2])
mkt2gdp_strategyLong = unlist(mkt2gdp_strategyLong)

a3sS = alpha2(mxD_short$RET,as.matrix(globalff[, c("SMB","HML","Mkt_RF")]))
a5sS = alpha2(mxD_short$RET,as.matrix(globalff[, c("SMB","HML","Mkt_RF","RMW","CMA")]))
a6sS = alpha2(mxD_short$RET,as.matrix(globalff[, c("SMB","HML","Mkt_RF","RMW","CMA","Mom")]))
mkt2gdp_strategyshort = c(mkt2gdp_retSharp_short,a3sS[[1]][1]*1200,a3sS[[1]][2],a5sS[[1]][1]*1200,
                          a5sS[[1]][2],a6sS[[1]][1]*1200,a6sS[[1]][2])
mkt2gdp_strategyshort = unlist(mkt2gdp_strategyshort)

a3s = unlist(alpha3(mkt2gdp_strategyLong[3]/1200,mkt2gdp_strategyshort[3]/1200,a3s[[2]],a3sS[[2]]))
a5s = unlist(alpha3(mkt2gdp_strategyLong[5]/1200,mkt2gdp_strategyshort[5]/1200,a5s[[2]],a5sS[[2]]))
a6s = unlist(alpha3(mkt2gdp_strategyLong[7]/1200,mkt2gdp_strategyshort[7]/1200,a6s[[2]],a6sS[[2]]))
mkt2gdp_strategy = c(mkt2gdp_retSharp,a3s[1]*1200,a3s[3],a5s[1]*1200,a5s[3],a6s[1]*1200,a6s[3])
mkt2gdp_strategy = unlist(mkt2gdp_strategy)
mkt2gdp2_results2 = rbind(mkt2gdp_strategyLong,mkt2gdp_strategyshort,mkt2gdp_strategy)

worldBank_wealth = fread(input = 'wealthData2.csv',header = TRUE)
worldBank_melt = melt(worldBank_wealth,id.vars = "Country",variable.name = "year",value.name = "wealth")
worldBank_melt[, year := as.integer(as.character(year))]
years = seq(2005,2015)
simpledt = data.table(year = rep(years,each=10),Country=unique(worldBank_melt$Country))
wbWealth = merge(simpledt,worldBank_melt,by=c("year","Country"),all.x=TRUE)
library(imputeTS)
setkey(wbWealth,Country,year)
wbWealth[, wealth2 := imputeTS::na.kalman(wealth,model = "StructTS"),by="Country"]
# na.kalman(x, model = "StructTS", smooth = TRUE, nit = -1, ...)
wbWealth = merge(wbWealth,subset(mktvalue[quarter==1],select = c("year","Country","mktcap")),by=c("year","Country"))
wbWealth[, mkt2wbWealth := mktcap / wealth2]
setkey(wbWealth,Country,year)
wbWealth[, retMcap := ROC(mktcap),by = "Country"]
wbWealth[, retwbWealth := ROC(wealth2),by = "Country"]
wbWealth[, retWealth := ROC(wealth),by = "Country"]
wbWealth[, retRatioWB := retMcap / retwbWealth]


ratios = merge(ratios,subset(wbWealth,select = c("year","Country","retRatioWB")),by=c("Country","year"))
setorder(setDT(ratios), year, -retRatioWB)[, indx := seq_len(.N), by = year][indx <= count/2, WBtop := 1]
# setorder(setDT(ratios), year, -mkt2gdp2)[, indx := seq_len(.N), by = year][indx >= count - 3, bottom := 1]
ratios[is.na(WBtop2), WBtop := 0]
# ratios[is.na(bottom), bottom := 0]
# ratios[Country=="uk"&year>=2013, top := 1]
# ratios[Country=="india"&year>=2013, top := 0]
mxD = merge(IntData,ratios[year>=2006],by=c("year","Country"))

mxD_long = mxD[year >= 2005 & WBtop2==1, .(RET = mean(av_return,na.rm = T)), by = c("year","month")]
mkt2gdp_retSharp_long = mxD_long[, .(ret = mean(RET)*1200, sharpe = mean(RET) / sd(RET) * sqrt(12))]
mxD_short = mxD[year >= 2005 & WBtop2==0, .(RET = mean(av_return,na.rm = T)), by = c("year","month")]
mkt2gdp_retSharp_short = mxD_short[, .(ret = mean(RET)*1200, sharpe = mean(RET) / sd(RET) * sqrt(12))]

mkt2gdp_port = data.table(year = mxD_long$year, month = mxD_long$month ,RET = mxD_long$RET - mxD_short$RET)
mkt2gdp_retSharp = mkt2gdp_port[, .(ret = mean(RET)*1200, sharpe = mean(RET) / sd(RET) * sqrt(12))]


a3s = alpha2(mxD_long$RET,as.matrix(globalff[, c("SMB","HML","Mkt_RF2")]))
a5s = alpha2(mxD_long$RET,as.matrix(globalff[, c("SMB","HML","Mkt_RF2","RMW","CMA")]))
a6s = alpha2(mxD_long$RET,as.matrix(globalff[, c("SMB","HML","Mkt_RF2","RMW","CMA","Mom")]))
mkt2gdp_strategyLong = c(mkt2gdp_retSharp_long,a3s[1]*1200,a3s[2],a5s[1]*1200,a5s[2],a6s[1]*1200,a6s[2])
mkt2gdp_strategyLong = unlist(mkt2gdp_strategyLong)

a3s = alpha2(mxD_short$RET,as.matrix(globalff[, c("SMB","HML","Mkt_RF2")]))
a5s = alpha2(mxD_short$RET,as.matrix(globalff[, c("SMB","HML","Mkt_RF2","RMW","CMA")]))
a6s = alpha2(mxD_short$RET,as.matrix(globalff[, c("SMB","HML","Mkt_RF2","RMW","CMA","Mom")]))
mkt2gdp_strategyshort = c(mkt2gdp_retSharp_short,a3s[1]*1200,a3s[2],a5s[1]*1200,a5s[2],a6s[1]*1200,a6s[2])
mkt2gdp_strategyshort = unlist(mkt2gdp_strategyshort)

a3s = alpha2(mkt2gdp_port$RET,as.matrix(globalff[, c("SMB","HML","Mkt_RF2")]))
a5s = alpha2(mkt2gdp_port$RET,as.matrix(globalff[, c("SMB","HML","Mkt_RF2","RMW","CMA")]))
a6s = alpha2(mkt2gdp_port$RET,as.matrix(globalff[, c("SMB","HML","Mkt_RF2","RMW","CMA","Mom")]))
mkt2gdp_strategy = c(mkt2gdp_retSharp,a3s[1]*1200,a3s[2],a5s[1]*1200,a5s[2],a6s[1]*1200,a6s[2])
mkt2gdp_strategy = unlist(mkt2gdp_strategy)
mkt2gdp2_results3 = rbind(mkt2gdp_strategyLong,mkt2gdp_strategyshort,mkt2gdp_strategy)

#### panel var ####
library(panelvar)
japan_returns = NULL
many_daily_returns = NULL
mm_cap = NULL
gc()
IntData[, date := paste0(year,month)]
IntData[, Country := as.factor(Country)]
IntData[, date := as.factor(date)]
mxD[, date := paste0(year,month)]
mxD[, Country := as.factor(Country)]
mxD[, date := as.factor(date)]
pvar_dt = mxD[year>=2005,c("Country","date","xlogret","dp","dg","avar1m","acorr1m","var1m","mkt2gdp")]
pvar_dt[, dp := log1p(dp)]
pvar_dt[, dg := log1p(dg)]
panlvar1 = pvargmm(dependent_vars = c("avar1m","acorr1m","dp","dg"),lags = 1, 
                   #exog_vars = "mkt2gdp",
                   #predet_vars = "var1m",
        data = pvar_dt,panel_identifier = c("Country","date"),
        transformation = "fod", 
        system_instruments = TRUE, system_constant = FALSE,
        progressbar = TRUE, steps = "mstep",pca_instruments = TRUE,
        collapse = TRUE)

pvargmm(dependent_vars = c("dp","dg","avar1m","acorr1m","var1m"),lags = 1, 
        #exog_vars = "mkt2gdp",predet_vars = "var1m",
        data = pvar_dt,panel_identifier = c("Country","date"),
        progressbar = TRUE, steps = "twostep",pca_instruments = TRUE,
        collapse = TRUE)

