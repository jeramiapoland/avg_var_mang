paks <- c("RCurl","data.table","tis","lubridate","ggplot2","stringr","sandwich","stargazer","pracma","RColorBrewer",
          "CADFtest","complexplus","readxl","reshape2","quantmod","xlsx","tikzDevice","MASS","timeSeries","vars","PortfolioEffectHFT",
          "PortfolioAnalytics","PerformanceAnalytics","backtest","tidyr","broom","stringdist","BH","parallel","doMC","foreach",
          "doParallel","lmtest","hypergeo","strucchange","formula.tools","multiwave","outliers","forecast","SharpeR","fastmatch",
          "bvarsv","boot","goftest","DescTools","dunn.test","generalCorr","cointReg","psd","plot3D","rootSolve") 
# note: tikzDevice requires a working latex installation
# and xlsx require rJava so a properly configured java (try javareconf)
for (p in paks){
  require(p,character.only = TRUE) || {install.packages(p) 
    require(p,character.only = TRUE)}
}
source(file = 'functions.R')

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
    for(l in c("1.5","3","NO")[2:3]){
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
