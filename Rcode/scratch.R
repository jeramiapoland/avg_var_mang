perf_dt3 = array(dim = c(length(sufx),length(spans),3,5,3),dimnames = list(Target = sufx,Sample = names(spans),
                                                                          Strategy = c("BH","SV","AV"),
                                                                          Measure = c("Return","Sharpe","Sortino","Kappa$_{3}$",
                                                                                      "Kappa$_{4}$"),
                                                                          Constraint = c("1.5","3","NO")))

tar_sds = c(.029,.035,sd(m_bh_returns))
m_data[,date := as.Date(paste0(year,"-",month,"-","28"))]
for(s in 1:length(spans)){
  sp = spans[[s]]
  fq = 12
  # sp1 = sp
  tmpsv_weights = (1/m_data[date%fin%sp]$mkt_var1m)
  tmpsv_returns = tmpsv_weights * m_bh_returns[m_bh_dates%fin% sp]
  sv_sd = sd(tmpsv_returns)
  m_av_weights = (1/m_data[date%fin%sp]$avg_var1m)
  m_av_returns = m_av_weights * m_bh_returns[m_bh_dates%fin% sp]
  # av_sd = sd(m_av_returns)
  # for(s in sufx){
  #   returns_dt["av",s,"NO",] = get(paste0("av_",s,"_returns"))
  #   returns_dt["sv",s,"NO",] = get(paste0("sv_",s,"_returns"))
  # }
  # for(p in c("av","sv")){
  #   for(s in sufx){
  #     w = get(paste0(p,"_",s,"_weights"))
  #     for(u in upper_lev){
  #       tw = replace(w, w>=u, u)
  #       returns_dt[p,s,as.character(u),] = tw * m_bh_returns
  #     }
  #   }
  # }
  r0 = c(m_bh_returns[m_bh_dates%fin% sp])
  # sp_logical = as.Date(dimnames(returns_dt)[[4]]) %fin% sp
  for(c in sufx){
    c_tarSV = (as.numeric(c)/1000) / sv_sd
    c_tarAV = (as.numeric(c)/1000) / av_sd
    for(l in c("1.5","3","NO")){
      # r1 = returns_dt["sv",c,l,sp_logical]
      # r2 = returns_dt["av",c,l,sp_logical]
      if(l=="NO"){
        r1 = tmpsv_returns * c_tarSV
        r2 = m_av_returns * c_tarAV
      } else{
        tmpsv_weights = replace(tmpsv_weights, tmpsv_weights>=as.numeric(l), as.numeric(l))
        r1 = tmpsv_weights * m_bh_returns * c_tarSV
        m_av_weights = replace(m_av_weights, m_av_weights>=as.numeric(l), as.numeric(l))
        r2 = m_av_weights * m_bh_returns * c_tarAV
      }
      var1 = VAR(y = data.table(r1,r2),p = 1,type = "none", season = fq)
      return_resid = data.table(r1 = var1$varresult$r1$residuals,r2 = var1$varresult$r2$residuals)
      rr1 = return_resid$r1
      rr2 = return_resid$r2
      perf_dt3[c,s,"BH",,l]  = c(as.character(round(mean(r0)*fq*100,3)),
                                as.character(round((mean(r0)/sd(r0))* sqrt(fq),3)),
                                as.character(round(sortinoR(r0,annualize = TRUE,freq = fq),3)),
                                as.character(round(Kappa03(r0),3)),
                                as.character(round(Kappa04(r0),3))
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
                 ratio.test2(rets1 = rr1,rets2 = rr2,ratio = "Kappa04")
                 # as.character(round(genRachev.ratio(r1),3)),
                 # ratio.test2(rets1 = rr1,rets2 = rr2,ratio = "genRachev.ratio")
      )
      svp_dt[is.na(svp_dt)] = 0
      perf_dt3[c,s,"SV",,l] = see.stars(matrix(as.numeric(svp_dt),nrow=1),seq(1,9,by=2),seq(2,10,by=2))[seq(1,9,by=2)]
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
                 ratio.test2(rets1 = rr2,rets2 = rr1,ratio = "Kappa04")
                 # as.character(round(genRachev.ratio(r2),3)),
                 # ratio.test2(rets1 = rr2,rets2 = rr1,ratio = "genRachev.ratio")
      )
      avp_dt[is.na(avp_dt)] = 0
      perf_dt3[c,s,"AV",,l] = see.stars(matrix(as.numeric(avp_dt),nrow=1),seq(1,9,by=2),seq(2,10,by=2))[seq(1,9,by=2)]
    }
  }
}