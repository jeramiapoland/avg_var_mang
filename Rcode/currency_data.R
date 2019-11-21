# currency data
##currency
forex <- fread(input = '../../sovereign_cds/CUR_20170322.csv',header = F)
names(forex) <- c("code","Dates","ex.rate")
forex[, Dates := as.Date(Dates,format="%Y-%m-%d")]
cur_codes <- fread(input = '../../sovereign_cds/cur_codes.csv',header = F)
names(cur_codes) <- c("code","country")
eur_countries <- fread(input = '../../sovereign_cds/euro_countries.csv', header = F)
names(eur_countries) <- c("country","code","ex.rate","start","settle")
cur_mut <- cur_codes$country
names(cur_mut) <- cur_codes$code
x_cur_mut <- cur_codes$code
names(x_cur_mut) <- cur_codes$country

# a skeleton on which to build the currency returns data
currencies = c("USD","EUR","GBP","JPY","AUD","CAD","CHF","CHY","MXN","SEK","NZD","HKD","SGD")
# cur_dt <- data.table()
# 
# cur_dt <- merge(cur_dt,forex,by.x=c("currency.code","Dates"),by.y = c("code","Dates"),all.x = T)
# 
# euro_sub = fread(input = '../../sovereign_cds/euro_join.csv')
# euro_sub[, euro_start := as.Date(euro_start,format="%Y-%m-%d")]
# 
# for (r in 1:nrow(euro_sub)){
#   cur_dt[(country == euro_sub$country[r]) & (Dates <= euro_sub$start[r]), currency.code := euro_sub$code[r]]
# }
# more_rates <- fread(input = '../../sovereign_cds/lvl_daily.csv',header = T)
# names(more_rates) <- c("Dates","ex.rate")
# more_rates[, currency := "LVL"]
# more_rates2 <- fread(input = '../../sovereign_cds/eek_daily.csv',header = T)
# names(more_rates2) <- c("Dates","ex.rate")
# more_rates2[, currency := "EEK"]
# more_rates3 <- fread(input = '../../sovereign_cds/ltl_daily.csv',header = T)
# setnames(more_rates3,c("Dates","ex.rate"))
# more_rates3[, currency := "LTL"]
# 
# more_rates4 <- rbindlist(list(more_rates,more_rates2,more_rates3))
# more_rates4[, Dates := as.Date(Dates,format="%b-%d-%Y")]
# 
# for (r in 1:nrow(more_rates4)){
#   cur_dt[(currency.code == more_rates4$currency[r]) & (Dates == more_rates4$Dates[r]), ex.rate := more_rates4$ex.rate[r]]
# }
# 
# cur_dt <- merge(cur_dt,subset(forex,subset = (code %in% c("SKK","SIT"))),
#                 by.x = c("currency.code","Dates"),by.y = c("code","Dates"),
#                 suffixes = c("",".other"),all.x = T)
# cur_dt[!is.na(ex.rate.other), ex.rate := ex.rate.other]
# cur_dt$ex.rate.other <- NULL

currency_forwards <- fread(input = '../../sovereign_cds/forward_rates_mod.csv')
currency_forwards[, Dates := as.Date(DATE,format="%m/%d/%Y")]
currency_forwards$DATE <- NULL
currency_forwards <- gather(currency_forwards,currency.code,cur.fwd.1w,-Dates)
currency_forwards <- as.data.table(currency_forwards)
currency_forwards[, currency.code := as.factor(currency.code)]
# cur_dt <- merge(cur_dt,currency_forwards,by=c("Dates","currency.code"),all.x = T)
# setkey(cur_dt,country,Dates)


# usd_future <- subset(futures_dt,subset = (Symbol == "DX1" & method == "OR"),select = c("Date","Settle"))
# usd_future[, currency := "USD"]
# usd_future[, Date := as.Date(Date,format="")]
# cur_dt <- merge(cur_dt,usd_future,by.x = c("currency.code","Dates"),by.y = c("currency","Date"),all.x = T)
# cur_dt[(currency.code=="USD")&(is.na(ex.rate)),cur.fwd.1w := Settle]
# cur_dt$Settle <- NULL

# usd_spot <- fread(input = '../../sovereign_cds/usd_spot.csv')
# usd_spot[,Date := as.Date(Date,format="%b %d, %Y")]
# usd_spot <- subset(usd_spot,select = c("Date","Price"))
# usd_spot[, currency.code := "USD"]
# setnames(usd_spot,"Price","ex.rate")
# cur_dt <- merge(cur_dt,usd_spot,by.y=c("Date","currency.code"),
#                 by.x=c("Dates","currency.code"),all.x = T)
# cur_dt[currency.code == "USD", ex.rate := usd.rate]
# cur_dt$usd.rate <- NULL
# setkey(cur_dt,country,Dates)

# invert exchange rate to use Corte Riddiough and Sarno excess return calculation
forex[, ex.rate := 1/ex.rate]
forex[, curr_ret := ROC(ex.rate,type = "discrete"), by = c("code")]
forex[, c("year","month"):= list(year(Dates),month(Dates))]
cvar_dt = forex[code %in% c("AUD","CAD","CHF","EUR","GBP","HKD","MXN","NZD","JPY","SEK"), .(cvar = var(curr_ret)), by = c("code","year","month")]
c_weights = c("AUD" = .052, "CAD" = .043, "CHF" = .036, "EUR" = .231, "GBP" = .093, "HKD" = .01, "MXN" = .018, "NZD" = .01, "JPY" = .178,
              "SEK" = .01)
c_weights = c_weights / sum(c_weights)
cvar_dt[, weight := c_weights[code]]
cvar_month = cvar_dt[!is.na(cvar), .(avg_cvar = mean(cvar), wavg_cvar = sum(cvar*weight)), by = c("year","month")]
