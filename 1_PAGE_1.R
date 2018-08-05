source('0_DATA_POLISHING.R', encoding="utf-8")


# -- PAGE 1 TOP -- #

# Total cap_mil
sum_cap_mil_last <-sum(sapply(KRX_DATA_BIO_bycode, function(x){
  x[1,][,c('cap_mil')]
}))/10000000
sum_cap_mil_last_text <- format(sum_cap_mil_last,big.mark=",",scientific=FALSE)

# Total vol_m
sum_vol_m_last <-sum(sapply(KRX_DATA_BIO_bycode, function(x){
  x[1,][,c('vol_m')]
}))/1000000
sum_vol_m_last_text <- format(sum_vol_m_last,big.mark=",",scientific=FALSE)

# Total vol_m / cap_mil
cap_vol <- (sum_vol_m_last/sum_cap_mil_last)*100
cap_vol <- round(cap_vol, digits = 2)
cap_vol_text <- paste0(cap_vol,'%')

# Total count
total_count <- length(KRX_DATA_BIO_bycode)


# -- PAGE 1 MID LEFT -- # 

# KOSPI, KOSDAQ NMLZTION
INDEX_ALL_NML<-data.frame(date=INDEX_ALL[["date"]])
INDEX_ALL_NML[["KOSDAQ"]] <- INDEX_ALL[["KOSDAQ"]]/INDEX_ALL[["KOSDAQ"]][[1]]
INDEX_ALL_NML[["KOSPI"]] <- INDEX_ALL[["KOSPI"]]/INDEX_ALL[["KOSPI"]][[1]]
INDEX_ALL_NML <- xts(INDEX_ALL_NML[,-1], order.by = INDEX_ALL_NML[,1])

# BIO Cap NMLZTION
KRX_DATA_BIO_cap_term <- data.frame(group_by(KRX_DATA_BIO, date) %>% summarise(sum(cap_mil))) %>% filter(date >= '2017-11-01' & date <= '2018-03-30')
KRX_DATA_BIO_cap_term <- rename(KRX_DATA_BIO_cap_term, KRX_BIO=sum.cap_mil.)
KRX_DATA_BIO_cap_term_NML <- mutate(KRX_DATA_BIO_cap_term, KRX_BIO.N = KRX_DATA_BIO_cap_term[['KRX_BIO']]/KRX_DATA_BIO_cap_term[['KRX_BIO']][[1]])[,c(1,3)]
KRX_DATA_BIO_cap_term <- xts(KRX_DATA_BIO_cap_term[,-1], order.by = KRX_DATA_BIO_cap_term[,1])
KRX_DATA_BIO_cap_term_NML <- xts(KRX_DATA_BIO_cap_term_NML[,-1], order.by = KRX_DATA_BIO_cap_term_NML[,1])
names(KRX_DATA_BIO_cap_term) <- 'KRX_BIO'
names(KRX_DATA_BIO_cap_term_NML) <- 'KRX_BIO.n'

# MERGE
MERGE.INDEX_BIO <- merge(INDEX_ALL_NML, KRX_DATA_BIO_cap_term_NML)-1

# dygraph(MERGE.INDEX_BIO) %>%
#   dyOptions(colors = RColorBrewer::brewer.pal(3, "BuGn")) %>%
#   dyOptions(fillGraph = TRUE, fillAlpha = 0.35) %>%
#   dyAxis("x", drawGrid = FALSE) %>%
#   dyAxis("y", label = "Cumulative Return (Point)", axisLineColor="white",drawGrid = FALSE, axisLabelFormatter = 'function(d){ return d.toFixed(2)  }') %>%
#   dySeries('KOSDAQ', label = "코스닥", strokeWidth = 1) %>%
#   dySeries('KOSPI', label = "코스피", strokeWidth = 1, drawPoints = FALSE, pointSize = 2) %>%
#   dySeries('KRX_BIO.n', label = "바이오", strokeWidth = 1)


# -- PAGE 1 MID RIGHT -- #

# biggest Cap_mil
cap_mil_lastday <- data.frame(group_by(KRX_DATA_BIO, name) %>% summarise(M.CAP = cap_mil[[1]], TRD.VOL = vol_m[[1]], TV_MK = vol_m[[1]]/cap_mil[[1]]))
cap_mil_lastday['M.CAP'] <- round(cap_mil_lastday['M.CAP'] / 1000000000, digits=0)
cap_mil_lastday['TRD.VOL'] <- round(cap_mil_lastday['TRD.VOL'] / 1000000, digits=0)
cap_mil_lastday['TV_MK'] <- round(cap_mil_lastday['TV_MK']*100, digits = 2)
cap_mil_lastday <- arrange(cap_mil_lastday, -M.CAP)
cap_mil_lastday['M.CAP'] <- format(cap_mil_lastday['M.CAP'], nsmall=0)
cap_mil_lastday['TRD.VOL'] <- format(cap_mil_lastday['TRD.VOL'], nsmall=0)
cap_mil_lastday <- rename(cap_mil_lastday, NAME=name)

# -- PAGE 1 BOTTOM LEFT -- #
top7_cap <- (KRX_DATA_BIO %>% group_by(name) %>% summarise(mean_cap = mean(cap_mil)) )
top7_cap <- arrange(top7_cap, -mean_cap)
top7_cap <- as.data.frame(top7_cap)
top7_cap <- top7_cap[[1]][1:7]

term_chart <- function(arg.){
  k_bio_term <- filter(KRX_DATA_BIO, date >= '2017-11-01' & date <= '2018-03-30')
  k <- split(filter(k_bio_term, name %in% top7_cap), filter(k_bio_term, name %in% top7_cap)[,"name"])
  k_xts <- lapply(k,function(x){
    xts(x[c('date',arg.)][,-1], order.by = x[c('date',arg.)][,1])
  })
  k_bind <- do.call(cbind,k_xts)
  return(k_bind)
}

# dygraph(term_chart('vol_m')/1000000) %>%
#   dyOptions(colors = RColorBrewer::brewer.pal(7, "GnBu")) %>%
#   dyOptions(fillGraph = TRUE, fillAlpha = 0.5) %>%
#   dyAxis("x", drawGrid = FALSE) %>%
#   dyAxis("y", axisLineColor="white",drawGrid = FALSE, axisLabelFormatter = 'function(d){ return d.toFixed(0)  }') %>%
#   dyLegend(width = 500)


# -- PAGE 1 BOTTOM RIGHT -- #

# Average Trading Volume Trend
KRX_BIO_Avr.VOL <- data.frame(group_by(KRX_DATA_BIO, date) %>% summarise(Avr.VOL = mean(vol_m)/1000000))
KRX_BIO_Avr.VOL['Avr.VOL'] <- round(KRX_BIO_Avr.VOL['Avr.VOL'], digits=0)
KRX_BIO_Avr.VOL <- xts(KRX_BIO_Avr.VOL[,-1], order.by = KRX_BIO_Avr.VOL[,1])
KRX_BIO_Avr.VOL_byWeek_title <- list()
KRX_BIO_Avr.VOL_byWeek_title <- unlist(as.Date(sapply(split(KRX_BIO_Avr.VOL, 'weeks'), function(x){
  index(x)[[1]]
})))
KRX_BIO_Avr.VOL_byWeek_mean <- list()
KRX_BIO_Avr.VOL_byWeek_mean <- unlist(sapply(split(KRX_BIO_Avr.VOL, 'weeks'), function(x){
  mean(x)
}))
KRX_BIO_Avr.VOL_byWeek <- data.frame(date = KRX_BIO_Avr.VOL_byWeek_title, Avr.VOL = KRX_BIO_Avr.VOL_byWeek_mean)

# Average Market Cap Trend 
KRX_BIO_M.CAP <- data.frame(group_by(KRX_DATA_BIO, date) %>% summarise(M.CAP = mean(cap_mil)/100000000))
KRX_BIO_M.CAP['M.CAP'] <- round(KRX_BIO_M.CAP['M.CAP'], digits=0)
KRX_BIO_M.CAP <- xts(KRX_BIO_M.CAP[,-1], order.by = KRX_BIO_M.CAP[,1])

KRX_BIO_M.CAP_byWeek_title <- list()
KRX_BIO_M.CAP_byWeek_title <- unlist(as.Date(sapply(split(KRX_BIO_M.CAP, 'weeks'), function(x){
  index(x)[[1]]
})))

KRX_BIO_M.CAP_byWeek_mean <- list()
KRX_BIO_M.CAP_byWeek_mean <- unlist(sapply(split(KRX_BIO_M.CAP, 'weeks'), function(x){
  mean(x)
}))

KRX_BIO_M.CAP_byWeek <- data.frame(date = KRX_BIO_M.CAP_byWeek_title, M.CAP = KRX_BIO_M.CAP_byWeek_mean)
KRX_BIO_Avr.VOL_M.CAP_byWeek <- left_join(KRX_BIO_Avr.VOL_byWeek, KRX_BIO_M.CAP_byWeek)

# ggplotly(
# ggplot(KRX_BIO_Avr.VOL_M.CAP_byWeek, aes(x=date, y=Avr.VOL)) +
#   geom_bar(stat="identity", fill="#1ab394", alpha=0.4) +
#   labs(title="Trading Volume(Aver.) Trend", 
#        subtitle="Average Trading Volume of BIO Sector in KRX MARKET", 
#        caption="Source: EBEST API & WEB DATA CRAWLING") +
#   theme(axis.title.x=element_blank(), axis.title.y=element_blank()) +
#   scale_x_date(date_labels = "%Y%m%d") +
#   geom_line(aes(x=date, y=M.CAP),stat="identity", size=1, linetype = "twodash", color="skyblue", position = 'jitter') +
#   scale_y_continuous(sec.axis = sec_axis(~.))
# )


# -- PAGE 1 BOTTOM RIGHT - 2 -- #

#ATR & WPR FUNCTION
KRX_DATA_BIO_ATR_WPR_bycode <- list()
for ( i in cap_mil_lastday[,'NAME'][1:10]){
  KRX_DATA_BIO_ATR_WPR_bycode[[ i ]] <- filter( KRX_DATA_BIO, name == i ) %>% arrange(date)
  KRX_DATA_BIO_ATR_WPR_bycode[[ i ]] <- mutate( KRX_DATA_BIO_ATR_WPR_bycode[[ i ]], ATR = ATR( KRX_DATA_BIO_ATR_WPR_bycode[[ i ]][,c("high","low","close")] , n=3)[,c('atr')])
  KRX_DATA_BIO_ATR_WPR_bycode[[ i ]] <- mutate( KRX_DATA_BIO_ATR_WPR_bycode[[ i ]], A.Ratio = (KRX_DATA_BIO_ATR_WPR_bycode[[ i ]][[ 'ATR' ]] / KRX_DATA_BIO_ATR_WPR_bycode[[ i ]][[ "close" ]]) )
  KRX_DATA_BIO_ATR_WPR_bycode[[ i ]] <- mutate( KRX_DATA_BIO_ATR_WPR_bycode[[ i ]], WPR = WPR( KRX_DATA_BIO_ATR_WPR_bycode[[ i ]][,c("high","low","close")] , n=5))
}

KRX_DATA_BIO_ATR_WPR_bycode_lastday <- do.call(rbind,lapply(KRX_DATA_BIO_ATR_WPR_bycode, function(x){
  x[nrow(x),]
}))
rownames(KRX_DATA_BIO_ATR_WPR_bycode_lastday) <- NULL
KRX_DATA_BIO_ATR_WPR_bycode_lastday <- KRX_DATA_BIO_ATR_WPR_bycode_lastday[c('name','close','sector','ATR','A.Ratio','WPR')]
KRX_DATA_BIO_ATR_WPR_bycode_lastday['ATR'] <- round(KRX_DATA_BIO_ATR_WPR_bycode_lastday['ATR'], digits=0)
KRX_DATA_BIO_ATR_WPR_bycode_lastday['A.Ratio'] <- sapply(round(KRX_DATA_BIO_ATR_WPR_bycode_lastday['A.Ratio']*100, digits=2), function(x){
  paste0(x,'%')
})
KRX_DATA_BIO_ATR_WPR_bycode_lastday['WPR'] <- round(KRX_DATA_BIO_ATR_WPR_bycode_lastday['WPR'], digits=2)
KRX_DATA_BIO_ATR_WPR_bycode_lastday <- KRX_DATA_BIO_ATR_WPR_bycode_lastday[,c(1,3,2,4,5,6)]

kable(KRX_DATA_BIO_ATR_WPR_bycode_lastday) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = T)


# dygraph(term_chart('close'))
# dygraph(term_chart('vol_m'))
# dygraph(term_chart('vol_s'))
# dygraph(term_chart('cap_mil'))

# 
# #STOCK PRICE CHART
# k <- KRX_DATA_BIO_bycode[[1]][,c(1:9)]
# k_xts <- xts(k[,-1], order.by = k[,1])[,1] 
# 
# KRX_DATA_BIO_bycode[1]


# # -- PAGE 1 BOTOOM RIGHT -- #
# 
# kable(dt) %>%
#   kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = T)
# 
# head(KRX_DATA_BIO)
# kable_styling(a, bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = T)
