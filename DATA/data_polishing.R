# library(readxl)
# library(tidyverse)
# library(reshape2)
# library(xts)
# library(dygraphs)
# library(plotly)
# library(quantmod)
# library(knitr)
# library(kableExtra)
# theme_set(theme_classic())
# 
# #OPTION
# options(crayon.enabled = FALSE)
# options(stringsAsFactors=FALSE)

# PLOSHING TICKERS IN MASTER 
MASTER <- read_excel('./DATA/RAW/MASTER.xlsx')
MASTER <- rename(MASTER, f_code = full_code)
MASTER <- data.frame(MASTER[,-1])

# PLOSHING TICKERS IN MASTER
NAVER_IDST <- read_excel('./DATA/RAW/NAVER_INDUSTRY.xlsx', sheet='STOCK')
NAVER_IDST <- data.frame(NAVER_IDST[-1])
NAVER_IDST <- rename(NAVER_IDST, s_code = X1)

# POLISHING INDEX
INDEX_ALL <- read.csv('./DATA/RAW/INDEX.csv')
colnames(INDEX_ALL) <- c('date', 'KOSDAQ', 'KOSPI')
INDEX_ALL[['date']] <- as.Date(INDEX_ALL[['date']])
INDEX_ALL <- arrange(INDEX_ALL, date)

# PLOSHING KRX_DATA
KRX_DATA_ALL <- read.csv('./DATA/RAW/KRX_DATA_ALL.csv')
KRX_DATA_ALL <- rename(KRX_DATA_ALL, f_code = ticker)
KRX_DATA_ALL <- left_join(KRX_DATA_ALL, MASTER, by = "f_code")
KRX_DATA_ALL <- rename(KRX_DATA_ALL, s_code = short_code)
KRX_DATA_ALL <- left_join(KRX_DATA_ALL, NAVER_IDST, by = "s_code")
KRX_DATA_ALL <- rename(KRX_DATA_ALL, sector = X2)
KRX_DATA_ALL <- rename(KRX_DATA_ALL, name = codeName)
KRX_DATA_ALL[,"X0"] <- NULL
KRX_DATA_ALL <- filter(KRX_DATA_ALL, vol_s!=0)
KRX_DATA_ALL <- rename(KRX_DATA_ALL, date = X)
KRX_DATA_ALL[,'date'] <- as.Date(KRX_DATA_ALL[,'date'])
KRX_DATA_ALL[,'cap_mil'] <- KRX_DATA_ALL[,'cap_mil'] * 1000000

# PICK BIO FROM KRX_DATA
KRX_DATA_BIO <- filter(KRX_DATA_ALL, sector == "PHM" | sector == "BIO")

# SPLIT BY NAME
KRX_DATA_BIO_bycode <- split(KRX_DATA_BIO, KRX_DATA_BIO[,'name'])



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

BIO_Market_CAP_and_KRX_chart <-  dygraph(MERGE.INDEX_BIO) %>%
  dyOptions(colors = RColorBrewer::brewer.pal(3, "BuGn")) %>%
  dyOptions(fillGraph = TRUE, fillAlpha = 0.35) %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dyAxis("y", label = "Cumulative Return (Point)", axisLineColor="white",drawGrid = FALSE, axisLabelFormatter = 'function(d){ return d.toFixed(2)  }') %>%
  dySeries('KOSDAQ', label = "KOSDAQ", strokeWidth = 1) %>%
  dySeries('KOSPI', label = "KOSPI", strokeWidth = 1, drawPoints = FALSE, pointSize = 2) %>%
  dySeries('KRX_BIO.n', label = "BIO", strokeWidth = 1)


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

kable(cap_mil_lastday[c(1:10),], align=c('l','c','c','c')) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = T)

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

page_1_bt_left <-  dygraph(term_chart('vol_m')/1000000) %>%
  dyOptions(colors = RColorBrewer::brewer.pal(7, "GnBu")) %>%
  dyOptions(fillGraph = TRUE, fillAlpha = 0.5) %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dyAxis("y", axisLineColor="white",drawGrid = FALSE, axisLabelFormatter = 'function(d){ return d.toFixed(0)  }') %>%
  dyLegend(width = 500)



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

Trading_Volume_and_Market_CAP <- ggplotly(
  ggplot(KRX_BIO_Avr.VOL_M.CAP_byWeek, aes(x=date, y=Avr.VOL)) +
    geom_bar(stat="identity", fill="#1ab394", alpha=0.4) +
    labs(title="Trading Volume(Aver.) & M.Cap Trend", 
         subtitle="Average Trading Volume of BIO Sector in KRX MARKET", 
         caption="Source: EBEST API & WEB DATA CRAWLING") +
    theme(axis.title.x=element_blank(), axis.title.y=element_blank()) +
    scale_x_date(date_labels = "%Y%m%d") +
    geom_line(aes(x=date, y=M.CAP),stat="identity", size=1, linetype = "twodash", color="skyblue", position = 'jitter') +
    scale_y_continuous(sec.axis = sec_axis(~.))
)

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

kable(KRX_DATA_BIO_ATR_WPR_bycode_lastday, align=c('l','l','c','c','c','c')) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = T)



# ------ Page2 ------ #

# Mar_CAP & Graph
page_2_m.c <- KRX_DATA_BIO_ATR_WPR_bycode[['celltrion']][c('date','cap_mil')]
page_2_m.c['cap_mil'] <- page_2_m.c['cap_mil']/1000000
page_2_m.c_xts <- xts(page_2_m.c[,-1], order.by = page_2_m.c[,1])
names(page_2_m.c_xts) <- 'M.CAP'

page_2_m.cap_chart <- dygraph(split(page_2_m.c_xts, 'months')[[5]][1:5]) %>%
  dyLegend(show = "never") %>%
  dyAxis("x", drawGrid = FALSE, axisLineColor='#f2f2f2') %>%
  dyAxis("y", drawGrid = FALSE, axisLineColor='#f2f2f2') %>%
  dySeries('M.CAP', strokeWidth = 1, drawPoints = TRUE, pointSize = 3)

#Last
page_2_m.c_last <- format(page_2_m.c[['cap_mil']][nrow(page_2_m.c)], big.mark=",", scientific=FALSE)

#MAX
page_2_m.c_max <- format(max(page_2_m.c[['cap_mil']]), big.mark=",", scientific=FALSE)

#ARV.
page_2_m.c_avr <- format(mean(page_2_m.c[['cap_mil']]), big.mark=",", scientific=FALSE)

#MIN
page_2_m.c_min <- format(min(page_2_m.c[['cap_mil']]), big.mark=",", scientific=FALSE)

# Trading Volumes & Graph

page_2_t.v <- KRX_DATA_BIO_ATR_WPR_bycode[['celltrion']][c('date','vol_m')]
page_2_t.v['vol_m'] <- page_2_t.v['vol_m']/1000000
page_2_t.v_xts <- xts(page_2_t.v[,-1], order.by = page_2_t.v[,1])
names(page_2_t.v_xts) <- 'T.VOL'

page_2_t.v_chart <- dygraph(split(page_2_t.v_xts, 'months')[[5]][1:5]) %>%
  dyLegend(show = "never") %>%
  dyAxis("x", drawGrid = FALSE, axisLineColor='#f2f2f2') %>%
  dyAxis("y", drawGrid = FALSE, axisLineColor='#f2f2f2') %>%
  dySeries('T.VOL', strokeWidth = 1, drawPoints = TRUE, pointSize = 3)

#Last
page_2_t.v_last <- format(page_2_t.v[['vol_m']][nrow(page_2_m.c)], big.mark=",", scientific=FALSE)

#MAX
page_2_t.v_max <- format(max(page_2_t.v[['vol_m']]), big.mark=",", scientific=FALSE)

#ARV.
page_2_t.v_avr <- format(mean(page_2_t.v[['vol_m']]), big.mark=",", scientific=FALSE)

#MIN
page_2_t.v_min <- format(min(page_2_t.v[['vol_m']]), big.mark=",", scientific=FALSE)


# ATR & WPR
page_2_ATR_WPR <- KRX_DATA_BIO_ATR_WPR_bycode[['celltrion']][c('ATR','WPR')]
page_2_ATR_WPR_top <- page_2_ATR_WPR[nrow(page_2_ATR_WPR),]

# page_2_ATR_WPR_kable <- kable(page_2_ATR_WPR_top, align=c('l','l')) %>%
#   kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = T) %>%
#   row_spec(0, background = "#f2f2f2") %>%
#   row_spec(1, background = "#f2f2f2")

page_2_ATR_WPR_max <- format(c(round(max(page_2_ATR_WPR[,'ATR'], na.rm = TRUE), digits=0), max(page_2_ATR_WPR[,'WPR'], na.rm = TRUE)), nsmall = 2)

page_2_ATR_WPR_avr <- format(c(round(mean(page_2_ATR_WPR[,'ATR'], na.rm = TRUE), digits=0), round(mean(page_2_ATR_WPR[,'WPR'], na.rm = TRUE),digits=2)), nsmall=2)

page_2_ATR_WPR_min <- format(c(round(min(page_2_ATR_WPR[,'ATR'], na.rm = TRUE), digits=0), round(min(page_2_ATR_WPR[,'WPR'], na.rm = TRUE), digits=0)), nsmall=2)

# Price & TV Chart
p_tv_chart <- KRX_DATA_BIO_ATR_WPR_bycode[['celltrion']][c('date','close','vol_m')]
p_tv_chart['vol_m'] <- p_tv_chart['vol_m']/1000000
p_tv_chart_xts <- xts(p_tv_chart[,-1], order.by = p_tv_chart[,1])

page_2_p_tv_chart <- dygraph(p_tv_chart_xts) %>%
  dyOptions(colors = RColorBrewer::brewer.pal(4, "GnBu")) %>%
  dyLegend(show = "never") %>%
  dyOptions(fillGraph = TRUE, fillAlpha = 0.3) %>%
  dyAxis("x", drawGrid = FALSE, axisLineColor='#f2f2f2') %>%
  dyAxis("y", drawGrid = FALSE, axisLineColor='#f2f2f2', axisLabelFormatter = 'function(d){ return d }') %>%
  dySeries('close', label = "CLOSE", strokeWidth = 1) %>%
  dySeries('vol_m', label = "VOL_M", strokeWidth = 1)

# TOTAL KABLE
Total_Kable <- KRX_DATA_BIO_ATR_WPR_bycode[['celltrion']]
Total_Kable <- Total_Kable[c('date','name','change','close','open','high','low','vol_s','ATR','A.Ratio','WPR','sector','marketName')]
Total_Kable['ATR'] <- round(Total_Kable['ATR'],digits=0)
Total_Kable['A.Ratio'] <- round(Total_Kable['A.Ratio'],digits=2)
Total_Kable['WPR'] <- round(Total_Kable['WPR'],digits=2)
Total_Kable <- arrange(Total_Kable, desc(date))
names(Total_Kable) <- c('DATE','NAME','CHANGE','CLOSE','OPEN','HIGH','LOW','VOL_S','ATR','A.RATIO','WPR','SECTOR','MARKET')
Total_Kable_render <- kable(Total_Kable[c(1:30),], align='c') %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = T)