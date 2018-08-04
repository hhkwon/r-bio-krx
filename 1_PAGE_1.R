source('0_DATA_POLISHING.R', encoding="utf-8")
head(KRX_DATA_BIO,4)
head(KRX_DATA_BIO_bycode,1)


# Total cap_mil

sum_cap_mil_last <-sum(sapply(KRX_DATA_BIO_bycode, function(x){
  x[1,][,c('cap_mil')]
}))/10000000
sum_cap_mil_last

# Total vol_m
sum_vol_m_last <-sum(sapply(KRX_DATA_BIO_bycode, function(x){
  x[1,][,c('vol_m')]
}))/1000000
sum_vol_m_last

# Total vol_m / cap_mil
cap_vol <- (sum_vol_m_last/sum_cap_mil_last)*100
cap_vol

# Total count
total_count <- length(KRX_DATA_BIO_bycode)
total_count


# biggest Cap_mil
top10_cap <- (KRX_DATA_BIO %>% group_by(name) %>% summarise(mean_cap = mean(cap_mil)) )
top10_cap <- arrange(top10_cap, -mean_cap)
top10_cap <- as.data.frame(top10_cap)
top10_cap <- top10_cap[[1]][1:10]

term_chart <- function(arg.){
  k <- split(filter(KRX_DATA_BIO, name %in% top10_cap), filter(KRX_DATA_BIO, name %in% top10_cap)[,"name"])
  k_xts <- lapply(k,function(x){
    xts(x[c('date',arg.)][,-1], order.by = x[c('date',arg.)][,1])
  })
  k_bind <- do.call(cbind,k_xts)
  return(k_bind)
}

dygraph(term_chart('close'))
dygraph(term_chart('vol_m'))
dygraph(term_chart('vol_s'))
dygraph(term_chart('cap_mil'))

#STOCK PRICE CHART
k <- KRX_DATA_BIO_bycode[[1]][,c(1:9)]
k_xts <- xts(k[,-1], order.by = k[,1])[,1] 

KRX_DATA_BIO_bycode[1]

#ATR FUNCTION

# ATR_ALL_by_ticker <- list()
# for ( i in KRX_DATA_ALL_ticker){
#   print(i)
#   ATR_ALL_by_ticker[[ i ]] <- filter( KRX_DATA_ALL, code == i ) %>% arrange(date)
#   ATR_ALL_by_ticker[[ i ]] <- mutate( ATR_ALL_by_ticker[[ i ]], ATR = ATR( ATR_ALL_by_ticker[[ i ]][,c("adj_high","adj_low","adj_close")] , n=3)[,c('atr')])
#   ATR_ALL_by_ticker[[ i ]] <- mutate( ATR_ALL_by_ticker[[ i ]], ATR_rate = (ATR_ALL_by_ticker[[ i ]][[ 'ATR' ]] / ATR_ALL_by_ticker[[ i ]][[ "adj_close" ]]) )
# }


#WPR FUNCTION