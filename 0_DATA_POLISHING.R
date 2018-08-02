library(readxl)
library(tidyverse)
library(reshape2)
library(xts)
library(dygraphs)
library(plotly)
library(quantmod)

#OPTION
options(crayon.enabled = FALSE)
options(stringsAsFactors=FALSE)

# PLOSHING TICKERS IN MASTER 
MASTER <- read_excel('./DATA/RAW/MASTER.xlsx')
MASTER <- rename(MASTER, f_code = full_code)
MASTER <- data.frame(MASTER[,-1])

# PLOSHING TICKERS IN MASTER
NAVER_IDST <- read_excel('./DATA/RAW/NAVER_INDUSTRY.xlsx', sheet='STOCK')
NAVER_IDST <- data.frame(NAVER_IDST[-1])
NAVER_IDST <- rename(NAVER_IDST, s_code = X1)

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
str(KRX_DATA_ALL)

# PICK BIO FROM KRX_DATA
KRX_DATA_BIO <- filter(KRX_DATA_ALL, sector == "제약" | sector == "생물공학")
# SPLIT BY NAME
KRX_DATA_BIO_bycode <- split(KRX_DATA_BIO, KRX_DATA_BIO[,'name'])

head(KRX_DATA_BIO_bycode,1)
