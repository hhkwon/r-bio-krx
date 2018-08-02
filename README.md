# R_BIO_KRX
> Stock Analysis in KRX Bio Sector 

[![r-image]][r-url]
[![python-image]][python-url]
[![aws-image]][aws-url]

<img src="header.png" width="300"  width="250">


## What is this?

KRX에 상장된 제약, 바이오 주식들을 분석합니다.\
본 프로젝트(R_BIO_KRX) 목적은 r과 shiny를 이용한 데이터 시각화 활용입니다.

+ R_BIO_KRX : https://github.com/hyukhwankwon/R_BIO_KRX


## DataSet

1. Raw Data
    + MASTER : stock tickers and info
    + INDEX : kospi, kosdqa index price    
    + KRX_DATA_ALL : all stock data in KRX market
    + NAVER_INDUSTRY : industry from NAVER stocks

2. Polished Data
    + MASTER
    + NAVER_IDST
    + KRX_DATA_ALL
    + KRX_DATA_BIO
    + KRX_DATA_BIO_bycode


## Library

```r
library(readxl)
library(tidyverse)
library(reshape2)
library(xts)
library(dygraphs)
library(plotly)
library(quantmod)
```

## Release History

* 0.0.1
    * First Commit (2018/08/02)



<!-- Markdown link & img dfn's -->
[r-image]: https://img.shields.io/badge/R-3.5.0-brightgreen.svg?style=flat-square
[r-url]: https://cran.r-project.org/bin/windows/base/
[python-image]: https://img.shields.io/badge/python-3.6-yellow.svg?style=flat-square
[python-url]: https://www.python.org/
[aws-image]: https://img.shields.io/badge/aws-pass-orange.svg?style=flat-square
[aws-url]: https://aws.amazon.com/