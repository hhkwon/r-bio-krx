# r-bio-krx
> Stock Analysis in KRX Bio Sector 

[![r-image]][r-url] [![python-image]][python-url] [![aws-image]][aws-url]

<img src="header.png" width="300"  width="250">


## What is this?

KRX에 상장된 제약, 바이오 주식들을 분석합니다.\
본 프로젝트(r-bio-krx) 목적은 r과 shiny를 이용한 데이터 시각화 활용입니다.

+ r-bio-krx : https://github.com/hyukhwankwon/r-bio-krx


## Language

> `R 3.5.0` `Python 3.6`


## DataSet

+ Raw Data
    + MASTER.csv ( _stock tickers and info_ )
    + INDEX.xlsx ( _kospi, kosdqa index price_ )
    + KRX\_DATA\_ALL.csv ( _all stock data in KRX market_ )
    + NAVER_INDUSTRY.xlsx ( _industry from NAVER stocks_ )

<!-- 2. Polished Data
    + MASTER
    + NAVER_IDST
    + KRX_DATA_ALL
    + KRX_DATA_BIO
    + KRX_DATA_BIO_bycode -->


## Process

1. Sourcing Raw Data
    + `python` ebest api
    + `Urllib` `BeautifulSoup` crawling 
2. Pre-processing
    + `dplyr` rename columns to 'f_code', 's_code', 'sector', 'name'
    + `dplyr` filter != 0 (zero omit), left_join
3. Modeling
    + group_by %>% summarise to summary all output data with grouping.
    + xts, do.call, cbinding with every single data before visualization.
4. Output Sharing
    + visualization: `ggplot2` `dygraph` `plotly`
    + sharing: `flexdashboard`, `shiny`


## Library

```r
R library <- c('readxl', 'tidyverse', 'reshape2', 'xts', 'dygraphs', 'plotly', 'quantmod')
```
```python
PYTHON import urllib import BeautifulSuop
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
