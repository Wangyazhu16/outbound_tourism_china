library(tidyverse)
library(skimr)
library(broom)
library(rvest)
library(xml2)
library(stringr)

# 读取货币代码表
cur_sym <- read_csv("currency_symbol.csv")

# 读取国家名称中英文对照表
country_eng_chn <- read_csv("country_eng_chn.csv")

# 生成英文国家名称与货币代码对应表
tab_ref <- cur_sym %>% 
  select(country_chn, symbol_std) %>% 
  left_join(country_eng_chn, by = c("country_chn")) %>% 
  select(country_eng, symbol_std)

# 生成现有国家名称与货币代码对应表
tab_sym <- raw1 %>% 
  left_join(tab_ref, by = c("country" = "country_eng")) %>% 
  select(country, symbol_std)

# 补全对应表缺失值
tab_sym[is.na(tab_sym$symbol_std),2] <- c(
  "AOA", "XCD", "AMD", "BSD", "BWP", "KYD", "NZD", "DOP", "ETB", "XPF", "USD",
  "HKD", "ILS", "XAF", "USD", "USD", "XPF", "NZD", "EUR", "SLL", "USD", "AAA",
  "UAH", "USD", "VUV"
)

# 爬虫：网址函数
url <- function(symbol, year) {
    url <- paste0("https://www.x-rates.com/average/?from=CNY&to=",
                  symbol,
                  "&amount=100&year=",
                  year)
}

# 爬虫：解析函数
web <- function(url) {
  download.file(url, destfile = "scrapedpage.html", quiet=TRUE)
  web <- read_html("scrapedpage.html", encoding = "UTF-8")
  rate <- web %>% 
    html_nodes(".avgRate") %>%
    html_text() %>% 
    str_trim()
  rate[6]
}

# 爬虫年份
year <- 2008:2016

# 生成爬虫网址列表
urls <- tab_sym$symbol_std %>% map(url, year)

# 解析网址爬取汇率
rate <- urls %>% map(~ map(., web))

# 给列表元素添加国家名称
names(rate) <- tab_sym$country

# 将列表元素转化为数据框
rate_cor <- rate %>% map(as.numeric) %>% map(as.data.frame)

# 数据框汇率列命名函数
chg_name <- function(x) {
  colnames(x) <- "exchange_rate"
  x
}

# 为数据框汇率列命名
rate_cor <- map(rate_cor, chg_name)







