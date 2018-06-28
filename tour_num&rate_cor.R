# 加载包
library(tidyverse)
library(skimr)
library(broom)

# 读取爬虫信息
source("exchange_rate.R")

# 读入数据
raw <- read_csv("outbound_tourism_china.csv")

# 选择TFR统计方式，筛选出TFR统计方式对应的国家
raw_cor <- raw %>% filter(SERIES == "TFR") %>% 
  # 增加国家列
  mutate(country = X1) %>% 
  # 删除SERIES列，change2016-2015列
  select(-X1, -SERIES, -25, -c(2:15)) %>% 
  # 将国家放在第一列
  select(country, everything())

# 生成国家列表
cnty <- as.list(raw_cor[,1])$country %>% as.list()

#将数据清洗为列表函数
tidy_list <- function(country) {
  num <- match(country, cnty)
  dat <- data.frame(year = 2008:2016, tour_num = t(raw_cor[num, -1]))
  dat
}

# 生成包含年份,游客人数,汇率列表
dat_cor <- map(cnty, tidy_list) %>% map2(rate_cor, bind_cols)

# 删除列表中包含缺失值的国家函数
rmv_na <- function(x) {
  if(is.na(x[1, 3])) {
    x <- NULL
  }
  if(sum(is.na(x[,2])) > 0){
    x <- NULL
  }
  x
}

# 删除包含缺失值的数据
dat_cor_cal <- dat_cor %>% map(rmv_na)

# 无缺失值国家名称
country <- tab_sym$country[!(dat_cor_cal %>% map(is.null) %>% unlist())]

# 生成数据完整国家年份,游客人数,汇率数据框
df_cor <- data.frame(country = country %>% rep(each = 9))%>%
  bind_cols(dat_cor_cal %>% bind_rows()) %>% 
  group_by(country) %>% 
  nest()

# 计算游客人数增长率函数
grow_tour <- function(x) {
  map2(x$tour_num[-9], x$tour_num[-1], growth)
}

# 计算汇率增长率函数
grow_rate <- function(x) {
  map2(x$exchange_rate[-9], x$exchange_rate[-1], growth)
}

# 生成游客人数,汇率增长率列表
growth_tour <- map(df_cor$data, grow_tour)
growth_rate <- map(df_cor$data, grow_rate)

# 生成游客数量增长率,汇率增长率数据框
df_grow_cor <- data.frame(
  country = country %>% rep(each = 8),
  grow_tour = growth_tour %>% unlist(),
  grow_rate = growth_rate %>% unlist()
) %>%
  group_by(country) %>%
  nest()

# 计算相关系数函数
cor_tour_rate <- function(x) {
  cor(x$grow_tour, x$grow_rate)
}

# 在数据框中增加相关系数列
df_grow_cor$cor <- map(df_grow_cor$data, cor_tour_rate) %>% unlist()
  

# grow_all <- function(x) {
# 
#   tour_grow <- map2(x$tour_num[-9], x$tour_num[-1], growth)
#   rate_grow <- map2(x$exchange_rate[-9], x$exchange_rate[-1], growth)
#   country = tab_sym$country[!(dat_cor_cal %>% map(is.null) %>% unlist())] %>% rep(each = 8) 
#   data.frame(
#     country = country,
#     grow_tour = growth_tour %>% unlist(),
#     grow_rate = growth_rate %>% unlist()
#   )
# } 


