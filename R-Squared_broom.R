# 加载包
library(tidyverse)
library(skimr)
library(broom)

# 读入数据
raw <- read_csv("outbound_tourism_china.csv")

# 查看数据概况
skim(raw)

#查看各种统计方式包含的国家数
raw$SERIES %>% table()

# 选择TFR统计方式，筛选出TFR统计方式对应的国家
raw1 <- raw %>% filter(SERIES == "TFR") %>% 
  # 增加国家列
  mutate(country = X1) %>% 
  # 删除SERIES列，change2016-2015列
  select(-X1, -SERIES, -25) %>% 
  # 将国家放在第一列
  select(country, everything())

# 生成国家列表
cnty <- as.list(raw1[,1])$country %>% as.list()

#将数据清洗为列表函数
tidy_list <- function(country) {
  num <- match(country, cnty)
  dat <- data.frame(year = 1995:2016, tour_num = t(raw1[num, -1]))
  return(dat)
}

# 将数据清洗为包含52个国家信息的列表
dat <- map(cnty, tidy_list)

# 建立线性模型，并取出r.squared函数
mode <- function(x) {
  # 线性回归模型
  mod <-  lm(data = x, formula = tour_num ~ year)
  # 取出摘要中r.squared
  r <- glance(mod)$r.squared
  return(r)
}

# 输出r.squared，country
r <- lapply(dat, mode) %>% unlist()
country <- cnty %>% unlist()

# 输出包含country，r.squared的数据框并按r.squared排序
df <- data.frame(country, r.squared = r, stringsAsFactors = F) %>% 
  arrange(r.squared)

# 输出散点图
ggplot(df, aes(x = r.squared, y = reorder(r.squared, country))) +
  geom_point() +
  scale_y_discrete(breaks = reorder(df$r.squared, df$country),
                   labels = df$country) +
  labs(title = "tourism ~ year  R-squared", x = "r.squared", y = "country")


