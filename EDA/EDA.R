library(tidyverse)
library(skimr)
raw <- read_csv("outbound_tourism_china.csv")
skim(raw)
raw$SERIES %>% table()
raw1 <- raw %>% filter(SERIES == "TFR") %>% 
  mutate(country = X1) %>% 
  select(-X1, -SERIES, -25) %>% 
  select(country, everything())

cnty <- as.list(raw1[,1])$country %>% as.list()

tidy_list <- function(country) {
  num <- match(country, cnty)
  dat <- data.frame(year = 1995:2016, tour_num = t(raw1[num, -1]))
  return(dat)
}
dat <- map(cnty, tidy_list)

mode <- function(x) {
  mod <-  lm(data = x, formula = tour_num ~ year)
  smy <- summary(mod)
  r <- smy[["r.squared"]]
  return(r)
}

r <- lapply(dat, mode) %>% unlist()
country <- cnty %>% unlist()

df <- data.frame(country = country, r.squared = r, stringsAsFactors = F) %>% 
  arrange(r.squared)

ggplot(df, aes(x = r.squared, y = country)) +
  geom_point() +
  labs(title = "tourism ~ year  R-squared", x = "r.squared", y = "country")

exg <- read_csv("exchange_rate.csv")

# growth <- function(x) {
#   min <- range(x, na.rm = T)[1]
#   max <- range(x, na.rm = T)[2]
#   grow <- (max - min) / min
# }
grow <- map_dbl(dat, growth)

df1 <- data.frame(country, grow) %>% 
  right_join(exg, by = c("country"))
mod2 <- lm(data = df1, grow ~ rate)

summary(mod2)

ggplot(df1, aes(x = rate, y = grow)) +
  geom_point()



