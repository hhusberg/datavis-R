## Checking fokus_pilot_data

library(tidyverse)
library(data.table)
library(ggplot2)
getwd()

fokus_p <- readr::read_csv2("fokus_p_dbr_wide.csv")
fokus_p <- fokus_p %>%
  mutate(ae0 = na_if(ae0, 999)) %>%
  mutate(ae3 = na_if(ae3, 999)) %>%
  mutate(ae7 = na_if(ae7, 999)) %>%
  mutate(ae9 = na_if(ae9, 999)) %>%
  mutate(ae10 = na_if(ae10, 999)) %>%
  mutate(ae11 = na_if(ae11, 999)) %>%
  mutate(ae15 = na_if(ae15, 999)) %>%
  mutate(ae16 = na_if(ae16, 999)) %>%
  mutate(ae17 = na_if(ae17, 999))

str(fokus_p)
glimpse(fokus_p)
fokus_p %>%
  ggplot(aes(x = ))

library(tidyr)
fokus_p_l <- fokus_p %>% gather(time, dbr, -c(id, gender, age, group))

glimpse(fokus_p_l)

fokus_p_l <- fokus_p_l %>%
  mutate(dbr = na_if(dbr, 999)) %>%
  mutate(id = as.factor(id))

fokus_p_l %>%
  ggplot(aes(x=time, y=dbr, group = id, color = id)) + geom_line()

summary(fokus_p_l)

# simulating data

  set.seed(1)
  df_fuck <- data.frame(id = rep(1:10, each = 72) %>% as.character,
                     time_p = rep(1:72, 10),
                     group = rep(1:2, each = 360),
                     age = rnorm(720, mean = 9, sd = 1),
                     gender = rep(1:2, 360),
                     dbr_ae = (sample.int(10, 720, replace = T) - 1)) %>%
    mutate(group = factor(group))
  