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
fokus_p_l <- fokus_p %>% gather(time, dbr, -c(id))

glimpse(fokus_p_l)

fokus_p_l <- fokus_p_l %>%
  mutate(dbr = na_if(dbr, 999)) %>%
  mutate(id = as.factor(id))

fokus_p_l %>%
  ggplot(aes(x=time, y=dbr, group = id, color = id)) + geom_line()
