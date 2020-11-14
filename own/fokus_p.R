## Checking fokus_pilot_data

library(tidyverse)
library(data.table)
library(ggplot2)
getwd()

fokus_p <- readr::read_csv2("fokus_p_dbr_wide_2.csv")
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
fpl_sim <- data.frame(id = rep(1:10, each = 72) %>% as.character,
                     time_p = rep(1:72, 10),
                     group = rep(1:2, each = 360),
                     age = rnorm(720, mean = 9, sd = 1),
                     gender = rep(1:2, 360),
                     dbr_ae = (sample.int(10, 720, replace = T) - 1)) %>%
    mutate(group = factor(group))

summary(fpl_sim)  
head(fpl_sim)
# introducing some missing data

# as.data.frame(lapply(fpl_sim, function(cc) cc[ sample(c(TRUE, NA), prob = c(0.85, 0.15), size = length(cc), replace = TRUE) ]))
# can't get it to work for just one column, applies NA:s to all data...

head(fpl_sim)
summary(fpl_sim)
fpl_sim %>%
    ggplot(aes(x=time_p, y=dbr_ae, group = id, color = id)) + geom_line()
  