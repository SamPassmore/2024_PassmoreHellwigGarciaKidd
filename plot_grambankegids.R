## This script plots the difference between languages

library(ggplot2)
library(dplyr)
library(tidyr)


gb_egids = read.csv("processed_data/merged_dataset.csv")

# make the EGIDS scale a factor
gb_egids$EGIDS = factor(gb_egids$EGIDS, levels = c("1", "2", "3", "4", "5", "6a", "6b", "7", "8a", "8b", "9"))


## Summarise as endangered or not

table(gb_egids$nochildren, gb_egids$GB020)

gb_egids_summary = gb_egids %>%
  summarise(across(starts_with("GB"), ~ sum(.x == 1, na.rm = TRUE), .by = nochildren) 

gb_egids_summary = t(gb_egids_summary[,-1]) %>% 
  as.data.frame()
colnames(gb_egids_summary) = c("stable", "endangered", "unknown")
gb_egids_summary$vars = rownames(gb_egids_summary)

gb_egdis_long = pivot_longer(gb_egids_summary, cols = c("stable", "endangered", "unknown"))

gb_egdis_long_ss = gb_egdis_long %>% 
  filter(vars %in% c("GB020", "GB021", "GB022"))

ggplot(data = gb_egdis_long, aes(x = value, y = vars, fill = name)) + 
  geom_col() 
