## This script plots the difference between languages

library(ggplot2)
library(dplyr)
library(tidyr)


gb_egids = read.csv("processed_data/merged_dataset.csv")
all_languages = read.csv('processed_data/language_endangerment.csv')

# make the EGIDS scale a factor
gb_egids$EGIDS = factor(gb_egids$EGIDS, levels = c("1", "2", "3", "4", "5", "6a", "6b", "7", "8a", "8b", "9"))

## Summarise as endangered or not within Grambank
cat("In Grambank, there are", sum(gb_egids$EGIDS.int >= 8, na.rm = TRUE), "languages that have no child speakers (including extinct and symbolically practiced languages) \n",
    "In Grambank, there are", sum(gb_egids$EGIDS.int >= 8 & gb_egids$EGIDS.int <= 11, na.rm = TRUE), "languages that have no child speakers (excluding extinct languages but including symbolically practiced languages)\n",
    "In Grambank, there are", sum(gb_egids$EGIDS.int >= 8 & gb_egids$EGIDS.int <= 10, na.rm = TRUE), "languages that have no child speakers (excluding extinct languages and symbolically practiced languages)\n",
    "In Grambank, there are", sum(gb_egids$EGIDS.int == 7, na.rm = TRUE), "languages that are vulnerable and have limited chlidren speakers.\n",
    "In Grambank, there are", sum(gb_egids$EGIDS.int < 7, na.rm = TRUE), "languages that are stable and available to study in children.\n")

# child speaker variable
gb_egids$EGIDS.child = cut(gb_egids$EGIDS.int, 
                           breaks=c(-Inf, 6.5, 7.5, Inf), 
                           labels=c("stable","few child speakers","no child speakers"))


# Counts of languages in each category
table(gb_egids$EGIDS.child)


# Counts of features by language endangerment
gb_egids_counts = gb_egids %>%
  group_by(EGIDS.child) %>% 
  summarise(across(starts_with("GB"), ~ sum(.x == 1, na.rm = TRUE))) 

counts_long = pivot_longer(gb_egids_counts, cols = starts_with("GB"))

# Counts of languages by feature
hist(colSums(gb_egids_counts[,-1]))

# Count of languages by feature
hist(colSums(gb_egids_counts[,-1]), breaks = 50)

# proportion of features by language endangerment
gb_egids_proportions = sapply(gb_egids_counts[,-1], function(x) x / sum(x))
gb_egids_proportions = data.frame(gb_egids_counts[,1], gb_egids_proportions)
proportion_long = pivot_longer(gb_egids_proportions, cols = starts_with("GB"))

# sort data by the count / proportion of no child speakers
order_idx = unique(counts_long$name[order(counts_long$value[counts_long$EGIDS.child == "no child speakers"])])
counts_long$name = factor(counts_long$name,
                              levels = order_idx)

order_idx = unique(proportion_long$name[order(proportion_long$value[proportion_long$EGIDS.child == "no child speakers"])])
proportion_long$name = factor(proportion_long$name,
                              levels = order_idx)


#### Plot the counts and proportions ####
ggplot(counts_long, aes(x = value, y = name, fill = EGIDS.child)) + 
  geom_bar(stat="identity") + 
  theme_classic() + 
  ggtitle("Counts of Endangered languages by Grambank feature") + 
  theme(legend.position = "bottom") 
  
ggplot(proportion_long, aes(x = value, y = name, fill = EGIDS.child)) + 
  geom_bar(stat="identity") + 
  theme_classic() + 
  ggtitle("Proportions of Endangered languages by Grambank feature") + 
  theme(legend.position = "bottom") 


