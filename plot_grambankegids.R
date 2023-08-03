## This script plots the difference between languages

library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)


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

# Keep feature where there is more than N languages
n_languages = 250
col_idx = colSums(gb_egids_counts[1:3,2:ncol(gb_egids_counts)]) > n_languages
gb_egids_counts = gb_egids_counts[,c(TRUE, col_idx)]

# Check that worked
x = assertthat::are_equal(names(col_idx)[col_idx], colnames(gb_egids_counts)[-1])
cat("We only analyse the", sum(col_idx), "features that have more than", n_languages, "languages coded")

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
p_count = ggplot(counts_long, aes(x = value, y = name, fill = EGIDS.child)) + 
  geom_col(position = "stack", width = 1) + 
  theme_classic() + 
  ggtitle("Counts of Endangered languages by Grambank feature") + 
  scale_fill_manual(values = c("#00b38a", "#f2ac42", "#ea324c"), na.value = alpha("#ea324c", alpha = 0.70)) + 
  xlab("Proportion") + 
  ylab("Grambank Feature") + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.y = element_text(size = 5)) 
  
p_proportions = ggplot(proportion_long, aes(x = value, y = name, fill = EGIDS.child)) + 
  # geom_bar(stat="identity") + 
  geom_col(position = "stack", width = 1) + 
  theme_classic() + 
  ggtitle("Proportions of Endangered languages by Grambank feature") + 
  scale_fill_manual(values = c("#00b38a", "#f2ac42", "#ea324c"),  na.value = alpha("#ea324c", alpha = 0.70)) + 
  xlab("Proportion") + 
  ylab("Grambank Feature") + 
   theme(legend.position = "bottom",
         legend.title = element_blank(),
         axis.text.y = element_text(size = 5)) 

ggsave(plot = p_count, filename = "figures/count_plot.png", width = 210, height = 290, units = "mm")
ggsave(plot = p_proportions, filename = "figures/proportions_plot.png", width = 210, height = 290, units = "mm")


#### Grambank, EGIDS, and K&G
kg = read.csv('data/journal_archive_data_2021.csv', sep = ";")
gb_egids$studied = ifelse(gb_egids$Name %in% kg$language, "Studied", "Not Studied")

grambank_titles = read.csv("https://raw.githubusercontent.com/glottobank/grambank-cldf/master/cldf/parameters.csv?token=GHSAT0AAAAAACD2VIPHYZ3OXEINBFPQUDKIZEP7TMQ")

grambankfeatures_idx = colnames(gb_egids)[str_detect(colnames(gb_egids), pattern = "GB[0-9]+")]

## Which features have no languages studied?
cat("The following features do not occur in any langauges studied by CAR:\n")
features_zero = names(which(colSums(gb_egids[gb_egids$studied == "Studied",grambankfeatures_idx], na.rm = TRUE) == 0))
cat(features_zero)
cat("These features occur in the following number of languages in Grambank:")
colSums(gb_egids[,features_zero], na.rm = TRUE)

pdf("figures/GB_plots.pdf")
for(i in grambankfeatures_idx){
  
  df = gb_egids[,c(i, "studied")]
  
  groupers = c("studied", i)
  
  plot_df = df %>% 
    group_by_at(groupers) %>% 
    summarise(n = n()) %>%
    mutate(freq = n / sum(n)) 
  
  colnames(plot_df) = c("studied", "variable", "n", "freq")

    
  title = grambank_titles$Name[which(i == grambank_titles$ID)]    
  title = paste0(i, ": ", title)
  
  p = ggplot(plot_df, aes(fill = factor(variable), y = freq, x = factor(studied))) + 
    geom_histogram(stat = "identity", position = "fill") +
    geom_text(aes(label = n), colour = "white", position = position_stack(vjust = 0.5)) + 
    theme_classic(base_size = 20) +
    ylim(c(0, 1)) + 
    ggtitle(title)
  
  print(p)
}
dev.off()
